##' This function creates a data.table from the Migration Statistics
##' that can be used to draw an arrow plot from. 
##'
##' This function operates on a Migration Statistics data table and
##' needs shapefiles that hold the corresponding geometry.
##' @title Generate data for arrow-plot
##' @param flows data.table of flows between regions. Result from
##'     get_flows()
##' @param shp data.table of shapefile of the same level as flows. So
##'     if "flows" is about flows between states, then the correct
##'     level of shp is also states.
##' @param name character string naming the origin unit
##' @param min_flow Minimum size of flows between origin and
##'     destination for which arrows are drawn.
##' @param ags Sometimes for one name are more than 1 ags key's in
##'     shp. If this happens, get_arrow_data throws an error and asks
##'     the user to specify the ags directly. 
##' @return data.table for drawing arrow plot
##' @export get_arrow_data
##' @author Konstantin
get_arrow_data <- function(flows, shp, name, min_flow = 0, ags = NULL) {

    origin <- place <- . <- flow <- o_region <- NULL
    xend <- yend <- centers <- NULL
    
    cols <- c("destination", "origin", "flow", "distance")
    
    stopifnot("expected columns not found. Please make sure 'flows' is result from get_flows()" = sum(cols == colnames(flows)) == length(cols))
    if (is.null(ags)) {
        ags <- get_ags(shp, name)
        ags <- ags[1]
    } 
    dtarrow <- copy(flows)
    dtarrow <- add_dest(dtarrow)
    dtarrow <- dtarrow[origin == ags, c(1,3,5)]
    colnames(dtarrow)[1] <- "place"
    dtarrow[place == ags, "o_region" := TRUE]
    ##flows <- add_o_row(flows, origin_as_row2(ags))
    dtarrow <- join_geom(dtarrow, shp)

    dtarrow <- arrow_end_points(dtarrow, rm_centers = FALSE)
    dtarrow <- dtarrow[, .(place, flow, o_region, xend, yend, centers)]
    dtarrow <- dtarrow[flow > min_flow | o_region == TRUE]
    return(dtarrow)
    
}

get_ags <- function(shp, name) {
    GEN <- AGS <- NULL
    ags <- shp[GEN == name, AGS]
    if (length(ags) > 1) {
        stop(sprintf("More than one ags found to region: %s. Please specify ags directly.", name))}
    return(ags)
}


add_dest <- function(flows) {
    flows2 <- copy(flows)
    flows2[, "o_region" := FALSE]
    return(flows2)
}

join_geom <- function(flows, shp) {
    i.geometry <- NULL
    
    flows2 <- copy(flows)
    shp2 <- copy(shp)
    setkeyv(flows2, "place") ## maybe bad to set key like that because it changes tables
    setkeyv(shp2, "AGS")
    flows2[shp2, "geom" := i.geometry]
    return(flows2)
}

arrow_end_points <- function(dt, rm_centers = TRUE) {

    origin <- xend <- yend <- o_region <- NULL
    
    ret_el <- function(l, idx) { el <- l[idx]; return(el) }

    dta <- dt
    dta$centers <- sf::st_centroid(dta$geom)
    dta$xend <- sapply(dta$centers, function(x) ret_el(x, 1))
    dta$yend <- sapply(dta$centers, function(x) ret_el(x, 2))
    dta[o_region == TRUE, "xend" := xend + 1]
    dta[o_region == TRUE, "yend" := yend + 1]
    if(rm_centers == TRUE) {
        dta[, "centers" := NULL]
    }
    
    return(dta)
}

##' This function draws an arrow plot that draws arrows from origin to
##' destinations. The size of the arrow corresponds to the size of the flow
##'
##' This function takes the dt from get_arrow_data to draw the base
##' map. Another data.table, dtarrow, is needed to draw the actual
##' arrows. Basically, dtarrow is a subset of dt that only contains
##' the rows where an arrow is to be drawn. This might special
##' destinations that the researcher cares about or only destinations
##' where the flow is of a certain size, to not clutter the plot too
##' much. Also, the origin has to be included in dtarrow.
##' 
##' @title Draw nice arrrow plot
##' @param dt data.table from get_arrow_data. Holds all the flows that
##'     could be drawn and all the geometry information.
##' @param dtarrow Subset of dt that only contains origin and the
##'     destinations where arrows are to be drawn.
##' @return plot
##' @export arrow_plot
##' @author Konstantin
arrow_plot <- function(dt, dtarrow) {

    ### o_idx I can check from the dt, where dest == TRUE, no need to
    ### make it an argument, I think?
    
    centers <- xend <- yend <- place <- flow <- o_region <- NULL
    o_idx <- which(dtarrow[, o_region] == TRUE)
    plot <- ggplot2::ggplot(sf::st_set_geometry(dt, dt$geom)) +
        ggplot2::geom_sf() +
        ggplot2::geom_curve(data = dtarrow,
                   ggplot2::aes(x = centers[[o_idx]][1],
                       y = centers[[o_idx]][2], xend = xend,
                       yend = yend, group = place, size = flow),
                   colour = "red", angle = 0, arrow = ggplot2::arrow())
    return(plot)
}

##' geom_splitflow() adds curved arrows to an arrowplot based on the
##' ratio of the split in the data.
##'
##' geom_splitflow() looks into the data.table for the columns
##' "ratio1" and "ratio2". These are taken to draw a curve based on
##' the same coordinates that arrow_plot() uses. The size of the curve
##' is ratio * flow.
##' @title Add splitted flow arrow
##' @param dtarrow The data.table with the data used for plotting the
##'     curves.
##' @param flow Either 1 or 2. If 1, a curve is drawn that uses column
##'     "ratio1", if 2, geom_splitflow() uses "ratio2" to determine
##'     the size of the flow. Colour and curvature are also based on
##'     flow.
##' @return ggplot object
##' @export geom_splitflow
##' @author Konstantin
geom_splitflow <- function(dtarrow, flow) {

    centers <- xend <- yend <- NULL
    
    o_idx <- which(dtarrow$o_region == TRUE)
    m <- ifelse(flow == 1, "ratio1", "ratio2")
    c <- ifelse(flow == 1, 0.2, - 0.2)
    colour <- ifelse(flow == 1, "blue", "green")
    dtplot <- copy(dtarrow)
    dtplot[, "flow" := get(m) * flow]
    dtplot[, flow]
    ggplot2::geom_curve(data = dtplot,
               ggplot2::aes(x = centers[[o_idx]][1], y = centers[[o_idx]][2],
                   xend = xend, yend = yend, size = flow),
               colour = colour, curvature = c, arrow = ggplot2::arrow()) 

}

##' add_ratios() takes a vector of ratios and adds the vector and 1
##' vector to a data.table. 
##'
##' add_ratios() takes a vector of ratios and adds the vector and 1
##' vector to a data.table. The resulting columns are named ratio1 and
##' ratio2. These columns can be used by geom_splitflow() to split the
##' flow and add the splitted flows to the arrow_plot. The all entries
##' of the vector of ratios are supposed to be in the \[0, 1 \], or to
##' be NA.
##' @title add ratios of splitted flows to data.table()
##' @param dt data.table where the ratios are added
##' @param ratios numeric vector of ratios, can have NA's in it.
##' @return data.table that is the same as dt but with two columns
##'     added: ratio1 and ratio2.
##' @export add_ratios
##' @author Konstantin
add_ratios <- function(dt, ratios) {

    stopifnot("vector of ratios must be of same length as there are rows in data.table"
    = length(ratios) == nrow(dt))
    r <- stats::na.omit(ratios)
    stopifnot("all entries of ratios must be either NA or numeric and, if numeric,  in [0,1]" = sum(c(r > 1),
    c(r < 0)) == 0)
    
    dtr <- copy(dt)
    dtr[, "ratio1" := NA]
    dtr[, "ratio2" := NA]
    dtr[, "ratio1" := ratios]
    dtr[, "ratio2" := 1 - ratios]

    return(dtr)
    
}




