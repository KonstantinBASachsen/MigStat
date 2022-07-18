##' This function creates a data.table from the Migration Statistics
##' that can be used to draw an arrow plot from. 
##'
##' This function operates on a Migration Statistics data table and
##' needs shapefiles that hold the corresponding geometry.
##' @title Generate data for arrow-plot
##' @param dt Migration Statistics data.table
##' @param shapes list of three data.tables. Each table holds the
##'     shapefiles of one of the following levels: states, districts,
##'     municipalities.
##' @param name character string naming the origin unit 
##' @param o_us level of origin unit, either st, di or mu
##' @param d_us level of destination units, either st, di or mu
##' @return data.table for drawing arrow-plot
##' @export get_arrow_data
##' @author Konstantin
get_arrow_data <- function(dt, shapes, name, o_us, d_us) {

    ## Wrapper for all the functions below that are used to create a
    ## suitable data.table to draw an arrow plot from
    
    ## check if name is found would be nice

    ..o_ags <- flow <- NULL
    
    o_col <- get_unit(o_us, dest = FALSE) ## from R/utils.R
    d_col <- get_unit(d_us, dest = TRUE) ## from R/utils.R
    o_ags <- get_ags(o_col)
    d_ags <- get_ags(d_col)

    stopifnot("region name is not found in data, check spelling and of o_us refers to the right regions"
    = name %in% unique(dt[, ..o_col][[1]]))
    
    ags <- dt[get(o_col) == name, ..o_ags][[1]][1]

    dtf <- where_to(dt, o_col, name, d_col = d_col)
    dtf <- origin_as_row(dtf, o_ags, name)
    dtf <- add_destinations_with_0_flows(dtf, d_us, shapes)
    dtf <- simplify_dt(dtf)
    dtf <- add_origin(dtf, ags)
    dtf <- join_geom(dtf, shapes, o_us, d_us)
    dtf <- arrow_end_points(dtf, rm_centers = FALSE)
    dtf <- na_flows_to_0(dtf)

    dtf[, "flow" := as.numeric(flow)]

    return(dtf)

}

where_to <- function(dt, o_col, o_name, d_col) {

    flow <- NULL
    
    o_ags <- get_ags(o_col)
    d_ags <- get_ags(d_col)
    dtf <- dt[, .SD, .SDcols = c(o_col, o_ags, d_col, d_ags)]
    dtf <- dtf[get(o_col) == o_name, "flow" := .N, by = d_col]
    dtf <- dtf[!is.na(flow), ]
    dtf <- dtf[, .SD[1], by = c(o_col, o_ags, d_col, d_ags)]
    return(dtf)
}

origin_as_row <- function(dt, o_ags, name) {

### This function is necessary because the origin can not necessarily
### be found among the destinations. This is because it might be of
### different type. For example if we consider moves from one
### municipality to all states
###
### It adds a new row with the name of the origin to the names of the
### destination and the ags of origin to the ags of destination. Seems
### a little weird but then one column can be used to join names or
### geoms
    ..o_ags <- NULL
    ags <- dt[, ..o_ags][[1]][1]
    new_row <- t(c(NA, NA, name, ags, NA))
    colnames(new_row) <- colnames(dt)
    dtr <- rbind(dt,  new_row)

    return(dtr)
}

add_destinations_with_0_flows <- function(dt, d_us, units) {

### maybe I can omit this and later perform a full join that joins the
### missing unit names and geometries
    
    unit <- get_unit(us = d_us, dest = TRUE)
    unit <- strsplit(unit, "_")[[1]][1]
    dtj <- join_units(dt, d_us, units[[unit]], dest = TRUE, full = TRUE)

    return(dtj)
}

simplify_dt <- function(dt) {

    dts <- dt[, 3:5]
    colnames(dts) <- c("place", "key", "flow")

    return(dts)
}

add_origin <- function(dt, ags) {

    flow <- NULL

    ## We add origin to the same column as destination, this we then
    ## call "key". If origin is also a destination, that is, some
    ## people who move from Saxony move to Saxony, the region appears
    ## twice. Once as destination, once as origin. In this case we
    ## want to add origin == TRUE to the row which is the origin, that
    ## is, flow == NA.

    ## If on the other hand origin is not a destination, there is only
    ## one row and we can set dest == TRUE

    if (nrow(dt[key == ags]) > 1) {
        dtd <- dt[, "origin" := fifelse(key == ags & !is.na(flow), TRUE, FALSE)]
    } else {
        dtd <- dt[, "origin" := fifelse(key == ags, TRUE, FALSE)]
    }
    

    return(dtd)
}

join_geom <- function(dt, units, o_us, d_us) {

    GF <- i.geometry <- . <- AGS <- NULL
    ### add test with sum(sapply(dtf$geom, is.null))
    o_unit <- get_unit(us = o_us, dest = FALSE)
    o_unit <- strsplit(o_unit, "_")[[1]][1]
    d_unit <- get_unit(us = d_us, dest = TRUE)
    d_unit <- strsplit(d_unit, "_")[[1]][1]
   
    dtj <- dt[units[[d_unit]][GF == 4, ], "geom" := i.geometry, on = .(key = AGS)]
    dtj <- dtj[units[[o_unit]][GF == 4, ], "geom" := i.geometry, on = .(key = AGS)]

    return(dtj)
}

arrow_end_points <- function(dt, rm_centers = TRUE) {

    origin <- xend <- yend <- NULL
    
    ret_el <- function(l, idx) { el <- l[idx]; return(el) }

    dta <- dt
    dta$centers <- sf::st_centroid(dta$geom)
    dta$xend <- sapply(dta$centers, function(x) ret_el(x, 1))
    dta$yend <- sapply(dta$centers, function(x) ret_el(x, 2))
    dta[origin == TRUE, "xend" := xend + 1]
    dta[origin == TRUE, "yend" := yend + 1]
    if(rm_centers == TRUE) {
        dta[, "centers" := NULL]
    }
    
    return(dta)
}


na_flows_to_0 <- function(dt) {

    flow <- NULL
    
    dtf <- data.table::copy(dt)
    dtf <- dtf[is.na(flow), "flow" := 0]

    return(dtf)
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
##' @param o_idx row number of origin region
##' @param dtarrow Subset of dt that only contains origin and the
##'     destinations where arrows are to be drawn.
##' @return plot
##' @export arrow_plot
##' @author Konstantin
arrow_plot <- function(dt, o_idx, dtarrow) {

    ### o_idx I can check from the dt, where dest == TRUE, no need to
    ### make it an argument, I think?
    
    centers <- xend <- yend <- place <- flow <- NULL
    
    plot <- ggplot2::ggplot(sf::st_set_geometry(dt, dt$geom)) +
        ggplot2::geom_sf() +
        ggplot2::geom_curve(data = dtarrow,
                   ggplot2::aes(x = centers[[o_idx]][1],
                       y = centers[[o_idx]][2], xend = xend,
                       yend = yend, group = place, size = flow),
                   colour = "red", angle = 0, arrow = ggplot2::arrow())
    return(plot)
}
