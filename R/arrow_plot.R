
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

add_dest <- function(dt, ags) {

    flow <- NULL
    
    dtd <- dt[, "dest" := fifelse(key == ags & !is.na(flow), TRUE, FALSE)]
    ## NA on flow is checked because currently
    ## add_destination_with_0_flows adds a
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

    dest <- xend <- yend <- NULL
    
    ret_el <- function(l, idx) { el <- l[idx]; return(el) }

    dta <- dt
    dta$centers <- sf::st_centroid(dta$geom)
    dta$xend <- sapply(dta$centers, function(x) ret_el(x, 1))
    dta$yend <- sapply(dta$centers, function(x) ret_el(x, 2))
    dta[dest == TRUE, "xend" := xend + 1]
    dta[dest == TRUE, "yend" := yend + 1]
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

arrow_plot <- function(dt, o_idx, dtarrow) {

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
