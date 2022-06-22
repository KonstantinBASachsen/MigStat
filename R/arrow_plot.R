
where_to <- function(dt, o_col, o_name, d_col) {

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

    ags <- dt[, ..o_ags][[1]][1]
    new_row <- t(c(NA, NA, name, ags, NA))
    colnames(new_row) <- colnames(dtf)
    dtr <- rbind(dt,  new_row)

    return(dtr)
}

add_destinations_with_0_flows <- function(dt, d_us, units) {

### maybe I can omit this and later perform a full join that joins the
### missing unit names and geometries
    
    unit <- get_unit(us = d_us, dest = TRUE)
    unit <- strsplit(unit, "_")[[1]][1]
    dtj <- join_units(dtf, d_us, shps[[unit]], dest = TRUE, full = TRUE)

    return(dtj)
}

simplify_dt <- function(dt) {

    dts <- dt[, 3:5]
    colnames(dts) <- c("place", "key", "flow")

    return(dts)
}

add_dest <- function(dt, ags) {

    dtd <- dt[, "dest" := fifelse(key == ags, TRUE, FALSE)]

    return(dtd)
}

join_geom <- function(dt, units, o_us, d_us) {

    ### add test with sum(sapply(dtf$geom, is.null))
    o_unit <- get_unit(us = o_us, dest = FALSE)
    o_unit <- strsplit(o_unit, "_")[[1]][1]
    d_unit <- get_unit(us = d_us, dest = TRUE)
    d_unit <- strsplit(d_unit, "_")[[1]][1]
   
    dtj <- dt[shps[[d_unit]][GF == 4, ], "geom" := i.geometry, on = .(key = AGS)]
    dtj <- dtj[shps[[o_unit]][GF == 4, ], "geom" := i.geometry, on = .(key = AGS)]

    return(dtj)
}

arrow_end_points <- function(dt, rm_centers = TRUE) {

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

    dtf <- data.table::copy(dt)
    dtf <- dtf[is.na(flow), "flow" := 0]

    return(dtf)
}
