##' Joins names of administrative units to migration statistics.
##'
##' This function uses the columns in the migration statistics that
##' hold the keys (e.g. ags) and uses this to join the corresponding
##' names from data.tables.
##' @title join unit names
##' @param dt data.table migration statistics
##' @param states data.table federal states
##' @param districts data.table districts
##' @param municipalities data.table municipalities
##' @param full If true, a full join is performed. This means that all
##'     unit names are joined to the data.table, even if no flow is
##'     present for those units. This is useful for plotting.
##' @return joined data.table with names of administrative regions
##' @import data.table
##' @export join_administries
##' @author Konstantin
join_administries <- function(dt, states, districts, municipalities, full) {

    dtj <- dt
    dtj <- join_states(dtj, states, full)
    dtj <- join_districts(dtj, districts, full)
    dtj <- join_munis(dtj, municipalities, full)

    return(dtj)
}


join_states <- function(dt, to_join, full) {

    dtj <- join_units(dt, us = "st", to_join = to_join, dest = TRUE, full)
    dtj <- join_units(dtj, us = "st", to_join = to_join, dest = FALSE, full)

    return(dtj)
}


join_districts <- function(dt, to_join, full) {

    dtj <- join_units(dt, us = "di", to_join = to_join, dest = TRUE, full)
    dtj <- join_units(dtj, us = "di", to_join = to_join, dest = FALSE, full)

    return(dtj)
}


join_munis <- function(dt, to_join, full) {

    dtj <- join_units(dt, us = "mu", to_join = to_join, dest = TRUE, full)
    dtj <- join_units(dtj, us = "mu", to_join = to_join, dest = FALSE, full)
    
    return(dtj)
}

join_units <- function(dt, us, to_join, dest, full) {

    unit <- get_unitcol(us, dest)
    ags <- get_agscol(unit)

    dtj <- do_join(dt, to_join, ags, unit, full)
    
    return(dtj)

}


##' Joins geometry or name of regions from shapefile to data table
##'
##' When working with the Migration Statistics it is often necessary
##' to join the geometry for plotting. Also joining official names of
##' regions is helpful. The joining is done based on the AGS.
##'
##' The join is either a left join or a full join, depending on
##' full. Even if full = FALSE all keys in dt are returned. Even if
##' the keys are not in shp.
##' @title Join shapefile to data table
##' @param dt Data table to join name or geometry of regions to.
##' @param shp data table of shapefile
##' @param key1 Name of column in dt that stores the AGS.
##' @param key2 Name of column in shp that stores the AGS.
##' @param col Name of new column in dt that is created by joining
##'     name or geometry of region
##' @param type Either "n" which joins names of regions or
##'     "g" which joins geometry information
##' @param full logical. If TRUE a full join is performed. This means
##'     if there are keys in shp that are not in dt, additional rows
##'     are created.
##' @return data table with new column
##' @import data.table
##' @export do_join
##' @author Konstantin
do_join <- function(dt, shp, type, col, key1, key2 = "AGS", full = FALSE) {
    if(! type %in% c("n", "g")) {
        stop("Type either n for name or g for geometry")
    }
    ## performs full join
    i.GEN <- AGS <- i.geometry <- NULL
    if(full == TRUE) {
        setkeyv(dt, key)
        unique_keys <- unique(c(dt[, get(key1)], shp[, get(key2)]))
        dtu <- dt[unique_keys]
    } else {
        dtu <- dt
    }
    setkeyv(dtu, key1)
    setkeyv(shp, key2)
    if (type == "n") {
        dtu[shp, (col) := i.GEN]
    }
    if (type == "g") {
        dtu[shp, (col) := i.geometry]
    }
    return(dtu)

}

