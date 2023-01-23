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

    dtj <- do_join(dt = dt, shp = to_join, type = "n", col = unit, key1 = ags, full = full)
    
    return(dtj)

}


