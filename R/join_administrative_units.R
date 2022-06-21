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
##' @return joined data.table with names of administrative regions
##' @import data.table
##' @export join_administries
##' @author Konstantin
join_administries <- function(dt, states, districts, municipalities) {

    dtj <- dt
    dtj <- join_states(dtj, states, T)
    dtj <- join_districts(dtj, districts, T)
    dtj <- join_munis(dtj, municipalities, T)

    dtj <- join_states(dtj, states, F)
    dtj <- join_districts(dtj, districts, F)
    dtj <- join_munis(dtj, municipalities, F)

    return(dtj)

}


join_states <- function(dt, units, dest) {

    unit <- get_unit("st", dest)
    ags <- get_ags(unit)
    
    dtj <- do_join(dt, units, ags, unit)

    return(dtj)
}


join_districts <- function(dt, units, dest) {

    unit <- get_unit("di", dest)
    ags <- get_ags(unit)

    dtj <- do_join(dt, units, ags, unit)
    
    return(dtj)
}


join_munis <- function(dt, units, dest) {

    unit <- get_unit("mu", dest)
    ags <- get_ags(unit)

    dtj <- do_join(dt, units, ags, unit)
    
    return(dtj)
}


do_join <- function(dt, units, key, col) {
    ## performs full join
    i.GEN <- AGS <- NULL
    setkeyv(dt, key)
    setkeyv(units, "AGS")
    unique_keys <- unique(c(dt[, get(key)], units[, AGS]))
    dtu <- dt[unique_keys]
    setkeyv(dtu, key)
    dtu[units, (col) := i.GEN]

    return(dtu)

}

