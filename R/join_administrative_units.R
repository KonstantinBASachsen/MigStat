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
##' @return NULL, updates dt instead
##' @import data.table
##' @export join_administries
##' @author Konstantin
join_administries <- function(dt, states, districts, municipalities) {

    join_states(dt, states, T)
    join_districts(dt, districts, T)
    join_munis(dt, municipalities, T)

    join_states(dt, states, F)
    join_districts(dt, districts, F)
    join_munis(dt, municipalities, F)

    return(NULL)

}


join_states <- function(dt, units, dest) {

    unit <- get_unit("st", dest)
    ags <- get_ags(unit)
    
    do_join(dt, units, ags, unit)

    return(NULL)
}


join_districts <- function(dt, units, dest) {

    unit <- get_unit("di", dest)
    ags <- get_ags(unit)

    do_join(dt, units, ags, unit)
    
    return(NULL)
}


join_munis <- function(dt, units, dest) {

    unit <- get_unit("mu", dest)
    ags <- get_ags(unit)

    do_join(dt, units, ags, unit)
    
    return(NULL)
}


do_join <- function(dt, units, key, col) {
    ## performs full join
    i.GEN <- NULL
    setkeyv(dt, key)
    setkeyv(units, "AGS")
    unique_keys <- unique(c(dt[, get(key)], units[, AGS]))
    dtu <- dt[unique_keys]
    setkeyv(dtu, key)
    dtu[units, (col) := i.GEN]

    return(dtu)

}

