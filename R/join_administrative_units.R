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

    sexony:::join_states(dt, states, T)
    sexony:::join_districts(dt, districts, T)
    sexony:::join_munis(dt, municipalities, T)

    sexony:::join_states(dt, states, F)
    sexony:::join_districts(dt, districts, F)
    sexony:::join_munis(dt, municipalities, F)

    return(NULL)

}


join_states <- function(dt, units, dest) {

    unit <- sexony:::get_unit("st", dest)
    ags <- sexony:::get_ags(unit)
    
    sexony:::do_join(dt, units, ags, unit)

    return(NULL)
}

join_districts <- function(dt, units, dest) {

    unit <- sexony:::get_unit("di", dest)
    ags <- sexony:::get_ags(unit)

    sexony:::do_join(dt, units, ags, unit)
    
    return(NULL)
}


join_munis <- function(dt, units, dest) {

    unit <- sexony:::get_unit("mu", dest)
    ags <- sexony:::get_ags(unit)

    sexony:::do_join(dt, munis, ags, unit)
    
    return(NULL)
}

do_join <- function(dt, units, key, col) {

    setkeyv(dt, key)
    setkeyv(units, "AGS")

    dt[units, (col) := i.GEN]

}

