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

##' Helper function that performs the join of names of administrative
##' units to migration statistics.
##' 
##' @title join names of administrative units from shapefile
##' @param dt migration statistics data.table
##' @param units data.table administrative units
##' @param key key column in migration statistics data.table
##' @param col column name of new column in migratin statistics. Holds
##'     the joined administrative units.
##' @import data.table
##' @return NULL, dt is updated
##' @author Konstantin
do_join <- function(dt, units, key, col) {

    setkeyv(dt, key)
    setkeyv(units, "AGS")

    dt[units, (col) := i.GEN]

}

