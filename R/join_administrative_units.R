join_administries <- function(dt, states, districts, municipalities) {

    join_states(dt, states, "d")
    join_districts(dt, districts, "d")
    join_munis(dt, municipalities, T)

    join_states(dt, states, "o")
    join_districts(dt, districts, "o")
    join_munis(dt, municipalities, F)

    return(NULL)

}


join_states <- function(dt, units, type) {

    stopifnot(type == "o" | type == "d")
    if (type == "d") {
        key <- "EF02U2"
        col <- "state_d"
    } else {
          key <- "EF03U2"
          col <- "state_o"
      }

    do_join(dt, units, key, col)

    return(NULL)
}

join_districts <- function(dt, units, type) {

    stopifnot(type == "o" | type == "d")
    if (type == "d") {
        key <- "EF02U4"
        col <- "district_d"
    } else {
          key <- "EF03U4"
          col <- "district_o"
      }

    do_join(dt, units, key, col)
    
    return(NULL)
}


join_munis <- function(dt, units, dest) {

    unit <- get_unit("mu", dest)
    ags <- get_ags(unit)
    ## print(class(dt))
    ## print(class(units))
    do_join(dt, munis, ags, unit)
    
    return(NULL)
}


do_join <- function(dt, units, key, col) {

    setkeyv(dt, key)
    setkeyv(units, "AGS")

    dt[units, (col) := i.GEN]

}

