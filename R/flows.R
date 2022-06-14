
get_wins <- function(dt, us) {

    wins <- get_flow(dt, us, T)

    return(wins)
}


get_losses <- function(dt, us) {

    losses <- get_flow(dt, us, F)

    return(losses)
    
}

get_flow <- function(dt, us, dest) {

### us unit short: abbreviation for administrative unit, either "st"
### for state, "di" for district or "mu" for municipality. If dest =
### T, then destinations are considered, that is: wins. If dest = F,
### then origins are considered, that is, losses
    
    unit <- get_unit(us, dest)
    ags <- get_ags(unit)
    dt[, "flow" := .N, by = unit]
    flows <- dt[, .SD[1], by = unit]
    flows <- flows[, .SD, .SDcols = c(unit, ags, "flow")]
    dt[, "flow" := NULL]
    
    return(flows)

}
