##' Get wins (in-migration) for regions
##'
##' This function returns the number of in-migrations for every region
##' of the specified type in the data.table.
##' @title Get wins per region
##' @param dt data.table
##' @param us "unit simple". One of the following strings: "st"
##'     (federal_states), "di" (districts) or "mu" (municipalities).
##' @return data.table with columns: unit, ags, flow
##' @export get_wins
##' @author Konstantin
get_wins <- function(dt, us) {

    wins <- get_flow(dt, us, T)

    return(wins)
}


##' Get losses (out-migration) for regions
##'
##' This function returns the number of out-migrations for every region
##' of the specified type in the data.table.
##' @title Get losses per region
##' @param dt data.table
##' @param us "unit simple". One of the following strings: "st"
##'     (federal_states), "di" (districts) or "mu" (municipalities).
##' @return data.table with columns: unit, ags, flow
##' @export get_losses
##' @author Konstantin
get_losses <- function(dt, us) {

    losses <- get_flow(dt, us, F)

    return(losses)    
}


get_flow <- function(dt, us, dest) {

### us unit short: abbreviation for administrative unit, either "st"
### for state, "di" for district or "mu" for municipality. If dest =
### T, then destinations are considered, that is: wins. If dest = F,
### then origins are considered, that is, losses
    
    unit <- get_unitcol(us, dest)
    ags <- get_agscol(unit)
    dtf <- dt
    dtf[, "flow" := .N, by = ags]
    flows <- dtf[, .SD[1], by = ags]
    flows <- flows[, .SD, .SDcols = c(unit, ags, "flow")]
    dt[, "flow" := NULL]
    
    return(flows)
}


## join_flows <- function(shapes, flows, key, unit) {
##     ### do I still need this function? I wrote it because I wanted to
##     ### retain all the regions with no flows for plotting but
##     ### join_administries now does a full join so all regions with no
##     ### flows are kept
##     i.flow <- GF <- NULL
##     ## unit <- colnames(flows)[unit_pos]
##     ## ags <- get_agscol(unit)
##     flows <- flows[stats::complete.cases(flows)]
## ##    unit <- strsplit(unit, "_")[[1]][1]
##     shape <- shapes[[unit]]
##     data.table::setkeyv(flows, key)
##     data.table::setkeyv(shape, "AGS")
 
##     shape <- shape[flows, "flows" := i.flow]
##     shape <- shape[GF == 4, ] ### other numbers hold differnt
##                               ### geometries, like without water I
##                               ### think I read in some documentation.

##     return(shape)    
## }


