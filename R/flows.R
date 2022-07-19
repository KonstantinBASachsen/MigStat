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


##' Computes flows between given regions.
##'
##' Computes all flows between given regions (bivariate
##' flows). Currently only supportes flows between regions of the same
##' type.
##' @title Flows between regions
##' @param dt data.table
##' @param us unit simple, one of the following: c("st", "di", "mu")
##' @param simplify If true a simplified data.table is return with the
##'     following columns only: ags (allgemeiner Gemeindeschlüssel,
##'     unique numeric identifier of region) of origin and
##'     destination, unit of origin and destination and flow.
##' @return data.table
##' @import data.table
##' @export get_flows
##' @author Konstantin
get_flows <- function(dt, us, simplify = TRUE) {

    unit_o <- get_unitcol(us, FALSE)
    unit_d <- get_unitcol(us, TRUE)
    ags_o <- get_agscol(unit_o)
    ags_d <- get_agscol(unit_d)
    dtf <- dt
    dtf[, "flow" := .N, by = c(ags_o, ags_d)]
    if (simplify == TRUE) {
        dtf <- dtf[, .SD, .SDcols = c(ags_o, ags_d, unit_o, unit_d, "flow")]
        dtf <- dtf[, .SD[1], by = c(ags_o, ags_d)]
    }
    return(dtf)
}
