##' computes the difference of in-migration and out-migration for all
##' regions in the od-flow data. Wins and losses are returned as well.
##'
##' The function makes sure that regions where no people in-migrated
##' or out-migrated are handled approbpriately. That is, missing
##' in-migration or out-migration is treated as 0.
##' @title net migration for regions
##' @param flows data.table holding od-flows. Returned by get_flows().
##' @param by optional. If output is to be grouped, here the grouping
##'     variables are given. If NULL, all columns except origin,
##'     destination, distance and flow are taken as grouping variables
##' @return data.table with ags, net flow and wins and losses.
##' @import data.table
##' @export get_net
##' @author Konstantin
get_net <- function(flows, by = NULL) {
    ### probably a good idea to supply "values" only optional and
    ### otherwise read them from the columns. Although then there
    ### might be some combinations missing that are not in the data

    ### not working right now when grouped = FALSE, because it still
    ### expects values

    ### maybe grouped is not really necessary because values already
    ### gives the grouping information
    i.losses <- NULL
    
    wins <- get_wins(flows, by)
##    wins <- include_missing_obs(wins, values, "wins")
    losses <- get_losses(flows, by)
    ## losses <- include_missing_obs(losses, values, "losses")
    ### in wins and losses the columns are not called "origin" and
    ### "destination" anymore but region. Still I need the other names
    ### of values, because they contain the grouping variables. What
    ### if no values are supplied? But then I can not join anyways
### because observations are not balanced.
    if (is.null(by)) {
        ### this removes wins[, "wins"] and losses[, "losses"]
        keys <- intersect(colnames(wins), colnames(losses))
    } else {
        keys <- c("region", by)
    }
    setkeyv(wins, keys)
    setkeyv(losses, keys)
    wins[losses, "losses" := i.losses]
    wins[, "net" := wins - losses]
    return(wins)
}


## get_net <- function(flows) {
##     flow <- region <- i.flow <- i.region <- NULL
##     w <- get_wins(flows)
##     l <- get_losses(flows)
##     keys <- unique(c(w[, region], l[, region]))
##     data.table::setkeyv(w, "region")
##     data.table::setkeyv(l, "region")
##     w <- w[keys, ]
##     w[is.na(wins), "wins" := 0]
##     l <- l[keys, ]
##     l[is.na(losses), "losses" := 0]
##     data.table::setkeyv(w, "region")
##     data.table::setkeyv(l, "region")
##     w[l, "losses" := i.losses]
##     w[, "net" := wins - losses]
##     setcolorder(w, c("region", "net", "wins", "losses"))
##     ### the next lines ensure that if there are regions in losses that
##     ### are not in wins, the name of the region is transferred to wins
##     row <- w[is.na(region)][l, "region" := i.region]
##     idx <- which(is.na(w[, region]))
##     w[idx] <- row
##     return(w)
## }


get_wins <- function(flows, by = NULL) {
    wins <- get_grouped(flows, reg = "destination", by = by)
    return(wins)
}


get_losses <- function(flows, by = NULL) {
    losses <- get_grouped(flows, reg = "origin", by = by)
    return(losses)
}

get_grouped <- function(flows, reg, by = NULL) {
    flow <- NULL
    
    stopifnot(reg %in% c("origin", "destination"))
    if(reg == "origin") {
        type <- "losses"
    } else {
        type  <- "wins"
    }
    dt <- copy(flows)
    if (is.null(by)) {
        nogroup <- c("origin", "destination", "flow", "distance", "region")
        by <- setdiff(colnames(flows), nogroup)
    }
    if (!is.null(by)) {
        message(sprintf("%s grouped by '%s'", type, paste(by, collapse = ", ")))
    }
    dt <- dt[, paste(type) := sum(flow), by = c(reg, by)]
    dt <- dt[, .SD[1], by = c(reg, by), .SDcols = c(type)]
    colnames(dt)[colnames(dt) %in% c("origin", "destination")]  <- "region"
    
    return(dt)
}

include_missing_obs <- function(dt, values, missing_col) {
    mc <- missing_col
    stopifnot("missing_col not in data.table" = mc %in% colnames(dt))
    keys <- do.call(data.table::CJ, values)
    setkeyv(dt, names(values))
    dtfull <- dt[keys, ]
    dtfull <- dtfull[order(mget(names(values)))]
    dtfull <- dtfull[is.na(get(mc)), paste(mc)  := 0]
    return(dtfull)
}

## ##' Get wins (in-migration) for regions
## ##'
## ##' This function returns the number of in-migrations for every region
## ##' of the specified type in the data.table.
## ##' @title Get wins per region
## ##' @param dt data.table
## ##' @param us "unit simple". One of the following strings: "st"
## ##'     (federal_states), "di" (districts) or "mu" (municipalities).
## ##' @return data.table with columns: unit, ags, flow
## ##' @export get_wins
## ##' @author Konstantin
## get_wins <- function(dt, us) {

##     wins <- get_flow(dt, us, T)

##     return(wins)
## }


## ##' Get losses (out-migration) for regions
## ##'
## ##' This function returns the number of out-migrations for every region
## ##' of the specified type in the data.table.
## ##' @title Get losses per region
## ##' @param dt data.table
## ##' @param us "unit simple". One of the following strings: "st"
## ##'     (federal_states), "di" (districts) or "mu" (municipalities).
## ##' @return data.table with columns: unit, ags, flow
## ##' @export get_losses
## ##' @author Konstantin
## get_losses <- function(dt, us) {

##     losses <- get_flow(dt, us, F)

##     return(losses)    
## }



## get_flow <- function(dt, us, dest) {

## ### us unit short: abbreviation for administrative unit, either "st"
## ### for state, "di" for district or "mu" for municipality. If dest =
## ### T, then destinations are considered, that is: wins. If dest = F,
## ### then origins are considered, that is, losses
    
##     unit <- get_unitcol(us, dest)
##     ags <- get_agscol(unit)
##     dtf <- dt
##     dtf[, "flow" := .N, by = ags]
##     flows <- dtf[, .SD[1], by = ags]
##     flows <- flows[, .SD, .SDcols = c(unit, ags, "flow")]
##     dt[, "flow" := NULL]
##     colnames(flows)[1] <- "region"
##     colnames(flows)[2] <- "ags"
##     return(flows)
## }


## ##' In-migration, out-migration or net-migration can be viwed as an
## ##' attribute of regions. join_to_shp() takes a data.table and joins
## ##' it to the data.table holding the shape information.
## ##'
## ##' @title join wins, losses or net migration to data.table of
## ##'     shapefile
## ##' @param shp data.table holding the shape information. Expected to
## ##'     have column "AGS" that is used to join.
## ##' @param dt data.table holding the flows (either wins, losses, or
## ##'     net). Might be output from get_wins(), get_losses() or
## ##'     get_net().
## ##' @param col Name of new column in returned shp data.table.
## ##' @param key_dt Name of column in dt that holds the AGS.
## ##' @return shp data.table with joined column
## ##' @import
## ##' data.table
## ##' @export
## ##' join_to_shp
## ##' @author Konstantin
## join_to_shp <- function(shp, dt, col, key_dt = "ags") {
##     i.flow <- NULL
##     shpj <- data.table::copy(shp)
##     data.table::setkeyv(shpj, "AGS")
##     data.table::setkeyv(dt, key_dt)
##     shpj[dt, (col) := i.flow]
##     return(shpj)
## }


