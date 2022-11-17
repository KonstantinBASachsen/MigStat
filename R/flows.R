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

    ### 15.11 missing flows to 0 not working. Maybe only if
    ### get_flows() already fills missing combinations? Seems bad to
    ### do it in get_flows() to many rows, not often needed. Better do
    ### it here
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

