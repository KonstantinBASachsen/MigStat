##' Retunes a common data.table of wins and losses and returnes the
##' net migration (wins - losses). Only designed to handle wins and
##' losses with respect to federal states.
##'
##' @title net migration of wins and losses of regions with respect to
##'     federal states.
##' @param wins data.table wih "wins" (number of moves into a region)
##'     per regions
##' @param losses data.table wih "losses" (number of moves out of a
##'     region) per regions
##' @param states data.table with columns AGS and GEN of federal
##'     states
##' @param ags_gen data.table with columns AGS and GEN of our
##'     interesting regions. This is neccessary to recognize that
##'     different ags might belong to the same region.
##' @return data.table with names of regions and their wins, losses
##'     and net migration with respect to federal states.
##' @author Konstantin
get_net2 <- function(wins, losses, states, ags_gen) {
    ## Erstellt gemeinsame data.table aus wins und losses und berechnet
    ## die Netto Migration
    i.GEN <- . <- AGS <- age_gr <- state <- name_r <- NULL
    keys_w <- colnames(wins)[! colnames(wins) %in% c("flow", "wins")]
    keys_l <- colnames(losses)[!colnames(losses) %in% c("flow", "losses")]
    net <- merge(wins, losses, by.x = keys_w, by.y = keys_l, all = TRUE)
    colnames(net)[colnames(net) == "flow.x"] <- "wins"
    colnames(net)[colnames(net) == "flow.y"] <- "losses"
    colnames(net)[colnames(net) == "destination"] <- "region"
    colnames(net)[colnames(net) == "EF03U2"] <- "state"
    net[ags_gen, "name_r" := i.GEN, on = .(region = AGS)]
    net[states, "name_bl" := i.GEN, on = .(state = AGS)]
    ## the following line makes sure that regions with different ags
    ## are treated as one. In this case the number of groups/ rows
    ## might be reduced. Might because if year is taken as grouping
    ## variable than different ags for the same region correspond to
    ## different years and because year is a group it does not reduce
    ## the overall number of groups.
    net <- net[, "wins" := sum(wins), by = .(age_gr, state, name_r, year)]
    net <- net[, "losses" := sum(losses), by = .(age_gr, state, name_r, year)]
    net <- net[, .SD[1], by = .(age_gr, state, name_r, year)]
    net[is.na(wins), "wins" := 0]
    net[is.na(losses), "losses" := 0]
    net[, "net" := wins - losses]
    ## net <- net[, "net" := sum(net), by = .(age_gr, state, name_r, year)]
    ## net <- net[, .SD[1], by = .(age_gr, state, name_r, year)]
### alternatively????
##    net <- net[, .(net = sum(net))]
    return(net)
}

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
    if (reg == "origin") {
        dt <- dt[, .("losses" = sum(flow)), keyby = c(reg, by)]
    }
    if (reg == "destination") {
        dt <- dt[, .("wins" = sum(flow)), keyby = c(reg, by)]
    }
    colnames(dt)[colnames(dt) %in% c("origin", "destination")]  <- "region"
    return(dt)
}

