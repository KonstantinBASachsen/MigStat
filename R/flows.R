##' Computes net migration, wins and losses from flows object
##'
##' This function expects output from get_flows() and returns the net
##' migration, wins and losses for all origin and destination regions
##' in 'flows'.
##'
##' The grouping variables of 'flows' are retained. That is, if the
##' origin-destination flows from get_flows() are grouped by age and
##' gender, the net migration, wins and losses are grouped by age and
##' gender as well.
##'
##' get_net() takes care to balance the observations. For example if
##' there is only in-migration to Saxony but no out-migration a 0 is
##' added for losses and the net migration is simply the wins.
##'
##' But region or group combinations that are not in the data at all
##' are not added. That is, if net migration for federal states is
##' desired and there are no migrations to or from Saxony, Saxony will
##' not be listed in the output. This is likely to change in some
##' future version.
##'
##' @title Net migration, wins and losses.
##' @param flows output from get_flows() or data.table with at least
##'     two columns: origin and destination
##' @return data.table with net-migration, wins and losses for all
##'     regions and groups.
##' @import data.table
##' @export get_net
##' @author Konstantin
##'
get_net <- function(mig, us = c("st", "di", "mu"), by = NULL,
                    fill = c("none", "group", "all"), values) {
    us <- match.arg(us)
    fill <- match.arg(fill)
    wins <- get_flows(mig, us_d = us, by = by, fill = fill,
                      values = values)
    keyw <- colnames(wins)[colnames(wins) != "wins"]
    losses <- get_flows(mig, us_o = us, by = by, fill = fill,
                        values = values)
    keyl <- colnames(losses)[colnames(losses) != "losses"]

    net <- do_join(dt1 = wins, dt2 = losses, join_col = "losses",
                   key1 = keyw, key2 = keyl, all = TRUE)
    net[is.na(wins), "wins" := 0]
    net[is.na(losses), "losses" := 0]
    net[, "net" := wins - losses]
    return(net)
}

## get_net <- function(flows) {
##     #### add by option to choose grouping columns and ignore all else?
##     losses <- get_losses(flows)
##     wins <- get_wins(flows)
##     ### is this check necessary?
##     key1 <- setdiff(colnames(wins), "wins")
##     key2 <- setdiff(colnames(losses), "losses")
##     if (sum(key1 == key2) != length(key1)) {
##         stop("group columns in wins and losses different")
##     }
##     net <- do_join(dt1 = wins, dt2 = losses, join_col = "losses",
##                    key1 = key1, key2 = key1, all = TRUE)
##     net[is.na(wins), "wins" := 0]
##     net[is.na(losses), "losses" := 0]
##     net[, "net" := wins - losses]
##     return(net)
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
    flow <- . <- NULL
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

