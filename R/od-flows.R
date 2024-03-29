##' get_flows() returns the sum of flows between every
##' origin-destination pair. These flows can be splitted by groups to
##' obtain the flows between regions group wise. Missing flows can be
##' added and filled with 0's.
##' 
##' @title Compute origin-destination flows from migration statistics.
##' @param dt Migration Statistics data.table where every row is one
##'     migration case.
##' @param us_o "unit simple origin" Type of regions for
##'     'origin'. Either "st": federal states "di": districts "mu":
##'     municipalities.
##' @param us_d "unit simple destination" Type of regions for
##'     'destination'. Either "st": federal states "di": districts "mu":
##'     municipalities.
##' @param by Character vector. Specifies the grouping
##'     variables. Origin-destination flows are returned for every
##'     combination of the grouping variables. See details.
##' @param fill Character specifying which missing observations are to
##'     be filled. Can be either 'none', 'groups' or 'all'. See
##'     details.
##' @param values A named list. This list is used if fill != 'none' to
##'     fill missing flows with 0 flows. The names of the list must
##'     correspond to 'origin', 'destination' and all variables
##'     specified by 'by'. The values of the elements specify the
##'     combinations that are to be filled with 0 flows. For example
##'     if values <- list("gender" = c("f", "m")) that every od pair
##'     has two rows, "m" and "f". If no males migrated from o to d a
##'     row is added to the od-flows that says 0 for gender "m".
##' @return data.table with columns: origin id, destination id, the
##'     group columns, the flow between regions
##' @details
##'
##' by:  can be used to return od-flows for different groups. If for
##' example by = c("gender", "marital status") flows will be returned
##' for every combination of gender and marital status together.
##' 
##' fill: Let's assume the od-flows are grouped by
##'     gender. Then there might be origin destination pairs where
##'     only females migrate. fill can be used to add the missing
##'     observation "male" and specify a flow of 0.
##'
##'     If fill == "groups" all missing combinations of the grouping
##'     variables are added but no new origin-destination pairs.
##'
##'     If fill == "all" origin-destination pairs and all
##'     grouping combinations are added.
##'
##'     If fill == "none" no observations are added.
##'
##'     The combinations to be added are taken from the "values"
##'     argument.
##'
##'    values: A named list with the first order elements
##'    corresponding to the grouping variables as well as "origin" and
##'    "destination". The second order elements are used to fill the
##'    missing observations
##' @import data.table
##' @export get_flows
##' @author Konstantin
get_flows <- function(dt,
                      us_o = c("none", "st", "di", "mu"),
                      us_d = c("none", "st", "di", "mu"),
                      by = NULL,
                      fill = c("none", "groups", "all"),
                      values = NULL) {
    us_o <- match.arg(us_o)
    us_d <- match.arg(us_d)
    fill <- match.arg(fill)
    if (us_o != "none" & us_d != "none") {
        type  <- "od"
    }
    if (us_o == "none" & us_d != "none" | us_o != "none" & us_d == "none") {
        type <- "net"
    }
    if (fill != "none") {
        check_input_values(values = values, type = type, by = by)
    }
    if (type == "od") {
        flows <- get_flows_only(dt = dt, by = by, us_o = us_o, us_d = us_d)
    }
    if (type == "net") {
        us <- c(us_o, us_d)
        us <- us[us != "none"]
         flows <- get_net_(dt = dt, us = us, by = by)
    }
    if (fill != "none") {
        check_input_elements(values = values, flows = flows, type)
        flows <- include_missing_obs(flows = flows, fill = fill,
                                     values = values, type = type)
    }
    return(flows)
}

get_net_ <- function(dt, us = c("st", "di", "mu"), by = NULL) {
    wins <- get_flows_only(dt, us_o = "none", us_d = us, by = by)
    keyw <- colnames(wins)[colnames(wins) != "wins"]
    losses <- get_flows_only(dt, us_o = us, us_d = "none", by = by)
    keyl <- colnames(losses)[colnames(losses) != "losses"]
    net <- do_join(dt1 = wins, dt2 = losses, join_col = "losses",
                   key1 = keyw, key2 = keyl, all = TRUE)
    net[is.na(wins), "wins" := 0]
    net[is.na(losses), "losses" := 0]
    net[, "net" := wins - losses]
    return(net)
}

##' Aggregates origin-destination flows to saldo/wins/losses/total per region
##'
##' Net migration, wins and losses by region
##' @param flows data.table of flows
##' @param by character, grouping variables to stratify results by
##' @return data.table
##' @import data.table
##' @export get_net
##' @author Konstantin
##' @examples
##' ### generate data
##' regions <- c("reg1", "reg2", "reg3")
##' flows <- data.table::CJ(regions, regions)
##' colnames(flows) <- c("origin", "destination")
##' flows[, flow := runif(n = nrow(flows), min = 100, max = 1000)]
##' flows[, gender := sample(c("f", "m"), size = nrow(flows), replace = TRUE)]
##'
##' net <- get_net(flows) ## does not work
##' net <- get_net(flows, by = "gender")
get_net <- function(flows, by = NULL) {
    . <- flow <- i.losses <- NULL
    losses <- flows[, .(losses = sum(flow)),
                    keyby = c("origin", by)]
    data.table::setnames(losses, "origin", "region")
    wins <- flows[, .(wins = sum(flow)),
                  keyby = c("destination", by)]
    data.table::setnames(wins, "destination", "region")
    data.table::setkeyv(losses, c("region",  by))
    data.table::setkeyv(wins, c("region", by))
    wins[losses, "losses" := i.losses]
    wins[, "saldo" := wins - losses]
    wins[, "total" := wins + losses]
    return(wins)
}

get_flows_only <- function(dt, us_o = NULL, us_d = NULL, by = NULL) {
    . <- flow <- NULL
    #### maybe update code such that it uses 'type'
    if (us_o != "none" & us_d != "none") {
        unit_o <- get_unitcol(us_o, FALSE)
        unit_d <- get_unitcol(us_d, TRUE)
        ags_o <- get_agscol(unit_o)
        ags_d <- get_agscol(unit_d)
        dt <- dt[, .(flow = .N), by = c(ags_o, ags_d, by)]
        dt[, c("origin", "destination") := .(get(ags_o), get(ags_d))]
        dt[, c(ags_o, ags_d) := NULL] ## just trying to rename columns?
        data.table::setcolorder(dt, c("origin", "destination", by, "flow"))
    }
    if (us_o != "none" & us_d == "none") {
        unit_o <- get_unitcol(us_o, FALSE)
        ags_o <- get_agscol(unit_o)
        dt <- dt[, .(losses = .N), by = c(ags_o, by)]
        dt[, c("region") := .(get(ags_o))]
        dt[, c(ags_o) := NULL] ## just trying to rename columns?
        data.table::setcolorder(dt, c("region", by, "losses"))
    }
    if (us_o == "none" & us_d != "none") {
        unit_d <- get_unitcol(us_d, TRUE)
        ags_d <- get_agscol(unit_d)
        dt <- dt[, .(wins = .N), by = c(ags_d, by)]
        dt[, c("region") := .(get(ags_d))]
        dt[, c(ags_d) := NULL] ## just trying to rename columns?
        data.table::setcolorder(dt, c("region", by, "wins"))
    }
    return(dt)
}

get_regions <- function(dt, shps, us = c("st", "di", "mu"),
                        type = c("data", "all")) {
    AGS <- NULL
    us <- match.arg(us)
    type <- match.arg(type)
    ags_o <- get_agscol(get_unitcol(us, F))
    ags_d <- get_agscol(get_unitcol(us, T))
    shp <- shps[[get_shp_unit(us)]] ### not sure if this works though
    shp <- clean_shp(shp, keep = "AGS")
    if (type == "data") {
        all_regions <- unique(c(dt[, get(ags_o)], dt[, get(ags_d)]))
    }
    if (type == "all") {
        all_regions <- unique(c(shp[, AGS], dt[, get(ags_o)], dt[, get(ags_d)]))
    }
    return(all_regions)
}

include_missing_obs <- function(flows, fill, values, type) {
    ### I think this function is not needed anymore. It is somewhere!
    ### I think I probably should replace it's usage by the more
    ### straight forward fill_obs()
    flow <- origin <- destination <- . <- region<- NULL
    values <- do.call(data.table::CJ, values)
    key <- names(values)
    if (fill == "groups") { ## != "none"?
        #### data.table::CJ produces all combinations of all
        #### values. So all od pairs as well. Many od-pairs might not
        #### be in flows and sometimes they should not be added to the
        #### data. This code here removes all rows of od-pairs that
        #### are not part of the original data.
        if (type == "od") {
            orig_od <- unique(flows[, .(origin, destination)])
            data.table::setkeyv(values, c("origin", "destination"))
            data.table::setkeyv(orig_od, c("origin", "destination"))
            values <- values[orig_od]
        }
        if (type == "net") {
            orig_od <- unique(flows[, .(region)])
            data.table::setkeyv(values, "region")
            data.table::setkeyv(orig_od, "region")
            values <- values[orig_od]
        }
    }
    data.table::setkeyv(flows, key)
    data.table::setkeyv(values, key)
    flows <- flows[values]
    flows[is.na(flow), "flow" := 0]
    return(flows)
}

##' Checks if number of rows in flows data.table is as expected
##'
##' @param flows data.table of flows data
##' @param all_pairs data.table that holds all covariate combinations
##' @return NULL
##' @author Konstantin
##' @export check_flow_number
check_flow_number <- function(flows, all_pairs) {
    if (nrow(flows) < nrow(all_pairs)) {
        warning("Rows are missing! Please use fill_obs().")
    }
    if (nrow(flows) >= nrow(all_pairs)) {
        message("Row number correct!")
    }
    return(NULL)
}

##' Fills missing observation in a flows data.table with 0's
##'
##' When fitting models to flows or even when just visualizing or
##' computing net statistics the analyses will be flawed because the
##' flows data.table is not complete in the sense that all
##' combinations of covariates like origin, destination, age etc. are
##' in the table. Only these combinations that result in a non 0.
##' flow. fill_obs() adds all the missing covariate combinations and
##' sets the flow of those to 0.
##' @param flows data.table
##' @param all_pairs data.table of all covariate combinations
##' @param key character specify variable names. Defaults to
##'     c("origin", "destination", "age_group", "year").
##' @return data.table
##' @export fill_obs
##' @import data.table
##' @author Konstantin
##' @examples
##' regions <- c("reg1", "reg2")
##' ### complete observations
##' all_pairs <- data.table::CJ(origin = regions,
##'                             destination = regions,
##'                             gender = c("m", "f"))
##' flows <- all_pairs[- c(1:4)] ## simulates missing flows from region 1
##' flows[, flow := c(100, 200, 150, 300)]
##' net <- get_net(flows, by = "gender") ## NA's
##' flows <- fill_obs(flows, all_pairs, c("origin", "destination", "gender"))
##' net2 <- get_net(flows, by = "gender") ## correct results
fill_obs <- function(flows, all_pairs,
                     key = c("origin", "destination", "age_group", "year")) {
    flow <- NULL
    data.table::setkeyv(flows, key)
    data.table::setkeyv(all_pairs, key)
    flows <- flows[all_pairs]
    flows[is.na(flow), "flow" := 0]
    stopifnot("Rows seem to be missing" = nrow(flows) >= nrow(all_pairs))
    return(flows)
}

##' Ungroups an MigStat::get_flows() object
##'
##' sum_flows can be used to ungroup flows returned by
##' MigStat::get_flows(). The usual workflow involves invoking
##' get_flows() with all grouping variables that might be of
##' interest. Sometimes not all grouping variables are needed. The
##' sum_flows() can be used to sum flows (haha) over the not needed
##' groups. All variables that are not mentioned in the by argument
##' are summed over.
##' @title Sum flows returned from MigStat::get_flows()
##' @param flows object returned by get_flows()
##' @param by character vector of variables which groups should be
##'     retained. All variables not mentioned here are summed over
##' @return A data.table
##' @import data.table
##' @export sum_flows
##' @author Konstantin
sum_flows <- function(flows, by) {
    flow <- NULL
    dt <- data.table::copy(flows)
    dt <- dt[, "flow" := sum(flow), by = by]
    dt <- dt[, .SD[1], by = by]
    dt <- dt[, .SD, .SDcols = c(by, "flow")]
    return(dt)
}

check_input_values <- function(values, type, by) {
    stopifnot("If you want to fill missing observation please specify values." =
                  !is.null(values))
    stopifnot("'values' should be a list" = is.list(values))
    stopifnot("'values' must be a named list with names corresponding to
                  variables specified by 'by' as well as 'origin' and 'destination'."
              = !is.null(names(values))) ## should check if "groups"
    ## used as fill but no 'by' specified
    if (type == "od") {
        needed <- c("origin", "destination", by)
        n_intersect <- length(intersect(names(values), needed))
        stopifnot("Elements of 'values' must contain 'origin', 'destination'
                  and all variables specified in 'by'" = n_intersect == length(needed)) ## hint which variables are missing
    }
    if (type == "net") {
        needed <- c("region", by)
        n_intersect <- length(intersect(names(values), needed))
        stopifnot("Elements of 'values' must contain 'region', and all variables specified in 'by'" = n_intersect == length(needed)) ## hint which variables are missing
    return(NULL)
    }
}

check_input_elements <- function(values, flows, type = c("od", "net")) {
    type  <- match.arg(type)
    if (type == "od") {
        check_input_elements_type_od(values = values, flows = flows)
    }
    if (type == "net") {
        check_input_elements_type_net(values = values, flows = flows)
    }
}

check_input_elements_type_od <- function(values, flows) {
    origin <- destination <- NULL
    origins_data <- unique(flows[, origin])
    origins_values <- unique(values[["origin"]])
    dest_data <- unique(flows[, destination])
    dest_values <- unique(values[["destination"]])
    if (class(origins_data) != class(origins_values)) {
        mes <- sprintf("Origin in data is of type %s, origin in values of type %s. Must be of same type.",
                       class(origins_data), class(origins_values))
        stop(mes)
    }
    if (class(dest_data) != class(dest_values)) {
        mes <- sprintf("Destination in data is of type %s, destination in values of type %s.
                           Must be of same type", class(dest_data), class(dest_values))
        stop(mes)
    }
    not_in_values <- setdiff(origins_data, origins_values)
    if (length(not_in_values) > 0) {
        mes <- sprintf("There are origins in the data that are not in values.
                           Flows from %s will be omitted although they are in the data.",
                       paste(not_in_values, collapse = ", "))
        warning(mes)
    }
    not_in_values <- setdiff(dest_data, dest_values)
    if (length(not_in_values) > 0) {
        mes <- sprintf("There are destination in the data that are not in values.
                           Flows to %s will be omitted although they are in the data.",
                       paste(not_in_values, collapse = ", "))
        warning(mes)
    }
}

check_input_elements_type_net <- function(values, flows) {
    region <- NULL
    region_data <- unique(flows[, region])
    region_values <- unique(values[["region"]])
    if (class(region_data) != class(region_values)) {
        mes <- sprintf("Region in data is of type %s, region in values of type %s. Must be of same type.",
                       class(region_data), class(region_values))
        stop(mes)
    }
    not_in_values <- setdiff(region_data, region_values)
    if (length(not_in_values) > 0) {
        mes <- sprintf("There are regions in the data that are not in values.
                           Flows from %s will be omitted although they are in the data.",
                       paste(not_in_values, collapse = ", "))
        warning(mes)
    }
}
