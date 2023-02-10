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
    if (fill != "none") {
        check_input_values(values = values, by = by)
    }
    flows <- get_flows_only(dt = dt, by = by, us_o = us_o, us_d = us_d)
    if (fill != "none") {
        check_input_elements_of_values(values = values, flows = flows)
        flows <- include_missing_obs(flows, fill = fill, values = values)
    }
    return(flows)
}

get_flows_only <- function(dt, us_o = NULL, us_d = NULL, by = NULL) {
    . <- flow <- NULL
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

get_regions <- function(dt, shps, us, type) {
    AGS <- NULL
    stopifnot(type %in% c("data", "all"))
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

include_missing_obs <- function(flows, fill, values) {
    flow <- origin <- destination <- . <- NULL
    values <- do.call(data.table::CJ, values)
    key <- names(values)
    if (fill == "groups") {
        #### data.table::CJ produces all combinations of all
        #### values. So all od pairs as well. Many od-pairs might not
        #### be in flows and sometimes they should not be added to the
        #### data. This code here removes all rows of od-pairs that
        #### are not part of the original data.
        orig_od <- unique(flows[, .(origin, destination)])
        data.table::setkeyv(values, c("origin", "destination"))
        data.table::setkeyv(orig_od, c("origin", "destination"))    
        values <- values[orig_od]
    }
    data.table::setkeyv(flows, key)
    data.table::setkeyv(values, key)
    flows <- flows[values]
    flows[is.na(flow), "flow" := 0]
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

check_input_values <- function(values, by) {
    stopifnot("If you want to fill missing observation please specify values." =
                  !is.null(values))
    stopifnot("'values' should be a list" = is.list(values))
    stopifnot("'values' must be a named list with names corresponding to
                  variables specified by 'by' as well as 'origin' and 'destination'."
              = !is.null(names(values))) ## should check if "groups"
                                         ## used as fill but no 'by' specified
    needed <- c("origin", "destination", by)
    n_intersect <- length(intersect(names(values), needed))
    stopifnot("Elements of 'values' must contain 'origin', 'destination'
                  and all variables specified in 'by'"
              = n_intersect == length(needed)) ## hint which variables are missing
    return(NULL)
}

check_input_elements_of_values <- function(values, flows) {
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
