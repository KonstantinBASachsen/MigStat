##' get_flows() returns the sum of flows between every
##' origin-destination pair. These flows can be splitted across groups
##' to obtain the flows between regions of people with certain
##' characteristics.  Optionally the distances between the centroids
##' of origin and destination are returned. Also, od pairs without
##' flow can be added.
##'
##' The function needs two tables. One with the regional information
##' and the other with the flows between those pairs.
##'
##' The first is based on a shapefile from the
##' "Bundesamt für Karthographie und Geodäsie". This holds region
##' identifier and geometry.
##'
##' The other holds the flows between regions. This is the migration
##' statistics.
##'
##' Currently the function returnes all flows between the od pairs of
##' a given type. The types are specified via the argument "us" and
##' refer to federal states, districts or municipalities.
##' 
##' @title Origin-destination flows and distances from migration
##'     statistics
##' @param dt Migration Statistics data.table where every row is one
##'     migration.
##' @param us "unit simple" between regions of which type are flows to
##'     be computed? Takes one of the following strings:
##'
##' "st": federal states "di": districts "mu": municipalities
##' @param by Character vector. For all values of the given variables
##'     the number of people that moved between regions is
##'     summarized. If for example "gender" is given the flows of
##'     males between regions is returned and the flow of females
##'     between regions.
##' @param fill Character specifying which missing observations are to
##'     be filled. See details.
##' @param values A named list of variable values. These values are
##'     used to fill missing flows.
##' @return data.table with columns: origin id, destination id, the
##'     group columns, the flow between regions
##' @details fill: Let's assume the od-flows are grouped by
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
get_flows <- function(dt, us, by = NULL, fill = c("none", "groups", "all"),
                      values = NULL) {
    ### Add check for !is.null(values) if fill != "none". Also check
    ### names(values) and probably also that origin and destination
    ### have at least all the elements that are in the data in origin
    ### and destination

    ### I think it might be good if the function
    ### returns all regions and fills empty flows with 0's

    ### I think I should not compute and join the distances
    ### here. Maybe this belongs to some other function

### Don't know why this function needs state_o and so on cols

    ### shp only needed for pops. Should be removed
    ### I don't like join_populations() in here and I don't know
    ### anymore why I needed this. Probably for gravity sampling
    flow <- NULL
    fill <- match.arg(fill)
    if (fill != "none") {
        stopifnot("If you want to fill missing observation please specify values." =
                      !is.null(values))
        stopifnot("'values' should be a list" = is.list(values))
        needed <- c("origin", "destination", by)
        n_names <- length(names(values))
        n_actual <- length(intersect(names(values), needed))
        stopifnot("'values' must be a named list" = n_names != 0) ### does not work
        stopifnot("Elements of 'values' must contain 'origin', 'destination' and all variables specified in 'by'" =n_actual == length(needed)) ## hint which variables are missing
    }

    flows <- get_flows_only(dt = dt, by = by, us = us)
    if (fill != "none") {
        flows <- include_missing_obs(flows, fill = fill, values = values)
    }
    ## if (dist == TRUE) {
    ##         dist <- get_distances(shp)
    ##         flows <- join_distances(flows, dist)
    ##         flows[, "od" := NULL] ### don't know why I need to do this
    ## }
    ### I think the next two lines I implemented to obtain the data
    ### structure needed for spflow
##    flows[, c("destination", "origin") := lapply(.SD, as.numeric), .SDcols = c("destination", "origin")]
##    flows[, c("destination", "origin") := lapply(.SD, as.factor), .SDcols = c("destination", "origin")] 
##    flows <- flows[, .SD, .SDcols = c("destination", "origin", "flow", "distance")]
    ## if (pops == TRUE) {
    ##     flows <- join_populations(flows, shp)
    ## }
    ## if (na_to_0) { flows[is.na(flow), flow := 0] } ### do I still need this?
    return(flows)

    ### probably the functions below do too much like adding columns I
    ### dont use. Maybe copying data.tables is also too much
}

get_flows_only <- function(dt, us, by = NULL) {
    . <- flow <- NULL
    unit_o <- get_unitcol(us, FALSE)
    unit_d <- get_unitcol(us, TRUE)
    ags_o <- get_agscol(unit_o)
    ags_d <- get_agscol(unit_d)
    dt <- dt[, .(flow = .N), by = c(ags_o, ags_d, by)]
    dt[, c("origin", "destination") := .(get(ags_o), get(ags_d))]
    dt[, c(ags_o, ags_d) := NULL]
    data.table::setcolorder(dt, c("origin", "destination", by, "flow"))
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

join_populations <- function(flows, shp) {
    ### joins population sizes based on flows object. This function
    ### has no own full = TRUE argument. That is, it takes the keys
    ### from the flow object. The standard in get_flows() is to call
    ### join_distances with full = TRUE. If population sizes are
    ### joined afterwards it joins sizes to empty flows as well.

### It seems to be not really intuitive to first call get flows to
### simulate random draws then. Maybe I change it at some point.
### not sure what happens if there should be populations missing
    i.EWZ <- rn <- NULL
    flows_pop <- copy(flows)
    flows_pop[, "rn" := 1:nrow(flows_pop)]
    setkeyv(flows_pop, "origin")
    setkeyv(shp, "AGS")
    flows_pop[shp, "pop_o" := i.EWZ]
    setkeyv(flows_pop, "destination")
    flows_pop[shp, "pop_d" := i.EWZ]
    flows_pop <- flows_pop[order(rn)]
    flows_pop[, "rn" := NULL]
    return(flows_pop)
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

