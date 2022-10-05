##' get_flows() returns the sum of flows between every
##' origin-destination pair. Also, the distances between the centroids
##' of origin and destination are returned. od pairs without flow are
##' added as well.
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
##' @param shp The shapefile with the level corresponding to us
##' @param us "unit simple" between regions of which type are flows to
##'     be computed? Takes one of the following strings:
##'
##' "st": federal states "di": districts "mu": municipalities
##' @param pops If population sizes should be joined. Feature will
##'     probably be removed later.
##' @param na_to_0 logical, if TRUE, NA flows, that is, od_pairs where
##'     no migration took place are set to 0.
##' @return data.table with four columns: origin id, destination id,
##'     distance, flow
##' @import data.table
##' @export get_flows
##' @author Konstantin
get_flows <- function(dt, shp, us, dist = FALSE, full = FALSE, pops = FALSE, na_to_0 = TRUE) {
    ### I think it might be good if the function returns all regions
    ### and fills empty flows with 0's

    ### I think I should not compute and join the distances
    ### here. Maybe this belongs to some other function
    
### Don't know why this function needs state_o and so on cols

    ### I don't like join_populations() in here and I don't know
    ### anymore why I needed this. Probably for gravity sampling
    flow <- NULL
    flows <- get_flows_only(dt, us)
    if (full == TRUE) {
        flows <- join_missing_regions(flows = flows, shp = shp)
    }
    if (dist == TRUE) {
            dist <- get_distances(shp)
            flows <- join_distances(flows, dist)
            flows[, "od" := NULL] ### don't know why I need to do this
    }
    ### I think the next two lines I implemented to obtain the data
    ### structure needed for spflow
##    flows[, c("destination", "origin") := lapply(.SD, as.numeric), .SDcols = c("destination", "origin")]
##    flows[, c("destination", "origin") := lapply(.SD, as.factor), .SDcols = c("destination", "origin")] 
##    flows <- flows[, .SD, .SDcols = c("destination", "origin", "flow", "distance")]
    if (pops == TRUE) {
        flows <- join_populations(flows, shp)
    }
    if (na_to_0) { flows[is.na(flow), flow := 0] }
    return(flows)

    ### probably the functions below do too much like adding columns I
    ### dont use. Maybe copying data.tables is also too much
}



join_missing_regions <- function(flows, shp) {

    combs <- create_region_combs(shp[, AGS])
    flows[, "od" := paste(origin, destination, sep = "_")]
    setkeyv(flows, "od")
### Although all region pairs are already contained in combs I need
### this because in the data are moves where origin is unknown.
    keys <- unique(c(combs[, od], flows[, od]))
    flows <- flows[keys]
    setkeyv(combs, "od")
    setkeyv(flows, "od")
    flows[combs, "origin" := i.origin]
    flows[combs, "destination" := i.destination]
    flows[, od := NULL]
    return(flows)
}

get_flows_only <- function(dt, us, simplify = TRUE) {
    . <- flow <- NULL
    unit_o <- get_unitcol(us, FALSE)
    unit_d <- get_unitcol(us, TRUE)
    ags_o <- get_agscol(unit_o)
    ags_d <- get_agscol(unit_d)
    dtf <- copy(dt)
    dtf[, "flow" := .N, by = c(ags_o, ags_d)]
    if (simplify == TRUE) {
        dtf <- dtf[, .SD, .SDcols = c(ags_o, ags_d, "flow")]
        dtf <- dtf[, .SD[1], by = c(ags_o, ags_d)]
        dtf <- dtf[, .(get(ags_o), get(ags_d), flow)] ### making sure cols are in right order
        colnames(dtf) <- c("origin", "destination", "flow")
    }
#### should do this in a separate step    
##    dtf <- dtf[, c(ags_o, ags_d) := lapply(.SD, as.numeric), .SDcols = c(ags_o, ags_d)]
    return(dtf)
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
