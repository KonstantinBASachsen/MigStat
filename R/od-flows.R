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
##' @param shps The shapefile with three levels: federal states,
##'     districts, municipalities.
##' @param us "unit simple" between regions of which type are flows to
##'     be computed? Takes one of the following strings:
##'
##' "st": federal states "di": districts "mu": municipalities
##' @param na_to_0 logical, if TRUE, NA flows, that is, od_pairs where
##'     no migration took place are set to 0.
##' @return data.table with four columns: origin id, destination id,
##'     distance, flow
##' @import data.table
##' @export get_flows
##' @author Konstantin
get_flows <- function(dt, shp, us, pops = FALSE, na_to_0 = TRUE) {
    ### Don't know why this function needs state_o and so on cols
    flow <- NULL
    flows <- get_flows_only(dt, us)
    dist <- get_distances(shp)
    flows <- join_distances(flows, dist)
    ### I think the next two lines I implemented to obtain the data
    ### structure needed for spflow
##    flows[, c("destination", "origin") := lapply(.SD, as.numeric), .SDcols = c("destination", "origin")]
##    flows[, c("destination", "origin") := lapply(.SD, as.factor), .SDcols = c("destination", "origin")] 
    flows <- flows[, .SD, .SDcols = c("destination", "origin", "flow", "distance")]
    if (pops == TRUE) {
        flows <- join_pops(flows, shps[[get_shp_unit(us)]])
    }
    if (na_to_0 == TRUE) {
        flows[is.na(flow), "flow" := 0]
    }
    return(flows)

    ### probably the functions below do too much like adding columns I
    ### dont use. Maybe copying data.tables is also too much
}

join_distances <- function(dt_flow, dt_dist, us, full = TRUE) {
    ### add join = FULL argument. Otherwise makes not much sense to
    ### compute all pairwise distances but only use non zero
    ### then. Best would be one function, that checks if all distances
    ### are required and only computes pairwise distances if non zero
    ### flows or full = TRUE
    od <- i.distance <- i.flow <- origin <- destination <- NULL
    
    dcol <- get_agscol(get_unitcol(us, dest = TRUE))
    ocol <- get_agscol(get_unitcol(us, dest = FALSE))
    flow_dist <- copy(dt_flow)
    distances <- copy(dt_dist)
    distances <- distances[, "od" := paste(destination, origin, sep = "_")]
    flow_dist <- flow_dist[, "od" := paste(get(dcol), get(ocol), sep = "_")]
    if (full == TRUE) {
        setkeyv(flow_dist, "od")
        unique_keys <- unique(c(flow_dist[, get("od")], distances[, get("od")]))
        flow_dist <- flow_dist[unique_keys]
    } 
    setkey(distances, od)
    setkey(flow_dist, od)
    flow_dist <- distances[flow_dist, "flow" := i.flow]
    flow_dist[, "od" := NULL]

    return(flow_dist)

}

join_distances <- function(region_pairs, distances) {
    i.distance <- NULL
    combs <- copy(region_pairs)
    dist <- copy(distances)
    combs <- combs[, "od" := paste(origin, destination, sep = "_")]
    dist <- dist[, "od" := paste(origin, destination, sep = "_")]
    setkeyv(combs, "od")
    setkeyv(dist, "od")
    combs[dist, "distance" := i.distance]
    return(combs)
    
}

get_distances <- function(shp) {

    EWZ <- geometry <- centers <- AGS <- i.GEN <- . <- destination <-
        origin <- od <- i.distance <- NULL
    ### computes pair wise distances between all units of type
### "us". Maybe I only need it for pairs with non zero flows?
    shp_dist <- copy(shp)
    shp_dist[, "centers" := st_centroid(geometry)]
    distances <- round(st_distance(shp_dist[, centers] / 1000), 0) 
    keys <- shp_dist[, AGS]
    colnames(distances) <- keys
    rownames(distances) <- keys

    distances <- setDT(data.frame(distances, check.names = FALSE))
    distances$destinations <- colnames(distances)
    dist_pairs <- melt(distances, id.vars = "destinations")
    colnames(dist_pairs) <- c("destination", "origin", "distance")
    dist_pairs[shp_dist, "o_name" := i.GEN, on = .(origin = AGS)]
    dist_pairs[shp_dist, "d_name" := i.GEN, on = .(destination = AGS)]

    return(dist_pairs)

}


get_flows_only <- function(dt, us, simplify = TRUE) {

    unit_o <- get_unitcol(us, FALSE)
    unit_d <- get_unitcol(us, TRUE)
    ags_o <- get_agscol(unit_o)
    ags_d <- get_agscol(unit_d)
    dtf <- copy(dt)
    dtf[, "flow" := .N, by = c(ags_o, ags_d)]
    if (simplify == TRUE) {
        dtf <- dtf[, .SD, .SDcols = c(ags_o, ags_d, "flow")]
        dtf <- dtf[, .SD[1], by = c(ags_o, ags_d)]
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
