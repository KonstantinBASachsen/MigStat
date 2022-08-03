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


get_distances <- function(shps, us) {

    EWZ <- geometry <- centers <- AGS <- i.GEN <- . <- destination <-
        origin <- od <- i.distance <- NULL
    ### computes pair wise distances between all units of type
    ### "us". Maybe I only need it for pairs with non zero flows?
    shp <- shps[[get_shp_unit(us)]]
    shp <- shp[EWZ != 0]
    shp[, "centers" := st_centroid(geometry)]
    distances <- round(st_distance(shp[, centers] / 1000), 0) 
    keys <- shp[, AGS]
    colnames(distances) <- keys
    rownames(distances) <- keys

    distances <- setDT(data.frame(distances, check.names = FALSE))
    distances$destinations <- colnames(distances)
    dist_pairs <- melt(distances, id.vars = "destinations")
    colnames(dist_pairs) <- c("destination", "origin", "distance")
    dist_pairs[shp, "o_name" := i.GEN, on = .(origin = AGS)]
    dist_pairs[shp, "d_name" := i.GEN, on = .(destination = AGS)]

    return(dist_pairs)

}


