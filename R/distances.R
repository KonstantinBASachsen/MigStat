join_distances <- function(region_pairs, distances) {
    i.distance <- origin <- destination <- NULL
    combs <- copy(region_pairs)
    dist <- copy(distances)
    combs <- combs[, "od" := create_od(origin, destination)]
    dist <- dist[, "od" := create_od(origin, destination)]
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
