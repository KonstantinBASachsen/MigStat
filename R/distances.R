join_distances <- function(region_pairs, distances) {
    i.distance <- origin <- destination <- NULL
    ### maybe I should remove those copy because of memory
    combs <- copy(region_pairs)
    dist <- copy(distances)
    ## combs <- combs[, "od" := create_od(origin, destination)]
    ## dist <- dist[, "od" := create_od(origin, destination)]
    ## setkeyv(combs, "od")
    ## setkeyv(dist, "od")
    setkeyv(combs, c("origin", "destination"))
    setkeyv(dist, c("origin", "destination"))
    combs[dist, "distance" := i.distance]
    return(combs)
    
}

get_distances <- function(shp) {
    #### there is room for improvement. I compute all pairwise
    #### distances, without using symmetrie. This is especially bad
    #### because half of diskspace and more importantly of memory
    #### could be saved
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
    distances <- as.table(distances)
    distances <- as.data.frame(distances)
    setDT(distances)
    colnames(distances) <- c("destination", "origin", "distance")
    distances[, "distance" := as.integer(distance)]
    return(distances)

}
