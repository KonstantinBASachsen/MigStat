##' Reads distances from .csv.
##'
##' @title Reads distances from .csv
##' @param file Full path to filw
##' @param type character, eiter "int" or "cha". If "int", colums are
##'     read as integers, if "cha", columns are read as character.
##' @return data.tale with distances
##' @export read_distances
##' @import data.table
##' @author Konstantin
read_distances <- function(file, type = c("int", "cha")) {
    type <- match.arg(type)
    if (type == "int") {
        col_types <- c("integer", "integer", "integer")
        dist <- data.table::fread(file, colClasses=col_types)
        warn <- "Reading as integer strips leading 0's from ags,
beware when joining other data. still it is preferred because it saves memory."
        warning(warn)
    }
    if (type == "cha") {
        col_types <- c("character", "character", "integer")
        dist <- data.table::fread(file, colClasses=col_types)
        warn <- "If memory is an issue better read as integer as it saves memory."
        warning(warn)
    }
    return(dist)
}

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
    geometry <- centers <- AGS <- distance <- NULL
    ### computes pair wise distances between all units of type
### "us". Maybe I only need it for pairs with non zero flows?
    shp_dist <- copy(shp)
    shp_dist[, "centers" := sf::st_centroid(geometry)]
    distances <- round(sf::st_distance(shp_dist[, centers] / 1000), 0) 
    keys <- shp_dist[, AGS]
    colnames(distances) <- keys
    rownames(distances) <- keys
    distances <- as.table(distances)
    distances <- as.data.frame(distances)
    setDT(distances)
    colnames(distances) <- c("destination", "origin", "distance")
    distances[, "distance" := as.integer(distance)]
    message("If reading distances from disk")
    return(distances)
}
