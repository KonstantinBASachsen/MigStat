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

get_distances <- function(shp, type = c("point_on_surface", "centroid")) {
    #### there is room for improvement. I compute all pairwise
    #### distances, without using symmetrie. This is especially bad
    #### because half of diskspace and more importantly of memory
    #### could be saved
    geometry <- centers <- AGS <- distance <- .SD <- NULL
    ### computes pair wise distances between all units of type
### "us". Maybe I only need it for pairs with non zero flows?
    type <- match.arg(type)
    shp_dist <- data.table::copy(shp) ##better to CJ od-pairs? copy necessary?
    if (type == "point_on_surface") {
        shp_dist[, "centers" := sf::st_point_on_surface(geometry)]
    } else {
        shp_dist[, "centers" := sf::st_centroid(geometry)]
    }
    distances <- round(sf::st_distance(shp_dist[, centers] / 1000), 0) 
    keys <- shp_dist[, AGS]
    ## bc distances matrix with rows and cols numbered consecutively
    ## but AGS not necessarily (I guess).
    colnames(distances) <- keys
    rownames(distances) <- keys
    distances <- as.table(distances)
    distances <- data.table::data.table(distances)
    cols <- c("origin", "destination", "distance")
    colnames(distances) <- cols
    distances[, (cols) := lapply(.SD, as.integer), .SDcols = cols]
    return(distances)
}

