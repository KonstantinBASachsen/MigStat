##' Read shape files
##'
##' This function reads shapefiles from path. It reads those that
##' store information about federal_states, districts and
##' municipalities
##' @title
##' @param path path to shape files. Expects directory structure where
##'     information for levels is stored in different files
##' @return list with data.tables as named elements
##' @import data.table, sf
##' @export read_shapes
##' @author Konstantin
read_shapes <- function(path) {

    muni_file  <- "VG250_GEM.shp"
    states_file <- "VG250_LAN.shp"
    districts_file <- "VG250_KRS.shp"

    munis <- sf::read_sf(paste(path, muni_file, sep = "/"))
    munis <- data.tablesetDT(munis)

    states <- sf::read_sf(paste(path, states_file, sep = "/"))
    states <- data.tablesetDT(states)

    districts <- sf::read_sf(paste(path, districts_file, sep = "/"))
    districts <- data.table::setDT(districts)

    shapes <- list("states" = states, "districts" = districts, "munis" = munis)
    return(shapes)
    
    }
