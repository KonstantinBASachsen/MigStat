clean_shp <- function(shps, us, keep =  c("AGS", "GEN", "EWZ", "geometry")) {

    AGS <- NULL
    
    shp_clean <- get_shp_region(shps, us, drop_gf = TRUE)
    shp_clean <- keep_cols(shp_clean, keep)
    return(shp_clean)
}

get_shp_region <- function(shps, us, drop_gf) {
    GF <- NULL
    shp <- shps[[get_shp_unit(us)]]
    if (drop_gf ==  TRUE) {
        shp <- shp[GF == 4]
    }
    return(shp)
    
}

get_shp_unit <- function(us) {

    if(us == "st") {unit <- "state"}
    if(us == "di") {unit <- "district"}
    if(us == "mu") {unit <- "muni"}

    return(unit)
    
}


##' Read shape files from path as data.tables
##'
##' This function reads shapefiles from path. It reads those that
##' store information about federal_states, districts and
##' municipalities
##' @title read shape files as data.tables
##' @param path path to shape files. Expects directory structure where
##'     information for levels is stored in different files
##' @return list with data.tables as named elements
##' @import data.table
##' sf
##' @author Konstantin
read_shapes <- function(path, year = 2013) {

    shapes <- list.files(path)
    shapes <- shapes[grep(".shp", shapes)]
    shapes <- shapes[grep(as.character(year), shapes)]
    
    muni_file  <- shapes[grep("gem", shapes)]
    states_file <- shapes[grep("lan", shapes)]
    districts_file <- shapes[grep("krs", shapes)]

    munis <- sf::read_sf(paste(path, muni_file, sep = "/"))
    munis <- data.table::setDT(munis)

    states <- sf::read_sf(paste(path, states_file, sep = "/"))
    states <- data.table::setDT(states)

    districts <- sf::read_sf(paste(path, districts_file, sep = "/"))
    districts <- data.table::setDT(districts)

    shapes <- list("state" = states, "district" = districts, "muni" = munis)
    return(shapes)    
    }
