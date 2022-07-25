read_examples <- function(mig_path = "~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav", shp_path = "/home/konstantin/Documents/sexony/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen" ) {

    dt <- read_migex(mig_path)
    shps <- read_shapes(shp_path) #
    data <- list("mig" = dt, "shps" = shps)

    return(data)
    
}

read_migex <- function(file) {

    df <- foreign::read.spss(file, to.data.frame = TRUE)
    dt <- data.table::setDT(df)
    
    return(dt)
    
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
read_shapes <- function(path) {

    muni_file  <- "VG250_GEM.shp"
    states_file <- "VG250_LAN.shp"
    districts_file <- "VG250_KRS.shp"

    munis <- sf::read_sf(paste(path, muni_file, sep = "/"))
    munis <- data.table::setDT(munis)

    states <- sf::read_sf(paste(path, states_file, sep = "/"))
    states <- data.table::setDT(states)

    districts <- sf::read_sf(paste(path, districts_file, sep = "/"))
    districts <- data.table::setDT(districts)

    shapes <- list("state" = states, "district" = districts, "muni" = munis)
    ### should get these names from function because I use them in utils as well
    return(shapes)    
    }


