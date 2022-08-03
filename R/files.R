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
    return(shapes)    
    }


read_inkar <- function(path, leading_0 = TRUE) {
    Kennziffer <- NULL
    inkar <- data.table::fread(inkar_csv, dec = ",")
### grap strings that start and end with [0-9] and add leading 0
### because in shapefiles and migration statistics the id is coded
### that way
    if (leading_0 == TRUE) {
        old <- typeof(inkar[, Kennziffer])
        inkar[, "Kennziffer" := as.character(Kennziffer)]
        inkar[, "Kennziffer" := gsub("(^[1-9]$)", "0\\1", Kennziffer)]
        new <- typeof(inkar[, Kennziffer])
        mes <- "Kennziffer from %s converted to %s and leading 0's added \n
                to make sure joining to shapefile works"
        message(sprintf(mes, old, new))
    }
    return(inkar)
}
