clean_shp <- function(shp, keep =  c("AGS", "GEN", "geometry")) {

    AGS <- GF <- NULL
    shp_clean <- copy(shp)
    shp_clean <- rename_ags_col(shp_clean)
    shp_clean <- ags_digits(shp_clean)
    if ("GF" %in% colnames(shp_clean)) {
        shp_clean[GF == 4, ]
    }
    shp_clean <- keep_cols(shp_clean, keep)
    return(shp_clean)
}

get_shp_unit <- function(us) {

    if(us == "st") {unit <- "state"}
    if(us == "di") {unit <- "district"}
    if(us == "mu") {unit <- "muni"}
    return(unit)
}

rename_ags_col <- function(shp) {
    #### in older shapefiles is no AGS column. Instead there is a KEY
    #### column which can be used for municipality AGS. However for
    #### districts and federal states it can not always be used
    #### because it might be set to NA. If this is the case, the
    #### column SHN can be used. This function checks these
    #### possibilities and returns the AGS column
    dt <- setDT(copy(shp))
    cols <- colnames(dt)
    if( "AGS" %in% cols) {
        ags <- "AGS"
        message("AGS column already present.")
    } else if("KEY" %in% cols) {
        nas <- dt[is.na(KEY), .N]
        if (nas == 0) {
            ags <- "KEY"
            message("KEY column found with 0 missings. Renamed to AGS")
        } else {
            ags <- "SHN"
            mes <- "KEY column found with %s missings. Renaming SHN to AGS instead"
            message(sprintf(mes, nas))
        }
        dt[, "AGS" := get(ags)]
        dt[, (ags) := NULL]
    } else {
        warning("No appropriate column found")
    }
    return(dt)
}


ags_digits <- function(shp) {
    ### In new shapefiles the AGS for federal states has two digits,
### the one for districts 5 and the one for municipalities 8. In older
### shapefiles the AGS may have 10 digits for municipalities,
### districts and federal states. The additional digits are 0's. They
### don't convey any information. To join the migration statistics
### with the shapefiles this function removes the unncessary
### digits. Probably better to write a function that checks if ags has
### right number already and if not recodes.
    
    dt <- copy(shp)
    n_reg <- uniqueN(dt[, GEN])
    n_digits <- unique(nchar(dt[, AGS]))
    if ( n_reg < 100) {
        if (n_digits != 2) {
            dt[, "AGS" := substr(AGS, 1, 2)]
            mes <- "Found %s regions, assuming states."
            message(sprintf(mes, n_reg))
        } else {
            message("AGS already right format")
        }
    } else if (100 < n_reg & n_reg < 600) {
        if (n_digits != 5) {
            mes <- "Found %s regions, assuming districts."
            message(sprintf(mes, n_reg))
            dt[, "AGS" := substr(AGS, 1, 5)]
        } else {
            message("AGS already right format")
        }
    } else {
        if (n_digits != 8) {
            mes <- "Found %s regions, assuming municipalities."
            message(sprintf(mes, n_reg))
        } else {
            message("AGS already right format")
        }
    }
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
##' @param year Year of shapefiles. Important because geometries and
##'     AGS's keys of regions change sometimes
##' @return list with data.tables as named elements
##' @import data.table sf
##' @export read_shapes
##' @author Konstantin
read_shapes <- function(path, year = 2014) {
    ### 2014 I only mention here because I want to use it in my test
    ### files and by setting this as a default I don't need to do it
    ### manually
    shapes <- list.files(path)
    if (length(shapes) == 0) {
        stop(sprintf("no files found in %s", path))
    }
    shapes <- shapes[grep(".shp", shapes)]
    if (length(shapes) == 0) {
        stop(sprintf("no shapefiles found in %s", path))
    }
    shapes <- shapes[grep(as.character(year), shapes)]
    
    muni_file  <- shapes[grep("gem", shapes)]
    states_file <- shapes[grep("lan", shapes)]
    districts_file <- shapes[grep("krs", shapes)]

    #### type = 6 makes sure that geometry is of type
    #### sfc_MULTIPOLYGON. See ?sf::read_sf
    munis <- sf::read_sf(file.path(path, muni_file), type = 6)
    munis <- data.table::setDT(munis)

    states <- sf::read_sf(file.path(path, states_file), type = 6)
    states <- data.table::setDT(states)

    districts <- sf::read_sf(file.path(path, districts_file), type = 6)
    districts <- data.table::setDT(districts)

    shapes <- list("state" = states, "district" = districts, "muni" = munis)
    return(shapes)    
    }
