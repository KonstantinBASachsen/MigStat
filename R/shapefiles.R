clean_shps <- function(shps_path, new_path, years) {
    n_years <- length(years)
    files <- list.files(shps_path)
    ### For every year there are the region types. Municipalities,
    ### districts and federal states. For every region type there are
    ### three files necessary. A .shp, a .dbf and a .shx. This there
### need to be n_years * 3 * 3 files in shp_path

    ### add option for more detailed check what files are missing
    n_needed <- n_years * 3 * 3
    n_files <- length(files)
    if (n_files < n_needed) {
        mes <- sprintf("There seem to be files missing! Expected %s, got %s",
                       n_needed, n_files)
        stop(mes)
    }
    shapes <- lapply(years, function(year) MigStat::read_shapes(shps_path, year))
    keep  <- c("AGS", "GEN", "BEZ", "geometry") ## added BEZ because GEN might not reveal differences
    shapes <- lapply(shapes, function(year)
        lapply(year, function(region) suppressMessages(clean_shp(region, keep = keep))))

    states <- get_shape_dt(shapes, 1, years)
    districts <- get_shape_dt(shapes, 2, years)
    munis <- get_shape_dt(shapes, 3, years)
    
    states <- smallup_shp(states)
    districts <- smallup_shp(districts)
    munis <- smallup_shp(munis)
    regions <- list("states" = states, "districts" = districts,
                    "munis" = munis)
    write <- FALSE
    if (write == TRUE) {
           sf::write_sf(states, new_path, append = FALSE)
           sf::write_sf(districts, file.path(new_path, "districts"), append = FALSE)
           sf::write_sf(munis, file.path(new_path, "munis"), append = FALSE)
 
    }
    return(regions)
}
    
get_shape_dt <- function(shapes, idx, years) {
    ### Eine kleien Hilfsfunktion, welche aus den eingelesenen
    ### shapefiles einen data.fram / data.table erstellt.

    dt <- lapply(shapes, function(year) return_el(year, idx))
    dt <- lapply(1:length(years), function(i) dt[[i]][, "year" := years[i]])
    dt <- rbindlist(dt)
    setkeyv(dt, c("AGS", "year"))
    return(dt)    
}    

smallup_shp <- function(shp) {
    AGS <- NULL
    shp[, "year_min" := as.numeric(NA)]
    shp[, "year_max" := as.numeric(NA)]
    shp[, "year_min" := min(year), by = AGS]
    shp[, "year_max" := max(year), by = AGS]
    shp[, "year" := NULL]
    group <- colnames(shp)[!colnames(shp) %in% "geometry"]
    shp <- shp[, .SD[1],  by = group]
    return(shp)
}


##' Returns clean data.table of shapefile ready to be joined to
##' Migration Statistics
##'
##' The shapefiles from the "Bundesamt für Kartographie and Geodäsie"
##' that can be downloaded here:
##' https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_1231/ do
##' have some issues. In older shapefiles there is no AGS
##' column. Instead the AGS is stored in either the column KEY or the
##' column SHN. Furthermore these columns have more digits than the
##' usual two for federal states, five for districts or 8 dor
##' municipalities. clean_shp() takes care of these issues and returns
##' a data.table that has the AGS column in the right
##' format. Currently this all happens in the background. I plan to
##' make it optional in the future.
##'
##' Additionally clean_shp() selects the right rows. In newer
##' shapefiles the column GF allows to use different geometries
##' depending on what is included (water for example). GF == 4 takes
##' the rows we want.
##' @title Clean shapefile and make it ready to be joined
##' @param shp The shapefile that is to be cleaned
##' @param keep The columns to be kept
##' @param us optional. If shp is a list of three it is supposed to
##'     hold three data.tables. One for federal states, one for
##'     districts, one for municipalities. us allows to choose one. 
##' @return data.table of nice and clean shapefile
##' @import data.table
##' @export clean_shp
##' @author Konstantin
clean_shp <- function(shp, keep =  c("AGS", "GEN", "geometry"), us = NULL) {

    ### test if BEZ in colnames before keep_cols
    if (!is.null(us)) {
        stopifnot(us %in% c("mu", "di", "st"))
    }
    if (is.list(shp) & length(shp) == 3) {
        message("shp assumed to be a list of region types")
        shp_clean <- shp[[get_shp_unit(us)]]
    } else {
        shp_clean <- copy(shp)
    }
    AGS <- GF <- NULL
    shp_clean <- rename_ags_col(shp_clean)
    shp_clean <- ags_digits(shp_clean)
    shp_clean <- rename_bez_col(shp_clean)
    if ("GF" %in% colnames(shp_clean)) {
        shp_clean <- shp_clean[GF == 4, ]
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
    KEY <- NULL
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

rename_bez_col <- function(shp) {
    #### in shapefiles from 2000 to 2014 the "BEZ" col which indicates
#### the type of region was called "DES". Here I rename it to BEZ
    stopifnot(is(shp, "data.table"))
    cols <- colnames(shp)
    if ("DES" %in% cols) {
        cols[cols == "DES"] <- "BEZ"
        colnames(shp) <- cols
    }
    return(shp)
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
    GEN <- AGS <- NULL
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
    if (length(shapes) == 0) {
        stop(sprintf("no shapefiles for year %s found in %s", year, path))
    }
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
