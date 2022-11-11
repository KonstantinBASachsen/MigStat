##' Reads shapefiles from disk and returns nicely formatted tables of
##' shapeinformation.
##'
##' This function reads shapefiles from disk. It expects that the year
##' is encoded in the file name like vg250_lan_2018.shp. It is also
##' important that every file has either "lan", "krs" or "gem" in its
##' name to indicate the region type. Later I plan to write a function
##' that accesses the data on the server of the Bundesamt für
##' Kartographie and Geodäsie. Then again these files do have some
##' problems that took some time fixing. So might be easier to
##' download shapes from somewhere else.
##'
##' When joining information from the shapefiles it is not immideatly
##' clear if I can just use the AGS or if other information like the
##' year is important as well. Can I simply take the AGS as key? Or do
##' I have to take the years into account as well? This would be the
##' case if there are AGS that refer to a different entity in
##' different years. If the entity is a bit iffernt, like some changes
##' in the borders or so this I ignore for now. I do not know, when
##' and if this might be important. Below I show some example code
##' that can be used to check informally these things. It does not run
##' but gives some guidelines.
##'
##' \code{ags2 <- districts[, uniqueN(GEN), by = AGS][V1 > 1, AGS]}
##'
##' \code{districts[AGS %in% ags2, .SD[1], by = c("AGS", "GEN")][
##' order(AGS)][, .(AGS, GEN, BEZ, year)]}
##'
##' From a first glance it seems that one AGS refers to roughly the
##' same entity on the level of districts
##'
##' \code{ags2 <- munis[, uniqueN(GEN), by = AGS][V1 > 1, AGS]}
##' \code{munis[AGS %in% ags2, .SD[1], by = c("AGS", "GEN")][
##' order(AGS)][, .(AGS, GEN, BEZ, year)]}
##'
##' The same applies for municipalities. There are keys that refer to
##' more than one name but again this can be ignored. Seems to almost
##' exclusively the case because the region is named a bit differently

##' @title Nicely formatted tables from shapefiles
##' @param shps_path Path to shapefiles. Expects rather strict file
##'     structure. See details.
##' @param new_path If specified three clean data.tables are written
##'     to new_path. One for federal states, one for districts and one
##'     for municipalities.
##' @param years Which years are processed
##' @param type Either "ags" or "complete". "ags" returns one ags only
##'     once. This should be fine for our purposes because one ags
##'     refers to roughly the same entity. If "complete" one ags is
##'     returned for every unique combination of GEN, BEZ and AGS. Gen
##'     is the name of the region and BEZ some classification.
##' @return list of three data.tables: states, districts and munis.
##' @export clean_shps
##' @import data.table
##' @author Konstantin
clean_shps <- function(shps_path, new_path = NULL, years, type = "ags") {
    if (type %in% c("ags", "complete") == FALSE) {
        stop("please specify type as 'ags' or 'complete'")
    }
    n_years <- length(years)
    files <- list.files(shps_path)
    ### For every year there are the region types. Municipalities,
    ### districts and federal states. For every region type there are
    ### three files necessary. A .shp, a .dbf, a .shx and a .prj. Thus
    ### there need to be n_years * 3 * 4 files in shp_path

    ### add option for more detailed check what files are missing
    n_needed <- n_years * 3 * 4
    n_files <- length(files)
    if (n_files < n_needed) {
        mes <- sprintf("There seem to be files missing! Expected %s, got %s",
                       n_needed, n_files)
        stop(mes)
    }
    shapes <- lapply(years, function(year) read_shapes(shps_path, year))
    keep  <- c("AGS", "GEN", "BEZ", "geometry") ## added BEZ because GEN might not reveal differences
    shapes <- lapply(shapes, function(year)
        lapply(year, function(region) suppressMessages(clean_shp(region, keep = keep))))

    states <- get_shape_dt(shapes, 1, years)
    districts <- get_shape_dt(shapes, 2, years)
    munis <- get_shape_dt(shapes, 3, years)
    
    states <- smallup_shp(states, type)
    districts <- smallup_shp(districts, type)
    munis <- smallup_shp(munis, type)
    regions <- list("states" = states, "districts" = districts,
                    "munis" = munis)
    if (!is.null(new_path)) {
        sf::write_sf(set_geom(states, geom_only = FALSE), file.path(new_path, "states.shp"))
        sf::write_sf(set_geom(districts, geom_only = FALSE), file.path(new_path, "districts.shp"))
        sf::write_sf(set_geom(munis, geom_only = FALSE), file.path(new_path, "munis.shp"))
    }
    return(regions)
}
    
get_shape_dt <- function(shapes, idx, years) {
    ### Eine kleien Hilfsfunktion, welche aus den eingelesenen
### shapefiles einen data.fram / data.table erstellt.

    ## would be nicer if it accepts "us" as input and sets idx
    ## accordingly.

    dt <- lapply(shapes, function(year) return_el(year, idx))
    dt <- lapply(1:length(years), function(i) dt[[i]][, "year" := years[i]])
    dt <- rbindlist(dt)
    setkeyv(dt, c("AGS", "year"))
    return(dt)    
}    

smallup_shp <- function(shp, type) {
    ### probably I dont really need year_min and year_max because
    ### joining with AGS only is fine, even if years differ. It seems
    ### that AGS do not refer to completely different entities.

    ### Although now there are some names missing. Maybe possible to
    ### use not first but last row in .SD[last]? I think in later
    ### years names were there
    AGS <- NULL
    shp[, "year_min" := as.numeric(NA)]
    shp[, "year_max" := as.numeric(NA)]
    shp[, "year_min" := min(year), by = AGS]
    shp[, "year_max" := max(year), by = AGS]
    shp[, "year" := NULL]
    if (type == "ags") {
        shp <- shp[, .SD[1],  by = AGS]
    } else {
        group <- colnames(shp)[!colnames(shp) %in% "geometry"]
        shp <- shp[, .SD[1],  by = AGS]
    }
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

### When I read in the shapefiles I know which region type it is so I
### need not check the number of regions. Should fix it sometime but
### it works for now.
    
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
    munis <- right_crs(munis)
    munis <- data.table::setDT(munis)

    states <- sf::read_sf(file.path(path, states_file), type = 6)
    states <- right_crs(states)
    states <- data.table::setDT(states)

    districts <- sf::read_sf(file.path(path, districts_file), type = 6)
    districts <- right_crs(districts)
    districts <- data.table::setDT(districts)
    
    shapes <- list("state" = states, "district" = districts, "muni" = munis)
    return(shapes)    
}

right_crs <- function(shp) {
    ### it seems from reading the shapfiles that at least for the
    ### years 2000 to 2012 the crs is WGS 84 / UTM 32N. For years from
    ### 2013 to 2018 it is ERTS 89/ UTM 32N. I don't know for other
    ### years. This function quickly checks the crs and transforms if
    ### necessary

    crs <- sf::st_crs(shp)$input
    if (grepl("ERTS89", crs) == FALSE) {
        shp <- sf::st_transform(shp, 25832) ## EPSG code for ERTS89 / UTM 32N
    }
    return(shp)
}
