
get_unitcol <- function(us, dest) {
    #####
    ### This function allows me to say which administrative region I
    ### am interested in and if I care about in or outmigration and it
    ### returns the column name
    stopifnot(us %in% c("st", "di", "mu"))    
    stopifnot(is.logical(dest))
    unit <- get_shp_unit(us)
    if (dest == TRUE) {unit <- paste0(unit, "_d")}
    if (dest == FALSE) {unit <- paste0(unit, "_o")}
    return(unit)
}

get_agscol<- function(unit) {

    ### this function returns the column holding the ags for thei
    ### interesting region and type of migration
    
    strings <- strsplit(unit, split = "_")[[1]]
    unit <- strings[1]
    type <- strings[2]
    stopifnot(unit %in% c("state", "district", "muni"))
    stopifnot(type %in% c("d", "o"))
    unit_num <- NA
    if(unit == "state") {unit_num <- "2"}
    if(unit == "district") {unit_num <- "4"}
    if(unit == "muni") {unit_num <- "5"}
    type_num <- NA
    if(type == "d") {type_num <- "02"}
    if(type == "o") {type_num <- "03"}
    ags_col <- paste0("EF", type_num, "U", unit_num)
    return(ags_col)
}


keep_cols <- function(dt, keep) {
    stopifnot(is.data.table(dt))
    dt_clean <- dt[, .SD, .SDcols = keep]
    return(dt_clean)
}

##' Looks in data.table for "geometry" column and uses it to create
##' simple features object.
##'
##' @title set geometry attribute in data.table
##' @param dt data.table with geometry attribute in column called
##'     "geometry"
##' @param geom_only If true all columns are dropped and only geometry
##'     set is returned.
##' @return If geom_only = FALSE, object of class("sf",
##'     "data.table"). If geom_only == TRUE, class("sfc")
##' @import sf
##' @export set_geom
##' @author Konstantin
set_geom <- function(dt, geom_only = T) {
    geometry <- NULL
    dtgeom <- sf::st_set_geometry(dt, dt[, geometry])
    if (geom_only == TRUE) {
        dtgeom <- sf::st_geometry(dtgeom)
    }
    return(dtgeom)
}

object_size <- function(object, units = "Mb") {
    size <- format(utils::object.size(object), units = units)
    return(size)
}


create_od <- function(o, d) {
    m1 <- as.matrix(o)
    m2 <- as.matrix(d)
    m <- paste(m1, m2, sep = "_")
    return(m)
}


unload_migstat <- function() {
    detach("package:MigStat", unload = TRUE)
}

##' Joins column from one data.table to another. Optionally performs
##' full join.
##'
##' @title Joins column fast
##' @param dt1 data.table where column is joined to
##' @param dt2 data.table to join column from
##' @param new_col Character, name of new column
##' @param join_col Character, name of column to be joined
##' @param key1 character, key in first data.table
##' @param key2 character, key in second data.table
##' @param ... Optional arguments passed to merge.
##' @return data.table, dt1 with joined column
##' @export do_join
##' @author Konstantin
do_join <- function(dt1, dt2, join_col, key1, key2 = "AGS", new_col = NULL, ...) {
### passing arguments in this way to merge...?
    if (!is.null(new_col)) {
        cols <- colnames(dt2)
        colnames(dt2)[grep(join_col, cols)] <- new_col
        keep <- c(key2, new_col)
    } else {
        keep <- c(key2, join_col)
    }
    dt1 <- data.table::merge.data.table(dt1,
                                        dt2[, .SD, .SDcols = keep],
                                        by.x = key1, by.y = key2, ...)
    return(dt1)
}

##' 
##'
##' Reads migration statistics or actually any other tabular data that
##' can be procesed by data.table::fread
##' @title Read migration statistics
##' @param path Path to migration statistics
##' @param file File name without ending
##' @param type character (optional) of file type.
##' @return data.table
##' @export read_mig
##' @importFrom data.table fread
##' @author Konstantin
read_mig <- function(path, file, type = "csv") {
### function assumes that the different mig versions given by type are
### saved and reads the chosen one. Seems a bit complicated. Maybe it
### is better to just specify the file name?
    if (grepl("\\.", file) == TRUE) {
        stop("Detected '.' in file. Please specify without file ending.")
    }
    file <-  paste0(file, ".", type)
    mig <- data.table::fread(file.path(path, file),
                             encoding = "UTF-8")
    return(mig)
}
