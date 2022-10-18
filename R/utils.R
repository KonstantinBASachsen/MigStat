
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

##' normalize() adds two numbers and returns the fraction of the first.
##'
##' This function simply adds two numbers and returns the fraction of
##' the first number of the sum.
##' @title return fraction of first number of sum
##' @param x first scalar
##' @param y second scalar
##' @return numeric
##' @export normalize
##' @author Konstantin
normalize <- function(x, y) {

    n <- x / sum(x,y)
    return(n)
}


search_vec <- function(phrase, vec) {
    res <- grep(phrase, vec, value = T)
    return(res)
}

keep_cols <- function(dt, keep) {
    stopifnot(is.data.table(dt))
    dt_clean <- dt[, .SD, .SDcols = keep]
    return(dt_clean)

}


fpath <- function(path, fname, type = NULL) {
    ### probably not working under windows
    fullpath <- file.path(path, fname)
    if (!is.null(type)) {
        fullpath <- paste(fullpath, type, sep = ".")
        }
    return(fullpath)
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

ggsave_d <- function(plot_name, plot, path, ...) {
    plot_path <- file.path(path, "plots")
    data_path <- file.path(path, "plot_data")
    if (! dir.exists(plot_path)) {
        dir.create(plot_path, recursive = TRUE)
        message(sprintf("Directory to save plot created: %s", plot_path))
    }
    if (! dir.exists(data_path)) {
        dir.create(data_path, recursive = TRUE)
        message(sprintf("Directory to save data from plot created: %s", data_path))
    }
    ggplot2::ggsave(filename = paste0(plot_name, ".pdf"), plot = plot, path = plot_path, ...)
    fpath <- file.path(data_path, paste0(plot_name, ".csv"))
    data.table::fwrite(x = plot$data, file = fpath)
    if(file.exists(fpath)) {
        message("csv of plot data written to disk")}
    ## not save bc file might exists and is not created anew
}
