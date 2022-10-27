
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


## fpath <- function(path, fname, type = NULL) {
##     ### probably not working under windows
##     fullpath <- file.path(path, fname)
##     if (!is.null(type)) {
##         fullpath <- paste(fullpath, type, sep = ".")
##         }
##     return(fullpath)
## }


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

##' Saves plot and data used plotting
##'
##' ggsave_d() takes a plot object and saves the plot. Additionally it
##' saves the data that was used to create the plot. If not present
##' ggsave_d() creates in path one directory called plots and one
##' called plot_data. The data is saved under the same name as the
##' plot and it is saved as .csv.
##' @title Save plot and data used for plotting
##' @param plot plot created with ggplot()
##' @param plot_name Name of plot file and .csv file on disk. Specify
##'     without file ending. So it is "regions_flow" instead of
##'     "regions_flow.pdf"
##' @param path Directory where "plots" and "data_plots"
##'     subdirectories are created
##' @param data Optional. If given this data is saved instead of the
##'     one in the plot object. This is useful if the data called by
##'     ggplot() is only used to draw map and some differnt data set
##'     is used to fill map.
##' @param ... Additional parameters passed to ggplot2::ggsave(). See
##'     ?ggplot2::ggsave
##'@return NULL. Saves plot and data to disk. Data is saved as
##'     .csv. Plot saved as .pdf by default but other formats can be
##'     used. See ?ggplot2::ggsave
##' @export ggsave_d
##' @importFrom methods is
##' @author Konstantin
ggsave_d <- function(plot, plot_name, path, data = NULL, ...) {
    ### now filename and plot args are swapped compared to
### ggplot2::ggsave

    ### checking name for file ending would be nice
    if (methods::is(plot) != "gg") {
        stop("Plot should be result from ggplot()")
    }
    ## if (grepl(".", plot_name) == TRUE) {
    ##     warning("Did you specify file ending in plot_name. Better without ending")
    ## } Apparently "." tests for any character
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
    if (is.null(data)) {
        if(ncol(plot$data) <= 1) {
            warning("looks like in plot$data is no actual data! Did you use different data set in plot as well? If so, specify using data argument")
        }
        dt <- copy(plot$data)
        message("data from plot object is saved")
    } else {
        dt <- copy(data)
    }
    
    if ("geom" %in% colnames(dt)) {
        dt[, "geom" := NULL]
        message("geom column dropped before saving")
    }
    if ("geometry" %in% colnames(dt)) {
        dt[, "geometry" := NULL]
        message("geometry column dropped before saving")
}
    data.table::fwrite(x = dt, file = fpath)
    if(file.exists(fpath)) {
        message("csv of plot data written to disk")}
    ## not save bc file might exists and is not created anew
}

create_od <- function(o, d) {
    m1 <- as.matrix(o)
    m2 <- as.matrix(d)
    m <- paste(m1, m2, sep = "_")
    return(m)
}

##' Custom ggplot theme
##'
##' This theme makes it easy to specify the desired grid for a ggplot
##' object. Inspired by https://www.youtube.com/watch?v=48-ymyX6PlU
##' @title Own brrrp theme
##' @param maj.x major x grid, logical
##' @param maj.y major y grid, logical
##' @param min.x minor x grid, logical
##' @param min.y minor y grid, logical
##' @return theme for ggplot object
##' @import ggplot2
##' @export theme_brrrp
##' @author Konstantin
theme_brrrp <- function(leg.pos = NULL, maj.x = FALSE, maj.y = FALSE, min.x = FALSE, min.y = FALSE) {
    if (inherits(c(maj.x, maj.y, min.x, min.y), "logical") == FALSE ) {
        stop("maj.x, maj.y, min.x, min.y all have to be TRUE or FALSE (logical)")
    }
    brrrp <- ggplot2::theme_bw()
    if(!maj.x) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    } 
    if(!maj.y) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    } 
    if(!min.x) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    } 
    if(!min.y) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
    if (!is.null(leg.pos)) {
        brrrp <- brrrp + theme(legend.position = leg.pos)
    }
    brrrp <- brrrp + theme(plot.title = element_text(colour = "blue", face = "bold",
                                                     hjust = 0.5))
    return(brrrp)
}

unload_migstat <- function() {
    detach("package:MigStat", unload = TRUE)
}
