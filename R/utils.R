
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
##' @param leg.pos legend position as coordinates like c(0.1, 0.7) for
##'     upper left or c(0.8, 0.2) for lower right
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

return_el <- function(l, idx) {
    el <- l[[idx]]
    return(el)
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
##' @param full logical, if TRUE full join is done
##' @return data.table, dt1 with joined column
##' @export do_join
##' @author Konstantin
do_join <- function(dt1, dt2, new_col, join_col, key1, key2 = "AGS", full = FALSE) {
    ## performs full join
    i.GEN <- AGS <- i.geometry <- NULL
    if(full == TRUE) {
        data.table::setkeyv(dt1, key1)
        unique_keys <- unique(c(dt1[, get(key1)], dt2[, get(key2)]))
        dtu <- dt1[unique_keys]
    } else {
        dtu <- dt1
    }
    data.table::setkeyv(dt1, key1)
    data.table::setkeyv(dt2, key2)
    dt1[dt2, (new_col) :=get(paste0("i.", join_col))]
    return(dtu)

}


read_mig <- function(path, year) {
  ### liest die Wanderungsdaten ein
    dt <- data.table::fread(path, encoding = "UTF-8")
    cols <- c("EF02U5", "EF03U5", "EF02U2", "EF03U2", "EF25")
    dt <- dt[, .SD, .SDcols = cols]
    return(dt)
}

read_mig <- function(path, type) {
### function assumes that the different mig versions given by type are
### saved and reads the chosen one. Seems a bit complicated. Maybe it
### is better to just specify the file name?
    
  if (! type %in% c("age", "complete", "sample", "reasonable", "simulation")) {
    stop("type either 'age', 'complete', 'sample', 'reasonable' or 'simulation'")
  }
  ## lol better with paste _type.csv
  if(type == "age") {
    file <- "mig.csv"
  } else if (type == "complete") {
    file <- "mig_complete.csv"
  } else if (type == "reasonable") {
    file <- "mig_reasonable.csv"
  } else if (type == "simulation") {
      file <- "moves_unif_year.csv"
  } else {
    file <- "mig_sample.csv"
  }
    mig <- data.table::fread(file.path(path, file),
                           encoding = "UTF-8")
    return(mig)
}

##' Sets paths for current working environment
##'
##' @title sets paths for working at the FDZ or at work
##' @param p_work paths for work
##' @param p_fdz paths for FDZ
##' @param fdz logical, working at the fdz or not?
##' @return list with paths
##' @author Konstantin
make_paths <- function(p_work, p_fdz, fdz) {
    stopifnot(is.logical(fdz))
    if (fdz == TRUE) {
        paths <- p_fdz
    } else {
        paths <- p_work
    }
}

guess_region <- function(dt) {
### lol probably I can check easier which is appropriate like checking
### number of characters of ags
    
  if(nrow(dt) < 1000) {
    region <- "Bundeslaender"
  }
  if(1000 < nrow(dt) & nrow(dt) < 1e+6) {
    region <- "Kreise"
  }
  if (1e+6 < nrow(dt)) {
    region <- "Gemeinden"
  }
  return(region)
}
