##' Reads the example migration statistics data set and one
##' shapefile. Mainly used to quickly load data for unit tests in a
##' consistent manner.
##' 
##' @title Read example migration statistics and corresponding
##'     shapefile.
##' @param mig_path Path to migration statistics
##' @param shp_path path to shapefile
##' @return named list with example migration statistics as first
##'     element and shapefile as second.
##' @author Konstantin
read_examples <- function(mig_path = def_p("mig"), shp_path = def_p("shp")) {

    dt <- read_migex(mig_path)
    shps <- read_shapes(shp_path) 
    data <- list("mig" = dt, "shps" = shps)

    return(data)
    
}

read_migex <- function(file) {

    df <- foreign::read.spss(file, to.data.frame = TRUE)
    dt <- data.table::setDT(df)
    
    return(dt)
    
}

def_p <- function(ds) {
    stopifnot(ds %in% c("mig", "shp"))
    mig_path <- "~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav"
    mig_path <- "/home/konstantin/Documents/Diss/inst/extdata/examples_fdz/WandZuzug_dummy_2010-2013_4480-2021.sav"
##    shp_path <-  "/home/konstantin/Diss/inst/extdata/vg250-ew_3112.utm32s.shape.ebenen/vg250-ew_ebenen"
    shp_path <-  "~/Documents/Diss/inst/extdata/shapes/no_ewz"
    shp_path <- "~/extdata/shapes31_test/no_ewz"
    if (ds == "mig") {
        path  <- mig_path
    }
    if (ds == "shp") {
        path <- shp_path
    }
    return(path)
}
