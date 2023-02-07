##' Sets paths for current working environment
##'
##' @title sets paths for working at the FDZ or at work
##' @param p_work paths for work
##' @param p_fdz paths for FDZ
##' @param fdz logical, working at the fdz or not?
##' @return list with paths
##' @author Konstantin
make_paths <- function(which = c("work", "kamenz", "dresden"), paths = NULL) {
    which <- match.arg(which)
    p_dresden <- list(
        ### not needed anymore
        fig = "E:\\FDZ_II\\Guckel\\Übergaben\\2022-11-02\\Graphiken/",
        mig = "E:\\FDZ_II\\Guckel\\Daten\\Zuzug",
        shps = "E:\\FDZ_II\\Guckel\\Daten\\shapeFiles\\Shapefiles_281022\\4480_Shapefiles_281022",
        data = "E:\\FDZ_II\\Guckel\\Daten"
    )
    p_kamenz <- list(
        fig = "H:\\Übergaben\\2022-11-25\\output",
        mig = "H:\\Daten",
        shps = "H:\\Daten\\shapes",
        dist = "H:\\Daten\\Distanzen"
    )
    p_work <- list(
        mig = "~/extdata/simulated_moves/",
        shps = "~/extdata/shapes31simple2/",
        dist = "~/extdata/distances/",
        fig = "~/Diss/exec/analysis/figs/fdz_sim_new"
    )
    path_list <- list("dresden" = p_dresden, "kamenz" = p_kamenz,
                      "work" = p_work)
    if (is.null(paths)) {
        paths <- path_list[[which]]
    } else {
        needed_paths <- c("mig", "shps", "dist", "fig")
        stopifnot(needed_paths %in% names(paths))
    }
    return(paths)
}

