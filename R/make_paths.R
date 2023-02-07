##' Sets paths for current working environment
##'
##' @title sets paths for working at the FDZ or at work
##' @param which Character that specifies which default paths are to
##'     be created.
##' @param paths Optional, if new paths are to be used. Must contain
##'     paths 'mig' for migration statistics, 'shps' for shapefiles,
##'     'dist' for distances and 'fig' for output of figures.
##' @return list with paths
##' @export make_paths
##' @author Konstantin
make_paths <- function(which = c("work", "kamenz", "dresden"), paths = NULL) {
    which <- match.arg(which)
    p_dresden <- list(
        ### not needed anymore
        fig = "E:\\FDZ_II\\Guckel\\Uebergaben\\2022-11-02\\Graphiken/",
        mig = "E:\\FDZ_II\\Guckel\\Daten\\Zuzug",
        shps = "E:\\FDZ_II\\Guckel\\Daten\\shapeFiles\\Shapefiles_281022\\4480_Shapefiles_281022",
        data = "E:\\FDZ_II\\Guckel\\Daten"
    )
    p_kamenz <- list(
        fig = "H:\\Uebergaben\\2022-11-25\\output",
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
        if (which == "dresden" | which == "kamenz") {
            warning("paths not correct because non ASCII characters")
        }
    } else {
        needed_paths <- c("mig", "shps", "dist", "fig")
        stopifnot("paths must contain 'mig', 'shps', 'dist' and 'fig'" = needed_paths %in% names(paths))
    }
    return(paths)
}

