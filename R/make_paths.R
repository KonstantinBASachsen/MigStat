## ##' Sets paths for current working environment
## ##'
## ##' @title sets paths for working at the FDZ or at work
## ##' @param which Character that specifies which default paths are to
## ##'     be created.
## ##' @param paths Optional, if new paths are to be used. Must contain
## ##'     paths 'mig' for migration statistics, 'shps' for shapefiles,
## ##'     'dist' for distances and 'fig' for output of figures.
## ##' @return list with paths
## ##' @export make_paths
## ##' @author Konstantin
## make_paths <- function(which = c("work", "kamenz", "dresden"), paths = NULL) {
##     which <- match.arg(which)
##     p_dresden <- list(
##         ### not needed anymore
##         fig = "E:\\FDZ_II\\Guckel\\Uebergaben\\2022-11-02\\Graphiken/",
##         mig = "E:\\FDZ_II\\Guckel\\Daten\\Zuzug",
##         shps = "E:\\FDZ_II\\Guckel\\Daten\\shapeFiles\\Shapefiles_281022\\4480_Shapefiles_281022",
##         data = "E:\\FDZ_II\\Guckel\\Daten"
##     )
##     p_kamenz <- list(
##         fig = "H:\\Uebergaben\\2022-11-25\\output",
##         mig = "H:\\Daten",
##         shps = "H:\\Daten\\shapes",
##         dist = "H:\\Daten\\Distanzen"
##     )
##     p_work <- list(
##         mig = "~/extdata/simulated_moves/",
##         shps = "~/Diss/exec/one_run/data/clean/shapes/31_12",
##         dist = "~/Diss/exec/one_run/data/clean/distances",
##         fig = "~/Diss/exec/analysis/figs/fdz_sim_new",
##         inkar = "~/Diss/exec/one_run/data/clean/inkar",
##         corrections = "~/Diss/exec/one_run/data/clean/corrections",
##         flows_districts = "~/Diss/exec/one_run/data/clean/flows_districts",
##         flows_states = "~/Diss/exec/one_run/data/clean/flows_states",
##         out = "~/Diss/exec/one_run/out"
##     )
##     path_list <- list("dresden" = p_dresden, "kamenz" = p_kamenz,
##                       "work" = p_work)
##     if (is.null(paths)) {
##         paths <- path_list[[which]]
##         if (which == "dresden" | which == "kamenz") {
##             warning("paths not correct because non ASCII characters")
##         }
##     } else {
##         needed_paths <- c("mig", "shps", "dist", "fig")
##         stopifnot("paths must contain 'mig', 'shps', 'dist' and 'fig'" = needed_paths %in% names(paths))
##     }
##     return(paths)
## }

if_not_exist_create <- function(path) {
    ### maybe use recursive option and just call make_project_paths or
    ### something. Don't I have this in MigStat?
    p <- path
    print(p)
    if (dir.exists(p) == TRUE) {
        message(sprintf("Directory %s already exists", p))
    }
    if (dir.exists(p) == FALSE) {
        dir.create(p)
        message(sprintf("Directory %s created", p))
    }
    return(NULL)
}

##' Returns a list of all paths for research project
##'
##' Returns a list of all paths that are needed for the project. This
##' list can then be used to create the paths on the file system.
##' @param project_path Parent directory where all other paths start from
##' @return list
##' @author Konstantin
##' @export make_paths
##' @examples
##' ## main_path <- "~/project"
##' ## paths <- make_paths(main_path)
##' ## if_not_exist_create(main_path)
##' ## lapply(paths, if_not_exist_create)
make_paths <- function(project_path) {
    paths <- list()
################# Data paths ###################
    paths$data <- file.path(project_path, "extdata")
###### Raw Data ##############
    paths$raw <- file.path(paths$data, "raw")
    paths$raw_shapes <- file.path(paths$raw, "shapes")
    paths$raw_corrections <- file.path(paths$raw, "corrections")
###### Clean Data ############
    paths$clean <- file.path(paths$data, "clean")
    paths$clean_shapes <- file.path(paths$clean, "shapes")
    paths$clean_corrections <- file.path(paths$clean, "corrections")
####### Distances ############
    paths$dist <- file.path(paths$clean, "distances")
########## INKAR #############
    paths$raw_ink <- file.path(paths$raw, "inkar")
    paths$clean_ink <- file.path(paths$clean, "inkar")
######### Flows states #######
    paths$raw_flows_bl <- file.path(paths$raw, "flows_states")
    paths$clean_flows_bl <- file.path(paths$clean, "flows_states")
    paths$raw_flows_di <- file.path(paths$raw, "flows_districts")
    paths$clean_flows_di <- file.path(paths$clean, "flows_districts")
    paths$clean_sim <- file.path(paths$clean, "simulations")
######## others #############    
    paths$provide <- "~/extdata/provide/"
    paths$exec <- file.path(project_path, "exec")
    paths$tidying <- file.path(paths$exec, "tidying")
    return(paths)
}
