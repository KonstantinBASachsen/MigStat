######################################################################
####################### Purpose of script ############################
######################################################################

## Here I collect functions that I use in the fdz. These are helper
## functions designed to ease a specific task. They are not general or
## robust enough to be used in other settings.


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




