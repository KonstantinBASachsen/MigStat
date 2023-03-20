##' Prepares data.table of flows for modeling
##'
##' 
##' @title Preparing flows for modeling
##' @param flows data.table of regional flows
##' @param omit_intra logical. If TRUE then observations where origin
##'     == destination are omitted.
##' @param log_offset numeric. Since log transform is used for the
##'     flows, flows need to be > 0. Flows that are 0, are set to
##'     log_offset.
##' @return data.table of flows
##' @import data.table
##' @export prepare_modeling
##' @author Konstantin
prepare_modeling <- function(flows, omit_intra, log_offset = 0.1) {
    stopifnot(is.logical(omit_intra))
    flow <- origin <- destination <- NULL
    flows[, "flow" := as.double(flow)]
    message("typeof(flow) now 'double'")
    flows[flow == 0, "flow" := log_offset]
    message(sprintf("0 flows set to %s for log transorm", log_offset))
    if (omit_intra == TRUE) {
        flows <- flows[! origin == destination, ] ## slow for large data?
    }
    return(flows)
}
