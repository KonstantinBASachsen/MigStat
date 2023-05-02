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

##' Adds abbreviated names to federal states
##'
##' @title Short labels for federal states 
##' @param shp shapefile for federal states
##' @return data.table
##' @import data.table
##' @export gen_state_labels
##' @author Konstantin
gen_state_labels <- function(shp) {
    AGS <- GEN <- . <- NULL
    lbls_dt <- shp[, .(AGS, GEN)]
    lbls_dt[, "GEN_abbr" := c("SH", "HH", "NS", "Bre", "NRW", "Hes",
                          "RLP", "BaW\u00FC", "Bay", "Saa", "Ber", "Bran",
                          "MP", "Sac", "LSA", "T")]
    return(lbls_dt)
}

##' Joins state labels to flow data and creates labels for plotting
##'
##' @title Joins state labels for plotting labels
##' @param flows data.table of flows
##' @param lbls_dt data.table of abbreviated names and AGS
##' @param sep seperator for labels. So labels will be Ber sep HH,
##'     default Ber > HH to indicate flows from Berlin to Hamburg
##' @return data.table
##' @import data.table
##' @export join_state_labels
##' @author Konstantin
join_state_labels <- function(flows, lbls_dt, sep = " > ") {
    AGS <- GEN_abbr <- lbl_o <- lbl_d <- . <- NULL
    flows <- merge(flows, lbls_dt[, .(AGS, GEN_abbr)], by.x =  "origin", by.y = "AGS")
    data.table::setnames(flows, "GEN_abbr", "lbl_o")
    flows <- merge(flows, lbls_dt[, .(AGS, GEN_abbr)], by.x =  "destination", by.y = "AGS")
    data.table::setnames(flows, "GEN_abbr", "lbl_d")
    flows[, "lbl" := paste(lbl_o, lbl_d, sep = sep)]
    flows[, c("lbl_o", "lbl_d") := NULL]
    return(flows)
}
