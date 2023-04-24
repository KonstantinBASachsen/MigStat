##' Corrects flows for changes in administrative boundaries
##'
##' correct_flows uses a table that is based upon the corrections for
##' changes in administrative boundaries from the BBSR to correct
##' flows between regions.
##' @title Corrects flows for changes in administrative boundaries
##' @param flows data.table of flows. Expects columns c("origin",
##'     "destination", "year")
##' @param dt data.table of conversion factors. Expects columns
##'     c("year", "ags_old", "ags_new", "conv_p").
##' @param round logical. If TRUE adusted flows are rounded to the
##'     nearest integer.
##' @return data.table
##' @import data.table
##' @export correct_flows
##' @author Konstantin
##' @examples
##' flows <- data.table::data.table(origin = c(1051, 15003, 1004),
##'                                      destination = c(16014, 16018, 16038),
##'                                      flow = c(100, 1000, 500),
##'                                      year = 1990)
##'
##' correction_dt <- data.table::data.table(ags_old = c(1004, 1051,
##' 15003, 16014, 16014, 16018, 16018, 16018, 1601, 16018, 16038),
##' ags_new = c(1004, 1051, 15003, 16065, 16068, 16051, 16067, 16068,
##' 16070, 16071, 16068),
##' conv_p = c(1, 1, 1, 0.962118141, 0.037881859, 0.334847584,
##'     0.241041861, 0.378831621, 0.004988537, 0.040290397, 1),
##' year = 1990)
##' 
##' flows2 <- correct_flows(flows, correction_dt)
correct_flows <- function(flows, dt, round = TRUE) {
    flow <- NULL
        #### checks flows data.table. "year" is always required. Other
#### grouping variables are not allowed.
    flows_cols <- c("origin", "destination", "year", "flow")
    cols <- setdiff(flows_cols, colnames(flows))
    if (length(cols) > 0) {
        cols <- paste(cols, collapse = ", ")
        stop(sprintf("Column(s) %s not found", cols))
    }
    cols <- setdiff(colnames(flows), flows_cols)
    if (length(cols) > 0) {
        cols <- paste(cols, collapse = ", ")
        stop(sprintf("Column(s) %s are not supposed to be in 'flows'.", cols))
    }
    ### checks data.table with the
    #### corrections
    cols <- setdiff(c("year", "ags_old", "ags_new", "conv_p"),
                    colnames(dt))
    if (length(cols) > 0) {
        cols <- paste(cols, collapse = ", ")
        stop(sprintf("Column(s) %s not found", cols))
    }
    check_ags_can_be_found(flows = flows, dt = dt, region = "origin")
    flows2 <- correct_flows_(flows, dt, key = "origin")
    check_ags_can_be_found(flows = flows, dt = dt, region = "destination")
    flows3 <- correct_flows_(flows2, dt, key = "destination")
    if (round == TRUE) {
        flows3[, "flow" := as.integer(round(flow, 0))]
    }
    return(flows3)
}

correct_flows_ <- function(flows, dt, key) {
    ags_old <- ags_new <- conv_p <- year <- flow <- NULL
    flow_new <- destination <- origin <- .SD <- . <- NULL
    key1 <- key ## key for joining/ adusting
    ## key2 key for grouping
    key2 <- setdiff(c("origin", "destination"), key1)
    ## all.x and allow.cartesian are necessary (?) bc for some ags there
    ## are several new ags, so several rows are joined
    flows2 <- merge(flows,
                    dt[, .(ags_old, ags_new, conv_p, year)],
                    by.x = c(key1, "year"),
                    by.y = c("ags_old", "year"),
                    all.x = TRUE, allow.cartesian = TRUE)
    flows2[, "flow_new" := flow * conv_p]
    keys <- c("ags_new", key2, "year")
    flows2 <- flows2[, "flow_new" := sum(flow_new),
                     keyby = keys]
    check_flows(flows2, flows, hard = TRUE)
    flows2 <- flows2[, .SD[1], keyby = keys]
    check_flows(flows2, flows, hard = FALSE)
    if (key1 == "origin") {
        flows2 <- flows2[, .(origin = ags_new, destination,
                             flow = flow_new, year)]
    }
    if (key1 == "destination") {
        flows2 <- flows2[, .(origin, destination = ags_new,
                             flow = flow_new, year)]
    }
    return(flows2)
}

check_ags_can_be_found <- function(flows, dt,
                                   region = c("origin", "destination")) {
    year <- origin <- ags_old <- NULL
    y_min <- flows[, min(year)]
    y_max <- flows[, max(year)]
    region <- match.arg(region)
    not_found_n <- lapply(y_min:y_max, function(y)
        length(setdiff(flows[year == y, unique(get(region))],
                       dt[year == y, unique(ags_old)])))
    not_found_n <- as.integer(not_found_n)
    if (sum(not_found_n) == 0) {
        message(sprintf("All AGS were found for '%s'.", region))
        tab <- NULL
    }
    if (sum(not_found_n) > 0) {
        tab <- data.table::data.table(y_min:y_max, not_found_n)
        warning(sprintf("Several AGS were not found for '%s'!", region))
        warning(print(tab))
    }
    return(tab)
}

check_flows <- function(flows_new, flows_old, hard = TRUE) {
    flow <- .SD <- flow_new <- NULL
    flows_exp <- flows_old[, sum(flow, na.rm = TRUE)]
    if (hard == TRUE) {
        cols <- c("origin", "destination", "year")
        flows_n <- flows_new[, .SD[1], keyby = cols][, sum(flow, na.rm = TRUE)]
        if (flows_exp != flows_n) {
            mes <- sprintf("Flows after merge not as expected! Expected %s, got %s",
                           flows_exp, flows_n)
            stop(mes)
        }
    }
    if (hard == FALSE) {
        flows_n <- flows_new[, sum(flow_new, na.rm = TRUE)]
        ## testing with flows_n == flows_exp bad idea bc sometimes are
        ## very small differences that do not matter but then test
        ## shows difference
        flows_diff <- flows_exp - flows_n
        if (abs(flows_diff) < 1) {
            mes <- sprintf("Flows after correction as expected: %s", flows_n)
            message(mes)
        }
        if (abs(flows_diff) >= 1) {
            mes <- sprintf("Adjusted flows not as expected! Expected %s, got %s. Might be because some AGS could not be found.",
                           flows_exp, flows_n)
            warning(mes)
        }
    }
}
