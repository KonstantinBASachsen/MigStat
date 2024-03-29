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
        message(sprintf("Column(s) '%s' will be used as additional key column(s).", cols))
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
    flows2 <- correct_flow(flows, dt, ags_col = "origin")
    check_ags_can_be_found(flows = flows, dt = dt, region = "destination")
    flows3 <- correct_flow(flows2, dt, ags_col = "destination")
    if (round == TRUE) {
        flows3[, "flow" := as.integer(round(flow, 0))]
    }
    return(flows3)
}

correct_flow <- function(dt, cor_dt, ags_col, year_col = "year") {
    . <- ags_old <- ags_new <- conv_p <- flow <- flow_new <- NULL
    by_x <- c(ags_col, year_col)
    by_y <- c("ags_old", "year")
    tab_c <- merge(dt, cor_dt[, .(ags_old, ags_new, conv_p, year)],
                   by.x = by_x, by.y = by_y,
                   allow.cartesian = TRUE,
                   all.x = TRUE)
    no_keys <- c("conv_p", "flow", "flow_new", "ags_new")
    keys <- setdiff(colnames(tab_c), no_keys)
    check_flows(tab_c, dt, keys, hard = TRUE)
    tab_c[, "flow_new" := flow * conv_p]
    no_keys <- c(ags_col, "conv_p", "flow", "flow_new")
    keys <- setdiff(colnames(tab_c), no_keys)
    tab_c <- tab_c[, .(flow_new = sum(flow_new)),
                   keyby = keys]
    check_flows(tab_c, dt, hard = FALSE)
    setnames(tab_c, c("ags_new", "flow_new"), c(ags_col, "flow"))
    return(tab_c)
}

##' checks if all ags can be found in correction data.table
##'
##' @title Check if ags are found in correction table
##' @param flows data.table of flows
##' @param dt data.table of correction information
##' @param region character either origin or destination
##' @return data.table of number of not found ags per year
##' @import data.table
##' @author Konstantin
check_ags_can_be_found <- function(flows, dt,
                                   region = c("origin", "destination")) {
    year <- ags_old <- NULL
    years <- sort(flows[, unique(year)])
    region <- match.arg(region)
    not_found_n <- lapply(years, function(y)
        length(setdiff(flows[year == y, unique(get(region))],
                       dt[year == y, unique(ags_old)])))
    not_found_n <- as.integer(not_found_n)
    if (sum(not_found_n) == 0) {
        message(sprintf("All AGS were found for '%s'.", region))
        tab <- NULL
    }
    if (sum(not_found_n) > 0) {
        tab <- data.table::data.table(years, not_found_n)
        warning(sprintf("Several AGS were not found for '%s'!", region))
        print(tab)
    }
    return(tab)
}

##' Checks if number of flows is stays the same when adjusting for
##' municipality changes
##'
##' @title Check if number of flows stays the same
##' @param flows_new data.table The new flows object that is to be
##'     checked.
##' @param flows_old data.table that is used as ground truth
##' @param keys character specifies the columns by which the flow is
##'     summed. See Details
##' @param hard logical This is really badly implemented. If hard,
##'     then the actual number of flows in flows_new is computed
##'     differently. This is because in correct_flows() check_flows()
##'     is used two times. The first time after joining the correction
##'     table to the flows table. If the flows are not the same here
##'     something has really gone wrong and exection s
##'     stopped. Thatswhy hard == TRUE.
##'
##' It is used for the second time after adjusting the flows with the
##' setting hard == FALSE. After adjusting the flows it might indeed
##' be possible that flows are not the same. One reason, and the only
##' reason I can think of, is that some ags were not found. Here
##' instead of stopping the execution only a warning is emitted and it
##' is up to the user to decide what to do.
##' @details keys When trying to adjust for regional changes first to
##'     every ags the new ags is joined and the relevant conversion
##'     factor. When this is done then for every old ags are sometimes
##'     many rows joined and thus the number of flows in the data
##'     increases. The "true" number of flows is obtained by grouping
##'     by "keys" and computing the sum of flows for every unique
##'     key. If no other grouping variables are present this can be
##'     achieved by simply summing over c("origin", "destination",
##'     "year") but there might be additional grouping columns like
##'     age or gender.
##' @import data.table
##' @return NULL
##' @author Konstantin
check_flows <- function(flows_new, flows_old, keys, hard = TRUE) {
    flow <- .SD <- flow_new <- NULL
    flows_exp <- flows_old[, sum(flow, na.rm = TRUE)]
    if (hard == TRUE) {
        flows_n <- flows_new[, .SD[1], keyby = keys][, sum(flow, na.rm = TRUE)]
        cond <- abs(flows_exp - flows_n) < 1
        if (cond == TRUE) {
            mes <- sprintf("Unadjusted flows after merge as expected! Flows: %s",
                           flows_exp)
            message(mes)
        }
        if (cond == FALSE) {
            mes <- sprintf("Udadjusted flows after merge not as expected! Expected %s, got %s",
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
    return(NULL)
}
