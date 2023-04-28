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
        message(sprintf("Column(s) '%s' will be used as additional key colum(s).", cols))
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
    print(flows3)
    if (round == TRUE) {
        flows3[, "flow" := as.integer(round(flow, 0))]
    }
    return(flows3)
}

correct_flow <- function(dt, cor_dt, ags_col, year_col = "year") {
    by_x <- c(ags_col, year_col)
    by_y <- c("ags_old", "year")
    tab_c <- merge(dt, cor_dt[, .(ags_old, ags_new, conv_p, year)],
                   by.x = by_x, by.y = by_y,
                   allow.cartesian = TRUE,
                   all.x = TRUE)
    check_flows(tab_c, dt, hard = TRUE)
    tab_c[, "flow_new" := flow * conv_p]
    no_keys <- c(ags_col, "conv_p", "flow", "flow_new")
    keys <- setdiff(colnames(tab_c), no_keys)
    tab_c <- tab_c[, .(flow_new = sum(flow_new)),
                   keyby = keys]
    check_flows(tab_c, dt, hard = FALSE)
    setnames(tab_c, c("ags_new", "flow_new"), c(ags_col, "flow"))
    return(tab_c)
}

##' Corrects the flows where either orgin or destination is the key.
##'
##' Expects two data.tables. This is bad design, I think it would be
##' better to allow the user to specify the column that is to be
##' adjusted and the column holding the AGS
##' @title Adjust flows for municipalitiy changes
##' @param flows data.table with columns origin, destination, flow, year
##' @param dt data.table correction information
##' @param key character origin or destination
##' @return data.table
##' @import data.table
##' @author Konstantin
correct_flows_ <- function(to_correct, corrections, ags_col,
                           year_col = "year") {
    ### probably not a good idea to bury check_flows() inhere
    ags_old <- ags_new <- conv_p <- year <- flow <- NULL
    flow_new <- destination <- origin <- .SD <- . <- NULL
    key1 <- c(ags_col, year_col) ## key for joining/ adusting
    print(paste("joinkey", key1))
    ## key2 key for grouping
    key2 <- setdiff(colnames(to_correct), c(key1, "flow", "flow_new"))
    print(paste("groupkey", key2))
    ## all.x and allow.cartesian are necessary (?) bc for some ags there
    ## are several new ags, so several rows are joined
    flows2 <- merge(to_correct,
                    corrections[, .(ags_old, ags_new, conv_p, year)],
                    by.x = key1,
                    by.y = c("ags_old", "year"),
                    all.x = TRUE, allow.cartesian = TRUE)
    flows2[, "flow_new" := flow * conv_p]
    keys <- c("ags_new", key2, "year")
    flows2 <- flows2[, .(flow_new = sum(flow_new)),
                     keyby = keys]
##     check_flows(flows2, flows, hard = TRUE)
    ## there might be several rows, why again?
    flows2 <- flows2[, .SD[1], keyby = keys]
    setnames(flows2, "ags_new", ags_col)
##    check_flows(flows2, flows, hard = FALSE)
    return(flows2)
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

##' Checks if number of flows is stays the same when adjusting for
##' municipality changes
##'
##' @title Check if number of flows stays the same
##' @param flows_new data.table The new flows object that is to be
##'     checked.
##' @param flows_old data.table that is used as ground truth
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
##' @import data.table
##' @return NULL
##' @author Konstantin
check_flows <- function(flows_new, flows_old, hard = TRUE) {
    flow <- .SD <- flow_new <- NULL
    flows_exp <- flows_old[, sum(flow, na.rm = TRUE)]
    if (hard == TRUE) {
        cols <- c("origin", "destination", "year", "EF25")
        flows_n <- flows_new[, .SD[1], keyby = cols][, sum(flow, na.rm = TRUE)]

        if (flows_exp == flows_n) {
            mes <- sprintf("Flows after merge as expected! Flows: %s",
                           flows_exp)
            message(mes)
        }
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
    return(NULL)
}
