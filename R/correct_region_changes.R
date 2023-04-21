correct_flows <- function(flows, dt) {
    ags_old <- ags_new <- . <- conv_p <- flow <- NULL
    flow_new <- destination <- .SD <- year <- NULL
    cols <- setdiff(c("year", "ags_old"), colnames(dt))
    if (length(cols) > 0) {
        cols <- paste(cols, collapse = ", ")
        stop(sprintf("Column(s) %s not found", cols))
    }
    check_ags_can_be_found(flows, dt)
## all.x and allow.cartesian are necessary (?) bc for some ags there
## are several new ags, so several rows are joined
    flows2 <- merge(flows,
                    dt[, .(ags_old, ags_new, conv_p, year)],
                    by.x = c("origin", "year"),
                    by.y = c("ags_old", "year"),
                    all.x = TRUE, allow.cartesian = TRUE)
    flows2[, "flow_new" := flow * conv_p]
    flows2 <- flows2[, "flow_new" := sum(flow_new),
                     by = .(ags_new, destination, year)]
    check_flows(flows2, flows, hard = TRUE)
    keys <- c("ags_new", "destination", "year")
    flows2 <- flows2[, .SD[1], keyby = keys]
    check_flows(flows2, flows, hard = FALSE)
##     flows2[!is.na(ags_new)] na's should stay so it is clear were
    ##     ags were not found

    ## cols are returned in wrong order, fix!
    return(flows2)
}

check_ags_can_be_found <- function(flows, dt) {
    year <- origin <- ags_old <- NULL
    y_min <- flows[, min(year)]
    y_max <- flows[, max(year)]
    not_found_n <- lapply(y_min:y_max, function(y)
        length(setdiff(flows[year == y, unique(origin)],
                       dt[year == y, unique(ags_old)])))
    not_found_n <- as.integer(not_found_n)
    if (sum(not_found_n) == 0) {
        message("All AGS were found")
        tab <- NULL
    }
    if (sum(not_found_n) > 0) {
        tab <- data.table::data.table(y_min:y_max, not_found_n)
        warning("Several AGS were not found!")
        warning(print(tab))
    }
    return(tab)
}

check_flows <- function(flows_new, flows_old, hard = TRUE) {
    flow <- .SD <- flow_new <- NULL
    flows_exp <- flows_old[, sum(flow)]
    if (hard == TRUE) {
        cols <- c("origin", "destination", "year")
        flows_n <- flows_new[, .SD[1], keyby = cols][, sum(flow)]
        if (flows_exp != flows_n) {
            mes <- sprintf("Flows after merge not as expected! Expected %s, got %s",
                           flows_exp, flows_n)
            stop(mes)
        }
    }
    if (hard == FALSE) {
        flows_n <- flows_new[, sum(flow_new, na.rm = TRUE)]
        if (flows_exp == flows_n) {
            mes <- sprintf("Flows after correction as expected: %s", flows_n)
            message(mes)
        }
        if (flows_exp != flows_n) {
            mes <- sprintf("Adjusted flows not as expected! Expected %s, got %s. Might be because some AGS could not be found.",
                           flows_exp, flows_n)
            warning(mes)
        }
    }
}
