ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)


get_grouped <- function(flows, reg, grouped = TRUE, by = NULL) {
    stopifnot(reg %in% c("origin", "destination"))
    if(reg == "origin") {
        type <- "losses"
    } else {
        type  <- "wins"
    }
    dt <- copy(flows)
    if (grouped == TRUE) {
        if (is.null(by)) {
            nogroup <- c("origin", "destination", "flow", "distance", "region")
            by <- setdiff(colnames(flows), nogroup)
        }
        message(sprintf("%s grouped by '%s'", type, paste(by, collapse = ", ")))

        dt <- dt[, paste(type) := sum(flow), by = c(reg, by)]
        dt <- dt[, .SD[1], by = c(reg, by), .SDcols = c(type)]
        colnames(dt)[colnames(dt) %in% c("origin", "destination")]  <- "region"
    } else {
        dt <- dt[, paste(type) := sum(flow), by = reg]
        dt <- dt[, .SD[1], by = reg, .SDcols = c(type)]
        colnames(dt)[colnames(dt) %in% c("origin", "destination")]  <- "region"
    }
    return(dt)
}



wins <- get_grouped(flows = flows, reg = "origin", grouped = TRUE, by = "age_gr")

get_losses <- function(flows, grouped, by = NULL) {
    losses <- get_grouped(flows, reg = "origin", grouped = grouped, by = by)
    return(losses)
}

get_wins <- function(flows, grouped, by = NULL) {
    wins <- get_grouped(flows, reg = "destination", grouped = grouped, by = by)
    return(wins)
}

get_wins(flows, TRUE)

