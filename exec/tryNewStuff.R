ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)



get_wins2 <- function(flows, grouped = TRUE, by = NULL) {
    wins <- copy(flows)
    if (grouped == TRUE) {
        if (is.null(by)) {
            nogroup <- c("origin", "destination", "flow", "distance")
            by <- setdiff(colnames(flows), nogroup)
        }
        message(sprintf("wins grouped by '%s' are returned", paste(by, collapse = ", ")))

        wins <- wins[, "wins" := sum(flow), by = c("destination", by)]
        wins <- wins[, .SD[1], by = c("destination", by)]
        wins <- wins[order(destination)]
        wins <- wins[, c("flow", "distance", "origin") := NULL]
    } else {
        wins <- wins[, "wins" := sum(flow), by = c("destination")]
        wins <- wins[, .SD[1], by = c("destination")]
        wins <- wins[order(destination)]
        wins <- wins[, "region" := destination]
        wins <- wins[, .(region, wins)]
    }
    return(wins)
}

wins <- get_wins2(flows, FALSE)
wins[, sum(wins)]


get_losses2 <- function(flows, grouped = TRUE, by = NULL) {
    losses <- copy(flows)
    if (grouped == TRUE) {
        if (is.null(by)) {
            nogroup <- c("origin", "destination", "flow", "distance")
            by <- setdiff(colnames(flows), nogroup)
        }
        message(sprintf("losses grouped by '%s' are returned", paste(by, collapse = ", ")))

        losses <- losses[, "losses" := sum(flow), by = c("origin", by)]
        losses <- losses[, .SD[1], by = c("origin", by)]
        losses <- losses[order(origin)]
        losses <- losses[, c("flow", "distance", "destination") := NULL]
    } else {
        losses <- losses[, "losses" := sum(flow), by = c("origin")]
        losses <- losses[, .SD[1], by = c("origin")]
        losses <- losses[order(origin)]
        losses <- losses[, "region" := origin]
        losses <- losses[, .(region, losses)]
    }
    return(losses)
}


losses <- get_losses2(flows, TRUE)


get_grouped <- function(flows, region, grouped = TRUE, by = NULL) {
    stopifnot(region %in% c("origin", "destination"))
    if(region == "origin") {
        type <- "losses"
    } else {
        type  <- "wins"
    }
    losses <- copy(flows)
    if (grouped == TRUE) {
        if (is.null(by)) {
            nogroup <- c("origin", "destination", "flow", "distance", "region")
            by <- setdiff(colnames(flows), nogroup)
        }
        message(sprintf("%s grouped by '%s' are returned", type, paste(by, collapse = ", ")))

        losses <- losses[, paste(type) := sum(flow), by = c(region, by)]
        losses <- losses[, .SD[1], by = c(region, by)]
        losses <- losses[order(get(region))]
        losses <- losses[, "region" := get(region)]
        losses <- losses[, c("flow", "distance", "destination", "origin") := NULL]
    } else {
        losses <- losses[, paste(type) := sum(flow), by = c(region)]
        losses <- losses[, .SD[1], by = c(region)]
        losses <- losses[order(get(region))]
        losses <- losses[, "region" := get(region)]
        losses <- losses[, .(get("region"), get(type))]
        colnames(losses) <- c("region", paste(type))
    }
    return(losses)
}

test <- get_grouped(flows, "destination", TRUE)
test <- get_grouped(flows, "origin", FALSE)
test[order(region)]
test[region == "09", sum(losses)]

mig[EF03U2 == "09", ]

test <- get_grouped(flows, "destination")

test[region == "09", sum(losses)]

mig[EF02U2 == "09", ]
var <- "wins"
test[,  paste(var) := type]

paste(var)

test[, .(get("gender"), get("region"), get(var))]

