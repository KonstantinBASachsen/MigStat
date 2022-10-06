ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)


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

wins <- get_grouped(flows = flows, region = "destination", grouped = TRUE, by = "gender")
var <- "gender"
type <- "wins"
colnames(flows) %in% c("region", type)
