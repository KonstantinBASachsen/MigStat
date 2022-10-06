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
    losses <- copy(flows)
    if (grouped == TRUE) {
        if (is.null(by)) {
            nogroup <- c("origin", "destination", "flow", "distance", "region")
            by <- setdiff(colnames(flows), nogroup)
        }
        message(sprintf("%s grouped by '%s' are returned", type, paste(by, collapse = ", ")))

        losses <- losses[, paste(type) := sum(flow), by = c(reg, by)]
##        losses <- losses[, "region" := get(reg)]
        losses <- losses[, .SD[1], by = c(reg, by), .SDcols = c(type)]
    } else {
        losses <- losses[, paste(type) := sum(flow), by = reg]
  ##      losses <- losses[, "region" := get(region)]
        losses <- losses[, .SD[1], by = reg, .SDcols = c(type)]
    }
    return(losses)
}

wins <- get_grouped(flows = flows, reg = "origin", grouped = FALSE)
var <- "gender"
type <- "wins"
colnames(flows) %in% c("region", type)
