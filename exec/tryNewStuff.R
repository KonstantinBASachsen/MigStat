ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = "gender", full = FALSE, dist = TRUE)


get_flows_only <- function(dt, us, by = NULL, simplify = TRUE) {
    . <- flow <- NULL
    unit_o <- get_unitcol(us, FALSE)
    unit_d <- get_unitcol(us, TRUE)
    ags_o <- get_agscol(unit_o)
    ags_d <- get_agscol(unit_d)
    dtf <- copy(dt)
    dtf[, "flow" := .N, by = c(ags_o, ags_d, by)]
    if (simplify == TRUE) {
        dtf <- dtf[, .SD, .SDcols = c(ags_o, ags_d, by, "flow")]
        dtf <- dtf[, .SD[1], by = c(ags_o, ags_d, by)]
        dtf[, c("origin", "destination") := .(get(ags_o), get(ags_d))]
        dtf[, c(ags_o, ags_d) := NULL]
        data.table::setcolorder(dtf, c("origin", "destination", by, "flow"))
        }
    return(dtf)
}

flows <- get_flows_only(mig, us = us, by = c("gender", "age_gr"), simplify = TRUE)
flows2 <- get_flows_only(mig, us = us, simplify = TRUE)

