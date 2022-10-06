ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)



wins <- get_wins(flows, TRUE)[order(region)]

keys <- setDT(expand.grid(unique(wins[, region]), unique(wins[, gender])))
keys <- keys[order(Var1)]
colnames(keys) <- c("region", "gender")

setkeyv(wins, c("region", "gender" ))
wins[keys]


values <- list("region" = shp[, AGS], "gender" = c("m", "f"), "age_gr" = c("0-6", "7-16", "16-99"))
values



names(values)

include_missing_obs <- function(dt, values, missing_col) {
    ms <- missing_col
    stopifnot("missing_col not in data.table" = ms %in% colnames(dt))
    keys <- do.call(data.table::CJ, values)
    setkeyv(dt, names(values))
    dtfull <- dt[keys, ]
    dtfull <- dtfull[order(mget(names(values)))]
    dtfull <- dtfull[is.na(get(ms)), paste(ms)  := 0]
    return(dtfull)
}

