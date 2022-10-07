ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)

get_regions <- function(dt, shps, us, type) {
    stopifnot(type %in% c("data", "all"))
    ags_o <- get_agscol(get_unitcol(us, F))
    ags_d <- get_agscol(get_unitcol(us, T))
    shp <- clean_shp(shps, us)
    if (type == "data") {
        all_regions <- unique(c(dt[, get(ags_o)], dt[, get(ags_d)]))
    }
    if (type == "all") {
        all_regions <- unique(c(shp[, AGS], dt[, get(ags_o)], dt[, get(ags_d)]))
    }
    return(all_regions)
}



    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)

regions <- get_regions(mig, shps, us, "all")
values <- list("origin" = regions, "destination" = regions,
               "gender" = c("m", "f"), "age_gr" = c("0-6", "7-16", "16-99"))
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), values = values)

net <- get_net(flows, b = "gender")
net[, sum(losses)]
net[, sum(wins)]



