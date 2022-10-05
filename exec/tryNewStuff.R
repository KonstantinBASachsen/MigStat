ex_dat <- read_examples()
mig <- ex_dat$mig

us <- "di"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(mig, shp, us, full = FALSE, dist = TRUE)
