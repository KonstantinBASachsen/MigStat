ex_dat <- read_examples()
mig <- ex_dat$mig

shp <- clean_shp(ex_dat$shps, "di")

flows <- get_flows(mig, shp, "di")
