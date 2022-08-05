
ex_dat <- read_examples()
shps <- ex_dat$shps
dtj <- join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = FALSE) ### join "official" names

us <- "st"
name <- "Berlin"

dtf <- get_flows(ex_dat$mig, shps, us)
shp <- clean_shp(ex_dat$shps, us = us)
dtarrow <- get_arrow_data(dtf, shp, name)


### should be exactly one origin, todo: rename dest to origin
expect_equal(dtarrow[o_region == TRUE, .N], 1)

### if origin is also destination, there are two places with the
### "name", this is the case for Berlin
## expect_equal(sum(grepl(name, dtf[, place])), 2) does not apply anymore



name <- "Mecklenburg-Vorpommern"
dtarrow <- get_arrow_data(dtf, shp, name)

### should be exactly one origin, todo: rename dest to origin
expect_equal(dtarrow[o_region == TRUE, .N], 1)

### if origin is also destination, there are two places with the
### "name", this is not the case for Mecklenburg-Vorpommern

expect_equal(sum(grepl(get_ags(shp, name), dtarrow[, place])), 1) 

