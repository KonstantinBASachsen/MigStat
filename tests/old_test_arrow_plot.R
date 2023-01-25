ex_dat <- read_examples()
shps <- ex_dat$shps
mig <- ex_dat$mig
##dtj <- join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = FALSE) ### join "official" names

us <- "st"
shp <- clean_shp(shps, us)
name <- "Berlin"

dtf <- get_flows(dt = mig, us = us, dist = TRUE)
### get_arrow_data looks buggy, I think I want to add the origin row
### and not replace dtarrow with it, see: dtarrow <- dtarrow[origin ==
### ags, c(1,3,5)]

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

### for "Leipzig" there are two ags keys in the district shape
### table. One for the city of leipzig and one for the region. This is
### supposed to throw an error.
us <- "di"
name <- "Leipzig"

dtf <- get_flows(ex_dat$mig, shp, us, dist = TRUE)
shp <- clean_shp(ex_dat$shps, us = us)
expect_error(get_arrow_data(dtf, shp, name))

### The error message asks to specify the key directly. This sould not
### produce an error
ags <- shp[GEN == "Leipzig", AGS]

dtarrow <- get_arrow_data(dtf, shp, name, ags = ags[1])

expect_equal(nrow(dtarrow), 4)

dtarrow <- get_arrow_data(dtf, shp, name, ags = ags[2])

expect_equal(nrow(dtarrow), 2)


