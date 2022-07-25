
ex_dat <- read_examples()
shps <- ex_dat$shps
dtj <- join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = FALSE) ### join "official" names

name <- "Berlin"
dtf <- get_arrow_data(dt = dtj, shapes = shps, name = name, o_us = "di",
               d_us = "st")

### should be exactly one origin, todo: rename dest to origin
expect_equal(dtf[origin == TRUE, .N], 1)

### if origin is also destination, there are two places with the
### "name", this is the case for Berlin
expect_equal(sum(grepl(name, dtf[, place])), 2) 



name <- "Mecklenburg-Vorpommern"
dtf <- get_arrow_data(dt = dtj, shapes = shps, name = name, o_us = "st",
               d_us = "st")

### should be exactly one origin, todo: rename dest to origin
expect_equal(dtf[origin == TRUE, .N], 1)

### if origin is also destination, there are two places with the
### "name", this is not the case for Mecklenburg-Vorpommern
expect_equal(sum(grepl(name, dtf[, place])), 1) 

