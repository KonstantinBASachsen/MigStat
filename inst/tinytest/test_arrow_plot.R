
map_path <- "/home/konstantin/Documents/sexony/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen"
example_path <- "~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav"

dt <- read_example(example_path)
shps <- read_shapes(map_path) # 
dtj <- join_administries(dt, shps$state, shps$district, shps$muni, full = FALSE) ### join "official" names

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

