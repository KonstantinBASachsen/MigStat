
map_path <- "/home/konstantin/Documents/project/Diss/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen"
example_path <- "~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav"

dt <- read_example(example_path)
shapes <- read_shapes(map_path)

units <- c(get_unitcol("st", T), get_unitcol("st", F), get_unitcol("di", T), get_unitcol("di", F),
           get_unitcol("mu", T), get_unitcol("mu", F))


dt <- join_administries(dt, shapes$state, shapes$district, shapes$muni, full = FALSE)
expect_equal(nrow(dt), 200) ## if no full join, no rows are added

dt <- join_administries(dt, shapes$state, shapes$district, shapes$muni, full = TRUE)


key <- get_agscol(get_unitcol("st", F))
expect_equal(length(unique(dt[, ..key][[1]])), 18)

expect_equal(sum(units %in% colnames(dt)), length(units))

