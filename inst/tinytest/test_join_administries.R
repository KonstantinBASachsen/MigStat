
map_path <- "/home/konstantin/Documents/sexony/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen"
example_path <- "~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav"

dt <- read_example(example_path)
shapes <- read_shapes(map_path)

dt <- join_administries(dt, shapes$state, shapes$district, shapes$muni, full = TRUE)

units <- c(get_unit("st", T), get_unit("st", F), get_unit("di", T), get_unit("di", F),
           get_unit("mu", T), get_unit("mu", F))

key <- get_ags(get_unit("st", F))
expect_equal(length(unique(dt[, ..key][[1]])), 18)

expect_equal(sum(units %in% colnames(dt)), length(units))

