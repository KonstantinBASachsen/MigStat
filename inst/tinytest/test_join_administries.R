library(tinytest)

map_path <- "/home/konstantin/Documents/sexony/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen"
example_path <- "~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav"

dt <- sexony::read_example(example_path)
shapes <- sexony::read_shapes(map_path)

sexony::join_administries(dt, shapes$states, shapes$districts, shapes$munis)

wanted_cols <- c("state_o", "district_d", "muni_o")

expect_equal(sum(wanted_cols %in% colnames(dt)), length(wanted_cols))
