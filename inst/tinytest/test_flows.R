map_path <- "/home/konstantin/Documents/sexony/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen"
example_path <- "~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav"

dt <- read_example(example_path)
shapes <- read_shapes(map_path)

dt <- join_administries(dt, shapes$state, shapes$district, shapes$muni)

losses <- sexony:::get_flow(dt, "di", F)
expect_equal(sum(losses[is.na(district_o) & !is.na(EF03U4), flow]), 34)

wins <- sexony:::get_flow(dt, "mu", T)
expect_equal(wins[muni_d == "München", flow], 8)
