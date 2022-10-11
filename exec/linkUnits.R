library("data.table")

shps <- read_examples()$shps

munis <- shps$muni
districts <- shps$district
states <- shps$state

munis[, "district_ags" := abbreviate(AGS, 5, strict = TRUE)]
munis <- do_join(munis, districts, "district_ags", "district", full = TRUE)

munis[, "state_ags" := abbreviate(AGS, 2, strict = TRUE)]
munis <- do_join(munis, states, "state_ags", "state", full = TRUE)

