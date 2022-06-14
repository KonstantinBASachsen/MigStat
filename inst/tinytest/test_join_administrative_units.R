library(tinytest)

map_path <- "/home/konstantin/Documents/sexony/data/vg250_3112.utm32s.shape.ebenen/vg250_ebenen"
muni_file  <- "VG250_GEM.shp"
states_file <- "VG250_LAN.shp"
districts_file <- "VG250_KRS.shp"


df <- read.spss("~/network/Rohdaten/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2010-2013_4480-2021.sav",
                to.data.frame = TRUE)
dt <- setDT(df)

munis <- read_sf(paste(map_path, muni_file, sep = "/"))
munis <- setDT(munis)

states <- read_sf(paste(map_path, states_file, sep = "/"))
states <- setDT(states)

districts <- read_sf(paste(map_path, districts_file, sep = "/"))
districts <- setDT(districts)

labels <- attr(dt, "variable.labels")
