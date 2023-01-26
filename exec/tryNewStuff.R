library("data.table")
ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)




    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)

regions <- get_regions(mig, shps, us, "all")
values <- list("origin" = regions, "destination" = regions,
               "gender" = c("m", "f"), "age_gr" = c("0-6", "7-16", "16-99"))
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), values = values)

net <- get_net(flows, b = "gender")
net[, sum(losses)]
net[, sum(wins)]



################## do_join() is very slow, why?
p_work <- list(
  mig = "~/extdata/simulated_moves/",
  shps = "~/extdata/shapes31simple2/",
  dist = "~/extdata/distances/",
  fig = "~/Diss/exec/analysis/figs/fdz_sim_new"
)

paths <- MigStat:::make_paths(p_work = p_work, fdz = FALSE)
mig <- MigStat:::read_mig(paths$mig, "simulation")
####################### Einlesen von shapefiles ##################
shps <- MigStat:::read_clean_shps(paths$shps, type = "ags")
## shps_all <- read_clean_shps(paths$shps, type = "complete")

####################### Einlesen von Distanzen ####################
## distances_mu.csv enthält alle paarweisen Distanzen zwischen allen
## Gemeindepaaren
dist <- data.table::fread(file.path(paths$dist, "distances_mu.csv"))
