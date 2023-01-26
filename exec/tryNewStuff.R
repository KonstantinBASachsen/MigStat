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
dist[, "testcol" := "bla"]
start <- Sys.time()
setkeyv(mig, c("EF03U5", "EF02U5"))
setkeyv(dist, c("origin", "destination"))
mig[dist, "distance" := i.distance]
stop <- Sys.time()
stop - start


do_join <- function(dt1, dt2, new_col, join_col, key1, key2 = "AGS", full = FALSE) {
    ## performs full join
    ### might be slower than setting keys and then dt1[dt2, "col" := i.col].
    data.table::setkeyv(dt1, key1)
    data.table::setkeyv(dt2, key2)
    jc <- paste0("i.", join_col)
    dt1[dt2, (new_col) := jc, env = (jc = paste0("i.", join_col))]
    return(dt1)
}

do_join <- function(dt1, dt2, new_col, join_col, key1, key2 = "AGS", full = FALSE) {
    ## performs full join
### might be slower than setting keys and then dt1[dt2, "col" := i.col].
    keep <- c(key2, join_col)
    dt1 <- data.table::merge.data.table(dt1, dt2[, ..keep], by.x = key1, by.y = key2)
    return(dt1)
}


mig <- MigStat:::read_mig(paths$mig, "simulation")
start2 <- Sys.time()
mig <- do_join(mig, dist, "distance", "distance", c("EF03U5", "EF02U5"), c("origin", "destination"))
stop2 <- Sys.time()
stop2 - start2

start3 <- Sys.time()
merge(mig, dist, by.x = c("EF03U5", "EF02U5"), by.y = c("origin", "destination"))
stop3 <- Sys.time()
stop3 - start3

