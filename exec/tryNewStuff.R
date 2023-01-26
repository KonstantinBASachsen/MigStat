library("data.table")
library("MigStat")
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
mig <- add_vars(mig, add_vars = "age_gr")
flows <- get_flows(mig, "st", by = "age_gr")
losses <- get_losses(flows)
wins <- get_wins(flows)

net <- do_join(dt1 = wins, dt2 = losses, join_col = "losses",
               key1 = key1, key2 = key2)

get_net <- function(flows) {
    #### add by option to choose grouping columns and ignore all else?
    losses <- get_losses(flows)
    wins <- get_wins(flows)
    ### is this check necessary?
    key1 <- setdiff(colnames(wins), "wins")
    key2 <- setdiff(colnames(losses), "losses")
    if (sum(key1 == key2) != length(key1)) {
        stop("group columns in wins and losses different")
    }
    net <- do_join(dt1 = wins, dt2 = losses, join_col = "losses",
                   key1 = key1, key2 = key1, all = TRUE)
    net[is.na(wins), "wins" := 0]
    net[is.na(losses), "losses" := 0]
    net[, "net" := wins - losses]
    return(net)
}

net <- get_net(flows)

do_join(dt1 = wins, dt2 = losses, join_col = "N",
                  key1 = "EF02U2", key2 = "EF03U2", new_col = "losses")


key1 <- c("region", "age_gr")
key2 <- c("region", "age_gr")

net[is.na(wins), "wins" := 0]
net[is.na(losses), "losses" := 0]
net[, "net" := wins - losses]

#### does it work when rows are missing?
losses <- get_losses(flows)
losses <- losses[- c(61:64)]
wins <- get_wins(flows)
wins <- wins[- c(61:64)]
net <- do_join(dt1 = wins, dt2 = losses, join_col = "losses",
               key1 = key1, key2 = key2, all = TRUE)
net[is.na(wins), "wins" := 0]
net[is.na(losses), "losses" := 0]
net[, "net" := wins - losses]


### filling observations does not work when same observations are
### missing. Is that a problem?

