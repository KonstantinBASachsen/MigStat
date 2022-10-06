ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = "gender", full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)


get_wins <- function(flows) {
    wins <- flows[, sum(flow), by = destination]
    wins <- wins[, .(region = destination, wins = V1)]
    return(wins)
}

get_losses <- function(flows) {
    losses <- flows[, sum(flow), by = origin]
    losses <- losses[, .(region = origin, losses = V1)]
    return(losses)
}

get_net <- function(flows) {
    flow <- region <- i.flow <- i.region <- NULL
    w <- get_wins(flows)
    l <- get_losses(flows)
    keys <- unique(c(w[, region], l[, region]))
    data.table::setkeyv(w, "region")
    data.table::setkeyv(l, "region")
    w <- w[keys, ]
    w[is.na(wins), "wins" := 0]
    l <- l[keys, ]
    l[is.na(losses), "losses" := 0]
    data.table::setkeyv(w, "region")
    data.table::setkeyv(l, "region")
    w[l, "losses" := i.losses]
    w[, "net" := wins - losses]
    setcolorder(w, c("region", "net", "wins", "losses"))
    ### the next lines ensure that if there are regions in losses that
    ### are not in wins, the name of the region is transferred to wins
    row <- w[is.na(region)][l, "region" := i.region]
    idx <- which(is.na(w[, region]))
    w[idx] <- row
    return(w)
}

