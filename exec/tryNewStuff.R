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

    wins <- get_wins(flows)
    losses <- get_losses(flows)
    
}

get_wins(flows)
get_losses(flows)

flows[, sum(flow), by = destination]
flows[, sum(flow, na.rm = TRUE), by = origin][order(origin)]

mig[EF02U2 == "02", ][, .N]

get_agscol("state_d")

print(flows[order(origin)], 200)


mig[, .N, by = EF03U2][order(EF03U2)]

flows[origin == "04"]
mig[EF03U2 == "04"]
unique(mig[, EF03U2])

flows <- get_flows_only(dt = mig,  us = "st")
flows[, sum(flow), by = destination]
flows[, sum(flow), by = origin][V1 != 0] == mig[, .N, by = EF03U2][order(EF03U2)]

mig[EF03U2 == "09" &  EF02U2 == "08", ]
mig[EF03U2 == "08" &  EF02U2 == "09", ]
