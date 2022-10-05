ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = "gender", full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE)


flows[, sum(flow), by = destination]
flows[, sum(flow), by = origin]

mig[EF02U2 == "02", ][, .N]

get_agscol("state_o")

print(flows[order(origin)], 200)


mig[, .N, by = EF03U2][order(EF03U2)]

flows[origin == "04"]
mig[EF03U2 == "04"]
unique(mig[, EF03U2])

flows <- get_flows_only(dt = mig,  us = "st")
flows[, sum(flow), by = destination]
flows[, sum(flow), by = origin][order(origin)] == mig[, .N, by = EF03U2][order(EF03U2)]
