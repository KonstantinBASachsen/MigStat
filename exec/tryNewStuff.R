ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)



wins <- get_wins(flows, TRUE)[order(region)]

keys <- setDT(expand.grid(unique(wins[, region]), unique(wins[, gender])))
keys <- keys[order(Var1)]
colnames(keys) <- c("region", "gender")

setkeyv(wins, c("region", "gender" ))
wins[keys]

all_regions <- c(shp[, AGS], "00")
values <- list("region" = all_regions, "gender" = c("m", "f"), "age_gr" = c("0-6", "7-16", "16-99"))
values2 <- list("origin" = all_regions, "destination" = all_regions, "gender" = c("m", "f"))

flows <- get_flows(mig, shp, "st", by = "gender", values = values2)
flows[order(origin, destination)]
flows[origin == "01", ]
