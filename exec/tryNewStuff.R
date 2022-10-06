ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = "gender", full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)



