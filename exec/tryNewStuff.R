ex_dat <- read_examples()
mig <- ex_dat$mig

shp <- clean_shp(ex_dat$shps, "di")

combs <- create_region_combs(shp[, AGS])
flows <- get_flows_only(mig, "di")

flows[, "od" := paste(origin, destination, sep = "_")]
setkeyv(flows, "od")
### Although all region pairs are already contained in combs I need
### this because in the data are moves where origin is unknown.
keys <- unique(c(combs[, od], flows[, od]))
flows <- flows[keys]
setkeyv(combs, "od")
setkeyv(flows, "od")
flows[combs, "origin" := i.origin]
flows[combs, "destination" := i.destination]
flows[, sum(flow, na.rm = TRUE)]
flows[, od := NULL]
flows[is.na(flow), flow := 0]
print(flows, 200)
