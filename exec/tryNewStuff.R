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


join_missing_regions <- function(flows, shp, na_to_0 = TRUE) {

    combs <- create_region_combs(shp[, AGS])
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
    flows[, od := NULL]
    if (na_to_0) { flows[is.na(flow), flow := 0] }
    return(flows)
}

flows <- join_missing_regions(flows = flows, shp = shp, na_to_0 = TRUE)

flows <- get_flows(mig, shp, "di")
