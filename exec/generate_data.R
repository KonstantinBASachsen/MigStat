library("MigStat")
library("data.table")
library("sf")
library("geosphere")
ps <- make_paths("work")
ps$inkar <- "~/extdata/"
shps_path <- "~/extdata/shapes31_test/no_ewz"
clean_path <- "~/extdata/shapes31simple2/"
dist_path <- "~/extdata/distances"

######################################################################
####################### Generate clean shapefiles ####################
## The following lines assume that the shapefiles are downloaded and
## preprocessed. These steps are done by download_shapes.R in
## Diss/exec. I plan to write some function that does this and which
## will be part of MigStat.

years <- 2000:2018
## "complete" returns a shapefile that has for every year all regions
## in it. This is nice for plotting, when the exact geometry for every
## year is necessary
regions <- clean_shps(shps_path, clean_path, years, "complete")
## "ags" only returns unique ags. That is, if the ags stays constant
## although the region changes this is not reflected in the data. Even
## if the underlying geometry changes. Probably it does not change by
## much though. This table can be used to generate distances or to
## fast join name of regions.
regions <- clean_shps(shps_path, clean_path, years, "ags")

## Why do I not save this data?

######################################################################
########################## Generate distances ########################
if (!dir.exists(dist_path)) {
    dir.create(dist_path)
}
dist_st <- get_distances(regions$states, type = "point_on_surface")
data.table::fwrite(dist_st, file.path(dist_path, "distances_st.csv"))
dist_di <- get_distances(regions$districts, type = "point_on_surface")
data.table::fwrite(dist_di, file.path(dist_path, "distances_di.csv"))
dist_mu <- get_distances(regions$munis)
data.table::fwrite(dist_mu, file.path(dist_path, "distances_mu.csv"))

######################################################################
######################### Simulating moves ###########################
ex_dat <- MigStat:::read_examples()
shps <- MigStat:::read_clean_shps(ps$shps, type = "complete")
dist <- read_distances(file.path(ps$dist, "distances_st.csv")) ## should only take path and "us" arg
inkar <- read_inkar(file.path(ps$inkar, "inkar_2021.csv"), leading_0 = FALSE)

### samples new Migstat rows
new_rows <- n_new_rows(ex_dat$mig, ex_dat$shps, "mu", "mu", 100)


get_formula <- function(params, vars) {
    stopifnot("params have to be numeric" = is.numeric(params))
    stopifnot("vars have to be character" = is.character(vars))
    terms <- paste(params, vars, sep = "*")
    terms <- paste(terms, collapse = "+")
    return(terms)
}

gravity_samples <- function(shps, inkar, dist_p, us = c("st", "di", "mu"),
                            params = c(0.5, 0.5, 3), period = 2017) {
    us <- match.arg(us)
    unit <- paste0(get_shp_unit(us), "s")
    shp <- shps[[unit]][year == period]
    dist_f <- paste0("distances_", us, ".csv")
    dist <- suppressWarnings(read_distances(file.path(dist_p, dist_f)))

    flows <- data.table::CJ(shp[, AGS], shp[, AGS])
    colnames(flows) <- c("origin", "destination")

    rb <- get_raumbezug(us)
    inkar <- inkar[Raumbezug == rb & Zeitbezug == period]
    bev <- "Bevölkerung gesamt"
    pred_dt <- inkar[Indikator == bev, ]
    pred_dt <- dcast(pred_dt, Kennziffer ~ Indikator, value.var = "Wert")

    key <- c("origin", "destination")
    flows <- do_join(flows, dist, "distance", key, key)
    flows <- do_join(flows, pred_dt, "Bevölkerung gesamt", "origin", "Kennziffer", "pop_o")
    flows <- do_join(flows, pred_dt, "Bevölkerung gesamt", "destination", "Kennziffer", "pop_d")
    setcolorder(flows, c("origin", "destination", "distance", "pop_o", "pop_d"))
    flows[distance == 0, "distance" := 20]

    vars <- c("log(pop_o)", "log(pop_d)", "log(distance)")
    formula <- get_formula(params, vars)
    flows[, "linpred" := eval(parse(text = formula))]
    flows[, "flow" := rpois(nrow(flows), exp(linpred))]
    return(flows)
}

samples <- gravity_samples(shps, inkar, ps$dist, us = "st",
                           params = c(0.6, 0.7, -3), period = 2017)
q <- quantile(samples$flow, 0.9)
plot(density(samples$flow), xlim = c(0, q))

