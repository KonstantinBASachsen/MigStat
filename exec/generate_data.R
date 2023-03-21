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

######################################################################
###################### Subset INKAR data set #########################
######################################################################
## This section subsets the INKAR data. The INKAR data can not be used
## in total at the FDZ. Instead I had to fill in the FDZ
## Verfahrensbeschreibung and add all INKAR variables that I want to
## use in the analyses. These variables I saved in a file
## "fdz_v8.csv". I use this file to subset the INKAR data so that it
## only contains these variables. This subset I then send to the FDZ.
inkar_vars <- "~/extdata/inkar_additions/fdz_v8.csv"
inkar_vars <- fread(inkar_vars)[, .(varname, Kurzname)]
inkar <- read_inkar(file.path(ps$inkar, "inkar_2021.csv"))
### subesetting INKAR
raumbezug <- c("Bundesländer", "Kreise", "Gemeinden")
inkar <- inkar[Raumbezug %in% raumbezug, ]
### check if all indicators that are part of the FDZ
### Verfahrensbeschreibung and thus of "inkar_vars" can be found in
### the data.
indikatoren <- inkar[, unique(Indikator)]
setdiff(inkar_vars[, Kurzname], indikatoren)
inkar <- inkar[Indikator %in% inkar_vars[, Kurzname]]
fpath <- "~/extdata/inkar_additions/inkar_fdz.csv"
fwrite(inkar, fpath)
