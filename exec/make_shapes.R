library("MigStat")

## This script assumes that the shapefiles are downloaded and
## preprocessed. These steps are done by download_shapes.R in
## Diss/exec. I plan to write some function that does this and which
## will be part of MigStat.

shps_path <- "~/extdata/shapes31_test/no_ewz"
clean_path <- "~/extdata/shapes31simple2/"

years <- 2000:2018
regions <- clean_shps(shps_path, clean_path, years, "complete")
regions <- clean_shps(shps_path, clean_path, years, "ags")
