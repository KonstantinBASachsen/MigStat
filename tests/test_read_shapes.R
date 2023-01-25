library("MigStat")
library("tinytest")
shp_path <- "~/extdata/shapes31_test/no_ewz"
shapes <- read_shapes(shp_path)

### administrative entities to read

units <- c("federal_states", "districts", "municipalities")

expect_equal(length(shapes), length(units)) 
