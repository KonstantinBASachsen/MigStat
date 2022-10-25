
shp_path <-  "~/Diss/inst/extdata/shapes/no_ewz"
shapes <- read_shapes(shp_path)

### administrative entities to read

units <- c("federal_states", "districts", "municipalities")

expect_equal(length(shapes), length(units)) 
