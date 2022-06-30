
map_path <- "/home/konstantin/Documents/project/Diss/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen"
shapes <- read_shapes(map_path)

### administrative entities to read

units <- c("federal_states", "districts", "municipalities")

expect_equal(length(shapes), length(units)) 
