ex_dat <- read_examples()
mig <- ex_dat$mig

shp <- clean_shp(ex_dat$shps, "st")
flows <- get_flows(mig, shp, "st", full = TRUE)

### all rows should be included in flows
expect_equal(flows[, sum(flow)], 200)
### make sure leading 0's are maintained. This should be the case if
### all ags's have same length
expect_equal(typeof(flows$origin), "character")
expect_equal(typeof(flows$destination), "character")
expect_equal(length(unique(nchar(flows$origin))), 1)
expect_equal(length(unique(nchar(flows$destination))), 1)


### Here I test if I got at least as many rows as there are regions
### squared. This is because get_flows() is supposed to add region
### pairs even if there is no flow. Here it might seem I can test for
### equality but in the data sometimes the origin is unknown. These I
### keep so there are region pairs that are not actually regions. They
### result because origin is unknown.

n_regions <- length(unique(shp[, AGS]))
expect_true(nrow(flows) >= n_regions^2, info = sprintf("expected %s, got %s", n_regions^2, nrow(flows)))

shp <- clean_shp(ex_dat$shps, "di")
flows <- get_flows(mig, shp, "di", full = TRUE)
### all rows should be included in flows
expect_equal(flows[, sum(flow)], 200)

### not yet implemented but region pairs with no flows should be
### returned as well and should show 0 flows
n_regions <- length(unique(shp[, AGS]))
expect_true(nrow(flows) >= n_regions^2, info = sprintf("expected %s, got %s", n_regions^2, nrow(flows)))
