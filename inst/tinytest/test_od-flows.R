ex_dat <- read_examples()
mig <- ex_dat$mig
### Here I create additional variables to check if the summary of
### flows between regions also works when data is grouped
mig[, "gender" := c(rep("m", 100), rep("f", 100))]
mig[, "age_gr" := fifelse(EF25 < 35, "0-35", ">35")]

shp <- clean_shp(ex_dat$shps, "st")
flows <- get_flows(mig, shp, "st", full = TRUE)

### all rows should be included in flows
expect_equal(flows[, sum(flow)], 200)
expect_equal(flows[origin == "09" & destination == "08", flow], 11)
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


#################### testing grouped od-flows ########################
######################################################################

mig[EF02U2 == "09" & EF03U2 == "08"]
flows <- get_flows(mig, shp, by = "gender", us = "st")
expect_equal(flows[origin == "08" & destination == "09", flow], c(6, 5))

flows <- get_flows(mig, shp, by = c("gender", "age_gr"), us = "st")
expect_equal(flows[origin == "08" & destination == "09", flow], c(3, 3, 1, 4))
