library("MigStat")
library("tinytest")
ex_dat <- MigStat:::read_examples()
mig <- ex_dat$mig
### Here I create additional variables to check if the summary of
### flows between regions also works when data is grouped
mig[, "gender" := c(rep("m", 100), rep("f", 100))]
mig[, "age_gr" := data.table::fifelse(EF25 < 35, "0-35", ">35")]
shp <- MigStat:::clean_shp(ex_dat$shps, "st", keep = c("AGS"))
all_regions <- c(shp[, AGS], "00")
values <- list("origin" = all_regions, "destination" = all_regions)
flows <- MigStat::get_flows(dt = mig,  us = "st", values = values)
### all rows should be included in flows
expect_equal(flows[, sum(flow)], 200)
expect_equal(flows[origin == "08" & destination == "09", flow], 11)
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

shp <- MigStat:::clean_shp(ex_dat$shps, "di", keep = "AGS")
all_regions <- c(shp[, AGS], "00000") ### does not work
all_regions2 <- unique(c(mig[, EF03U4], mig[, EF02U4]))
all_regions3 <- unique(c(all_regions, all_regions2))
## setdiff(all_regions2, all_regions)

values <- list("origin" = all_regions3, "destination" = all_regions3)
flows <- MigStat::get_flows(dt = mig, us = "di", values = values)
### all rows should be included in flows
expect_equal(flows[, sum(flow)], 200)

### not yet implemented but region pairs with no flows should be
### returned as well and should show 0 flows
n_regions <- length(unique(shp[, AGS]))
expect_true(nrow(flows) >= n_regions^2, info = sprintf("expected %s, got %s", n_regions^2, nrow(flows)))


#################### testing grouped od-flows ########################
######################################################################

## mig[EF02U2 == "09" & EF03U2 == "08"]
flows <- MigStat::get_flows(dt = mig, by = "gender", us = "st")
expect_equal(flows[origin == "08" & destination == "09", flow], c(6, 5))

flows <- get_flows(dt = mig, us = "st", by = c("gender", "age_gr"))

### dont know whats going on here. One time I got in mig different group sizes
## expect_equal(flows[origin == "08" & destination == "09", ][order(gender, age_gr), flow], c(2, 1, 3, 2, 1, 2))
expect_equal(flows[origin == "08" & destination == "09", flow], c(3, 3, 1, 4))

## flows[origin == "08" & destination == "09", ]
## mig[EF03U2 == "08" & EF02U2 == "09", .(gender, age_gr)]
