ex_dat <- read_examples()
mig <- ex_dat$mig
shps <- ex_dat$shps
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)

########################### get_net() ##########################################
################################################################################

## since every win of one region is a loss of some other region, they
## cancel each other out

### values is taken to fill missing observations, This is necessary
### because a missing observation means a 0 flow. For summarizing data
### it is important to fill the missing observations. Lets say I want
### to get the net migration of Delitzsch of people aged between 16
### and 24. 10 left Delitzsch, nobody moved in. Net is wins - losses
### so I need to fill the missing row with 0. "Values" is supposed to
### hold all possible combinations that are filled with 0, if it is
### not in the data.

### This values should work but it does not, because there are AGS's
### in the data that seem to be not valid
## values <- list("region" = c(unique(shp[, AGS]), "00000000"))
## mis_o <- setdiff(flows[, origin], values[[1]])
## mis_d <- setdiff(flows[, destination], values[[1]])

all_regions <- unique(c(mig[, EF03U2], mig[, EF02U2]))
values <- list("origin" = all_regions, "destination" = all_regions)
values <- list("region" = all_regions)
net <- get_flows(dt = mig, us_o = "none", us_d = "st", fill = "all", values = values)
expect_equal(net[, sum(net)], 0)
expect_equal(net[, sum(losses)], 200)
expect_equal(net[, sum(wins)], 200)


### checks if all regions in wins and all in losses are part of net as
### well. This is not always the case, even if get_flows() was invoked
### with full = TRUE. This is so because there might be regions that
### are only origin or only destination. get_net() takes care of this
### by handling regions that are not origin regions as 0 losses and
### regions that are not destinations as 0 wins.

## n_regions <- length(unique(c(losses[, region], wins[, region])))
expect_equal(nrow(net), 17)

########################### get_net() grouped operations #######################
################################################################################
us <- "st"
all_regions <- get_regions(dt = mig, shps = shps, us = us, type = "all")

values <- list("region" = all_regions, "gender" = c("m", "f"),
               "age_gr" = c("0-6", "7-16", "16-99"))
net <- get_flows(mig, us_o = us, by = c("gender", "age_gr"),
                          fill = "groups", values = values)
expected <- net[region == "05", wins]
actual <- mig[EF02U2 == "05", .N, by = .( gender, age_gr)][, N]
expect_equal(intersect(expected, actual),union(expected, actual) )

expected <- net[region == "05", losses]
actual <- mig[EF03U2 == "05", .N, by = .( gender, age_gr)][, N]
expect_equal(intersect(expected, actual), union(expected, actual) )


###################### get_net() filling missing rows ################
######################################################################

### Lets say a region has only wins and no losses. In this case it is
### important that in the net table there is a line where losses is
### set to 0. In the test data, this is true for example region 02.
### region 00 instead has no wins but losses. The regions in the net
### table should always be the union of origin and destination in the
### mig data.

regions <- union(mig[, EF02U2], mig[, EF03U2])
net <- get_flows(mig, us_o = "none", us_d = "st")
mes <- "Number regions in net equals number regions in union of origin and destination"
expect_equal(data.table::uniqueN(net[, region]), length(regions),
             info = mes)


## flows <- get_flows(mig, "st", by = "age_gr")
## net <- get_net(flows)

## net[, .N, by = region]
## net[region == "04"]

## mig[EF02U2 == "04" | EF03U2 == "04"]
