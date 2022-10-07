ex_dat <- read_examples()
mig <- ex_dat$mig
shps <- ex_dat$shps
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)


#################### get_wins() and get_losses() ###############################
################################################################################

us <- "mu"
shp <- clean_shp(shps, us)
## dt <- join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = TRUE)
flows <- get_flows(mig, shp, us)
losses <- get_losses(flows, grouped = FALSE)
expect_equal(losses[region == "09162000", losses], 6)

wins <- get_wins(flows, grouped = FALSE)
expect_equal(wins[region == "11000000", wins], 6)

### add test if sum flows after get_flows equals rows of data.table
expect_equal(losses[, sum(losses)], nrow(mig))
expect_equal(wins[, sum(wins)], nrow(mig))
### add test if nrow(get_flows()) == n_regions^2!


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


values <- list("region" = unique(c(flows[, origin], flows[, destination])))
net <- get_net(flows, values = values, grouped = FALSE)
expect_equal(net[, sum(net)], 0)
expect_equal(net[, sum(losses)], 200)
expect_equal(net[, sum(wins)], 200)

### checks if all regions in wins and all in losses are part of net as
### well. This is not always the case, even if get_flows() was invoked
### with full = TRUE. This is so because there might be regions that
### are only origin or only destination. get_net() takes care of this
### by handling regions that are not origin regions as 0 losses and
### regions that are not destinations as 0 wins.

n_regions <- length(unique(c(losses[, region], wins[, region])))
expect_equal(nrow(net), n_regions)
