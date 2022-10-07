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
net <- get_net(flows, grouped = FALSE)
expect_equal(net[, sum(net)], 0)

### checks if all regions in wins and all in losses are part of net as
### well. This is not always the case, even if get_flows() was invoked
### with full = TRUE. This is so because there might be regions that
### are only origin or only destination. get_net() takes care of this
### by handling regions that are not origin regions as 0 losses and
### regions that are not destinations as 0 wins.

n_regions <- length(unique(c(losses[, region], wins[, region])))
expect_equal(nrow(net), n_regions)
