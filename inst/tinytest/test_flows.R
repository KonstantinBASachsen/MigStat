ex_dat <- read_examples()
mig <- ex_dat$mig
shps <- ex_dat$shps


#################### get_wins() and get_losses() ###############################
################################################################################

us <- "mu"
shp <- clean_shp(shps, us)
## dt <- join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = TRUE)
flows <- get_flows(mig, shp, us)
losses <- get_losses(flows)
expect_equal(losses[region == "09162000", losses], 6)

wins <- get_wins(flows)
expect_equal(wins[region == "11000000", wins], 6)

### add test if sum flows after get_flows equals rows of data.table
expect_equal(losses[, sum(losses)], nrow(mig))
expect_equal(wins[, sum(wins)], nrow(mig))
### add test if nrow(get_flows()) == n_regions^2!


mig[EF02U5 == "11000000"]

get_agscol("muni_o")
mig
colnames(mig)


########################### get_net() ##########################################
################################################################################

