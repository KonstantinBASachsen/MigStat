ex_dat <- read_examples()
shps <- ex_dat$shps

dt <- join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = TRUE)

losses <- get_losses(dt, "di")
expect_equal(sum(losses[is.na(region) & !is.na(ags), flow]), 34)

wins <- get_wins(dt, "mu")
expect_equal(wins[region == "München", flow], 8)

### add test if sum flows after get_flows equals rows of data.table
### add test if nrow(get_flows()) == n_regions^2
