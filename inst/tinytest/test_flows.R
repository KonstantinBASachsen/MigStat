ex_dat <- read_examples()
shps <- ex_dat$shps

dt <- join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = TRUE)

losses <- get_losses(dt, "di")
expect_equal(sum(losses[is.na(district_o) & !is.na(EF03U4), flow]), 34)

wins <- get_wins(dt, "mu")
expect_equal(wins[muni_d == "München", flow], 8)

### add test if sum flows after get_flows equals rows of data.table
