library("MigStat")
library("tinytest")

ex_dat <- MigStat:::read_examples()
shps <- ex_dat$shps

units <- c(MigStat:::get_unitcol("st", T), MigStat:::get_unitcol("st", F),
           MigStat:::get_unitcol("di", T), MigStat:::get_unitcol("di", F),
           MigStat:::get_unitcol("mu", T), MigStat:::get_unitcol("mu", F))


dt <- MigStat:::join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = FALSE)
expect_equal(nrow(dt), 200) ## if no full join, no rows are added

dt <- MigStat:::join_administries(ex_dat$mig, shps$state, shps$district, shps$muni, full = TRUE)
key <- MigStat:::get_agscol(MigStat:::get_unitcol("st", F))
expect_equal(length(unique(dt[, ..key][[1]])), 18)

expect_equal(sum(units %in% colnames(dt)), length(units))

## do_join(ex_dat$mig, shps$state, "n", key1 = "EF02U2", col = "state_o")
