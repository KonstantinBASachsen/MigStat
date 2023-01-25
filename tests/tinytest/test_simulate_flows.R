library("MigStat")
library("tinytest")
### could be deleted because I do not use the functions anymore
ex_dat <- MigStat:::read_examples()
shps <- ex_dat$shps
mig <- ex_dat$mig

s <- MigStat:::sample_move(mig, shps, us_o = "st", us_d = "mu")
expect_equal(nchar(s$ags_o), 2)
expect_equal(nchar(s$ags_d), 8)

s <- MigStat:::sample_move(mig, shps, us_o = "di", us_d = "st")
expect_equal(nchar(s$ags_o), 5)
expect_equal(nchar(s$ags_d), 2)

s <- MigStat:::sample_move(mig, shps, us_o = "st", us_d = "st")
row <- MigStat:::new_row(dt = mig, shps = shps, sample_list = s)
actual_cols <- colnames(row)[!is.na(row)] ## not NA cols
needed_cols <- c("EF02U2", "EF03U2", "state_d", "state_o")
expect_equal(length(setdiff(actual_cols, needed_cols)), 0)

s <- MigStat:::sample_move(mig, shps, us_o = "di", us_d = "mu")
row <- MigStat:::new_row(dt = mig, shps = shps, sample_list = s)
actual_cols <- colnames(row)[!is.na(row)] ## not NA cols
needed_cols <- c("EF02U5", "EF03U4", "district_o", "muni_d")
expect_equal(length(setdiff(actual_cols, needed_cols)), 0)

rows <- MigStat:::n_new_rows(mig, shps, "st", "mu", 10)
expect_equal(nrow(rows), 10)
