ex_dat <- MigStat:::read_examples() ## still reads AGS cols as character...
mig <- ex_dat$mig
shps <- ex_dat$shps
shps <- MigStat:::read_clean_shps("/home/konstantin/extdata/shapes31simple2", type = "ags")


#### testing do_join
n_cols <- ncol(mig)
mig[, "EF02U2" := as.integer(EF02U2)]
MigStat:::do_join(mig, shps$states, "st_o", "GEN", key1, "AGS")
n_st <- data.table::uniqueN(mig[, EF02U2])
expect_equal(ncol(mig), n_cols + 1) ### column was joined
expect_equal(data.table::uniqueN(mig[, st_o]), n_st) ## all values were joined
