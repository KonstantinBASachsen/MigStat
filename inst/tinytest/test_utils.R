ex_dat <- MigStat:::read_examples() ## still reads AGS cols as character...
mig <- ex_dat$mig
shps <- ex_dat$shps
shps <- read_clean_shps("/home/konstantin/extdata/shapes31simple2", type = "ags")


#### testing do_join
n_cols <- ncol(mig)
mig[, "EF02U2" := as.integer(EF02U2)]
mig <- do_join(dt1 = mig, dt2 = shps$states,
                         join_col = "GEN", key1 = "EF02U2", key2 = "AGS",
                         new_col = "st_o")
n_st <- data.table::uniqueN(mig[, EF02U2])
mes <- "Wrong number of cols after join"
expect_equal(ncol(mig), n_cols + 1, info = mes) ### column was joined
mes <- "Not all names of federal states joined"
expect_equal(data.table::uniqueN(mig[, st_o]), n_st, info = mes) ## all values were joined
