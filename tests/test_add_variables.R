ex_dat <- MigStat:::read_examples() ## still reads AGS cols as character...
mig <- ex_dat$mig
shps <- ex_dat$shps
shps <- MigStat:::read_clean_shps("/home/konstantin/extdata/shapes31simple2", type = "complete")


### I want to check if variables are added correctly. Particularly, if
### the grouping of some ags to one municipality works. 
mig[, "year" := EF12U3] ## should do this only once when reading data,
## add_vars() expects "years"
regions <- mig[c(1, 14, 20, 50, 74, 90, 123, 189, 200), EF03U5]
ags_gen <- unique(shps$munis[AGS %in% regions, .(AGS, GEN)])
ags_gen[, "GEN" := "test"]
#### as integer because shps data stores ags as integer. Maybe I
#### should adjust read_examples() accordingly?
mig[, "EF02U2" := as.integer(EF03U2)]
mig[, "EF03U2" := as.integer(EF03U2)]
mig[, "EF03U5" := as.integer(EF03U5)]
mig[, "EF02U5" := as.integer(EF02U5)]
mig <- MigStat:::add_vars(mig, ags_gen = ags_gen, add_empty = FALSE)
tinytest::expect_equal(data.table::uniqueN(mig[, group_o]), 5)
tinytest::expect_equal(data.table::uniqueN(mig[, group_d]), 5)
tinytest::expect_equal(mig[, .N, by = age_gr][, N], c(75, 90, 20, 15))
tinytest::expect_equal(mig[, .N, by = year_gr][, N], 200)

