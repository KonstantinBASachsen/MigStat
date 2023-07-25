ps <- make_paths("~/Diss/")
regions <- MigStat:::read_clean_shps(ps$clean_shapes)

expect_equal(nrow(regions$districts), 478)
expect_equal(nrow(regions$munis), 16874)
#### script does not increase test coverage
