clean_path <- "~/extdata/shapes31simple/"
regions <- read_clean_shps(clean_path)

expect_equal(nrow(regions$districts), 478)
expect_equal(nrow(regions$munis), 16874)
