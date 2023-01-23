library(tinytest)
library(MigStat)

path <- "~/Documents/MigStat"
devtools::load_all(path)
test_package("MigStat")
