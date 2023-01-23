ex_dat <- read_examples() ## still reads AGS cols as character...
mig <- ex_dat$mig
mig[, "AGS" := as.integer(AGS)]
shps <- ex_dat$shps
shps <- read_clean_shps("/home/konstantin/extdata/shapes31simple2")


mig[, "year" := EF12U3] ## should do this only once when reading data
ags_gen <- shps$states[, .(AGS, GEN)]
ags_gen[, "GEN" := c(rep("west", 10), rep("ost", 6))]
mig <- add_vars(mig, ags_gen = ags_gen)
