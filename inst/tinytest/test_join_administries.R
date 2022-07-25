ex_dat <- read_examples()
shps <- ex_dat$shps

units <- c(get_unitcol("st", T), get_unitcol("st", F), get_unitcol("di", T), get_unitcol("di", F),
           get_unitcol("mu", T), get_unitcol("mu", F))


dt <- join_administries(ex_dat$mig, shapes$state, shapes$district, shapes$muni, full = FALSE)
expect_equal(nrow(dt), 200) ## if no full join, no rows are added

dt <- join_administries(dt, shapes$state, shapes$district, shapes$muni, full = TRUE)
key <- get_agscol(get_unitcol("st", F))
expect_equal(length(unique(dt[, ..key][[1]])), 18)

expect_equal(sum(units %in% colnames(dt)), length(units))

