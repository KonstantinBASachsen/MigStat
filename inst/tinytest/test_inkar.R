inkar_csv <- "/home/konstantin/Downloads/inkar_2021.csv"
inkar <- MigStat::read_inkar(inkar_csv)
ex_dat <- read_examples()
######################################################################
###################### test read_inkar() #############################

table(inkar[, Zeitbezug])

expect_equal(typeof(inkar[, Wert]), "double")
expect_equal(typeof(inkar[, Zeitbezug]), "character") ## there are e.g. 2010 bis 2019, so better not convert to numeric

### did adding 0's worked?
key_lengths <- nchar(inkar[, Kennziffer])
ones <- which(key_lengths == 1)
### dont know what Kennziffer 0 means but all keys with only one char
### must not be 1:9
expect_equal(nrow(inkar[ones][ Kennziffer != "0"]), 0)


######################################################################
###################### test join_inkar_vars() ########################

us <- "st"
zb <- "2013"
rb <- get_raumbezug(us)
shp <- clean_shp(ex_dat$shps, us)
shp_cols <- ncol(shp)
indic <- unique(inkar[, Indikator])
idx <- c(428, 86, 344, 196, 193, 419, 240, 161, 153, 480)
vars <- indic[idx]
avail <- unlist(lapply(vars, function(x) check_availability(inkar, rb, zb, x))),
shp <- join_inkar_vars(shp, inkar, vars, us, zb)
expect_equal(ncol(shp), shp_cols + length(vars[avail != 0]))

