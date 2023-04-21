ps <- make_paths()
ps$data <- "~/Diss/exec/one_run/data/clean/"
ps$cor <- file.path(ps$data, "corrections")
shp <- read_clean_shps(ps$shps, "complete")$districts
mig <- data.table::fread(file.path(ps$mig, "moves2000-2018.csv"))
correct <- data.table::fread(file.path(ps$cor, "districts_19.csv"))
flows <- get_flows(mig, "di", "di", by = "year")

### wrong dt, test for colnames
expect_error(correct_flows(flows = flows, dt = shp),
             pattern = "not found")

### because mig was created with ags from date 01-01, some ags can not
### be found and thus the flows after adustment are fewer (flows from
### and to ags that are not found are missing).
expect_warning(correct_flows(flows = flows, dt = correct),
               pattern = "AGS were not found")

## in 2012 all ags are found so flows after adustment should be the
## same
expect_message(correct_flows(flows = flows[year == 2012], dt = correct),
               pattern = "as expected")
## if there are other grouping variables present, correct_flows won't
## produce the correct results
flows <- get_flows(mig, "di", "di",
                   by = c("year", "EF25"))
expect_error(correct_flows(flows = flows[year == 2012], dt = correct),
             pattern = "not supposed to be in")
