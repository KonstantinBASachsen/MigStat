ps <- make_paths()
ps$data <- "~/Diss/exec/one_run/data/clean/"
ps$cor <- file.path(ps$data, "corrections")
shp <- read_clean_shps(ps$shps, "complete")$districts
mig <- data.table::fread(file.path(ps$mig, "moves2000-2018.csv"))
correct <- data.table::fread(file.path(ps$cor, "districts_19.csv"))
flows <- get_flows(mig, "di", "di", by = "year")
flows_gr <- get_flows(mig, "di", "di",
                   by = c("year", "EF25"))

## if there are other grouping variables present, correct_flows won't
## produce the correct results
expect_error(correct_flows(flows = flows_gr[year == 2012], dt = correct),
             pattern = "not supposed to be in")

### wrong dt, test for colnames
expect_error(correct_flows(flows = flows, dt = shp),
             pattern = "not found")

## there might be origins that are not found and there might be
## destinations that are not found. Here I test for all four
## combinations of found/ not found if the correct message/ warnings
## are emitted. In the data for the years 2011 to 2015 all ags are
## found. For all of the other years ags are missing for both origin
## and destination.

### I currently only test if ags were found. I do not test for the
### reported number of flows

## all ags are found
## in 2012 all ags are found so flows after adustment should be the
## same
expect_message(correct_flows(flows = flows[year == 2012], dt = correct),
               pattern = "All AGS were found for 'origin'.")
expect_message(correct_flows(flows = flows[year == 2012], dt = correct),
               pattern = "All AGS were found for 'destination'.")


## all origins are found, destinations are missing
not_found <- setdiff(flows[year == 2000, unique(origin)], correct[year == 2000, ags_old])
flows_test <- flows[! origin %in% not_found & year == 2000]
expect_message(correct_flows(flows_test, correct),
               pattern = "All AGS were found for 'origin'.")
expect_warning(correct_flows(flows_test, correct),
               pattern = "Several AGS were not found for 'destination'!")

## all destinations are found, origins are missing
not_found <- setdiff(flows[year == 2000, unique(destination)], correct[year == 2000, ags_old])
flows_test <- flows[! destination %in% not_found & year == 2000]
expect_message(correct_flows(flows_test, correct),
               pattern = "All AGS were found for 'destination'.")
expect_warning(correct_flows(flows_test, correct),
               pattern = "Several AGS were not found for 'origin'!")

## both: origins and destinations are missing
expect_warning(correct_flows(flows[year == 2000, ], correct[year == 2000]),
               pattern = "Several AGS were not found for 'origin'!")
expect_warning(correct_flows(flows[year == 2000, ], correct[year == 2000]),
               pattern = "Several AGS were not found for 'destination'!")

######################################################################
########### Test if specific corrections are correct #################
######################################################################
### No changes, same ags, so nothing should be changed
cor_test <- correct[year == 1990]
test_flows <- data.table(origin = c(1001, 15003),
                         destination = c(1051, 15003),
                         flow = c(100, 1000),
                         year = 1990)
test_flows2 <- correct_flows(test_flows, correct)
expect_equal(test_flows2[, flow], test_flows[, flow])
### Eingemeindungen. Parts if the origins in the next test_flows were
### all matched to Sömmerda 16068. This means that parts of the flows
### of the origins have to be matched to 16068.
test_flows <- data.table(origin = c(16014, 16018, 16038),
                         destination = c(1051, 15003, 1004),
                         flow = c(100, 1000, 500),
                         year = 1990)
test_flows2 <- correct_flows(test_flows, correct)
### by looking at cor_test[ags_new == 16068] I determine the number of
### flows from 16068
expected <- round(100 * 0.03788186 + 1000 * 0.37883162 + 500 * 1)
expect_equal(test_flows2[origin == 16068, sum(flow)], expected)

### same as above but with the incorporated municipalities as
### destinations
test_flows <- data.table(origin = c(1051, 15003, 1004),
                         destination = c(16014, 16018, 16038),
                         flow = c(100, 1000, 500),
                         year = 1990)
test_flows2 <- correct_flows(test_flows, correct)
expect_equal(test_flows2[destination == 16068, sum(flow)], expected)
