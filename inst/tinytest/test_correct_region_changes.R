ps <- make_paths()
ps$data <- "~/Diss/exec/one_run/data/clean/"
ps$cor <- file.path(ps$data, "corrections")
shp <- read_clean_shps(ps$shps, "complete")$districts
mig <- data.table::fread(file.path(ps$mig, "moves2000-2018.csv"))
correct <- data.table::fread(file.path(ps$cor, "districts_19.csv"))
flows <- get_flows(mig, "di", "di", by = "year")
flows_gr <- get_flows(mig, "di", "di",
                   by = c("year", "EF25"))

## if there are other grouping variables present, these are taken as
## additional grouping columns. A message is issued in this case
expect_message(correct_flows(flows = flows_gr[year == 2012], dt = correct),
             pattern = "additional key column")

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
test_flows <- data.table::data.table(origin = c(1001, 15003),
                         destination = c(1051, 15003),
                         flow = c(100, 1000),
                         year = 1990)
test_flows2 <- correct_flows(test_flows, correct)
expect_equal(test_flows2[, flow], test_flows[, flow])
### Eingemeindungen. Parts if the origins in the next test_flows were
### all matched to Sömmerda 16068. This means that parts of the flows
### of the origins have to be matched to 16068.
test_flows <- data.table::data.table(origin = c(16014, 16018, 16038),
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
test_flows <- data.table::data.table(origin = c(1051, 15003, 1004),
                         destination = c(16014, 16018, 16038),
                         flow = c(100, 1000, 500),
                         year = 1990)
test_flows2 <- correct_flows(test_flows, correct)
expect_equal(test_flows2[destination == 16068, sum(flow)], expected)


######################################################################
#### Test if specific corrections in simple example are correct ######
######################################################################

ids <- c("id1", "id1", "id1", "id2", "id2", "id2", "id4", "id4")
group <- c("A", "B", "C", "A", "B", "C", "A", "B")
flow <- c(1, 2, 3, 4, 5, 6, 7, 8)
year <- 2015
dest <- c("id4", "id4", "id4", "id5","id5", "id5", "id2", "id2")
tab1 <- data.table(region = ids, group = group, flow = flow, year = year)
tab1 <- data.table(origin = ids, destination = dest,
                   group = group, flow = flow, year = year)
# Create the second data table
ids <- c("id1", "id1", "id2", "id2", "id2", "id4", "id5")
ids_new <- c("id1", "id3", "id2", "id3", "id4", "id4", "id5")
conv <- c(0.9, 0.1, 0.7, 0.2, 0.1, 1, 1)
year <- 2015
tab2 <- data.table(ags_old = ids, ags_new = ids_new, conv_p = conv, year = year)

#### do corrections for origin work?
tab_c <- correct_flow(tab1, tab2, ags_col = "origin")
setcolorder(tab_c, c("origin", "destination", "group", "flow"))
tab_c <- tab_c[order(origin, destination, group), ]

expect_equal(tab_c[origin == "id1" & destination == "id4", flow], c(0.9, 1.8, 2.7))
expect_equal(tab_c[origin == "id3" & destination == "id4", flow], c(0.1, 0.2, 0.3))
#### do corrections for destination work?
tab_c <- correct_flow(tab1, tab2, ags_col = "destination")
setcolorder(tab_c, c("origin", "destination", "group", "flow"))
tab_c <- tab_c[order(origin, destination, group), ]
expect_equal(tab_c[origin == "id1" & destination == "id4", flow], c(1, 2, 3))
expect_equal(tab_c[origin == "id4" & group == "A", flow], c(4.9, 1.4, 0.7))
expect_equal(tab_c[origin == "id4" & group == "B", flow], c(5.6, 1.6, 0.8))
