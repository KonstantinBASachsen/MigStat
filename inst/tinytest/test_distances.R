paths <- make_paths()
dist_f <- file.path(paths$dist, "distances_st.csv")
expect_warning(read_distances(dist_f), "integer")
expect_warning(read_distances(dist_f, type = "cha"), "integer")
