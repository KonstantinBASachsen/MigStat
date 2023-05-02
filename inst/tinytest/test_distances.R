paths <- make_paths()
dist_f <- file.path(paths$dist, "distances_st.csv")
## Currently I do not use read_distances because I just compute them
## using get_distances. Will change when I analyze municipalities

## expect_warning(read_distances(dist_f), "integer") ## I think bc ##
## distance_pos and ## distance_centroid ## are included
## expect_warning(read_distances(dist_f, type = "cha"), "integer")
