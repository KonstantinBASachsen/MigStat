library("data.table")
library("MigStat")
library("ggplot2")
library("sf")

### reading data
fig_path <- "./figs/"
mig_path <- "~/Diss/inst/extdata/Wanderungsdaten FDZ/Dokumente StaLa/WandZuzug_dummy_2014ff_4480-2021.sav"
shps_path <- "~/Diss/inst/extdata/vg250_3112.utm32s.shape.ebenen/vg250_ebenen/"
shps_path <- "~/Diss/inst/extdata/vg250-ew_3112.utm32s.shape.ebenen/vg250-ew_ebenen/"
dt <- MigStat:::read_migex(mig_path)
shps <- MigStat:::read_shapes(shps_path)

### create fake data
us <- "di"
mig <- MigStat::n_new_rows(dt, shps, us, us, 2000)
shp <- MigStat:::clean_shp(shps, us, c("AGS", "GEN", "EWZ", "geometry"))

### wins
wins <- MigStat::get_wins(mig, us)
shp_w <- MigStat:::join_to_shp(shp, wins, "wins")

ggplot(MigStat:::set_geom(shp_w, F)) +
    geom_sf(aes(fill = wins))
ggsave(MigStat:::fpath(fig_path, "wins", "pdf"))

### losses
losses <- MigStat::get_wins(mig, us)
shp_l <- MigStat:::join_to_shp(shp, losses, "losses")

ggplot(MigStat:::set_geom(shp_l, F)) +
    geom_sf(aes(fill = losses))
ggsave(MigStat:::fpath(fig_path, "losses", "pdf"))

### net migration
net <- MigStat:::get_net(mig, us)
shp_n <- MigStat:::join_to_shp(shp, net, "net")

ggplot(MigStat:::set_geom(shp_n, F)) +
    geom_sf(aes(fill = net))
ggsave(MigStat:::fpath(fig_path, "net", "pdf"))


#### relationship with area (as a not working proxy for population
#### size)
areas <- round(sf::st_area(MigStat:::set_geom(shp, F)) / 1000, 0)

shp_n[, "areas" := areas]

pdf(MigStat:::fpath(fig_path, "area_net", "pdf"))
plot(x = shp_n$areas, y = shp_n$net, type = "p")
dev.off()

cor(shp_n$areas, shp_n$net)


### Distribution of flows
flows <- MigStat::get_flows(mig, shps, "di")
flows[, .N, by = flow]
flows[flow == 0, "flows" := 0.01]
hist(log(flows[, flow]))

mig_st <- MigStat::n_new_rows(dt, shps, "st", "st", 2000)
flows_st <- MigStat::get_flows(mig_st, shps, "st")
flows_st[, .N, by = flow]
hist(log(flows_st$flow))


### create neighbour object

mid_points <- sf::st_point_on_surface(MigStat:::set_geom(shp))
shp_nb <- list(
    "by_contiguity" = spdep::poly2nb(MigStat:::set_geom(shp)) ##,
##    "by_distance" = spdep::dnearneigh(mid_points,d1 = 0, d2 = 1500), ## 
##    "by_knn" = spdep::knn2nb(spdep::knearneigh(mid_points, 3))
)

pdf(MigStat:::fpath(fig_path, "neighbours", "pdf"))
plot(MigStat:::set_geom(shp))
plot(shp_nb$by_contiguity, mid_points, add = TRUE)
title("Contiguity") 
dev.off()


### create data structure for model

idx <- !(colnames(shp) %in% c("geometry", "GEN", "EWZ"))
shp <- shp[, ..idx]

m_cols <- c("areas") ## columns included in model
shp[,  (m_cols) := lapply(.SD, scale), .SDcols = m_cols]

shp_net <- 
  spflow::sp_network_nodes(
    network_id = "districts",
    node_neighborhood = spdep::nb2mat(shp_nb$by_contiguity),
    node_data = shp,
    node_key_column = "AGS")


shp_net_pairs <- spflow::sp_network_pair(
  orig_net_id = "districts",
  dest_net_id = "districts",
  pair_data = flows,
  orig_key_column = "origin",
  dest_key_column = "destination")

shp_multi_net <- spflow::sp_multi_network(shp_net, shp_net_pairs)


results_default <- spflow::spflow(
  flow_formula = log(1 + flow) ~ . + G_(log( 1 + distance)),
  sp_multi_network = shp_multi_net)

results_default
