samples_gravity <- function(shp, size, dist = NULL) {
    
    if (is.null(dist)) { dist <- get_distances(shp) }

    dist[, "od" := paste(destination, origin, sep = "_")]
    ags <- shp[order(AGS), AGS]
    combs <- create_region_combs(ags)
    combs <- join_dist(combs, dist)
    combs <- join_populations(combs, shp)
    combs <- gravity_probs(combs)
    rows <- sample_gravity(combs, 10000)
    rows <- group_samples(rows)
    combs <- join_samples(combs, rows)
    combs <- keep_cols(combs, c("origin", "destination", "distance", "flow"))
    return(combs)
}

create_region_combs <- function(ags) {
    combs <- setDT(expand.grid(ags, ags))
    colnames(combs) <- c("origin", "destination")
    combs[, "od" := paste(destination, origin, sep = "_")]
    return(combs)
    }


gravity_probs <- function(flows) {
    flows_probs <- copy(flows)
    flows_probs[distance != 0, "probs" := pop_o * pop_d / distance^2]
    flows_probs[distance == 0, "probs" := 0]
    normalize <- flows_probs[, sum(probs)]
    flows_probs[, "probs" := probs / normalize]
##    flows_probs[, "od_pair" := paste(origin, destination, sep = "_")]
    return(flows_probs)
}

sample_gravity <- function(combs, size = 1000) {
    rows <- sample(combs[, od],
                   size = size, replace = TRUE, prob = combs[, probs])
    rows <- setDT(data.frame(rows))
    colnames(rows) <- "od"
        return(rows)
}

group_samples <- function(samples) {
    s_gr <- samples[, "flow" := .N, by = od]
    s_gr <- s_gr[, .SD[1], by = "od"]
    return(s_gr)
}


join_samples <- function(reg_combinations, samples) {
    combs <- copy(reg_combinations)
    s <- copy(samples)
    setkeyv(combs, "od")
    setkeyv(s, "od")
    ss <- s[combs[, od]] ## fills missing od pairs with NA flows
    combs <- combs[ss, "flow" := i.flow]
    combs[is.na(flow), "flow" := 0]
    return(combs)
    }

