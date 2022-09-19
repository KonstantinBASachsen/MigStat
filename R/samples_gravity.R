samples_gravity <- function(shp, size, dist = NULL, probs = FALSE) {
    destination <- origin <- AGS <- NULL
    if (is.null(dist)) { dist <- get_distances(shp) }

    dist[, "od" := paste(destination, origin, sep = "_")]
    ags <- shp[order(AGS), AGS]
    combs <- create_region_combs(ags)
    combs <- join_distances(combs, dist)
    combs <- join_populations(combs, shp)
    combs <- gravity_probs(combs)
    rows <- sample_gravity(combs, 10000)
    rows <- group_samples(rows)
    combs <- join_samples(combs, rows)
    if (probs == TRUE) {
        combs <- keep_cols(combs, c("origin", "destination",
                                    "distance", "flow", "probs"))
    } else {
        combs <- keep_cols(combs, c("origin", "destination",
                                    "distance", "flow"))
    }
    
    return(combs)
}

create_region_combs <- function(ags) {
### this should be used by get_flows() as well
    origin <- destination <- NULL
    combs <- setDT(expand.grid(ags, ags))
    colnames(combs) <- c("origin", "destination")
    combs[, "od" := paste(destination, origin, sep = "_")]
    return(combs)
    }


gravity_probs <- function(reg_combinations) {
    distance <- pop_o <- pop_d <- probs <- NULL
    flows_probs <- copy(reg_combinations)
    flows_probs[distance != 0, "probs" := pop_o * pop_d / distance^2]
    flows_probs[distance == 0, "probs" := 0]
    normalize <- flows_probs[, sum(probs)]
    flows_probs[, "probs" := probs / normalize]
##    flows_probs[, "od_pair" := paste(origin, destination, sep = "_")]
    return(flows_probs)
}

sample_gravity <- function(combs, size = 1000) {
    od <- probs <- NULL
    rows <- sample(combs[, od],
                   size = size, replace = TRUE, prob = combs[, probs])
    rows <- setDT(data.frame(rows))
    colnames(rows) <- "od"
        return(rows)
}

group_samples <- function(samples) {
    od <- NULL
    s_gr <- samples[, "flow" := .N, by = od]
    s_gr <- s_gr[, .SD[1], by = "od"]
    return(s_gr)
}


join_samples <- function(reg_combinations, samples, na_to_0 = TRUE) {
    od <- i.flow <- flow <- NULL
    combs <- copy(reg_combinations)
    s <- copy(samples)
    setkeyv(combs, "od")
    setkeyv(s, "od")
    ss <- s[combs[, od]] ## fills missing od pairs with NA flows
    combs <- combs[ss, "flow" := i.flow]
    if (na_to_0 == TRUE) { combs[is.na(flow), "flow" := 0] }
    return(combs)
    }

