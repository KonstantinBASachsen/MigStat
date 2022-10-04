##' Creates random moves according to Gravity Model between regions.
##'
##' This function creates random moves between origin-destination
##' pairs (od-pairs) of regions. The probability of one od-pair to be
##' drawn is proportional to (population_o * population_d) /
##' distance_od^2. This is the classical gravity model.
##'
##' It returns a data.table. Every row is one move between two regions.
##' 
##' @title Random data based on gravity model
##' @param shp The shapefile of desired regions. Either all states,
##'     all districts or all municipalities.
##' @param size Sample size.
##' @param dist Vector of pairwise distances. If not provided it is
##'     created. This can take a long time.
##' @param probs If TRUE, probabilites of every od-pair to be sampled
##'     are returned. Useful for debugging. 
##' @return It returns a data.table. Every row is one move between two regions.
##' @import data.table
##' @export samples_gravity
##' @author Konstantin
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


gravity_probs <- function(reg_combinations, correction = TRUE) {
    distance <- pop_o <- pop_d <- probs <- origin <- destination <- NULL
    flows_probs <- copy(reg_combinations)
    flows_probs[distance != 0, "probs" := pop_o * pop_d / distance^2]
    flows_probs[origin == "11" & destination == "12", "probs" := probs / 10]
    flows_probs[origin == "12" & destination == "11", "probs" := probs / 10]    
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

