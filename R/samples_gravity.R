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
##' @param shps data.table The shapefile of all regions including
##'     states districts and municipalities. (Will be changed in
##'     future)
##' @param inkar data.table INKAR data set used to obtain the
##'     predictors.
##' @param us character, Oberservational unit, c("st", "di", "mu") for
##'     "states", "districts" or "municipalities."
##' @param size Sample size.
##' @param dist Vector of pairwise distances. If not provided it is
##'     created. This can take a long time.
##' @param probs If TRUE, probabilites of every od-pair to be sampled
##'     are returned. Useful for debugging.
##' @return It returns a data.table. Every row is one move between two
##'     regions.
##' @import data.table
##' @export samples_gravity
##' @author Konstantin
samples_gravity <- function(shps, inkar, us = c("st", "di", "mu"),
                            size, dist = NULL, probs = FALSE) {
    destination <- origin <- AGS <- i.distance <- NULL
    if (is.null(dist)) { dist <- get_distances(shp) }
    ### I want to return o_name and d_name as well. get_distances()
### returns those so maybe I can say join_distances() to keep them.
    us <- match.arg(us)
    message("distances done")
    unit <- get_shp_unit(us)
    unit <- paste0(unit, "s") ## in shps all in plural
    shp <- shps[[unit]]
    ags <- shp[order(AGS), AGS]
    message("creating sample space")
    combs <- create_region_combs(ags)
    message("sample space created")
    ##    combs <- join_distances(combs, dist)
    setkeyv(combs, c("origin", "destination"))
    setkeyv(dist, c("origin", "destination"))
    combs[dist, "distance" := i.distance]
    message("distances joined")
    combs <- join_inkar_vars(shp, inkar, "Bev\u00F6lkerung gesamt", us, "2017")
    message("populations joined")
    combs <- gravity_probs(combs)
    message("sample probabilities created")
    message("sampling starts")
    rows <- sample_gravity(combs, size)
    message("sampling complete")
    ## rows <- group_samples(rows)
    ## combs <- join_samples(combs, rows)
    ## if (probs == TRUE) {
    ##     combs <- keep_cols(combs, c("origin", "destination",
    ##                                 "distance", "flow", "probs"))
    ## } else {
    ##     combs <- keep_cols(combs, c("origin", "destination",
    ##                                 "distance", "flow"))
    ## }
    
    return(rows)
}

create_region_combs <- function(ags) {
### this should be used by get_flows() as well

#### takes a long time to compute this for municipalities. Maybe good
#### to save it
    
    origin <- destination <- NULL
    combs <- data.table::CJ(ags, ags)
    colnames(combs) <- c("origin", "destination")
    
##    combs[, "od" := create_od(origin, destination)]
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
    od <- probs <- origin <- destination <- i.distance <- NULL
    message("creating od pairs")
    combs[, "od" := create_od(origin, destination)]
    message("od pairs created")
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


## join_populations <- function(flows, shp) {
##     ### joins population sizes based on flows object. This function
##     ### has no own full = TRUE argument. That is, it takes the keys
##     ### from the flow object. The standard in get_flows() is to call
##     ### join_distances with full = TRUE. If population sizes are
##     ### joined afterwards it joins sizes to empty flows as well.

## ### It seems to be not really intuitive to first call get flows to
## ### simulate random draws then. Maybe I change it at some point.
## ### not sure what happens if there should be populations missing
##     i.EWZ <- rn <- NULL
##     flows_pop <- copy(flows)
##     flows_pop[, "rn" := 1:nrow(flows_pop)]
##     setkeyv(flows_pop, "origin")
##     setkeyv(shp, "AGS")
##     flows_pop[shp, "pop_o" := i.EWZ]
##     setkeyv(flows_pop, "destination")
##     flows_pop[shp, "pop_d" := i.EWZ]
##     flows_pop <- flows_pop[order(rn)]
##     flows_pop[, "rn" := NULL]
##     return(flows_pop)
## }
