get_net <- function(dt, us) {
    w <- get_wins(dt, us)
    l <- get_losses(dt, us)
    keys <- unique(c(w[, ags], l[, ags]))
    setkeyv(w, "ags")
    setkeyv(l, "ags")
    w <- w[keys, ]
    w[is.na(flow), "flow" := 0]
    l <- l[keys, ]
    l[is.na(flow), "flow" := 0]
    setkeyv(w, "ags")
    setkeyv(l, "ags")
    w[l, "flow" := flow - i.flow]
    ### this next lines ensure that if there are regions in losses
    ### that are not in wins, the name of the region is transfered to
    ### wins
    row <- w[is.na(region)][l, "region" := i.region]
    idx <- which(is.na(w[, region]))
    w[idx] <- row
    return(w)
}


##' Get wins (in-migration) for regions
##'
##' This function returns the number of in-migrations for every region
##' of the specified type in the data.table.
##' @title Get wins per region
##' @param dt data.table
##' @param us "unit simple". One of the following strings: "st"
##'     (federal_states), "di" (districts) or "mu" (municipalities).
##' @return data.table with columns: unit, ags, flow
##' @export get_wins
##' @author Konstantin
get_wins <- function(dt, us) {

    wins <- get_flow(dt, us, T)

    return(wins)
}


##' Get losses (out-migration) for regions
##'
##' This function returns the number of out-migrations for every region
##' of the specified type in the data.table.
##' @title Get losses per region
##' @param dt data.table
##' @param us "unit simple". One of the following strings: "st"
##'     (federal_states), "di" (districts) or "mu" (municipalities).
##' @return data.table with columns: unit, ags, flow
##' @export get_losses
##' @author Konstantin
get_losses <- function(dt, us) {

    losses <- get_flow(dt, us, F)

    return(losses)    
}


get_flow <- function(dt, us, dest) {

### us unit short: abbreviation for administrative unit, either "st"
### for state, "di" for district or "mu" for municipality. If dest =
### T, then destinations are considered, that is: wins. If dest = F,
### then origins are considered, that is, losses
    
    unit <- get_unitcol(us, dest)
    ags <- get_agscol(unit)
    dtf <- dt
    dtf[, "flow" := .N, by = ags]
    flows <- dtf[, .SD[1], by = ags]
    flows <- flows[, .SD, .SDcols = c(unit, ags, "flow")]
    dt[, "flow" := NULL]
    colnames(flows)[1] <- "region"
    colnames(flows)[2] <- "ags"
    return(flows)
}

##' get_flows() returns the sum of flows between every
##' origin-destination pair. Also, the distances between the centroids
##' of origin and destination are returned. od pairs without flow are
##' added as well.
##'
##' The function needs two tables. One with the regional information
##' and the other with the flows between those pairs.
##'
##' The first is based on a shapefile from the
##' "Bundesamt für Karthographie und Geodäsie". This holds region
##' identifier and geometry.
##'
##' The other holds the flows between regions. This is the migration
##' statistics.
##'
##' Currently the function returnes all flows between the od pairs of
##' a given type. The types are specified via the argument "us" and
##' refer to federal states, districts or municipalities.
##' 
##' @title Origin-destination flows and distances from migration
##'     statistics
##' @param dt Migration Statistics data.table where every row is one
##'     migration.
##' @param shps The shapefile with three levels: federal states,
##'     districts, municipalities.
##' @param us "unit simple" between regions of which type are flows to
##'     be computed? Takes one of the following strings:
##'
##' "st": federal states "di": districts "mu": municipalities
##' @param na_to_0 logical, if TRUE, NA flows, that is, od_pairs where
##'     no migration took place are set to 0.
##' @return data.table with four columns: origin id, destination id,
##'     distance, flow
##' @import data.table
##' @export get_flows
##' @author Konstantin
get_flows <- function(dt, shps, us, na_to_0 = TRUE) {
    flow <- NULL
    flows <- get_flows_only(dt, us)
    dist <- get_distances(shps, us)
    flows <- join_distances(flows, dist, us, full = TRUE)
    ### I think the next two lines I implemented to obtain the data
    ### structure needed for spflow
##    flows[, c("destination", "origin") := lapply(.SD, as.numeric), .SDcols = c("destination", "origin")]
##    flows[, c("destination", "origin") := lapply(.SD, as.factor), .SDcols = c("destination", "origin")] 
    flows <- flows[, .SD, .SDcols = c("destination", "origin", "flow", "distance")]
    if (na_to_0 == TRUE) {
        flows[is.na(flow), "flow" := 0]
    }
    return(flows)

    ### probably the functions below do too much like adding columns I
    ### dont use. Maybe copying data.tables is also too much
}

## join_flows <- function(shapes, flows, key, unit) {
##     ### do I still need this function? I wrote it because I wanted to
##     ### retain all the regions with no flows for plotting but
##     ### join_administries now does a full join so all regions with no
##     ### flows are kept
##     i.flow <- GF <- NULL
##     ## unit <- colnames(flows)[unit_pos]
##     ## ags <- get_agscol(unit)
##     flows <- flows[stats::complete.cases(flows)]
## ##    unit <- strsplit(unit, "_")[[1]][1]
##     shape <- shapes[[unit]]
##     data.table::setkeyv(flows, key)
##     data.table::setkeyv(shape, "AGS")
 
##     shape <- shape[flows, "flows" := i.flow]
##     shape <- shape[GF == 4, ] ### other numbers hold differnt
##                               ### geometries, like without water I
##                               ### think I read in some documentation.

##     return(shape)    
## }


