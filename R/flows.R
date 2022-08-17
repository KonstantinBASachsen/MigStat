##' computes the difference of in-migration and out-migration for all
##' regions of one of the following types: states, districts,
##' municipalities.
##'
##' The function makes sure that regions where no people in-migrated
##' or out-migrated are handled approbpriately. That is, missing
##' in-migration or out-migration is treated as 0.
##' @title net migration for regions
##' @param mig data.table of Migration Statistics
##' @param us One of: "st", "di" or "mu", short hand for "states",
##'     "districts", "municipalities".
##' @return data.table with region, ags and net flow.
##' @import data.table
##' @export get_net
##' @author Konstantin
get_net <- function(mig, us) {
    ags <- flow <- region <- i.flow <- i.region <- NULL
    w <- get_wins(mig, us)
    l <- get_losses(mig, us)
    keys <- unique(c(w[, ags], l[, ags]))
    data.tabl::setkeyv(w, "ags")
    data.tabl::setkeyv(l, "ags")
    w <- w[keys, ]
    w[is.na(flow), "flow" := 0]
    l <- l[keys, ]
    l[is.na(flow), "flow" := 0]
    data.tabl::setkeyv(w, "ags")
    data.tabl::setkeyv(l, "ags")
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

##' In-migration, out-migration or net-migration can be viwed as an
##' attribute of regions. join_to_shp() takes a data.table and joins
##' it to the data.table holding the shape information.
##'
##' @title join wins, losses or net migration to data.table of
##'     shapefile
##' @param shp data.table holding the shape information. Expected to
##'     have column "AGS" that is used to join.
##' @param dt data.table holding the flows (either wins, losses, or
##'     net). Might be output from get_wins(), get_losses() or
##'     get_net().
##' @param col Name of new column in returned shp data.table.
##' @param key_dt Name of column in dt that holds the AGS.
##' @return shp data.table with joined column
##' @import
##' data.table
##' @export
##' join_to_shp
##' @author Konstantin
join_to_shp <- function(shp, dt, col, key_dt = "ags") {
    i.flow <- NULL
    shpj <- data.table::copy(shp)
    data.table::setkeyv(shpj, "AGS")
    data.table::setkeyv(dt, key_dt)
    shpj[dt, (col) := i.flow]
    return(shpj)
}
