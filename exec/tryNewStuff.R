ex_dat <- read_examples()
mig <- ex_dat$mig
mig$gender <- sample(c("m", "f"), nrow(mig), replace = TRUE)
mig$age_gr <- sample(c("0-6", "7-16", "16-99"), nrow(mig), replace = TRUE)
    
us <- "st"
shp <- clean_shp(ex_dat$shps, us)
flows <- get_flows(dt = mig, shp = shp, us = us, by = c("gender", "age_gr"), full = FALSE, dist = TRUE)
flows <- get_flows(dt = mig, shp = shp, us = us, full = TRUE, dist = FALSE, na_to_0=TRUE)



wins <- get_wins(flows, TRUE)[order(region)]

keys <- setDT(expand.grid(unique(wins[, region]), unique(wins[, gender])))
keys <- keys[order(Var1)]
colnames(keys) <- c("region", "gender")

setkeyv(wins, c("region", "gender" ))
wins[keys]


values <- list("region" = c(shp[, AGS], "00"), "gender" = c("m", "f"), "age_gr" = c("0-6", "7-16", "16-99"))
values



names(values)

include_missing_obs <- function(dt, values, missing_col) {
    ms <- missing_col
    stopifnot("missing_col not in data.table" = ms %in% colnames(dt))
    keys <- do.call(data.table::CJ, values)
    setkeyv(dt, names(values))
    dtfull <- dt[keys, ]
    dtfull <- dtfull[order(mget(names(values)))]
    dtfull <- dtfull[is.na(get(ms)), paste(ms)  := 0]
    return(dtfull)
}

get_net <- function(flows, values, grouped, by = NULL) {

    ### probably a good idea to supply "values" only optional and
    ### otherwise read them from the columns. Although then there
    ### might be some combinations missing that are not in the data

    ### maybe grouped is not really necessary because values already
    ### gives the grouping information
    
    wins <- get_wins(flows, grouped, by)
    wins <- include_missing_obs(wins, values, "wins")
    losses <- get_losses(flows, grouped, by)
    losses <- include_missing_obs(losses, values, "losses")
    setkeyv(wins, names(values))
    setkeyv(losses, names(values))
    wins[losses, "losses" := i.losses]
    wins[, "net" := wins - losses]
    return(wins)
}

net <- get_net(flows, values[1], FALSE)

net[, sum(wins)]
net[, sum(losses)]
net[, sum(net)]
?do.call
