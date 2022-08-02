clean_shp <- function(shps, us, keep =  c("AGS", "GEN", "EWZ", "geometry")) {

    AGS <- NULL
    
    shp_clean <- get_shp_region(shps, us, drop_gf = TRUE)
    shp_clean[, "AGS" := as.numeric(AGS)] ## should print message or warning 
    shp_clean <- keep_cols(shp_clean, keep)
    return(shp_clean)
}

get_shp_region <- function(shps, us, drop_gf) {
    GF <- NULL
    shp <- shps[[get_shp_unit(us)]]
    if (drop_gf ==  TRUE) {
        shp <- shp[GF == 4]
    }
    return(shp)
    
}

get_shp_unit <- function(us) {

    if(us == "st") {unit <- "state"}
    if(us == "di") {unit <- "district"}
    if(us == "mu") {unit <- "muni"}

    return(unit)
    
}
