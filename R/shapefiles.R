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
