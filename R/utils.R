
get_unit <- function(us, dest) {

    ### This function allows me to say which administrative region I
    ### am interested in and if I care about in or outmigration and it
    ### returns the column name
    
    stopifnot(us %in% c("st", "di", "mu"))
    stopifnot(is.logical(dest))
    
    if(us == "st") {unit <- "state"}
    if(us == "di") {unit <- "district"}
    if(us == "mu") {unit <- "muni"}
    if (dest == TRUE) {unit <- paste0(unit, "_d")}
    if (dest == FALSE) {unit <- paste0(unit, "_o")}

    return(unit)
}

get_ags <- function(unit) {

    ### this function returns the column holding the ags for thei
    ### interesting region and type of migration
    
    strings <- strsplit(unit, split = "_")[[1]]
    unit <- strings[1]
    type <- strings[2]
    stopifnot(unit %in% c("state", "district", "muni"))
    stopifnot(type %in% c("d", "o"))
    unit_num <- NA
    if(unit == "state") {unit_num <- "2"}
    if(unit == "district") {unit_num <- "4"}
    if(unit == "muni") {unit_num <- "5"}
    type_num <- NA
    if(type == "d") {type_num <- "02"}
    if(type == "o") {type_num <- "03"}

    ags_col <- paste0("EF", type_num, "U", unit_num)

    return(ags_col)
    
}

##' normalize() adds two numbers and returns the fraction of the first.
##'
##' This function simply adds two numbers and returns the fraction of
##' the first number of the sum.
##' @title return fraction of first number of sum
##' @param x first scalar
##' @param y second scalar
##' @return numeric
##' @export normalize
##' @author Konstantin
normalize <- function(x, y) {

    n <- x / sum(x,y)
    return(n)
}
