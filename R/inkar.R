##' The inkar data set holds very many interesting variables on
##' different regional levels and for different years. This function
##' joins given variables to a data table from a shapefile.
##'
##' The function assumes that the id column is called "AGS".
##'
##' The package is designed in a way that assumes that shp comes from
##' the function clean_shp; although that is not required.
##' 
##' @title Join variables from inkar data set to table.
##' @param shp The data.table with at least one column called "AGS"
##'     which holds the identifier for regions.
##' @param inkar inkar data.table
##' @param vars character vector with variable names from inkar data
##'     that are to be joined.
##' @param us unit_simple: "st", "di" or "mu".
##' @param year The yearwhich is to be joined. Assumed to be a
##'     character .
##' @return data.table with joined variables.
##' @import data.table
##' @export join_inkar_vars
##' @author Konstantin
join_inkar_vars <- function(shp, inkar, vars, us, year) {
    ink <- get_inkar_vars(inkar, vars, us, year)
    shp_ink <- join_inkar(shp, ink)
    return(shp_ink)
}


join_inkar <- function(shp, ink) {
    ### add message that wide == TRUE is expected
    Kennziffer <- Indikator <- NULL
    data.table::setkeyv(shp, "AGS")
    data.table::setkeyv(ink, "Kennziffer")
    shp_joined <- shp[ink]
    return(shp_joined)

}

get_inkar_vars <- function(inkar, vars, us, year, wide = TRUE) {
    Raumbezug <- Zeitbezug <- Indikator <- NULL
    Kennziffer <- Wert  <- . <- NULL
        
    rb <- get_raumbezug(us)
    ink <- inkar[Raumbezug == rb & Zeitbezug == year & Indikator %in% vars, ]
    ink <- ink[, .SD[1], by = c("Indikator", "Wert")]
    ink <- ink[, .(Kennziffer, Indikator, Wert)]
    if(wide == TRUE) {
        ink <- data.table::dcast(ink, Kennziffer ~ Indikator, value.var = "Wert")
    }

    return(ink)
}

get_raumbezug <- function(us) {
    
    stopifnot(us %in% c("st", "di", "mu"))
    if (us == "st") {
        rb <- "Bundesl\U00E4nder"
    } else if (us == "di") {
        rb <- "Kreise"
    } else {
        rb <- "Gemeinden"
    }
    return(rb)
}

##' Reads the inkar.csv dataset. Returns a data.table. Optionally
##' converts the regional identiferi column, "Kennziffer", to
##' character and adds leading 0's to one digit id's. This ensures
##' that the data can be joined to shapefile and migration statistics.
##'
##' @title Read inkar data set as data.table
##' @param path Path to inkar.csv
##' @param leading_0 If TRUE, converts the regional identiferi column,
##'     "Kennziffer", to character and adds leading 0's to one digit
##'     id's. See details.
##' @return data.table
##' @import data.table
##' @export read_inkar
##' @author Konstantin
read_inkar <- function(path, leading_0 = TRUE) {
    Kennziffer <- NULL
    inkar <- data.table::fread(path, dec = ",")
### grap strings that start and end with [0-9] and add leading 0
### because in shapefiles and migration statistics the id is coded
### that way
    if (leading_0 == TRUE) {
        old <- typeof(inkar[, Kennziffer])
        inkar[, "Kennziffer" := as.character(Kennziffer)]
        inkar[, "Kennziffer" := gsub("(^[1-9]$)", "0\\1", Kennziffer)]
        new <- typeof(inkar[, Kennziffer])
        mes <- "Kennziffer from %s converted to %s and leading 0's added \n
                to make sure joining to shapefile works"
        message(sprintf(mes, old, new))
    }
    return(inkar)
}


