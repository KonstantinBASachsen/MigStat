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
##' @param year The year which is to be joined. Assumed to be a
##'     character .
##' @param check logical, if TRUE, availability of vars for us and
##'     year is checked and if unavailable warning returned
##' @return data.table with joined variables.
##' @import data.table
##' @export join_inkar_vars
##' @author Konstantin
join_inkar_vars <- function(shp, inkar, vars,
                            us = c("st", "di", "mu"), year,
                            check = FALSE) {
    us <- match.arg(us)
    rb <- get_raumbezug(us)
    if (check == TRUE) { ## cause really slow
        avail <- unlist(lapply(vars, function(x) check_availability(inkar, rb, year, x)))
        vars <- vars[avail != 0]
    }
    ink <- get_inkar_vars(inkar, vars, rb, year, name_col = "Indikator")
    shp_ink <- join_inkar(shp, ink)
    return(shp_ink)
}

check_availability <- function(inkar, rb, zb, var) {
    Raumbezug <- Zeitbezug <- Indikator <- NULL
    n <- inkar[Raumbezug == rb &  Zeitbezug == zb & Indikator == var, .N]
    if(n == 0) {
        mes <- " '%s' is not available for %s and %s"
        warning(sprintf(mes, var, zb, rb), call. = FALSE)
    }
    return(n)
}

join_inkar <- function(shp, ink) {
    ### add message that wide == TRUE is expected
    data.table::setkeyv(shp, "AGS")
    data.table::setkeyv(ink, "AGS")
    shp_joined <- shp[ink]
    return(shp_joined)
}

##' Extracts variables from INKAR in a either long or wide format.
##'
##' @title Extract INKAR variables
##' @param inkar data.table inkar data
##' @param vars character, variable names
##' @param rb character, "Raumbezug" like: "Bundesländer", "Kreise"
##'     etc.
##' @param zb numeric, Zeitbezug
##' @param wide logical, if TRUE, data are returned in wide format, if
##'     false, in long format.
##' @param name_col character, name of column where variable names
##'     "vars" are stored. Usually "Indikator", in this package
##'     "varname".
##' @return data.table
##' @import data.table
##' @export get_inkar_vars
##' @author Konstantin
get_inkar_vars <- function(inkar, vars, rb, zb, wide = TRUE,
                            name_col = c("varname", "Indikator")) {
    #### Extrahiert Prediktoren aus dem INKAR Datensatz. 
    Raumbezug <- Zeitbezug <- Indikator <- NULL
    Kennziffer <- Wert <- varname <- . <- NULL
    name_col <- match.arg(name_col)
    if (name_col == "Indikator") {
        ink <- inkar[Raumbezug == rb & Zeitbezug %in% zb & Indikator %in%
                     vars, ]
        ink <- ink[, .(Kennziffer, Indikator, Zeitbezug, Wert)]
    }
    if (name_col == "varname") {
        ink <- inkar[Raumbezug == rb & Zeitbezug %in% zb & varname %in%
                     vars, ]
        ink <- ink[, .(Kennziffer, varname, Zeitbezug, Wert)]
    }
    if (wide == TRUE) {
        if (name_col == "Indikator") {
            ink <- data.table::dcast(ink, Kennziffer + Zeitbezug ~ Indikator,
                                     value.var = "Wert")
        }
        if (name_col == "varname") {
            ink <- data.table::dcast(ink, Kennziffer + Zeitbezug ~ varname,
                                     value.var = "Wert")
        }
        colnames(ink)[colnames(ink) == "Kennziffer"] <- "AGS"
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
##' @param file Ffull path to inkar.csv
##' @param tolower logical, if TRUE, varnames are all set to lower
##'     case. If no column 'varname' in data, tolower = TRUE is ignored.
##' @param to_num logical. If TRUE, c("Zeitbezug", "Wert",
##'     "Kennziffer", "ID") are converted to numeric.
##' @param leading_0 If TRUE, converts the regional identiferi column,
##'     "Kennziffer", to character and adds leading 0's to one digit
##'     id's. See details.
##' @return data.table
##' @import data.table
##' @export read_inkar
##' @author Konstantin
read_inkar <- function(file, tolower = TRUE, to_num = FALSE,
                       leading_0 = FALSE) {
    Kennziffer <- Zeitbezug <- .SD <- varname <- NULL
    inkar <- data.table::fread(file, dec = ",", encoding = "UTF-8")
### grap strings that start and end with [0-9] and add leading 0
### because in shapefiles and migration statistics the id is coded
### that way
    if (leading_0 == TRUE) {
        old <- typeof(inkar[, Kennziffer])
        inkar[, "Kennziffer" := as.character(Kennziffer)]
        inkar[, "Kennziffer" := gsub("(^[1-9]$)", "0\\1", Kennziffer)]
        new <- typeof(inkar[, Kennziffer])
        mes <- "Kennziffer from %s converted to %s and leading 0's added 
                to make sure joining to shapefile works"
        message(sprintf(mes, old, new))
    }
    if (tolower == TRUE & "varname" %in% colnames(inkar)) {
        inkar <- inkar[, "varname" := tolower(varname)]
        message("Variable names in 'varname' converted to lower case.")
    }
    if (tolower == TRUE & !"varname" %in% colnames(inkar)) {
        message("tolower = TRUE ignored because 'varname' not in data")
    }
    if (to_num == TRUE) {
        num_cols <- c("Wert", "Kennziffer", "ID")
##        inkar[Zeitbezug == "2016/2017/2018", "Zeitbezug" := NA]
        inkar[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
        message("Columns ", paste(num_cols, collapse = ", "), " converted to numeric")
    }
    return(inkar)
}

##' Joins predictors to data.table of flows 
##'
##' Keys: In flows origin and year as well as destination and year are
##' used for joining
##' @title Predictors to flows
##' @param flows data.table of flows
##' @param design_mat data.table of predictors
##' @param key character of length == 2, keys in design_mat used for
##'     joining. Firs element of key is expected to be the AGS and the
##'     second key the time period
##' @return data.table
##' @import data.table
##' @export join_design_mat
##' @author Konstantin
join_design_mat <- function(flows, design_mat,
                            key = c("AGS", "Zeitbezug")) {
    #### Fügt die Prediktoren aus dem INKAR datensatz zu den
#### aggregierten Wanderungen hinzu
    stopifnot("key must be of length 2. First element of key for the AGS and the second for the period" = length(
                  key) == 2)
    cols <- colnames(design_mat)
    vars <- cols[!cols %in% key]
    ### better to just use merge? I think I read somewhere that then bc of methods
    ### dispatch merge.data.table is used
    flows <- merge(flows, design_mat,
                   by.x = c("origin", "year"),
                   by.y = key,
                   all.x = TRUE)
    data.table::setnames(flows, vars, paste0(vars, "_o"))
    flows <- merge(flows, design_mat,
                   by.x = c("destination", "year"),
                   by.y = key,
                   all.x = TRUE)
    data.table::setnames(flows, vars, paste0(vars, "_d"))
    return(flows)
}
