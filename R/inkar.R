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

get_inkar_rows <- function(inkar, vars, us, year) {
    Raumbezug <- Zeitbezug <- Indikator <- NULL
    Kennziffer <- Wert  <- . <- NULL
        
    rb <- get_raumbezug(us)
    ink <- inkar[Raumbezug == rb & Zeitbezug == year & Indikator %in% vars, ]
    ink <- ink[, .SD[1], by = c("Indikator", "Wert")]
    ink <- ink[, .(Kennziffer, Indikator, Wert)]

    return(ink)
}

join_inkar_rows <- function(shp, ink) {
    Kennziffer <- Indikator <- NULL
    data.table::setkeyv(shp, "AGS")
    data.table::setkeyv(ink, "Kennziffer")
    ink <- data.table::dcast(ink, Kennziffer ~ Indikator, value.var = "Wert")
    shp_joined <- shp[ink]
    return(shp_joined)

}

