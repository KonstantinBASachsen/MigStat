get_raumbezug <- function(us) {
    
    stopifnot(us %in% c("st", "di", "mu"))
    if (us == "st") {
        rb <- "Bundesländer"
    } else if (us == "di") {
        rb <- "Kreise"
    } else {
        rb <- "Gemeinden"
    }
    return(rb)
}

get_inkar_rows <- function(inkar, vars, us, year) {
    Raumbezug <- Zeitbezug <- Indikator <- Kennziffer <- Wert <- NULL
    rb <- get_raumbezug(us)
    ink <- inkar[Raumbezug == rb & Zeitbezug == zb & Indikator %in% cols]
    ink <- ink[, .SD[1], by = c("Indikator", "Wert")]
    ink <- ink[, .(Kennziffer, Indikator, Wert)]

    return(ink)
}

join_inkar_rows <- function(shp, ink) {
    Kennziffer <- Indikator <- NULL
    setkeyv(shp, "AGS")
    setkeyv(ink, "Kennziffer")
    ink <- dcast(ink, Kennziffer ~ Indikator, value.var = "Wert")
    shp_joined <- shp[ink]
    return(shp_joined)

}
