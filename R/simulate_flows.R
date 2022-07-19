add_move <- function(dt, shps, name_o, name_d, us_o, us_d) {

    GEN <- AGS <- NULL
    
    unit_o <- get_shpunit(us_o)
    unit_d <- get_shpunit(us_d)
    
    ags_o <- shps[[unit_o]][GEN == name_o, AGS]
    ags_d <- shps[[unit_d]][GEN == name_d, AGS]

    unit_ocol <- get_unitcol(us_o, dest = FALSE)
    unit_dcol <- get_unitcol(us_d, dest = TRUE)
    ags_ocol <- get_agscol(unit_ocol)
    ags_dcol <- get_agscol(unit_dcol)

    new_cols <- c(unit_ocol, unit_dcol, ags_ocol, ags_dcol)
    new_values <- c(name_o, name_d, ags_o, ags_d)
    n_rows <- nrow(dt)

    new_row <- as.data.table(t(rep(NA, ncol(dt))))
    colnames(new_row) <- colnames(dt)
    new_row[, (new_cols) := as.list(new_values)]
    dtnew <- rbind(dt, new_row)

    return(dtnew)
}
