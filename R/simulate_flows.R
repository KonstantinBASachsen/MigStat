
new_move <- function(dt, shps, name_o, name_d, us_o, us_d, n, ret_df = FALSE) {

    ### add test für BaWü, because there more than one ags is returned
### from shapefile
    #### if there is a NA function breaks
    GEN <- AGS <- GF <- NULL

    n_rows <- nrow(dt)
    unit_o <- get_shpunit(us_o)
    unit_d <- get_shpunit(us_d)
    ags_o <- shps[[unit_o]][GF == 4 & GEN == name_o, AGS]
    ags_d <- shps[[unit_d]][GF == 4 & GEN == name_d, AGS]
    unit_ocol <- get_unitcol(us_o, dest = FALSE)
    unit_dcol <- get_unitcol(us_d, dest = TRUE)
    ags_ocol <- get_agscol(unit_ocol)
    ags_dcol <- get_agscol(unit_dcol)

    ### better to add columns and then add new row using join?
    stopifnot("Ensure that columns that are joined are already in data.table"
              = c(unit_ocol, unit_dcol) %in% colnames(dt))

    new_cols <- c(unit_ocol, unit_dcol, ags_ocol, ags_dcol)
    new_values <- c(name_o, name_d, ags_o, ags_d)
    ## stopifnot(print(sprintf("Something went wrong with the new values %s", paste(new_values, collapse = " "))) =
    ##               length(new_values) == 4)
    stopifnot("something went wrong with the new values" = length(new_values) == 4)

    new_row <- as.data.table(t(rep(NA, ncol(dt))))
    colnames(new_row) <- colnames(dt)
    new_row[, (new_cols) := as.list(new_values)]
    new_row <- new_row[rep(1, n)]

    if(ret_df == TRUE) { dtnew <- rbind(dt, new_row) }
    else{dtnew <- new_row}

    return(dtnew)
}
