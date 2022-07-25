##' Create n new moves randomly chosen from origin to
##' destination. Sampled from the uniform distribution. This means,
##' every origin-destination pair has the same probability.
##'
##' The n new moves are returned as a data.table with n rows. The
##' table has the same structure as the migration statistics.
##' 
##' @title n moves from uniformly chosen pair of origin and
##'     destination.
##' @param dt data.table which structure (colnames) is to be
##'     maintained.
##' @param shps The shapfile holding the regions from which a sample
##'     is taken. Sample is taken from the AGS keys.
##' @param us_o One of: "st", "di", "mu", regional level of randomly
##'     sampled origin.
##' @param us_d One of: "st", "di", "mu", regional level of randomly
##'     sampled destination.
##' @param n Number of samples
##' @return data.table of sampled rows
##' @export n_new_rows
##' @author Konstantin
n_new_rows <- function(dt, shps, us_o, us_d, n) {

    ### This function is not vectorized. Drawing samples takes too
    ### long. To vectorize, sample_move needs to be vectorized. I
    ### think this I will only approach if I am really sure I need
    ### this often enough.
    
    l <- lapply(1:n, function(x)  new_row(dt, shps, sample_move(dt, shps, us_o, us_d)))
    l <- data.table::rbindlist(l)
    
    return(l)
}

new_row <- function(dt, shps, sample_list) {

### creates a data.table with one row and columns corresponding to the
### original data.table. All values are NA, except the name of origin
### and destination and the columns that store the AGS of origin and
### destination.
    
    ### add test für BaWü, because there more than one ags is returned
### from shapefile
#### if there is a NA function breaks

    ### I do not need to pass dt, colnames are sufficient
    GF <- AGS <- GEN <- NULL
    
    s <- sample_list
    unit_o <- get_shpunit(s$us_o)
    unit_d <- get_shpunit(s$us_d)
    name_o <- shps[[unit_o]][GF == 4 & AGS == s$ags_o, GEN]
    name_d <- shps[[unit_d]][GF == 4 & AGS == s$ags_d, GEN]
    unit_ocol <- get_unitcol(s$us_o, dest = FALSE)
    unit_dcol <- get_unitcol(s$us_d, dest = TRUE)
    ags_ocol <- get_agscol(unit_ocol)
    ags_dcol <- get_agscol(unit_dcol)


    new_cols <- c(unit_ocol, unit_dcol, ags_ocol, ags_dcol)
    new_values <- c(name_o, name_d, s$ags_o, s$ags_d)
    ## stopifnot(print(sprintf("Something went wrong with the new values %s", paste(new_values, collapse = " "))) =
    ##               length(new_values) == 4)
    stopifnot("something went wrong with the new values" = length(new_values) == 4)

    new_row <- as.data.table(t(rep(NA, ncol(dt))))
    colnames(new_row) <- colnames(dt)
    new_row[, (new_cols) := as.list(new_values)]

    return(new_row)
}

sample_move <- function(dt, shps, us_o, us_d) {

    GF <- AGS <- NULL
    
    unit_o <- get_shpunit(us_o)
    unit_d <- get_shpunit(us_d)
    samplespace_o <- unique(shps[[unit_o]][GF == 4, AGS])
    samplespace_d <- unique(shps[[unit_d]][GF == 4, AGS])
    sample_o <- round(stats::runif(1, 1, length(samplespace_o)), 0)
    ags_o <- samplespace_o[sample_o]
    sample_d <- round(stats::runif(1, 1, length(samplespace_d)), 0)
    ags_d <- samplespace_d[sample_d]
    sample <- list()
    sample$ags_o <- ags_o
    sample$ags_d <- ags_d
    sample$us_o <- us_o
    sample$us_d <- us_d

    return(sample)

}

