##' Creates five point summary for faster box plots
##'
##' 
##' @title five point summary by group
##' @param mig Migration Statistics data.table
##' @param col Name of column that is to be summarized
##' @param by Character vector of columns that are used as group. Five
##'     point summary is computed for every group
##' @param probs vector of quantiles to be displayed.
##' @return data.table of five point summaries
##' @author Konstantin
get_box_data <- function(mig, col, by,
                          probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
    ### for following functions to handle variables I rename the
    ### grouping variable (except) year to 'group'. This does not work
    ### when more than two grouping variables are given. Does also not
    ### work when two grouping variables are given and both are not
### year.
    ## if (length(by[by != "year"]) > 1) {
    ##     stop("Excluding 'year' only one by variable is allowed currently")
    ## }
    ##    dt_box <- mig[, stats::fivenum(get(col)), keyby = by]
    dt_box <- mig[, c(stats::quantile(get(col), probs), sum(get(col))), keyby = by]
    stats <- c("ymin", "y25", "ymed", "y75", "ymax", "N")
    dt_box[, `:=`("what", stats), keyby = by]
    formula <- sprintf("%s ~ what", paste(by, collapse = " + "))
    dt_box <- data.table::dcast(dt_box, formula = formula, value.var = "V1")
    ## if ("year" %in% by) {
    ##     ### dont know if this is a good idea. maybe just hand in
    ##     ### year_short directly if desired
    ##     dt_box[, `:=`("year_short", substr(as.factor(year), 3, 4))]
    ## }
    cols <- colnames(dt_box)
    gr <- by[by != "year"]
    if (length(gr) > 0) {
        cols[grepl(gr, cols)] <- "group"
        colnames(dt_box) <- cols
    }
    return(dt_box)
}


##' Makes a boxplot from table returned by get_box_data(). Expects
##' "year_short" as x-axis and "group" as faceting variable
##'
##' 
##' @title Boxplot with facets
##' @param dt data.table returned by get_box_data()
##' @param title Plot title
##' @param group variable used for faceting
##' @return plot object
##' @author Konstantin
##' @import ggplot2
custom_boxplot <- function(dt) {
    ## Plotted die 5 Punkte Zusammenfassung, welche mit get_box_data()
    ## erstellt wurde.
    box_dist <- ggplot2::ggplot(dt) +
        ggplot2::geom_boxplot(
                     ggplot2::aes(x = "year_short", min = "ymin",
                                  lower = "y25", middle = "ymed",
                                  upper = "y75", max = "ymax",
                                  group = group), stat = "identity") 
    ## ggplot2::facet_wrap(ggplot2::vars(labels), drop = FALSE) +
    ## theme_brrrp(maj.y = TRUE) +
    ## ggplot2::theme(
    ##     axis.text.x = ggplot2::element_text(angle = 45)) +
    ## ggplot2::ggtitle(title) +
    ## ggplot2::xlab("Jahr") +
    ## ggplot2::ylab("Distanz in km")
    return(box_dist)
}

make_facet_labels <- function(dt_box, group) {
     . <- N <- NULL
    lbls <- dt_box[, .(paste(group, min(N), sep = " > ")), keyby = group]
    dt_box <- do_join(dt_box, lbls, "V1", "group", "group", "labels")
    return(dt_box)
}

reorder_lbls_levels <- function(dt_box) {
    lvls <- unique(dt_box[, labels])
    west <- grep("west", lvls, value = TRUE)
    ost <- grep("ost", lvls, value = TRUE)
    sachsen <- grep("sachsen", lvls, value = TRUE)
    new_lvls <- c(as.character(lvls[!lvls %in% c(west, ost, sachsen)]), "", sachsen, ost, west)
    dt_box[, "labels" := factor(labels, levels = new_lvls)]
    return(dt_box)
}
