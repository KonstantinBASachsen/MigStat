##' Creates five point summary for faster box plots
##'
##' 
##' @title five point summary by group
##' @param mig Migration Statistics data.table
##' @param col Name of column that is to be summarized
##' @param by Character vector of columns that are used as group. Five
##'     point summary is computed for every group
##' @return data.table of five point summaries
##' @author Konstantin
get_box_data <- function(mig, col, by) {
    ### berechnet die 5-Punkte Zusammenfassung, welche ich für
    ### boxplots verwende. Die Zusammenfassung wird für "col"
    ### berechnet. Jeweils für alle Gruppen welche durch "by". Kann
    ### auch ggplot automatisch machen, dauert dann aber deutlich
### länger.

    ## replace fivenum with boxplot.stats?
    dt_box <- mig[, stats::fivenum(get(col)), by = by]
    stats <- c("ymin", "y25", "ymed", "y75", "ymax")
    dt_box[, "what" := stats, by = by]
    formula <- sprintf("%s ~ what", paste(by, collapse = " + "))
    dt_box <- data.table::dcast(dt_box, formula = formula, value.var = "V1")
    dt_box[, "year_short" := substr(as.factor(year), 3, 4)]
### bad but works for now, bc custom_boxplot() expects "group" col
    cols <- colnames(dt_box)
    cols[grepl("group", cols)] <- "group"
    colnames(dt_box) <- cols
    return(dt_box)
}

##' Makes a boxplot from table returned by get_box_data(). Expects
##' "year_short" as x-axis and "group" as faceting variable
##'
##' 
##' @title Boxplot with facets
##' @param dt data.table returned by get_box_data()
##' @param title Plot title
##' @return plot object
##' @author Konstantin
##' @import ggplot2
custom_boxplot <- function(dt, title) {
    ## Plotted die 5 Punkte Zusammenfassung, welche mit get_box_data()
    ## erstellt wurde.
    group <- NULL
    box_dist <- ggplot2::ggplot(dt, ggplot2::aes_string("year_short")) +
        ggplot2::geom_boxplot(
                     ggplot2::aes_string(x = "year_short",
                                         min = "ymin", lower = "y25",
                                         middle = "ymed", upper = "y75", max = "ymax",
                                         group = "year"), stat = "identity") +
    ggplot2::facet_wrap(ggplot2::vars(group), drop = FALSE) +
    theme_brrrp(maj.y = TRUE) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("Jahr") +
    ggplot2::ylab("Distanz in km")
    return(box_dist)
}
