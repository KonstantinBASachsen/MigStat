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
get_box_data <- function (mig, col, by) {
    ### for following functions to handle variables I rename the
    ### grouping variable (except) year to 'group'. This does not work
    ### when more than two grouping variables are given. Does also not
    ### work when two grouping variables are given and both are not
### year.
    if (length(by[by != "year"]) > 1) {
        stop("Excluding 'year' only one by variable is allowed currently")
    }
    dt_box <- mig[, stats::fivenum(get(col)), keyby = by]
    stats <- c("ymin", "y25", "ymed", "y75", "ymax")
    dt_box[, `:=`("what", stats), keyby = by]
    formula <- sprintf("%s ~ what", paste(by, collapse = " + "))
    dt_box <- data.table::dcast(dt_box, formula = formula, value.var = "V1")
    if ("year" %in% by) {
        ### dont know if this is a good idea. maybe just hand in
        ### year_short directly if desired
        dt_box[, `:=`("year_short", substr(as.factor(year), 3, 4))]
    }
    cols <- colnames(dt_box)
    gr <- by[by != "year"]
    cols[grepl(gr, cols)] <- "group"
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

get_n_box <- function(mig, box_data, group) {
    N <- V1 <- NULL
  ### should be part of get_box_data. Also hard coding of empty factor
  ## level is bad
    n_o2 <- mig[, .N, keyby = c(group, "year")]
    n_o <- n_o2[, min(N), by = c(group)]
    n_o[, "labels" := paste(get(group), V1 - 1, sep = " n > ")]
    ### here I enter an empty factor level because I did the same in
    ### the mig data by calling add_variables() or so. The empty level
    ### makes sure that the layout of the facets is how it is supposed
    ### to be.
    lvls <- c(n_o[1:5, labels], "", n_o[6:8, labels])
    out <- list(lvls, n_o2)
  return(out)
}
