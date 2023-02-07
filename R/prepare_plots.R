##' Create factor based on quantiles
##'
##' The function takes a numeric vector and quantiles. The quantiles
##' work as break points. The function returnes a factor based upon
##' these break points.
##' @param x The numeric vector
##' @param probs the vector of quantiles that work as break point
##' @param na.rm If true, missing values are excluded.
##' @return The factor created using the quantiles as break points.
##' @author Konstantin
cut_on_quantiles <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
    ## add check if x is numeric

    ## only compute quantiles for x > 3 because due to data protection
    ## law observations from 1 to 3 have to be reported as one
    ## category.
    breaks <- stats::quantile(x[x > 3], probs, na.rm = na.rm)
    breaks <- c(0, 3, unique(breaks))
    xf <- cut(x, breaks, include.lowest = FALSE)
    return(xf)
}

##' Returns labels and breaks of a factor.
##'
##' When creating maps using ggplot and geom_sf() it is often
##' convenient to group the filling column based on its quantiles. In
##' this way differences can be spotted better. To retain a colouring
##' that makes sense I transform the grouped variable into numeric and
##' than add the factor levels in the legend. This function returns
##' the needed breaks and labels for use in scale_fill_continuous()
##' @param x vector for which breaks and labels are returned.
##' @return A list holding breaks and labels
##' @import data.table
##' @export quantile_labels
##' @author Konstantin
quantile_labels <- function(x) {
    lbs <- unique(x)
    brks <- as.numeric(lbs)
    li <- list(labels = lbs, breaks = brks)
    return(li)
}

##' Cuts a column based on quantiles and ads this factor as numeric as
##' well.
##'
##' When creating a map with geom_sf it is often preferable to show
##' quantiles. For a reasonable colouring this quantized variable has
##' to be a numeric one. For the labels we need the factor. This
##' function computes both and ads them to a data.table
##' @param data data.table
##' @param col Column used for cutting.
##' @param probs Prababilities to base quantiles upon
##' @param by cut column separately for every group given by 'by'.
##' @return NULL, data.table is updated.
##' @import data.table
##' @export cols_on_quantiles
##' @author Konstantin
cols_on_quantiles <- function(data, col, probs = seq(0, 1, 0.2), by = NULL) {
    col_cutted <- paste0(col, "_cutted")
    col_cutted_num <- paste0(col_cutted, "_num")
    data[, (col_cutted) := cut_on_quantiles(get(col), probs = probs, na.rm = TRUE),
         keyby = by]
    data[, (col_cutted_num) := as.numeric(get(col_cutted), na.rm = TRUE),
         keyby = by]
    return(NULL)
}

##' Custom ggplot theme
##'
##' This theme makes it easy to specify the desired grid for a ggplot
##' object. Inspired by https://www.youtube.com/watch?v=48-ymyX6PlU
##' @title Own brrrp theme
##' @param leg.pos legend position as coordinates like c(0.1, 0.7) for
##'     upper left or c(0.8, 0.2) for lower right
##' @param maj.x major x grid, logical
##' @param maj.y major y grid, logical
##' @param min.x minor x grid, logical
##' @param min.y minor y grid, logical
##' @return theme for ggplot object
##' @import ggplot2
##' @export theme_brrrp
##' @author Konstantin
theme_brrrp <- function(leg.pos = NULL, maj.x = FALSE, maj.y = FALSE, min.x = FALSE, min.y = FALSE) {
    if (inherits(c(maj.x, maj.y, min.x, min.y), "logical") == FALSE ) {
        stop("maj.x, maj.y, min.x, min.y all have to be TRUE or FALSE (logical)")
    }
    brrrp <- ggplot2::theme_bw()
    if(!maj.x) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    } 
    if(!maj.y) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    } 
    if(!min.x) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    } 
    if(!min.y) {
        brrrp <- brrrp + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
    if (!is.null(leg.pos)) {
        brrrp <- brrrp + theme(legend.position = leg.pos)
    }
    brrrp <- brrrp + theme(plot.title = element_text(colour = "blue", face = "bold",
                                                     hjust = 0.5))
    return(brrrp)
}
