ret_el <- function(l, idx) {
  el <- l[[idx]]
  return(el)
}

##' Name elements of list and create attribute "name" for each element
##'
##' A little helper function that names all elements of a list and
##' additionally creates a new attribute for every element of the list
##' called 'name' that has the same value as the named element. This
##' is useful to use lapply() and use the names of the elements.
##' @title Name elements of list and create attribute "name" for each element
##' @param list a list that is to be named
##' @param names a vector of the same length as list that is used for naming
##' @return list
##' @export name_list
##' @author Konstantin
##' @examples
##' l <- list(1, 3, "A", "model")
##' n <- c("intercept", "beta", "group", "fit1")
##' ## name_list(l, n) ### throws error
##' l <- list(list(1), list(3), list("A"), list("model"))
##' name_list(l, n)
name_list <- function(list, names) {
    stopifnot("list and names must be of same length" = length(list) == length(names))
    stopifnot("All elements of 'list' must be lists as well" = all(vapply(list, is.list, logical(1))))
    names(list) <- as.character(names)
    message("Elements of list named.")
    for (n in names(list)) {
        list[[n]]$name <- n
    }
    message("Name attribute for each element created")
    return(list)
}

##' Create a data.table from a list of model outputs
##'
##' Intended to be used on list of outputs returned by clean_extract()
##' @title data.table from list of models
##' @param list list of model outputs created by clean_extract()
##' @param names optional. Vector of names
##' @param id_col character. Name of column that holds the names
##' @return data.table
##' @import data.table
##' @export list_to_dt
##' @author Konstantin
##' @examples
##' x <- rnorm(100)
##' y <- 0.1 * x + rnorm(100)
##' group <- sample(c("A", "B", "C"), 100, replace = TRUE)
##' dt <- data.table::data.table(x, y, group)
##' models <- lapply(c("A", "B", "C"), function(g) lm(y ~ x, data = dt[group == g]))
##' extracts <- lapply(models, extract_fit)
##' output <- lapply(extracts, clean_extract)
##' list_to_dt(output, c("A", "B", "C"))
list_to_dt <- function(list, names = NULL, id_col = "model") {
    if (is.null(names(list)) & is.null(names)) {
        warning("list not named and no names specified. Models names sequentially")
    }
    if (!is.null(names)) {
        stopifnot("list and names must be of same length" = length(list) == length(names))
        names(list) <- as.character(names)
    }
    models <- data.table::rbindlist(list, idcol = id_col)
    return(models)
}
