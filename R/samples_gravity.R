##' Samples flows between regions according to gravity model
##'
##' Currently only population sizes of origin and destination are
##' supported
##' @title Simulations from Gravity Model
##' @param dt data.table with regions and columns "pop_o", "pop_d" and
##'     "distance".
##' @param params numeric, the three coefficients for pop_o, pop_d and
##'     distance (in this order). Coefficient for distance should be
##'     negative for results that make sense.
##' @param n Sum of flows to be generated. This n is used to adjust
##'     the parameter 'k' in the simulation to generate approximately
##'     'n' samples.
##' @param distribution character, either "pois" for samples drawn
##'     from poission distribution or "negbinom" for samples from
##'     negative binomial distribution.
##' @param theta numeric, to choose variance if distribution =
##'     "negbinom". See ?MASS::rnegbinom
##' @return data.table with new column "flows".
##' @importFrom MASS rnegbin
##' @import data.table
##' @export samples_gravity
##' @author Konstantin
samples_gravity <- function(dt, params = c(0.5, 0.5, -3), n = 5e6,
                            distribution = c("pois", "negbinom"),
                            theta = NULL) {
    ## add check if coef dist < 0
    linpred <- distance <- .N <- NULL
    distribution <- match.arg(distribution)
    stopifnot("Not all columns in data" = c("pop_o", "pop_d", "distance") %in% colnames(dt))
    stopifnot("Distances == 0 in data" = dt[distance == 0, .N] == 0)
    vars <- c("log(pop_o)", "log(pop_d)", "log(distance)")
    formula <- get_formula(params, vars) ## not so nice with get_formula
    dt[, "linpred" := eval(parse(text = formula))]
    lambda <- n / nrow(dt)
    k <- log(lambda / dt[, mean(exp(linpred))])
    lambda <- exp(k + dt[, linpred])
    if (distribution == "pois") {
        dt[, "flow" := rpois(nrow(dt), lambda)]
        message(sprintf("samples drawn from poisson distribution with expectation %s",
                        round(mean(lambda), 0)))
    }
    if (distribution == "negbinom") {
        stopifnot("When 'negbinom' theta necessary" = !is.null(theta))
        dt[, "flow" := MASS::rnegbin(n = nrow(dt), mu = lambda, theta)]
        ## var <- round(mean(lambda + lambda^2 / theta), 0) ## not working, mixture, no sum
        message(sprintf("samples drawn from negative binomial distribution with expectation %s",
                        round(mean(lambda), 0)))
    }
    return(dt)
}

get_formula <- function(params, vars, for_fit = TRUE) {
    stopifnot("params have to be numeric" = is.numeric(params))
    stopifnot("vars have to be character" = is.character(vars))
    terms <- paste(params, vars, sep = "*")
    terms <- paste(terms, collapse = "+")
    return(terms)
}

