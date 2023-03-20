##' Extracts needed values and data from fit
##'
##' Computes predictions, exp() of predictions and extracts the
##' observed data into one data.table. Also, coefficients, se's,
##' rsquared and adjusted r_squared are extracted.
##' @title Conveniently extract values from fitted model
##' @param fit model fit
##' @return List
##' @author Konstantin
##' @import data.table
##' @export extract_fit
extract_fit <- function(fit) {
    preds <- predict(fit)
    obs <- fit$model[, 1]
    call <- as.character(fit$call)[2]
    preds_obs <- data.table::data.table("predicted" = preds, "observed" = obs,
                            "observed_exp" = exp(obs))
    coefs <- data.table::data.table("coefs" = names(coef(fit)),
               "estimate" = round(coef(fit), 2),
               "se" = round(sqrt(diag(vcov(fit))), 2))
    r_squared <- round(summary(fit)$r.squared, 2)
    adj_r_squared <- round(summary(fit)$adj.r.squared, 2)
    model <- list("call" = call, "coefs" = coefs,
                  "r_squared" = r_squared,
                  "adj_r_squared" = adj_r_squared)
    extracted <- list("preds" = preds_obs, "model" = model)
    return(extracted)
}

##' Plot predicted vs observed on log scale used for modeling and
##' original scale
##'
##' @title plot omdel fit
##' @param extract Extracted data from model, output of extract_fit().
##' @param lbls optional, character. If specified instead of plotting
##'     points the labels will be plotted.
##' @param ... Additional parameters passed to text()
##' @return NULL
##' @author Konstantin
##' @import graphics
##' @export plot_fit
plot_fit <- function(extract, lbls = NULL, ...) {
    stopifnot("Expects list with element 'preds" = "preds" %in% names(extract))
    x <- NULL
    graphics::par(mfrow = c(1, 2))
    preds <- extract$preds$predicted
    obs <- extract$preds$observed
    obs_exp <- extract$preds$observed_exp
    if (is.null(lbls)) {
        graphics::plot(preds, obs, main = "log scale",
             xlab = "Predicted", ylab = "Observed")
        graphics::curve(x * 1, add = TRUE, col = "red")
        graphics::plot(preds, obs_exp, main = "original scale",
             xlab = "Predicted", ylab = "Observed")
        graphics::curve(exp(x), add = TRUE, col = "red")
    }
    if (!is.null(lbls)) {
        graphics::plot(preds, obs, main = "log scale",
             xlab = "Predicted", ylab = "Observed",
             type = "n")
        graphics::curve(x * 1, add = TRUE, col = "red")
        graphics::text(preds, obs, labels = lbls, ...)
        graphics::plot(preds, obs_exp, main = "original scale",
             xlab = "Predicted", ylab = "Observed", type = "n")
        graphics::text(preds, obs_exp, labels = lbls, ...)
        graphics::curve(exp(x), add = TRUE, col = "red")
    }
    return(NULL)
}

