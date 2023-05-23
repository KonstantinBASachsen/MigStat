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
    ## as.numeric bc if plm predicions have attributes that make plotting difficult
    ## the column names do not make sense anymore but the code works
    call <- as.character(fit$call)[2]
    coefs <- data.table::data.table("coefs" = names(coef(fit)),
               "estimate" = round(coef(fit), 2),
               "se" = round(sqrt(diag(vcov(fit))), 2))
    if (inherits(fit, "lm") == TRUE & inherits(fit, "glm") == FALSE) {
        preds <- as.numeric(predict(fit))
        obs <- fit$model[, 1]
        preds_obs <- data.table::data.table("predicted" = preds, "observed" = obs,
                                            "observed_exp" = exp(obs))
        r_squared <- round(summary(fit)$r.squared, 2)
        adj_r_squared <- round(summary(fit)$adj.r.squared, 2)
        link  <- "identity"
        aic <- NA
    }
    if (inherits(fit, "glm") == TRUE) {
        preds <- as.numeric(predict(fit))
        obs <- fit$model[, 1]
        preds_obs <- data.table::data.table("predicted" = preds, "observed" = obs,
                                            "observed_exp" = exp(obs))
        preds_obs <- data.table::data.table("predicted" = preds, "observed" = log(obs),
                                            "observed_exp" = obs)
        r_squared <- NA
        adj_r_squared <- NA
        aic <- fit$aic
        link <- fit$family$link
    }
    if (inherits(fit, "plm") == TRUE) {
        r_squared <- round(as.numeric(summary(fit)$r.squared[1]), 2)
        adj_r_squared <- round(as.numeric(summary(fit)$r.squared[2]), 2)
    }
    model <- list("call" = call, "coefs" = coefs,
                  "r_squared" = r_squared,
                  "adj_r_squared" = adj_r_squared,
                  "aic" = aic,
                  "link" = link)
    extracted <- list("preds" = preds_obs, "model" = model)
    return(extracted)
}

##' When fitting many models comparison between them is easy if all
##' relevant information is in one data.table. clean_extract() can be
##' called on extract_fit(model) to convert the list of the model
##' information returned by extract_fit() to a data.table. These
##' data.tables can be combined easily to one data.table that
##' encompasses the output of several models
##'
##' @title Convert List of model extracts to data.table
##' @param extract list from extract_fit(model)
##' @return data.table
##' @export clean_extract
##' @import data.table
##' @author Konstantin
##' @examples
##' fit <- lm(dist ~ speed, data = cars)
##' extract <- extract_fit(fit)
##' out <- clean_extract(extract)
clean_extract <- function(extract) {
    model_out <- ret_el(extract, 2)
    coefs <- ret_el(model_out, 2)
    rsq <- ret_el(model_out, 3)
    rsq <- as.numeric(rsq)
    a_rsq <- ret_el(model_out, 4)
    a_rsq <- as.numeric(a_rsq)
    aic <- ret_el(model_out, 5)
##    coefs[, "model" := groups]
    coefs[, "rsquared" := rsq]
    coefs[, "a_rsquared" := a_rsq]
    coefs[, "aic" := aic]
    cols <- c( "coefs", "estimate", "se","rsquared", "a_rsquared", "aic")
    data.table::setcolorder(coefs, cols)
    return(coefs)
}

##' Plot predicted vs observed on log scale used for modeling and
##' original scale
##'
##' @title plot omdel fit
##' @param extract Extracted data from model, output of extract_fit().
##' @param lbls optional, character. If specified instead of plotting
##'     points the labels will be plotted.
##' @param title character, optional. If set 'title' is added as
##'     heading.
##' @param title_size numeric, multiplicative factor for title size.
##' @param fast logical, if TRUE "." is used as pch. This increases
##'     plotting speed a lot and is encouraged if there are many
##'     points. It also reduces overplotting.
##' @param ... Additional parameters passed to text()
##' @return NULL
##' @author Konstantin
##' @import graphics
##' @export plot_fit
plot_fit <- function(extract, lbls = NULL, title = NULL,
                     title_size = 1.5, fast = FALSE, ...) {
    stopifnot("Expects list with element 'preds" = "preds" %in% names(extract))
    preds <- extract$preds$predicted
    obs <- extract$preds$observed
    obs_exp <- extract$preds$observed_exp
    graphics::par(mfrow = c(1, 2))
    if (fast) {
        pch <- "."
    } else  {
        pch <- 16
    }
    x <- NULL
    if (is.null(lbls)) {
        graphics::plot(preds, obs, main = "log scale",
                       xlab = "Predicted", ylab = "Observed",
                       pch = pch, cex = 4, col = alpha("black", 0.3))
        graphics::curve(x * 1, add = TRUE, col = "red")
        graphics::plot(preds, obs_exp, main = "original scale",
                       xlab = "Predicted", ylab = "Observed",
                       pch = pch, cex = 4, col = alpha("black", 0.3))
        graphics::curve(exp(x), add = TRUE, col = "red")
    }
    if (!is.null(lbls)) {
        graphics::plot(preds, obs, main = "log scale",
                       xlab = "Predicted", ylab = "Observed",
                       type = "n", , pch = pch, cex = 4, col = alpha("black", 0.3))
        graphics::curve(x * 1, add = TRUE, col = "red")
        graphics::text(preds, obs, labels = lbls, ...)
        graphics::plot(preds, obs_exp, main = "original scale",
                       xlab = "Predicted", ylab = "Observed", type = "n",
                       pch = pch, cex = 4, col = alpha("black", 0.3))
        graphics::text(preds, obs_exp, labels = lbls, ...)
        graphics::curve(exp(x), add = TRUE, col = "red")
    }
    if (!is.null(title)) {
        mtext(title, side = 3, line = -1.5, cex = title_size, outer = TRUE,
              font = 2)
    }
    return(NULL)
}

## plot_fit_glm <- function(extract, lbls = NULL, title = NULL,
##                      title_size = 1.5, ...) {
##     stopifnot("Expects list with element 'preds" = "preds" %in% names(extract))
##     preds <- extract$preds$predicted
##     obs <- extract$preds$observed
##     obs[obs == 0] <- 0.1
##     obs_exp <- extract$preds$observed_exp
##     graphics::par(mfrow = c(1, 2))
##     q <- quantile(obs, 0.99)
##     x <- NULL
##     if (is.null(lbls)) {
##         graphics::plot(preds, log(obs), main = "log scale",
##                        xlab = "Predicted", ylab = "Observed")
##         graphics::curve(x * 1, add = TRUE, col = "red")
##         graphics::plot(preds, obs, main = "original scale",
##                        xlab = "Predicted", ylab = "Observed",
##                        ylim = c(0, q))
##         graphics::curve(exp(x), add = TRUE, col = "red")
##     }
##     if (!is.null(lbls)) {
##         graphics::plot(preds, log(obs), main = "log scale",
##                        xlab = "Predicted", ylab = "Observed",
##                        type = "n")
##         graphics::curve(x * 1, add = TRUE, col = "red")
##         graphics::text(preds, obs, labels = lbls, ...)
##         graphics::plot(preds, obs, main = "original scale",
##                        xlab = "Predicted", ylab = "Observed", type = "n",
##                        ylim = c(0, q))
##         graphics::text(preds, obs_exp, labels = lbls, ...)
##         graphics::curve(exp(x), add = TRUE, col = "red")
##     }
##     if (!is.null(title)) {
##         mtext(title, side = 3, line = -1.5, cex = title_size, outer = TRUE,
##               font = 2)
##     }
##     return(NULL)
## }

##' Saves data and plot returned from plot_fit()
##'
##' The saving of the plot returned by plot_fit() and the accompanying
##' data is a little tedious because the plot and the data to be saved
##' depend upon extract. The name of the plot file and the title of
##' the plot typically depend upon the name attribute of
##' extract. save_model_plot() is intended to be used with lapply() to
##' automate some of these tedious tasks.
##' @title Saves data and plot returned from plot_fit()
##' @param extract list of model information, returned by
##'     extract_fit()
##' @param path Path were data and plot are saved. Two subfolders are
##'     created if they do not exist: path/plots and path/plot_data.
##' @param main character, plot title. The name attribute of extract
##'     is appended.
##' @param name_suffix additional suffix for the plot name. The plot
##'     name will be paste0(extract$name, name_suffix).
##' @param lbls character, optional. If not NULL, lbls are plotted
##'     instead of points.
##' @param title_size numeric, optional, size of the plot title
##' @param fast logical, if TRUE "." is used as pch. This increases
##'     plotting speed a lot and is encouraged if there are many
##'     points. It also reduces overplotting.
##' @param ... additional parameters passed to save_plot()
##' @return NULL
##' @export save_model_plot
##' @author Konstantin
save_model_plot <- function(extract, path, main, name_suffix,
                            lbls = NULL, title_size = 1.5, fast, ...) {
    stopifnot("extract must have name attribute" = is.character(extract$name))
    e <- extract
    main <- sprintf(main, e$name)
    plt_name <- paste0(e$name, name_suffix)
    save_plot(plot_fit(extract = e, lbls = lbls, title = main,
                       title_size = title_size, fast = fast),
              path = path, plot_name = plt_name, data = e$preds, ...)
    return(NULL)
}
