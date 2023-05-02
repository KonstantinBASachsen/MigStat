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
    preds <- as.numeric(predict(fit))
    obs <- fit$model[, 1]
    call <- as.character(fit$call)[2]
    preds_obs <- data.table::data.table("predicted" = preds, "observed" = obs,
                            "observed_exp" = exp(obs))
    coefs <- data.table::data.table("coefs" = names(coef(fit)),
               "estimate" = round(coef(fit), 2),
               "se" = round(sqrt(diag(vcov(fit))), 2))
    if (inherits(fit, "lm") == TRUE) {
        r_squared <- round(summary(fit)$r.squared, 2)
        adj_r_squared <- round(summary(fit)$adj.r.squared, 2)
    }
    if (inherits(fit, "plm") == TRUE) {
        r_squared <- round(as.numeric(summary(fit)$r.squared[1]), 2)
        adj_r_squared <- round(as.numeric(summary(fit)$r.squared[2]), 2)
    }
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
##' @param title character, optional. If set 'title' is added as
##'     heading.
##' @param title_size numeric, multiplicative factor for title size.
##' @param ... Additional parameters passed to text()
##' @return NULL
##' @author Konstantin
##' @import graphics
##' @export plot_fit
plot_fit <- function(extract, lbls = NULL, title = NULL,
                     title_size = 1.5, ...) {
    stopifnot("Expects list with element 'preds" = "preds" %in% names(extract))
    preds <- extract$preds$predicted
    obs <- extract$preds$observed
    obs_exp <- extract$preds$observed_exp
    graphics::par(mfrow = c(1, 2))
    x <- NULL
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
    if (!is.null(title)) {
        mtext(title, side = 3, line = -1.5, cex = title_size, outer = TRUE,
              font = 2)
    }
    return(NULL)
}

##' Adds abbreviated names to federal states
##'
##' @title Short labels for federal states 
##' @param shp shapefile for federal states
##' @return data.table
##' @import data.table
##' @export gen_state_labels
##' @author Konstantin
gen_state_labels <- function(shp) {
    AGS <- GEN <- . <- NULL
    lbls_dt <- shp[, .(AGS, GEN)]
    lbls_dt[, "GEN_abbr" := c("SH", "HH", "NS", "Bre", "NRW", "Hes",
                          "RLP", "BaW\u00FC", "Bay", "Saa", "Ber", "Bran",
                          "MP", "Sac", "LSA", "T")]
    return(lbls_dt)
}
##' Joins state labels to flow data and creates labels for plotting
##'
##' @title Joins state labels for plotting labels
##' @param flows data.table of flows
##' @param lbls_dt data.table of abbreviated names and AGS
##' @param sep seperator for labels. So labels will be Ber sep HH,
##'     default Ber > HH to indicate flows from Berlin to Hamburg
##' @return data.table
##' @import data.table
##' @export join_state_labels
##' @author Konstantin
join_state_labels <- function(flows, lbls_dt, sep = " > ") {
    AGS <- GEN_abbr <- lbl_o <- lbl_d <- . <- NULL
    flows <- merge(flows, lbls_dt[, .(AGS, GEN_abbr)], by.x =  "origin", by.y = "AGS")
    data.table::setnames(flows, "GEN_abbr", "lbl_o")
    flows <- merge(flows, lbls_dt[, .(AGS, GEN_abbr)], by.x =  "destination", by.y = "AGS")
    data.table::setnames(flows, "GEN_abbr", "lbl_d")
    flows[, "lbl" := paste(lbl_o, lbl_d, sep = sep)]
    flows[, c("lbl_o", "lbl_d") := NULL]
    return(flows)
}

##' Wrapper that saves conveniently plots from model extracts
##'
##' When extracting output from a model fit I often times use
##' plot_fit() to quickly evaluate the fit of the model. This function
##' takes a list of extract_fit() and saves plots created using
##' plot_fit() from these extracts.
##' @title Save plot_fit() from many models
##' @param extracts list of extracts from extract_fit()
##' @param path character path. In "path", if not existend three
##'     subfolders: "plots", "plot_data" and "models" are created.
##' @param title Title for every one of the plots
##' @param lbls character, optional if not NULL then lbls instead of
##'     points are plotted.
##' @param name_suffix character, optional, if given, name_suffix is
##'     appended to name of plots.
##' @param ... optional parameters are passed to plot_fit()
##' @return NULL
##' @export save_model_plots
##' @author Konstantin
save_model_plots <- function(extracts, path, title, lbls = NULL,
                             name_suffix = NULL, ...) {
    for (i in seq_along(extracts)) {
        plt_name <- paste0("fit", i, name_suffix)
        save_plot(plot_fit(extracts[[i]], title = title, lbls = lbls, ...),
                  plt_name, save_data = TRUE, path = path, data = extracts[[i]]$preds)
    }
    return(NULL)
}

##' Saves model output in extracts_fit(fit)$model
##'
##' @title Saves everything in extracts_fit(fit)$model
##' @param extracts List of extracts from extract_fit()
##' @param path Output path, saves output in path/models.
##' @param name_suffix character, optional. If not NULL, suffix is
##'     appended to file names
##' @return NULL
##' @export save_model_output
##' @author Konstantin
save_model_output <- function(extracts, path, name_suffix = NULL) {
    for (i in seq_along(extracts)) {
        name <- paste0("fit", i, name_suffix)
        saving_data(extracts[[i]]$model, path, name)
    }
    return(NULL)
}

ret_el <- function(l, idx) {
  el <- l[[idx]]
  return(el)
}

fit_models <- function(age_groups, years, data, formula) {
    ### would be better as more general function that fits models for
### subsets of data. Maybe loop explicitly over subsets?
    age_gr <- NULL ## why no cmd note for "year"
    n_age <- length(age_groups)
    n_y <- length(years)
    fits <- vector(mode = "list", 
                   length = length(n_age * n_y))
    for (y in years) {
        for (g in age_groups) {
            n <- paste(y, g, sep = "_")
            fits[n] <- lapply(formula, function(x) 
                lm(x, data = data[age_gr == g & year == y]))
        }
    }
    fits <- fits[-1] ## first elements always NULL. Looks dangerous!
    return(fits)
}

clean_output <- function(extracts) {
    ## should have one extract as input. The lapply should be outside
    ## of the function. Also I should be able to check n_coefs by
    ## checking the length?
    ## groups <- names(extracts)
    ## extracts <- extracts[[1]]
    model_out <- ret_el(extracts, 2)
    coefs <- ret_el(model_out, 2)
    rsq <- ret_el(model_out, 3)
    rsq <- as.numeric(rsq)
    a_rsq <- ret_el(model_out, 4)
    a_rsq <- as.numeric(a_rsq)
##    coefs[, "model" := groups]
    coefs[, "rsquared" := rsq]
    coefs[, "a_rsquared" := a_rsq]
    cols <- c( "coefs", "estimate", "se","rsquared", "a_rsquared")
    setcolorder(coefs, cols)
    return(coefs)
}

save_plots <- function(extracts, path, title, save_data, lbls = NULL, name_suffix = NULL, 
          ...) {
  n_plots <- length(extracts)
  message(paste(n_plots, "plots will be saved"))
  for (n in names(extracts)) {
    if (is.null(lbls)) {
      plt_name <- paste0("fit", n, name_suffix)
    }
    if (!is.null(lbls)) {
      plt_name <- paste0("fit", n, name_suffix, "_lbls")
    }
    plt_title <- paste(title, plt_name, "_")
    save_plot(plot_fit(extracts[[n]], title = title, lbls = lbls, 
                       ...), plt_name, save_data = save_data, path = path, data = extracts[[n]]$preds)
  }
  return(NULL)
}
