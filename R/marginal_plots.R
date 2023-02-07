##' Prepares data for plotting marginal distribution of od-flows.
##'
##' Prepares data for plotting marginal distribution of od-flows and
##' marginal distribution of logarithm of od-flows in
##' plot_marg(). Returns a list with data and estimated mean and sd to
##' overlay a normal distribution as well as the same for the
##' logarithm of od-flows.
##' @title Data for plotting marginal distribution of od-flows.
##' @param mig Migration Statistics.
##' @param us Region type for calculating od-flows. Either "mu", "di"
##'     or "st"
##' @return List of data and parameters.
##' @import stats
##' @author Konstantin
make_marg_data <- function(mig, us) {
    flow <- . <- flow_l <- NULL
  dt <- get_flows(mig, us)
  dt[, "flow_l" := log(flow)]
  dt[, "flow" := as.double(flow)]
  p <- dt[, .("m" = mean(flow), "sd" = stats::sd(flow))]
  p_l <- dt[, .("m_l" = mean(flow_l), "sd_l" = sd(flow_l))]
  out <- list("data" = dt, "params" = p, "params_l" = p_l)
  return(out)
}

##' Plots marginal distribution of od-flows. Expects output from
##' make_marg_data()
##'
##' @title Plot marginal distribution of od-flows.
##' @param data Output from make_marg_data()
##' @param log logical, if true logarithm of od-flows is plotted
##' @param bw band with for geom_density. If NULL, default band with
##'     is used.
##' @return ggplot object
##' @import data.table
##' @author Konstantin
plot_marg <- function(data, log = TRUE, bw = NULL) {
    variable <- value <- NULL
  stopifnot(is.logical(log))
  dt <- data[["data"]]
  dt <- data.table::melt(na.omit(dt),
                         id.vars = c("origin", "destination"))
  reg <- guess_region(dt)
  title <- sprintf("Wanderungsintensitaet zwischen %sn, 2000 - 2018. \n mit Normalverteilung,
                   ", reg)
  if (log) {
    dt <- dt[variable == "flow_l"]
    lbl_x <- "logarithmierte Wanderungsintensitaet"
    title <- paste("Logarithmierte", title, sep = " ")
    p <- data[["params_l"]]
  } else {
    dt <- dt[variable == "flow"]
    lbl_x <- "Wanderungsintensitaet, n"
    p <- data[["params"]]
  }
  if (is.null(bw)) {
    plot_marg <- ggplot(dt) +
      geom_density(aes(value)) 
  } else {
    plot_marg <- ggplot(dt) +
      geom_density(aes(value), bw = bw) 
  }
  plot_marg <- plot_marg + 
    stat_function(fun = stats::dnorm, args = list(mean = p[[1]], sd = p[[2]]),
                  colour = "blue", linetype = "dotted")  +
    MigStat::theme_brrrp() +
    ggtitle(title) +
    xlab(lbl_x) +
    ylab("Dichte") 
  return(plot_marg)
}

guess_region <- function(dt) {
### lol probably I can check easier which is appropriate like checking
### number of characters of ags
    
  if(nrow(dt) < 1000) {
    region <- "Bundeslaender"
  }
  if(1000 < nrow(dt) & nrow(dt) < 1e+6) {
    region <- "Kreise"
  }
  if (1e+6 < nrow(dt)) {
    region <- "Gemeinden"
  }
  return(region)
}
