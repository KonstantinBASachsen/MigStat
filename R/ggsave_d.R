##' Saves plot and data used plotting
##'
##' ggsave_d() takes a plot object and saves the plot. Additionally it
##' saves the data that was used to create the plot. If not present
##' ggsave_d() creates in path one directory called plots and one
##' called plot_data. The data is saved under the same name as the
##' plot and it is saved as .csv.
##' @title Save plot and data used for plotting
##' @param plot plot created with ggplot()
##' @param plot_name Name of plot file and .csv file on disk. Specify
##'     without file ending. So it is "regions_flow" instead of
##'     "regions_flow.pdf"
##' @param path Directory where "plots" and "data_plots"
##'     subdirectories are created
##' @param data Optional. If given this data is saved instead of the
##'     one in the plot object. This is useful if the data called by
##'     ggplot() is only used to draw map and some differnt data set
##'     is used to fill map.
##' @param excel logical, if true data is saved as .xlsx as opposed to
##'     .csv. If the data to be saved is large this might take a long
##'     time. Saving as .csv currently does not work if data is a list
##'     of data frames.
##' @param ... Additional parameters passed to ggplot2::ggsave(). See
##'     ?ggplot2::ggsave
##'@return NULL. Saves plot and data to disk. Data is saved as
##'     .csv or .xlsx. Plot saved as .pdf by default but other formats can be
##'     used. See ?ggplot2::ggsave
##' @import openxlsx
##' @export ggsave_d
##' @importFrom methods is
##' @author Konstantin
ggsave_d <- function(plot, plot_name, path, save_data = FALSE,
                     data = NULL, excel = TRUE, ...) {
    ### now filename and plot args are swapped compared to
### ggplot2::ggsave
  
  ## dont know anymore why I call copy
  
  ## either make saving data optional or make usage similar to ggplot::ggsave()
  ## so this can be used without struggle if data is not to be saved

### checking name for file ending would be nice

    if (methods::is(plot) != "gg") {
        stop("Plot should be result from ggplot()")
    }
    ## if (grepl(".", plot_name) == TRUE) {
    ##     warning("Did you specify file ending in plot_name. Better without ending")
    ## } Apparently "." tests for any character
    plot_path <- file.path(path, "plots")
    data_path <- file.path(path, "plot_data")
    if (! dir.exists(plot_path)) {
        dir.create(plot_path, recursive = TRUE)
        message(sprintf("Directory to save plot created: %s", plot_path))
    }
    if (! dir.exists(data_path)) {
        dir.create(data_path, recursive = TRUE)
        message(sprintf("Directory to save data from plot created: %s", data_path))
    }
    ggplot2::ggsave(filename = paste0(plot_name, ".pdf"), plot = plot, path = plot_path, ...)
    if (is.null(data)) {
        if(ncol(plot$data) <= 1) {
            warning("looks like in plot$data is no actual data! Did you use different data set in plot as well? If so, specify using data argument")
        }
        dt <- copy(plot$data)
##        message("data from plot object is saved") seems wrong place for this message
    } else {
        dt <- copy(data)
    }
    
    if ("geom" %in% colnames(dt)) {
        dt[, "geom" := NULL]
        message("geom column dropped before saving")
    }
    if ("geometry" %in% colnames(dt)) {
        dt[, "geometry" := NULL]
        message("geometry column dropped before saving")
    }
    if (excel == TRUE & save_data == TRUE) {
      fpath <- file.path(data_path, paste0(plot_name, ".xlsx"))
      openxlsx::write.xlsx(x = dt, fpath)
    } else if (excel == FALSE & save_data == TRUE) {
      fpath <- file.path(data_path, paste0(plot_name, ".csv"))
      data.table::fwrite(x = dt, file = fpath) 
    } else if (save_data == FALSE) {
        message("plot data not saved")
    }
    if(file.exists(fpath)) {
        message("plot data written to disk")}
    ## not save bc file might exists and is not created anew
}
