##' Saves plot and data used plotting
##'
##' ggsave_d() takes a plot object and saves the plot. Additionally it
##' saves the data that was used to create the plot. If not present
##' ggsave_d() creates in path one directory called plots and one
##' called plot_data. The data is saved under the same name as the
##' plot and it is saved as .csv.
##' @title Save plot and data used for plotting
##' @param plt plot created with ggplot()
##' @param plot_name Name of plot file and .csv file on disk. Specify
##'     without file ending. So it is "regions_flow" instead of
##'     "regions_flow.pdf"
##' @param path Directory where "plots" and "data_plots"
##'     subdirectories are created
##' @param base logical, set to TRUE if plot is generated with base
##'     graphics. FALSE if plot is generated with ggplot2.
##' @param save_data logical. If FALSE plot data is not saved.
##' @param data Optional. If given this data is saved instead of the
##'     one in the plot object. This is useful if the data called by
##'     ggplot() is only used to draw map and some differnt data set
##'     is used to fill map.
##' @param excel logical, if true data is saved as .xlsx as opposed to
##'     .csv. If the data to be saved is large this might take a long
##'     time. Saving as .csv currently does not work if data is a list
##'     of data frames.
##' @param width width of plot in pixels
##' @param height height of plot in pixels
##' @param ... Additional parameters passed to ggplot2::ggsave(). See
##'     ?ggplot2::ggsave
##'@return NULL. Saves plot and data to disk. Data is saved as .csv or
##'     .xlsx. Plot saved as .pdf by default but other formats can be
##'     used. See ?ggplot2::ggsave
##' @import openxlsx
##' @export save_plot
##' @importFrom methods is
##' @author Konstantin
save_plot <- function(plt, plot_name, path, base = TRUE, save_data = FALSE,
                      data = NULL, excel = TRUE, width = 10, height = 0.7 * width,
                      ...) {
    ## now filename and plot args are swapped compared to
    ## ggplot2::ggsave
    ## dont know anymore why I call copy
    ## either make saving data optional or make usage similar to ggplot::ggsave()
    ## so this can be used without struggle if data is not to be saved
    ## checking name for file ending would be nice

    ## if (is.null(plot)) { ## class of plot(cars) is null actually
    ##     stop("Please specify plot as either object (only ggplots) or function call (both base-r and ggplot).")
    ## }
    if (grepl("\\.", plot_name) == TRUE) {
        stop("Detected '.' in plot_name. Please specify without file ending.")
    }
    ps <- make_plot_dirs(path)
    if (base == FALSE) {
        ggplot2::ggsave(filename = paste0(plot_name, ".pdf"), plot = plt, path = ps$plot_path, ...)
        dt <- return_data_gg(plot = plt, data = data)
    }
    if  (base == TRUE) {
        if (is.null(data) == TRUE & save_data == TRUE) { ## should go in return_data_base
            stop("'plt' seems to be base-r, please provide data for saving or set save_data = FALSE.")
        }
        base_save(plt = plt, plot_name = plot_name, path = ps$plot_path,
                  width = width, height = height, ...)
        ## I should write return_data_base that checks if plt returns
        ## a data.table and if so, returns this table and if not, returns data
        dt <- data.table::copy(data)
    }
    dt <- drop_geometry(dt)
    saving_plot_data(dt = dt, save_data = save_data, excel = excel,
                     plot_name = plot_name, paths = ps)
    ## not save bc file might exists and is not created anew
}

base_save <- function(plt, plot_name, path, width, height) {
    grDevices::pdf(file.path(path, paste0(plot_name, ".pdf")),
        width = width, height = height)
    plt
    grDevices::dev.off()
}

make_plot_dirs <- function(path) {
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
    ps <- list("plot_path" = plot_path, "data_path" = data_path)
    return(ps)
}

return_data_gg <- function(plot, data) {
    ## is called by ggsave_d and save_d. If data is NULL and plot is
    ## no ggplot, doesn't work, so check
    if (is.null(data)) {
        if(ncol(plot$data) <= 1) {
            warning("looks like in plot$data is no actual data! Did you use different data set in plot as well? If so, specify using data argument")
        }
        dt <- data.table::copy(plot$data)
        ##        message("data from plot object is saved") seems wrong place for this message
    } else {
        dt <- data.table::copy(data)
    }
    return(dt)
}



drop_geometry <- function(dt) {
    data.table::setDT(dt) ## save to do this?
    if ("geom" %in% colnames(dt)) {
        dt[, "geom" := NULL]
        message("geom column dropped before saving")
    }
    if ("geometry" %in% colnames(dt)) {
        dt[, "geometry" := NULL]
        message("geometry column dropped before saving")
    }
    return(dt)
}

##' Saves data not created in plots. Conforms to the FDZ requirements.
##'
##' @title Saves data
##' @param dt data to be saved
##' @param out_path paths object.  If out_path does not exist path and
##'     subfolders: "plots" and "plot_data" are created. Data is saved
##'     in "plot_data". Although this is misleading.
##' @param name name of file to be saved. Without ending
##' @param excel logical, if TRUE, data is saved in excel format, if
##'     FALSE then in .csv
##' @return NULL
##' @author Konstantin
##' @importFrom data.table fread
##' @importFrom openxlsx write.xlsx
##' @export saving_data
saving_data <- function(dt, out_path, name, excel = TRUE) {
    if (grepl("\\.", name) == TRUE) {
        stop("Detected '.' in name. Please specify without file ending.")
    }
    out_path <- make_plot_dirs(out_path)
    saving_plot_data(dt, save_data = TRUE, excel = excel,
                     plot_name = name, paths = out_path)
}

saving_plot_data <- function(dt, save_data, excel, plot_name, paths) {
    ps <- paths
    if (excel == TRUE & save_data == TRUE) {
        fpath <- file.path(ps$data_path, paste0(plot_name, ".xlsx"))
        openxlsx::write.xlsx(x = dt, fpath)
    } else if (excel == FALSE & save_data == TRUE) {
        fpath <- file.path(ps$data_path, paste0(plot_name, ".csv"))
        data.table::fwrite(x = dt, file = fpath) 
    } else if (save_data == FALSE) {
        message("plot data not saved")
    }
    if (save_data == TRUE) { ## Why do I do this?
        if (file.exists(fpath)) {
            message("plot data written to disk")
        } else {
            warning("plot data not written to disk although it should have")
            ## badly done because fpath might not exist so it can not be
            ## checked if file was written to disk
        }
    }
    return(NULL)
}
