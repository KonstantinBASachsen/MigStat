temp_path <- "~/MigStat/inst/temp"

#### Does plot file exist?
fn <- "test_base"
save_plot(plot(cars), fn, temp_path, base = TRUE, save_data = FALSE)
file <- file.path(temp_path, "plots", paste0(fn, ".pdf"))
expect_true(file.exists(file))

## errors should pop up
f_call <- expression(save_plot(plot(cars), paste0(fn, ".pdf"), temp_path,
                         base = TRUE, save_data = FALSE))
expect_error(eval(f_call), pattern = "file ending")

f_call <- expression(save_plot(plot(cars), fn, temp_path,
                         base = TRUE, save_data = TRUE))
expect_error(eval(f_call), pattern = "saving")

## saving data as either xlsx or csv
dt <- data.frame(cars)
f_call <- expression(save_plot(plot(cars), fn, temp_path,
                         base = TRUE, save_data = TRUE,
                         data = dt))
file <- file.path(temp_path, "plot_data", paste0(fn, ".xlsx"))
eval(f_call)
expect_true(file.exists(file))

f_call <- expression(save_plot(plot(cars), fn, temp_path,
                         base = TRUE, save_data = TRUE,
                         data = dt, excel = FALSE))
file <- file.path(temp_path, "plot_data", paste0(fn, ".csv"))
eval(f_call)
expect_true(file.exists(file))

### delete created folder
unlink(temp_path, recursive = TRUE)
