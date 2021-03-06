
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MigStat

<!-- badges: start -->
<!-- badges: end -->

The goal of MigStat is to ease working with Migration Statistics
(Wanderungsstatistik). It’s main functionalities are:

-   create maps with arrows to indicate destination and size of
    migration flows
-   calculate migration wins and losses for regions
-   calculate bivariate flows (“Umzüge”) to create [Circular Migration
    Flow
    Plots](https://www.r-bloggers.com/2014/03/circular-migration-flow-plots-in-r/)
-   join names of administrative units from official shape files

## Installation

You can install the development version of MigStat from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KonstantinBASachsen/MigStat")
```

## Examples

Currently the examples assume the user has access to the example
Migration Statistics (Beispieldaten der Wanderungsstatistik). So you can
not actually run the examples. It is merely intended to show the
workings.

### Example load data and join administrative units

``` r

library(MigStat)
## basic example code
map_path <- "Path to shape files"
example_path <- "path to example data Wanderungsstatistik"

dt <- read_example(example_path)
shps <- read_shapes(map_path) # read shapefiles. Assumes data is
                              # organized in levels. Can be downloaded
                              # [here](https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_1231/2013/)

dtj <- join_administries(dt, shps$state, shps$district, shps$muni, full = FALSE) ### join "official" names
```

### Create circular migration flow plot

``` r

dtf <- get_flows(dtj, "st", simplify = TRUE)
dtf <- dtf[complete.cases(dtf)]
mig_chord(dtf[, 3:5])
```

### Create Arrow Plot

``` r

name <- "Baden-Württemberg" ## name of origin region
o_us <- "st" # unit of origin region, one of the following: st: state,
             # di: district, mu: municipality
d_us <- "st" # units of destination. 

dtf <- get_arrow_data(dt = dt, shapes = shps, name = name,
                      o_us = o_us, d_us = d_us)

o <- which(dtf$dest == TRUE) ## index of origin region (so it should
                             ## read dtf$origin == TRUE). Is to be changed

real_flow <- which(dtf$flow > 0) ## only draw arrows if there is an actual flow

dtarrow <- dtf[unique(c(o, real_flow))] ## pick origin and
                                        ## destinations with actual
                                        ## flow

o <- which(dtarrow$dest == TRUE) ## index of origin region (so it
                             ## should read dtf$origin == TRUE). Is to
                             ## be changed

arrow_plot(dtf, o, dtarrow) ## draw plot
```
