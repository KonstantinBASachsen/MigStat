
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MigStat

The goal of MigStat is to ease working with Migration Statistics
(Wanderungsstatistik). It’s main functionalities are:

-   prepare data to enable the fitting of spatial origin-destination
    flow models using
    [spflow](https://cran.r-project.org/web/packages/spflow/index.html)
-   easy join of regional characteristics from the
    [inkar](https://www.inkar.de/) data
-   generate random (currently only uniform) moves.
-   make visualizations easy by:
    -   creating maps with arrows to indicate destinations and size of
        migration flows from origin
    -   calculate migration wins and losses for regions
    -   calculate bivariate flows (“Umzüge”) to create [Circular
        Migration Flow
        Plots](https://www.r-bloggers.com/2014/03/circular-migration-flow-plots-in-r/)

## Installation

You can install the development version of MigStat from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KonstantinBASachsen/MigStat")
```

# General Overview

The package evolves around three data sets: - [Migration
Statistics](https://www.forschungsdatenzentrum.de/de/10-21242-12711-2013-00-00-1-1-0)
The example data set gives an idea about the data structure and what
observations can be expected. Unfortunately there are only 200 moves.
For testing of visualizations and models this is not enough in my
opinion. [^1] - [inkar](https://www.inkar.de/) This data has very many
interesting regional characteristics for many years and on different
administrative levels like federal states, districts and
municipalities. -
[shapefile](https://daten.gdz.bkg.bund.de/produkte/vg/vg250-ew_ebenen_1231/2014/vg250-ew_12-31.utm32s.shape.ebenen.zip)
(2014 is not correct, 2013 should be used) This is used to obtain the
geometry information of the regions and the population sizes.

## Examples

Currently the examples assume the user has access to the example
Migration Statistics (Beispieldaten der Wanderungsstatistik). So you can
not actually run the examples. It is merely intended to show the
workings.

### Example load data and join administrative units

``` r

library(MigStat)

ex_dat <- MigStat:::read_examples() ## reads shapefile and example migration statistics
shps <- ex_dat$shps
inkar_csv <- "/home/konstantin/Downloads/inkar_2021.csv"
inkar <- MigStat::read_inkar(inkar_csv)
#> Kennziffer from integer converted to character and leading 0's added 
#>                 to make sure joining to shapefile works
```

Next we want to generate random moves between regions. First we specify
for which regions. Moves can be drawn between origin and destination.
Both can be of one of the following: - federal states (st) - districts
(di) - municipalities (mu)

In this case we generate moves where origin and destination are both
federal states. Currently only uniformly distributed moves are created.
This means every origin-destination pair has the same probability that
somebody moves between them. This is very unrealistic.

``` r
us <- "st" ## "unit_simple" on of c("st", "di", "mu")

dt <- MigStat::n_new_rows(dt = ex_dat$mig, shps = shps, us_o = us, us_d = us, n = 1000)
```

The data is organized such that every row represents one move:

``` r

dt
#>       EF01 EF02U1 EF02U2 EF02U3 EF02U4 EF02U5 EF03U1 EF03U2 EF03U3 EF03U4
#>    1:   NA     NA     05     NA     NA     NA     NA     14     NA     NA
#>    2:   NA     NA     08     NA     NA     NA     NA     05     NA     NA
#>    3:   NA     NA     04     NA     NA     NA     NA     10     NA     NA
#>    4:   NA     NA     08     NA     NA     NA     NA     13     NA     NA
#>    5:   NA     NA     16     NA     NA     NA     NA     16     NA     NA
#>   ---                                                                    
#>  996:   NA     NA     09     NA     NA     NA     NA     16     NA     NA
#>  997:   NA     NA     02     NA     NA     NA     NA     01     NA     NA
#>  998:   NA     NA     06     NA     NA     NA     NA     05     NA     NA
#>  999:   NA     NA     14     NA     NA     NA     NA     12     NA     NA
#> 1000:   NA     NA     11     NA     NA     NA     NA     14     NA     NA
#>       EF03U5 EF05U3 EF07 EF12U3 EF19 EF20 EF25                state_o
#>    1:     NA     NA   NA     NA   NA   NA   NA                Sachsen
#>    2:     NA     NA   NA     NA   NA   NA   NA    Nordrhein-Westfalen
#>    3:     NA     NA   NA     NA   NA   NA   NA               Saarland
#>    4:     NA     NA   NA     NA   NA   NA   NA Mecklenburg-Vorpommern
#>    5:     NA     NA   NA     NA   NA   NA   NA              Thüringen
#>   ---                                                                
#>  996:     NA     NA   NA     NA   NA   NA   NA              Thüringen
#>  997:     NA     NA   NA     NA   NA   NA   NA     Schleswig-Holstein
#>  998:     NA     NA   NA     NA   NA   NA   NA    Nordrhein-Westfalen
#>  999:     NA     NA   NA     NA   NA   NA   NA            Brandenburg
#> 1000:     NA     NA   NA     NA   NA   NA   NA                Sachsen
#>                   state_d
#>    1: Nordrhein-Westfalen
#>    2:   Baden-Württemberg
#>    3:              Bremen
#>    4:   Baden-Württemberg
#>    5:           Thüringen
#>   ---                    
#>  996:              Bayern
#>  997:             Hamburg
#>  998:              Hessen
#>  999:             Sachsen
#> 1000:              Berlin
```

We are interested in the number of people moving between regions. The
distance between the centroids of regions is reported as well.

``` r

flows <- get_flows(dt, shps, us = "st")
flows
```

### Create circular migration flow plot

``` r

migest::mig_chord(flows[, 1:3]) ### for this names of regions would be nice
```

### Create Arrow Plot

Now we can create an arrow plot that shows where and how many people
move from origin.

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

[^1]: The data set is actually not really needed. What is needed are the
    column names and some missings here and there. I am not sure if it
    is allowed to share the data structure publicely. I will find out
    and if so, I remove the dependence on the example data and provide a
    function to recreate it.
