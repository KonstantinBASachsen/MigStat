
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
dt <- MigStat::n_new_rows(dt = ex_dat$mig, shps = shps, us_o = us, us_d = us, n = 1000) # code should be vectorized, takes unnecessary long to run
```

The data is organized such that every row represents one move:

``` r

dt
#>       EF01 EF02U1 EF02U2 EF02U3 EF02U4 EF02U5 EF03U1 EF03U2 EF03U3 EF03U4
#>    1:   NA     NA     12     NA     NA     NA     NA     03     NA     NA
#>    2:   NA     NA     09     NA     NA     NA     NA     11     NA     NA
#>    3:   NA     NA     11     NA     NA     NA     NA     15     NA     NA
#>    4:   NA     NA     13     NA     NA     NA     NA     03     NA     NA
#>    5:   NA     NA     01     NA     NA     NA     NA     01     NA     NA
#>   ---                                                                    
#>  996:   NA     NA     05     NA     NA     NA     NA     04     NA     NA
#>  997:   NA     NA     08     NA     NA     NA     NA     12     NA     NA
#>  998:   NA     NA     08     NA     NA     NA     NA     15     NA     NA
#>  999:   NA     NA     10     NA     NA     NA     NA     11     NA     NA
#> 1000:   NA     NA     14     NA     NA     NA     NA     10     NA     NA
#>       EF03U5 EF05U3 EF07 EF12U3 EF19 EF20 EF25            state_o
#>    1:     NA     NA   NA     NA   NA   NA   NA      Niedersachsen
#>    2:     NA     NA   NA     NA   NA   NA   NA             Berlin
#>    3:     NA     NA   NA     NA   NA   NA   NA     Sachsen-Anhalt
#>    4:     NA     NA   NA     NA   NA   NA   NA      Niedersachsen
#>    5:     NA     NA   NA     NA   NA   NA   NA Schleswig-Holstein
#>   ---                                                            
#>  996:     NA     NA   NA     NA   NA   NA   NA             Bremen
#>  997:     NA     NA   NA     NA   NA   NA   NA        Brandenburg
#>  998:     NA     NA   NA     NA   NA   NA   NA     Sachsen-Anhalt
#>  999:     NA     NA   NA     NA   NA   NA   NA             Berlin
#> 1000:     NA     NA   NA     NA   NA   NA   NA           Saarland
#>                      state_d
#>    1:            Brandenburg
#>    2:                 Bayern
#>    3:                 Berlin
#>    4: Mecklenburg-Vorpommern
#>    5:     Schleswig-Holstein
#>   ---                       
#>  996:    Nordrhein-Westfalen
#>  997:      Baden-Württemberg
#>  998:      Baden-Württemberg
#>  999:               Saarland
#> 1000:                Sachsen
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

<img src="man/figures/README-circ_plot-1.png" width="100%" />

### Create Arrow Plot

Now we can create an arrow plot that shows where and how many people
move from origin. First we extract from the shapefile the data we need.

``` r

shp <- MigStat:::clean_shp(shps = shps, us = us) # grabs the correct administrative column and drops unnecessary columns

name <- "Baden-Württemberg" ## name of origin region
dtarrow <- MigStat::get_arrow_data(flows, shp, name)
dtarrow
#>     place flow o_region     xend    yend                  centers
#>  1:    01    2    FALSE 553652.9 6004156 POINT (553652.9 6004156)
#>  2:    02    5    FALSE 567310.6 5933674 POINT (567310.6 5933674)
#>  3:    03    4    FALSE 511363.6 5846065 POINT (511363.6 5846065)
#>  4:    04    5    FALSE 482904.8 5894462 POINT (482904.8 5894462)
#>  5:    05    5    FALSE 399578.3 5703959 POINT (399578.3 5703959)
#>  6:    06    3    FALSE 501846.2 5605263 POINT (501846.2 5605263)
#>  7:    07    3    FALSE 388849.7 5529902 POINT (388849.7 5529902)
#>  8:    08    6     TRUE 503073.9 5376508 POINT (503072.9 5376507)
#>  9:    09    2    FALSE 677821.7 5423448 POINT (677821.7 5423448)
#> 10:    10    4    FALSE 351467.0 5472192   POINT (351467 5472192)
#> 11:    11    4    FALSE 798734.2 5825934 POINT (798734.2 5825934)
#> 12:    12    7    FALSE 799146.5 5822278 POINT (799146.5 5822278)
#> 13:    13    6    FALSE 732862.3 5961981 POINT (732862.3 5961981)
#> 14:    14    7    FALSE 804516.1 5664713 POINT (804516.1 5664713)
#> 15:    15    3    FALSE 685498.7 5765494 POINT (685498.7 5765494)
#> 16:    16    3    FALSE 642505.2 5641040 POINT (642505.2 5641040)
MigStat::arrow_plot(shp, dtarrow) ## draw plot, moves from BaWü to BaWü are a bit hidden
```

<img src="man/figures/README-arrows_new-1.png" width="100%" />

## Spatial Origin-Destination Flow Model

### Join covariates

Next we want to model migration flows. First we want to add some
covariates from the inkar data. The inkar data has `length(indic)`
different regional characteristics

``` r

zb <- "2013" ### level of covariates for 2013
indic <- unique(inkar[, Indikator]) ### these are all available covariates
idx <- c(428, 86, 344, 196, 193, 419, 240, 161, 153, 480)
vars <- indic[idx] 
shp <- MigStat::join_inkar_vars(shp = shp, inkar = inkar, vars = vars, us = us, zb = zb)
#> Warning: 'Krankenhäuser mit Regelversorgung' is not available for 2013 and
#> Bundesländer
#> Warning: 'Nahversorgung Grundschulen Anteil der Bev. 1km Radius' is not
#> available for 2013 and Bundesländer
#> Warning: 'Breitbandversorgung mit 1000 Mbit/s in %' is not available for 2013
#> and Bundesländer
#> Warning: 'Erholungsfläche je Einwohner' is not available for 2013 and
#> Bundesländer
#> Warning: 'Männliche Arbeitslose' is not available for 2013 and Bundesländer
colnames(shp)[5] <- "Anteil Minijobs"
### with missing values the model can not be fit. Also excluding them
### does not work because then there are nodes missing. Thus we simply
### impute them with the sample average.
vars <- vars[vars %in% colnames(shp)] ## because not all desired vars could be joined we check if they are indeed part of shp
cn <- colnames(shp)
m1 <- shp[, mean(get(cn[5]), na.rm = TRUE)]
m2 <- shp[, mean(get(cn[7]), na.rm = TRUE)]
shp[is.na(get(cn[5])), cn[5] := m1]
shp[is.na(get(cn[7])), cn[7] := m2]
```

Now we looked a bit at the data; the data is in the right format and we
joined regional characteristics to be able to easily include covariates
to our model. This is essentially what the package is made for.

The next steps rely on the
[spflow](https://cran.r-project.org/web/packages/spflow/index.html)
package to build and estimate a spatial origin-flow model. This code is
not part of MigStat. It is merely intended to show how one can proceed
with data analysis.

### Create spatial weight matrix

``` r

set_geom <- function(dt, geom_only = T) {
    dtgeom <- sf::st_set_geometry(dt, dt[, geometry])
    if (geom_only == TRUE) {
        dtgeom <- sf::st_geometry(dtgeom)
    }

    return(dtgeom)
}

mid_points <- sf::st_point_on_surface(set_geom(shp))
shp_nb <- list(
    "by_contiguity" = spdep::poly2nb(set_geom(shp)) ##,
##    "by_distance" = spdep::dnearneigh(mid_points,d1 = 0, d2 = 1500), ## 
##    "by_knn" = spdep::knn2nb(spdep::knearneigh(mid_points, 3))
)

plot(set_geom(shp))
plot(shp_nb$by_contiguity, mid_points, add = TRUE)
title("Contiguity") 
```

<img src="man/figures/README-weight_matrix-1.png" width="100%" />

For efficient estimation of the model, the following data structure is
needed:

``` r

idx <- !(colnames(shp) %in% c("geometry", "GEN"))
shp <- shp[, ..idx]

m_cols <- c("EWZ", vars) ## columns included in model
shp[,  (m_cols) := lapply(.SD, scale), .SDcols = m_cols]

shp_net <- 
  spflow::sp_network_nodes(
    network_id = "states",
    node_neighborhood = spdep::nb2mat(shp_nb$by_contiguity),
    node_data = shp,
    node_key_column = "AGS")


shp_net_pairs <- spflow::sp_network_pair(
  orig_net_id = "states",
  dest_net_id = "states",
  pair_data = flows,
  orig_key_column = "origin",
  dest_key_column = "destination")

shp_multi_net <- spflow::sp_multi_network(shp_net, shp_net_pairs)
```

``` r

hist(flows[, log(flow)]) ### actually, log flows should be approximately normal
```

<img src="man/figures/README-model_estimation-1.png" width="100%" />

``` r

results_default <- spflow::spflow(
  flow_formula = log(1 + flow) ~ . + G_(log( 1 + distance)),
  sp_multi_network = shp_multi_net)

results_default
#> --------------------------------------------------
#> Spatial interaction model estimated by: MLE  
#> Autocorrelation structure: model_9 (SDM)  
#> Observations: 256  
#> 
#> --------------------------------------------------
#> Coefficients:
#>                                       est    sd  t.stat  p.value
#> rho_d                               -0.08  0.08   -1.06     0.24
#> rho_o                               -0.05  0.08   -0.58     0.33
#> rho_w                               -0.12  0.15   -0.83     0.28
#> (Intercept)                          8.93  2.21    4.05     0.08
#> (Intra)                             -0.66  5.59   -0.12     0.46
#> DEST_`Anteil Minijobs`              -0.03  0.09   -0.29     0.41
#> DEST_Auszubildende                   0.09  0.08    1.22     0.22
#> DEST_Berufsschülerinnen             -0.11  0.12   -0.91     0.26
#> DEST_Bildungswanderer                0.11  0.06    1.77     0.16
#> DEST_Binnenwanderungsvolumen         0.09  0.11    0.80     0.28
#> DEST_EWZ                             0.07  0.04    1.76     0.16
#> DEST_`Anteil Minijobs`.lag1         -0.68  0.32   -2.11     0.14
#> DEST_Auszubildende.lag1              0.76  0.33    2.28     0.13
#> DEST_Berufsschülerinnen.lag1        -0.36  0.17   -2.07     0.14
#> DEST_Bildungswanderer.lag1           0.14  0.18    0.75     0.29
#> DEST_Binnenwanderungsvolumen.lag1    0.52  0.25    2.12     0.14
#> DEST_EWZ.lag1                       -0.21  0.24   -0.85     0.28
#> ORIG_`Anteil Minijobs`              -0.03  0.09   -0.31     0.40
#> ORIG_Auszubildende                   0.12  0.08    1.56     0.18
#> ORIG_Berufsschülerinnen              0.06  0.12    0.52     0.35
#> ORIG_Bildungswanderer                0.06  0.07    0.87     0.27
#> ORIG_Binnenwanderungsvolumen        -0.10  0.11   -0.96     0.26
#> ORIG_EWZ                             0.05  0.04    1.36     0.20
#> ORIG_`Anteil Minijobs`.lag1         -0.62  0.33   -1.91     0.15
#> ORIG_Auszubildende.lag1              0.42  0.33    1.26     0.21
#> ORIG_Berufsschülerinnen.lag1        -0.27  0.17   -1.60     0.18
#> ORIG_Bildungswanderer.lag1           0.59  0.18    3.22     0.10
#> ORIG_Binnenwanderungsvolumen.lag1    0.63  0.25    2.49     0.12
#> ORIG_EWZ.lag1                        0.25  0.24    1.01     0.25
#> INTRA_`Anteil Minijobs`              0.24  0.36    0.67     0.31
#> INTRA_Auszubildende                 -0.03  0.32   -0.08     0.47
#> INTRA_Berufsschülerinnen            -0.75  0.49   -1.53     0.18
#> INTRA_Bildungswanderer               0.35  0.25    1.38     0.20
#> INTRA_Binnenwanderungsvolumen        0.44  0.42    1.04     0.24
#> INTRA_EWZ                            0.10  0.16    0.60     0.33
#> INTRA_`Anteil Minijobs`.lag1        -0.14  1.31   -0.11     0.47
#> INTRA_Auszubildende.lag1             0.66  1.36    0.49     0.36
#> INTRA_Berufsschülerinnen.lag1       -0.08  0.68   -0.11     0.46
#> INTRA_Bildungswanderer.lag1         -0.93  0.75   -1.24     0.22
#> INTRA_Binnenwanderungsvolumen.lag1  -0.19  1.01   -0.19     0.44
#> INTRA_EWZ.lag1                      -0.78  0.99   -0.79     0.29
#> log(1 + distance)                    0.00  0.05   -0.07     0.48
#> 
#> --------------------------------------------------
#> R2_corr: 0.3598889
```

[^1]: The data set is actually not really needed. What is needed are the
    column names and some missings here and there. I am not sure if it
    is allowed to share the data structure publicely. I will find out
    and if so, I remove the dependence on the example data and provide a
    function to recreate it.
