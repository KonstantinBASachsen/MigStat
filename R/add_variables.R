group_regions <- function(mig, shp) {
    GEN <- . <- AGS <- EF03U2 <- group_o <- EF03U5 <- NULL
    EF02U2 <- group_d <- EF02U5 <- NULL
    ags_sachsen <- "14" 
    ags_bl <- as.character(1:16)
    ags_bl[1:9] <- paste0("0", ags_bl[1:9])
    ags_ost <- c("12", "13", "15", "16") ### ohne Berlin, ohne Sachsen
    ags_west <- setdiff(ags_bl, c(ags_sachsen, ags_ost)) #### mit Berlin

#### Hier greife ich aus dem shapefile die AGS für unsere
#### interessanten Gemeinden heraus. Dies sollte bestenfalls im
#### korrekten Jahr geschehen, da sich die AGS durchaus ändern
#### können. Dies implementiere ich später.
    ### Ist es nicht einfacher/ klarer, direkt die AGS an
### group_regions() zu übergeben?

    ### looks complicated and it takes a long time
    regions <- c("Hoyerswerda", "Riesa", "Oschatz", "Delitzsch", "Altenburg")
    ags_names <- shp[GEN %in% regions, .(AGS, GEN)]
    
    mig[, "group_o" := as.character(NA)]
    mig[EF03U2 %in% ags_west, group_o := "west"]
    mig[EF03U2 %in% ags_ost, group_o := "ost"]
    mig[EF03U2 %in% ags_sachsen, group_o := "sachsen"]
##    mig[EF03U5 %in% ags_regions, group_o := "unsere"]
    mig[EF03U5 %in% ags_names[1, AGS], group_o := ags_names[1, GEN]]
    mig[EF03U5 %in% ags_names[2, AGS], group_o := ags_names[2, GEN]]
    mig[EF03U5 %in% ags_names[3, AGS], group_o := ags_names[3, GEN]]
    mig[EF03U5 %in% ags_names[4, AGS], group_o := ags_names[4, GEN]]
    mig[EF03U5 %in% ags_names[5, AGS], group_o := ags_names[5, GEN]]
    message(mig[, .N, by = group_o])
    mig[, "group_d" := as.character(NA)]
    mig[EF02U2 %in% ags_west, group_d := "west"]
    mig[EF02U2 %in% ags_ost, group_d := "ost"]
    mig[EF02U2 %in% ags_sachsen, group_d := "sachsen"]
##    mig[EF02U5 %in% ags_regions, group_d := "unsere"]
    mig[EF02U5 %in% ags_names[1, AGS], group_d := ags_names[1, GEN]]
    mig[EF02U5 %in% ags_names[2, AGS], group_d := ags_names[2, GEN]]
    mig[EF02U5 %in% ags_names[3, AGS], group_d := ags_names[3, GEN]]
    mig[EF02U5 %in% ags_names[4, AGS], group_d := ags_names[4, GEN]]
    mig[EF02U5 %in% ags_names[5, AGS], group_d := ags_names[5, GEN]]
    message(mig[, .N, by = group_d])
    return(NULL)
}
