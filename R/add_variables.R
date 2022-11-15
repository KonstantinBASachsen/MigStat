group_regions <- function(mig, ags_gen) {
    GEN <- . <- AGS <- EF03U2 <- group_o <- EF03U5 <- NULL
    EF02U2 <- group_d <- EF02U5 <- NULL
    ags_sachsen <- 14
    ags_bl <- 1:16
    ags_ost <- c(12, 13, 15, 16) ### ohne Berlin, ohne Sachsen
    ags_west <- setdiff(ags_bl, c(ags_sachsen, ags_ost)) #### mit Berlin
    
    mig[, "group_o" := as.character(NA)]
    mig[EF03U2 %in% ags_west, group_o := "west"]
    mig[EF03U2 %in% ags_ost, group_o := "ost"]
    mig[EF03U2 %in% ags_sachsen, group_o := "sachsen"]
    
    setkeyv(mig, "EF03U5")
    setkeyv(ags_gen, "AGS")
    mig[ags_gen, "group_o" := i.GEN]
    message(mig[, .N, by = group_o])

    mig[, "group_d" := as.character(NA)]
    mig[EF02U2 %in% ags_west, group_d := "west"]
    mig[EF02U2 %in% ags_ost, group_d := "ost"]
    mig[EF02U2 %in% ags_sachsen, group_d := "sachsen"]

    setkeyv(mig, "EF02U5")
    setkeyv(ags_gen, "AGS")
    mig[ags_gen, "group_d" := i.GEN]
    message(mig[, .N, by = group_d])
    return(NULL)
}
