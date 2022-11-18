add_vars <- function(mig, add_vars = c("age_gr", "year_gr", "regions_gr"), ags_gen = NULL, add_empty = TRUE) {
    EF25 <- NULL
    if ("age_gr" %in% add_vars) {
        ages <- c("Kind", "Jung", "Erwachsen", "Senior")
        mig[, "age_gr" := as.character(NA)]
        mig[, "age_gr" := fifelse(EF25 < 18, ages[1],
                                  fifelse(EF25 < 30, ages[2],
                                          fifelse(EF25 < 60, ages[3], ages[4])))]
        mes <- sprintf("Created 'age_gr' with age categories %s", paste(ages, collapse = " "))
        message(mes)
    }
    if ("year_gr" %in% add_vars) {
        yg <- c("00-10", "11-15", "16-18")
        mig[, "year_gr" := as.character(NA)]
        mig[, "year_gr" := fifelse(year < 2011, yg[1],
                                   fifelse(year >= 2011 & year < 2016, yg[2], yg[3]))]
        mes <- sprintf("Created 'year_gr' with categories %s", paste(yg, collapse = " "))
    }
    if ("regions_gr" %in% add_vars) {
        if (is.null(ags_gen) == TRUE) {
            stop("For grouping regions, please specify data.table ags_gen with pairs of AGS and GEN.")}
        group_regions(mig, ags_gen, add_empty)
    }
############# Gemeindereform im Jahr 2008 #################
    ## 2008 gab es eine große Gemeindereform in Sachsen, durch diese 
    ## haben sich die AGS verändert. Deswegen führe ich eine Variable ein,
    ## welche das angibt
    ## mig[, "reform" := fifelse(year < 2008, 0, 1)]
    return(mig)
}

group_regions <- function(mig, ags_gen, add_empty = FALSE) {
    EF03U2 <- EF02U2 <- i.GEN <- group_o <- group_d <- NULL
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
    if (add_empty == TRUE) {
        empty_region(mig)
        message("group_o, group_d now factors with NA level so in plots comparison groups ost, west, Sachsen are in the bottom")
    }
    return(NULL)
}

empty_region <- function(mig) {
    ## This I should integrate into group_regions or maybe I shouldn't
    ## because of the empty factor level
    ## Hier bringe ich die Regionen in die richtige Reihenfolge und
    ## bereite sie für das Erstellen von Grafiken vor.
    group_o <- group_d <- NULL
    regions <- c("Altenburg", "Delitzsch", "Hoyerswerda", "Oschatz",
                 "Riesa", "sachsen", "ost", "west")
    ## Ich füge "" als factor level hinzu, damit an der richtigen Stelle
    ## im Plot ein freies Feld ist. Ist nicht super, aber ich weiß nicht,
    ## wie ich ggplot sagen kann, dass ein facet frei bleiben soll.
    lvls <- c(regions[1:5], "", regions[6:8])
    mig[, "group_o" := factor(group_o, levels = c(lvls, NA))]
    mig[, "group_d" := factor(group_d, levels = c(lvls, NA))]
    return(NULL)
}
