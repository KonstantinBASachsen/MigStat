######################################################################
####################### Purpose of script ############################
######################################################################

## Here I collect functions that I use in the fdz. These are helper
## functions designed to ease a specific task. They are not general or
## robust enough to be used in other settings.

read_mig <- function(path, year) {
  ### liest die Wanderungsdaten ein
    dt <- data.table::fread(path, encoding = "UTF-8")
    cols <- c("EF02U5", "EF03U5", "EF02U2", "EF03U2", "EF25")
    dt <- dt[, .SD, .SDcols = cols]
    return(dt)
}
##' Creates five point summary for faster box plots
##'
##' 
##' @title five point summary by group
##' @param mig Migration Statistics data.table
##' @param col Name of column that is to be summarized
##' @param by Character vector of columns that are used as group. Five
##'     point summary is computed for every group
##' @return data.table of five point summaries
##' @author Konstantin
get_box_data <- function(mig, col, by) {
    ### berechnet die 5-Punkte Zusammenfassung, welche ich für
    ### boxplots verwende. Die Zusammenfassung wird für "col"
    ### berechnet. Jeweils für alle Gruppen welche durch "by". Kann
    ### auch ggplot automatisch machen, dauert dann aber deutlich
    ### länger.
    dt_box <- mig[, stats::fivenum(get(col)), by = by]
    stats <- c("ymin", "y25", "ymed", "y75", "ymax")
    dt_box[, "what" := stats, by = by]
    formula <- sprintf("%s ~ what", paste(by, collapse = " + "))
    dt_box <- data.table::dcast(dt_box, formula = formula, value.var = "V1")
    dt_box[, "year_short" := substr(as.factor(year), 3, 4)]
### bad but works for now, bc custom_boxplot() expects "group" col
    cols <- colnames(dt_box)
    cols[grepl("group", cols)] <- "group"
    colnames(dt_box) <- cols
    return(dt_box)
}

custom_boxplot <- function(dt, title) {
    ## Plotted die 5 Punkte Zusammenfassung, welche mit get_box_data()
    ## erstellt wurde.
    group <- NULL
    box_dist <- ggplot(dt, aes_string("year_short")) +
        geom_boxplot(aes_string(x = "year_short", min = "ymin", lower = "y25",
                     middle = "ymed", upper = "y75", max = "ymax",
                     group = "year"), stat = "identity") +
    facet_wrap(vars(group), drop = FALSE) +
    MigStat::theme_brrrp(maj.y = TRUE) +
    theme(
        axis.text.x = element_text(angle = 45)) +
    ggtitle(title) +
    xlab("Jahr") +
    ylab("Distanz in km")
    return(box_dist)
}


get_net <- function(wins, losses) {
    ### Erstellt gemeinsame data.table aus wins und losses uns
    ### berechnet die Netto Migration
    keys_w <- colnames(wins)[! colnames(wins) %in% c("flow", "wins")]
    keys_l <- colnames(losses)[!colnames(losses) %in% c("flow", "losses")]
    net <- merge(wins, losses, by.x = keys_w, by.y = keys_l, all = TRUE)
    colnames(net)[colnames(net) == "flow.x"] <- "wins"
    colnames(net)[colnames(net) == "flow.y"] <- "losses"
    colnames(net)[colnames(net) == "destination"] <- "region"
    colnames(net)[colnames(net) == "EF03U2"] <- "state"
    net[is.na(wins), "wins" := 0]
    net[is.na(losses), "losses" := 0]
    net[, "net" := wins - losses]
    net[ags_gen, "name_r" := i.GEN, on = .(region = AGS)]
    net[shps$states, "name_bl" := i.GEN, on = .(state = AGS)]
### the following line makes sure that regions with different ags are
### treated as one. In this case the number of groups/ rows is not
### reduced
    net <- net[, "net" := sum(net), by = .(age_gr, state, name_r)]
    net <- net[, .SD[1], by = .(age_gr, state, name_r)]
### alternatively????
##    net <- net[, .(net = sum(net))]
    return(net)
}


plot_age_st <- function(net, lab_years) {
    ## erstellt plot Gewinne/Verluste nach Bundesländern
    name_r <- NULL
  title <- sprintf("Gewinne/ Verluste der Regionen nach Bundesland \n der Jahre %s", lab_years)
  plot <- ggplot(net) +
    geom_col(aes_string("name_bl", "net", fill = "age_gr"), 
             position = "dodge") +
    MigStat::theme_brrrp(maj.x = TRUE) +
    coord_flip() +
    labs(fill = "Alter gruppiert") +
    facet_wrap(vars(name_r)) +
    theme_brrrp(leg.pos = c(0.85, 0.2), maj.x= FALSE, 
                maj.y = TRUE)  +
    ylab("Gewinn/Verlust") +
    xlab("") +
    ggtitle(title)
  return(plot)
}

plot_age_dist <- function(dt, title) {
    ## Erstellt plots mit Altersverteilung
    grp <- NULL
  plot_age <- ggplot(stats::na.omit(dt, "grp")) +
    geom_line(aes_string("year", "flow", colour = "age_gr")) +
    facet_wrap(vars(grp), scales = "free", drop = FALSE) +
    MigStat::theme_brrrp(leg.pos = c(0.85, 0.5), maj.y = TRUE) +
    ylab("Anzahl") +
    xlab("Jahr") +
    labs("colour" = "Alter gruppiert") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 45))
    
  return(plot_age)
}

