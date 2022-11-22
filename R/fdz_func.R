######################################################################
####################### Purpose of script ############################
######################################################################

## Here I collect functions that I use in the fdz. These are helper
## functions designed to ease a specific task. They are not general or
## robust enough to be used in other settings.


##' Sets paths for current working environment
##'
##' @title sets paths for working at the FDZ or at work
##' @param p_work paths for work
##' @param p_fdz paths for FDZ
##' @param fdz logical, working at the fdz or not?
##' @return list with paths
##' @author Konstantin
make_paths <- function(p_work, p_fdz, fdz) {
    stopifnot(is.logical(fdz))
    if (fdz == TRUE) {
        paths <- p_fdz
    } else {
        paths <- p_work
    }
}

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

    ## replace fivenum with boxplot.stats?
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

##' Makes a boxplot from table returned by get_box_data(). Expects
##' "year_short" as x-axis and "group" as faceting variable
##'
##' 
##' @title Boxplot with facets
##' @param dt data.table returned by get_box_data()
##' @param title Plot title
##' @return plot object
##' @author Konstantin
##' @import ggplot2
custom_boxplot <- function(dt, title) {
    ## Plotted die 5 Punkte Zusammenfassung, welche mit get_box_data()
    ## erstellt wurde.
    group <- NULL
    box_dist <- ggplot2::ggplot(dt, ggplot2::aes_string("year_short")) +
        ggplot2::geom_boxplot(
                     ggplot2::aes_string(x = "year_short",
                                         min = "ymin", lower = "y25",
                                         middle = "ymed", upper = "y75", max = "ymax",
                                         group = "year"), stat = "identity") +
    ggplot2::facet_wrap(ggplot2::vars(group), drop = FALSE) +
    theme_brrrp(maj.y = TRUE) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("Jahr") +
    ggplot2::ylab("Distanz in km")
    return(box_dist)
}

##' Retunes a common data.table of wins and losses and returnes the
##' net migration (wins - losses). Only designed to handle wins and
##' losses with respect to federal states.
##'
##' @title net migration of wins and losses of regions with respect to
##'     federal states.
##' @param wins data.table wih "wins" (number of moves into a region)
##'     per regions
##' @param losses data.table wih "losses" (number of moves out of a
##'     region) per regions
##' @param states data.table with columns AGS and GEN of federal
##'     states
##' @param ags_gen data.table with columns AGS and GEN of our
##'     interesting regions. This is neccessary to recognize that
##'     different ags might belong to the same region.
##' @return data.table with names of regions and their wins, losses
##'     and net migration with respect to federal states.
##' @author Konstantin
get_net2 <- function(wins, losses, states, ags_gen) {
    ## Erstellt gemeinsame data.table aus wins und losses unsberechnet
    ## die Netto Migration
    i.GEN <- . <- AGS <- age_gr <- state <- name_r <- NULL
    keys_w <- colnames(wins)[! colnames(wins) %in% c("flow", "wins")]
    keys_l <- colnames(losses)[!colnames(losses) %in% c("flow", "losses")]
    net <- merge(wins, losses, by.x = keys_w, by.y = keys_l, all = TRUE)
    colnames(net)[colnames(net) == "flow.x"] <- "wins"
    colnames(net)[colnames(net) == "flow.y"] <- "losses"
    colnames(net)[colnames(net) == "destination"] <- "region"
    colnames(net)[colnames(net) == "EF03U2"] <- "state"
    net[ags_gen, "name_r" := i.GEN, on = .(region = AGS)]
    net[states, "name_bl" := i.GEN, on = .(state = AGS)]
    ## the following line makes sure that regions with different ags
    ## are treated as one. In this case the number of groups/ rows
    ## might be reduced. Might because if year is taken as grouping
    ## variable than different ags for the same region correspond to
    ## different years and because year is a group it does not reduce
    ## the overall number of groups.
    net <- net[, "wins" := sum(wins), by = .(age_gr, state, name_r, year)]
    net <- net[, "losses" := sum(losses), by = .(age_gr, state, name_r, year)]
    net <- net[, .SD[1], by = .(age_gr, state, name_r, year)]
    net[is.na(wins), "wins" := 0]
    net[is.na(losses), "losses" := 0]
    net[, "net" := wins - losses]
    ## net <- net[, "net" := sum(net), by = .(age_gr, state, name_r, year)]
    ## net <- net[, .SD[1], by = .(age_gr, state, name_r, year)]
### alternatively????
##    net <- net[, .(net = sum(net))]
    return(net)
}

##' Plots net migration of regions wrt. federal states. Best used with
##' input from get_net2.
##'
##' @title plotting net migration wrt. to federal states
##' @param net data.table with regions and net migration wrt to
##'     federal states. Preferably output from get_net2().
##' @param lab_years Which years are plotted? lab_years is inserted in the title
##' @return gpglot object
##' @author Konstantin
plot_age_st <- function(net, lab_years) {
    ## erstellt plot Gewinne/Verluste nach Bundesländern
    name_r <- NULL
    title <- sprintf("Netto Migration der Regionen nach Bundesland \n der Jahre %s", lab_years)
    plot <- ggplot2::ggplot(net) +
        ggplot2::geom_col(ggplot2::aes_string("name_bl", "net", fill = "age_gr"), 
                          position = "dodge") +
        theme_brrrp(maj.x = TRUE) +
        ggplot2::coord_flip() +
        ggplot2::labs(fill = "Alter gruppiert") +
        ggplot2::facet_wrap(ggplot2::vars(name_r)) +
        theme_brrrp(leg.pos = c(0.85, 0.2), maj.x= FALSE, 
                    maj.y = TRUE)  +
        ggplot2::ylab("Gewinn/Verlust") +
        ggplot2::xlab("") +
        ggplot2::ggtitle(title)
    return(plot)
}

##' plots number of moves to or from a region (depending on the input)
##' for different age groups over the course of years
##'
##' @title plots age distribution of moves
##' @param dt data.table with columns year, flow, age_gr and grp. Grp
##'     is used as facet variable
##' @param title Title of plot
##' @return ggplot object
##' @author Konstantin
plot_age_dist <- function(dt, title) {
    ## Erstellt plots mit Altersverteilung
    grp <- NULL
    plot_age <- ggplot2::ggplot(stats::na.omit(dt, "grp")) +
        ggplot2::geom_line(ggplot2::aes_string("year", "flow", colour = "age_gr")) +
        ggplot2::facet_wrap(ggplot2::vars(grp), scales = "free", drop = FALSE) +
        theme_brrrp(leg.pos = c(0.85, 0.5), maj.y = TRUE) +
        ggplot2::ylab("Anzahl") +
        ggplot2::xlab("Jahr") +
        ggplot2::labs("colour" = "Alter gruppiert") +
        ggplot2::ggtitle(title) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
    
  return(plot_age)
}

