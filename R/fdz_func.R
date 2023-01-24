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

