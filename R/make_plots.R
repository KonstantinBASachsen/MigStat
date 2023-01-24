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
