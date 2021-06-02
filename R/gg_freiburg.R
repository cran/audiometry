library(ggplot2)


#' Boltzmann's function
#'
#' s-shaped curve , originally used as discrimination function to draw
#' the "normal" curves in the Freiburger Sprachtest before I could find
#' the official norm values. Could still be usefull for someone seeking
#' to add something like that to her plots be it as example or for simulation.
#' The function is given as
#' y = (exp(-4*(L-L_50))\*s_50)^-1
#' this is similar to a logistic regression result but with parameterization
#' that is expecially usefull here:
#'
#' @param L sound pressure level for which the intelligibility is to be computed
#' @param L_50 sound pressure level at 50\% intelligibility
#' @param s_50 intelligibility at L_50, happens to be 8\% in Freiburger Zahlentest
#' and 5\% in Freiburger Einsilbertest (values taken from S. Hoth, Der Freiburger
#' Sprachtest, HNO 2016, 64:540-48).
#'
#' @return predicted intelligibility
#' @export
#'
#' @examples
#' # Freiburger Einsilbertest has L_50 = 29.3 dB and s_50 at 5 %/dB.
#' # Compute the expected intelligibility at 20, 30 and 40 dB SPL
#'
#' boltzmann(L = c(20, 30, 40), L_50 = 29.3, s_50 = .05)
boltzmann <- function(L, L_50 = 18.4, s_50 = .08){
  1/(1+exp(-4*(L-L_50)*s_50))
}

#' Freiburger Sprachtest data with ggplot2
#'
#' Draws the most influential speech intelligibility test in German
#' speaking countries.
#' This function serves as a starting point for plotting data in way that
#' reflects the usual representation of Freiburger Sprachtest results.
#'
#' @param data a data.frame that is given to ggplot for initialization
#' @param mapping same as mapping in ggplot2::ggplot
#' @param horizontal logical whether to orient the plot horizontally
#' @param xlab label on the x axis
#' @param ylab label on the y axis
#' @param x_ticks_at vector of x values where numbers on x axis should appear. This
#'        is seq(0, 110, 10) in the DIN but c(0, seq(5, 120, 15)) in Muster 13.
#' @param y_ticks_at corresponding to x_ticks_at for the y axis. Set to seq(0,100,10)
#'        to mimick the DIN, seq(0, 100, 20) to mimick Muster 13.
#' @param plot_reference logical whether to plot the normal hearing curves for numbers and
#'        syllables
#' @param plot_reference_lwd_1 line width for reference line 1
#' @param plot_reference_lwd_2 line width for reference line 2
#' @param plot_reference_color_1 line color for reference line 1
#' @param plot_reference_color_2 line color for reference line 2
#' @param plot_discr_loss_scale logical whether to print discrimination loss values in the middle of the plot (likely to change in later versions)
#' @param plot_discr_loss_scale_values numeric vector of discrimination loss values (likely to change in later versions)
#' @param plot_discr_loss_scale_color color value of discrimination loss values (likely to change in later versions)
#' @param NC_alpha deprecated parameter that was used only in version 0.2.0
#' @param HV_color deprecated parameter that was used only in version 0.2.0
#'
#' @return a ggplot suitable for adding Freiburger Sprachtest data as geom_*
#' @export
#'
#' @examples library(ggplot2)
#' data.frame(loud = c(20, 35, 50, 65), perc = c(0,10,65,100)) |>
#' gg_freiburg(aes(x = loud, y = perc)) +
#'   geom_point() +
#'     geom_line()
#'
#'
#' id = gl(25,4)
#' gender=gl(2,25, label =c("Frauen", "M\u00e4nner"))
#' x = rep(c(35, 50, 65, 80), 25)
#' y = 100*boltzmann(jitter(x,3), 45, .03)
#' example <- data.frame(Patient=id, Geschlecht = gender, x=x, y=y)
#' p <- gg_freiburg() +
#'         geom_boxplot(aes(x = x, y = y, group = x), example) +
#'         geom_line(aes(x = x, y = y, color = Geschlecht, group = id), example)
#' print(p)
#'
#' gg_freiburg(plot_reference_lwd_1 = 2.5, plot_reference_lwd_2 = 3,
#'            plot_reference_color_1 = "green", plot_reference_color_2 = "pink")
gg_freiburg <- function(data = data.frame(),
                        mapping = aes(),
                        horizontal = FALSE,
                        xlab = "Sprachschallpegel [dB]",
                        ylab = "Sprachverstehen [%]",
                        x_ticks_at = seq(0, 110, 10),
                        y_ticks_at = seq(0, 100, 20),
                        plot_reference = TRUE,
                        plot_reference_lwd_1 = .8,
                        plot_reference_lwd_2 = .8,
                        plot_reference_color_1 = "darkgrey",
                        plot_reference_color_2 = "darkgrey",
                        plot_discr_loss_scale = TRUE,
                        plot_discr_loss_scale_values = seq(0, 90, 10),
                        plot_discr_loss_scale_color = "darkgrey",
                        NC_alpha = NULL, HV_color=NULL){
  num = x = y = text = intel = z = s = NULL  # for checking in presence of non-standard evaluation

  if(!is.null(NC_alpha)|| !is.null(HV_color))
           warning("WARNING: Use of deprecated parameter in geom_freiburg.")

  if(!horizontal){
  p <- ggplot(data = data, mapping = mapping) +
    scale_x_reverse(name =xlab,
                       breaks = x_ticks_at, expand= c(0,0),
                       labels =  as.character(x_ticks_at),
                       minor_breaks = seq(120, 0, -5), limits = c(120,0)) +
    scale_y_continuous(name = ylab,
                       position="right", limits=c(0,100),
                       breaks = y_ticks_at, expand= c(0,0),
                       labels  = as.character(y_ticks_at),
                       minor_breaks = seq(0,100, 10)) +
    theme_light() +
    theme(aspect.ratio = 2) +
    coord_flip()
  }
  else
  {
    p <- ggplot(data = data, mapping = mapping) +
      scale_x_continuous(name =xlab,
                      breaks = x_ticks_at, expand= c(0,0),
                      labels =  as.character(x_ticks_at),
                      minor_breaks = seq(0, 120, 5), limits = c(0, 120)) +
      scale_y_continuous(name = ylab,
                         position="left", limits=c(0,100),
                         breaks = y_ticks_at, expand= c(0,0),
                         labels  = as.character(y_ticks_at),
                         minor_breaks = seq(0,100, 10)) +
      theme_light() +
      theme(aspect.ratio = .5)
  }

  if(plot_reference) {
    #tab2 ist table 2 in DIN45626-1:1995-08
    tab2 <- data.frame(p = seq(0, 100, 10),
                       z = c(12, 13.7, 15, 16.3, 17.5, 18.4, 20.1, 21.5, 23.2, 25.3, 28.3),
                       s = c(17.1, 19.5, 22.1, 24.3, 26.7, 29.3, 32.3, 35.7, 39.2, 43, 47.3))
    p <- p +
      geom_line(data = tab2, mapping=aes(x=z, y=p),
                color = plot_reference_color_1,
                lwd = plot_reference_lwd_1) +
      geom_line(data = tab2, mapping=aes(x=s, y=p),
                color = plot_reference_color_2,
                lwd = plot_reference_lwd_2)
  }

  if(plot_discr_loss_scale) {
    p <- p + geom_text(aes(x=x, y=y, label=text),
                data.frame(x = 18.4+ plot_discr_loss_scale_values, y = 50),
                label=as.character(plot_discr_loss_scale_values),
                color = plot_discr_loss_scale_color)

  }

  return(p)
}
