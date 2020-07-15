library(ggplot2)


#' Boltzmann's function
#'
#' s-shaped curve , used as discrimination function to draw the "normal" curves
#' in the Freiburger Sprachtest.
#' Given as
#' y = (exp(-4*(L-L_50))\*s_50)^-1
#' this is similar to a logistic regression
#' result but with parameterization usefull here:
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
#' # Compute the expected intelligbility at 20, 30 and 40 dB SPL
#'
#' boltzmann(L = c(20, 30, 40), L_50 = 29.3, s_50 = .05)
boltzmann <- function(L, L_50 = 18.4, s_50 = .08){
  1/(1+exp(-4*(L-L_50)*s_50))
}

#' Freiburger Sprachtest data with ggplot2
#'
#' Probably the most influential speech intelligibility test in German speaking countries.
#' This function serves as a starting point for plotting data in way that reflects the
#' usual representation of Freiburger Sprachtest results.
#'
#' @param data a data.frame that is given to ggplot for initialization
#' @param NC_alpha value between 0 and 1 defining how prominent the "normal" curves are.
#' @param HV_color color of the Hörverlust-Scale in the
#' middle of the diagram.
#'
#' @return a ggplot suitable for adding Freiburger Sprachtest data as geom_*
#' @export
#'
#' @examples library(ggplot2)
#' id = gl(25,4)
#' gender=gl(2,25, label =c("Frauen", "M\u00e4nner"))
#' x = rep(c(35, 50, 65, 80), 25)
#' y = 100*boltzmann(jitter(x,3), 45, .03)
#' example <- data.frame(Patient=id, Geschlecht = gender, x=x, y=y)
#' p <- gg_freiburg(NC_alpha = 1, HV_color = "grey") +
#'         geom_boxplot(aes(x = x, y = y, group = x), example) +
#'         geom_line(aes(x = x, y = y, color = Geschlecht, group = id), example)
#' print(p)
gg_freiburg <- function(data = data.frame(), NC_alpha = .6, HV_color="grey"){
  num = x = y = text = intel = NULL  # for checking in presence of non-standard evaluation
  n <- .9
  spl = seq(18.4-n*1/.08,18.5+n*1/.08,length.out = 50)
  num <- data.frame(spl = spl,
                    intel = 100*boltzmann(spl, 18.4, .08))
  spl = seq(29.3-n*1/.05,29.3+n*1/.05,length.out = 50)
  sil <- data.frame(spl = spl,
                    intel = 100*boltzmann(spl, 29.3, .05))
  p <- ggplot(data) +
    geom_line(data=num, mapping=aes(x=spl, y = intel), alpha = NC_alpha) +
    geom_line(data=sil, mapping=aes(x=spl, y = intel), alpha = NC_alpha) +
    scale_x_reverse(name ="Sprechschallpegel in dB",
                       breaks = c(0, seq(5, 110, 15)), expand= c(0,0),
                       labels =  as.character(c(0, seq(5, 110, 15))),
                       minor_breaks = seq(120, 0, -5), limits = c(120,0)) +
    scale_y_continuous(name = "Verst\u00e4ndlichkeit in %",
                       position="right", limits=c(0,100),
                       breaks = seq(0,100,20), expand= c(0,0),
                       labels  = as.character(seq(0,100,20)),
                       minor_breaks = seq(0,100, 10)) +
    geom_text(aes(x=x, y=y, label=text), color = HV_color,
              data.frame(x = seq(18.4, 78.4,10), y = 50,
                         text =as.character(seq(0,60,10)))) +
    geom_text(aes(x=100, y=45, label = "H\u00f6rverlust (dB)"), color = HV_color,
              angle = 90) +
    theme_light() + theme(aspect.ratio = 1.5) +
    coord_flip()
}
