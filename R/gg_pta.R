# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


library(ggplot2)

#' Make a primer for pure tone audiograms with ggplot2
#'
#' Call this to start building a plot based on pure tone audiometry.
#'
#' This function is called instead of ggplot2::ggplot with a data.frame and will
#' return a ggplot with fixed axes, fixed axis ratio, ...
#'
#' @import ggplot2
#' @param data data.frame that contains the data, later to be added to the plot.
#' If no such data.frame is available, can be data = data.frame(0)
#' @param theme theme for plotting in ggplot2. Can be set to NULL.
#' A different theme can always be added later
#' @param xlab string containing the x axis label
#' @param ylab string containing the y axis label
#' @param lettermark either "R" or "L" or c("R", "L") to add a letter
#'                   describing the left or right side (see lettermarksize)
#' @param lettermarksize size of letter for lettermark
#' @param xlim limits of the frequencies displayed at the x axis.
#' @param xbreaks frequencies at which major line breaks should be drawn. Must be
#' of same length as \code{xlabels}
#' @param minor_xbreaks frequencies at which minor line breaks should be drawn
#' @param x_base_lwd if positive, a line to mark the 0 dB threshold level
#' is drawn, the line width of which is given by x_base_lwd. Set to -1 to
#' turn the line of
#' @param xlabels vector of strings as frequency axis labels. Must be of same length
#' as \code{xbreaks}.
#' @param ylim limits of the decibels on the y axis
#' @param yposition side on which to label the y axis: either "right" or "left"
#'
#' @return a ggplot with standard axis ratio, given axis etc. to add geoms to
#'
#' @author Bernhard Lehnert
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' fig1 <- gg_pta(data.frame())
#' print(fig1)
#'
#' fig2 <- gg_pta(data.frame(), xlab="Frequency [Hz]", xlim=c(125,12000),
#'                xbreaks = c(125, 250, 500, 1000, 2000, 4000, 8000, 12000),
#'                xlabels = c("125", "250", "500", "1k", "2k", "4k", "8k", "12k"))
#' print(fig2)
#'
#' expl <- data.frame(x=rep(c(500, 1000, 2000, 4000), 200),
#'                    y=5 + 70*rbeta(200,1,5))
#' fig3 <- gg_pta(expl, lettermark = "R",
#'                xlab="frecuencia", ylab="volumen") +
#'              geom_boxplot(aes(x=x, y=y, group=x)) +
#'              theme_grey()
#' print(fig3)
gg_pta <- function(data,
                   theme = theme_light,
                   lettermark = NULL,
                   lettermarksize = 30,
                   xlab = "Frequency in Hertz (Hz)",
                   ylab = "Hearing Levels in Decibels (dB)",
                   xlim = c(125, 8000),
                   xbreaks = c(125, 250, 500, 1000, 2000, 4000, 8000),
                   minor_xbreaks = c(750, 1500, 3000),
                   x_base_lwd = 1.0,
                   xlabels = c("125","250", "500", "1000", "2000",
                               "4000", "8000"),
                   ylim = c(120,-10),
                   yposition = "left")
    {
    p <- ggplot(data) +

    theme() +

    scale_x_continuous(name = xlab,
                       position="top",
                       trans = "log2",
                       breaks = xbreaks,
                       minor_breaks = minor_xbreaks,
                       labels = xlabels,
                       limits = xlim) +

    scale_y_reverse(name = ylab,
                    breaks=seq(150,-30,-10),
                    minor_breaks = NULL,
                    limits = ylim,
                    position = yposition) +

    coord_fixed(ratio =.05)

    if(x_base_lwd > 0)
        p <- p + geom_hline(yintercept = 0, lwd = x_base_lwd)
    if("R" %in% lettermark)
        p <- p + geom_text(data=data.frame(0),
                           mapping=aes(x=250, y=90), label="R", alpha=.2, size=lettermarksize)
    if("L" %in% lettermark)
        p <- p + geom_text(data=data.frame(0),
                           mapping=aes(x=4e3, y=90), label="L", alpha=.2, size=lettermarksize)

    return(p)
}
