% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_pta.R
\name{gg_pta}
\alias{gg_pta}
\title{Make a primer for pure tone audiograms with ggplot2}
\usage{
gg_pta(
  data = data.frame(),
  theme = theme_light,
  lettermark = NULL,
  lettermarksize = 30,
  xlab = "Frequency in Hertz (Hz)",
  ylab = "Hearing Levels in Decibels (dB)",
  xlim = c(125, 8000),
  xbreaks = c(125, 250, 500, 1000, 2000, 4000, 8000),
  minor_xbreaks = c(750, 1500, 3000),
  x_base_lwd = 1,
  xlabels = c("125", "250", "500", "1000", "2000", "4000", "8000"),
  ylim = c(120, -10),
  yposition = "left"
)
}
\arguments{
\item{data}{data.frame that contains the data, later to be added to the plot.
If no such data.frame is available, can be data = data.frame(0)}

\item{theme}{theme for plotting in ggplot2. Can be set to NULL.
A different theme can always be added later}

\item{lettermark}{either "R" or "L" or c("R", "L") to add a letter
describing the left or right side (see lettermarksize)}

\item{lettermarksize}{size of letter for lettermark}

\item{xlab}{string containing the x axis label}

\item{ylab}{string containing the y axis label}

\item{xlim}{limits of the frequencies displayed at the x axis.}

\item{xbreaks}{frequencies at which major line breaks should be drawn. Must be
of same length as \code{xlabels}}

\item{minor_xbreaks}{frequencies at which minor line breaks should be drawn}

\item{x_base_lwd}{if positive, a line to mark the 0 dB threshold level
is drawn, the line width of which is given by x_base_lwd. Set to -1 to
turn the line of}

\item{xlabels}{vector of strings as frequency axis labels. Must be of same length
as \code{xbreaks}.}

\item{ylim}{limits of the decibels on the y axis}

\item{yposition}{side on which to label the y axis: either "right" or "left"}
}
\value{
a ggplot with standard axis ratio, given axis etc. to add geoms to
}
\description{
Call this to start building a plot based on pure tone audiometry.
}
\details{
This function is called instead of ggplot2::ggplot with a data.frame and will
return a ggplot with fixed axes, fixed axis ratio, ...
}
\examples{
library(ggplot2)
fig1 <- gg_pta(data.frame())
print(fig1)

fig2 <- gg_pta(data.frame(), xlab="Frequency [Hz]", xlim=c(125,12000),
               xbreaks = c(125, 250, 500, 1000, 2000, 4000, 8000, 12000),
               xlabels = c("125", "250", "500", "1k", "2k", "4k", "8k", "12k"))
print(fig2)

expl <- data.frame(x=rep(c(500, 1000, 2000, 4000), 200),
                   y=5 + 70*rbeta(200,1,5))
fig3 <- gg_pta(expl, lettermark = "R",
               xlab="frecuencia", ylab="volumen") +
             geom_boxplot(aes(x=x, y=y, group=x)) +
             theme_grey()
print(fig3)
}
\author{
Bernhard Lehnert
}
