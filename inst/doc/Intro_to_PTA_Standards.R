## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, fig.dim=c(5,5)----------------------------------------------------
exmpl <- data.frame(f = c(125,250,500,1000,2000,3000,4000,8000),
                    t = c(15,10,25,35,40,55,75,60))
library(audiometry)
library(ggplot2)
gg_pta(data.frame(exmpl)) + 
  geom_point(aes(x=f, y=t), color="red", size=2.5) +
  geom_line(aes(x=f, y=t), color="red", lwd=.8)

## -----------------------------------------------------------------------------
ex1 <- data.frame(time = gl(3, 6),
                  f = rep(c(250, 500, 1000, 2000, 4000, 8000), 3),
                  t = c(60, 65, 40, 25, 10, 5, 
                        15, 25, 20, 10, 5, 0,
                        5, 5, 10, 0, 0, 10))

## -----------------------------------------------------------------------------
library(ggplot2)
ggpl <- ggplot(ex1)

## -----------------------------------------------------------------------------
library(ggplot2)
library(audiometry)
ggpl <- gg_pta(ex1)

## ---- fig.dim=c(6,5)----------------------------------------------------------
gg_pta(ex1) +
  geom_point(aes(x = f, y = t, color = time), size = 3, alpha=.5) +
  geom_line(aes(x = f, y = t, color = time, lty = time), lwd=1)

## ---- fig.dim=c(5,5)----------------------------------------------------------
gg_pta(data.frame(0), xlab = "frequenza [Hz]", ylab = "volume [dB]",
       xlim = c(125, 12000), x_base_lwd = -1)

## ---- fig.dim=c(5,5)----------------------------------------------------------
gg_pta(data.frame(0), lettermark = c("R", "L"), lettermarksize = 40)

## ---- echo=FALSE--------------------------------------------------------------
id = gl(25, 9)
f = rep(c(250, 500, 750, 1000, 1500, 2000, 3000, 4000, 8000), 25)
t = c(replicate(13, c(20, 20, 20, 25, 25, 35, 40, 45, 40) +
                   sample(seq(-15,10,2.5),9, replace=TRUE)),
      replicate(12, c(10, 5, 10, 10, 10, 10, 15, 5, 15) +
                   sample(seq(-10,10,2.5),9, replace=TRUE)))
treat = c(rep("placebo", 15*9), rep("verum", 10*9))

some.data <- data.frame(id = id, freq = f, thres = t, treatment = treat)

## ---- fig.dim=c(5,5)----------------------------------------------------------
gg_pta(some.data) + 
  geom_line(aes(x = freq, y = thres, group = id, color = treatment)) + 
  theme_grey() + 
  theme(legend.position = c(0.5, 0.2))

## ---- fig.dim=c(6,5)----------------------------------------------------------
library(ggbeeswarm)
gg_pta(some.data, xlim=c(125, 10000), x_base_lwd = 1.5,
       xlab = "Frequenz [Hz]", ylab = "Lautstärkepegel [dB HL]") +
  geom_beeswarm(aes(x = freq, y = thres, color=treatment), cex = 1.5) +
  theme_linedraw()

## ---- fig.dim=c(5,5)----------------------------------------------------------
library(ggthemes)
gg_pta(data.frame(0)) +
  geom_segment(aes(x = 500, y = 29, xend = 4000, yend = 29), colour = "red", lwd=2) +
  geom_segment(aes(x = 500, y = 31, xend = 4000, yend = 31), colour = "green", lwd=2) +
  theme_gdocs()


