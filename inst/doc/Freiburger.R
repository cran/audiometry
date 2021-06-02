## -----------------------------------------------------------------------------
expl <- data.frame(test = gl(2,4, labels = c("Zahlen", "Einsilber")),
                   spl = c(20, 25, 30, 35, 35, 50, 65, 80), 
                   int = c(30, 60, 85, 95, 40, 95, 100, 95))

## ---- fig.dim=c(6,5)----------------------------------------------------------
library(ggplot2)
library(audiometry)
gg_freiburg(expl) +
  geom_point(aes(x=spl, y=int, group=test), color="red") +
  geom_line(aes(x=spl, y=int, group=test), color="red")
  

## ---- fig.dim=c(6,5)----------------------------------------------------------
data.frame(loud1 = c(10, 20, 30, 40), perc1 = c(0, 25, 75, 100),
           loud2 = c(20, 35, 50, 65), perc2 = c(0,10,65,100)) |>
    gg_freiburg(horizontal = TRUE) +
           geom_point(aes(x = loud1, y = perc1)) +
           geom_line(aes(x = loud1, y = perc1)) +
           geom_point(aes(x = loud2, y = perc2)) +
           geom_line(aes(x = loud2, y = perc2))

## ---- fig.dim = c(6,3)--------------------------------------------------------
gg_freiburg() + 
  theme(aspect.ratio = 1)

## ---- fig.dim=c(6,5)----------------------------------------------------------
gg_freiburg(xlab = "How loud it was", ylab = "How much was understood", 
            x_ticks_at = seq(5, 120, 15), y_ticks_at = c(0, 25, 50, 75, 100),
            plot_discr_loss_scale = FALSE)

## ---- fig.dim=c(6,5)----------------------------------------------------------
gg_freiburg(xlab = "How loud it was", ylab = "How much was understood", 
            x_ticks_at = seq(5, 120, 15), y_ticks_at = c(0, 25, 50, 75, 100),
            plot_discr_loss_scale = FALSE, horizontal = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  library(audiometry)
#  example(gg_freiburg)

## ---- fig.dim=c(6,5)----------------------------------------------------------
gg_freiburg(plot_discr_loss_scale = TRUE, plot_discr_loss_scale_color = "red")

## ---- fig.dim=c(6,5)----------------------------------------------------------
gg_freiburg(plot_discr_loss_scale_values = seq(0, 60, 10)) +
  geom_text(aes(x=100, y=45, label = "H\u00f6rverlust (dB)"), color="darkgrey", angle = 90)

## ---- fig.dim=c(6,5)----------------------------------------------------------
gg_freiburg() +
  geom_label(aes(x=98, y=50, label = "H\u00f6rverlust f\u00fcr\nZahlen (dB)"), 
             color="darkgrey")

