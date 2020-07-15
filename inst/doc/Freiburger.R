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
  

