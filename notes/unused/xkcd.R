library(ggplot2)
library(sysfonts)
library(xkcd)

xkcd_font <- "xkcd"

theme_xkcd <- theme(
  panel.background = element_blank(), 
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.text.y = element_text(colour="black"), 
  axis.text.x = element_text(colour="black"),
  panel.border = element_blank(),
  text = element_text(size=20, family=xkcd_font)
)

xkcd_line <- function(A, C){
  return(list(
    geom_line(mapping=A, colour="white", size=3), 
    geom_line(mapping=A, colour=C, size=1.5, position="jitter")))
}

