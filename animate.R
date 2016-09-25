library(animation)
source("main.R")

trace.animate <- function () {
  plots
}

saveGIF(trace.animate(), interval=1.0, movie.name="anim.gif")