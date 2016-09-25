library(animation)
source("main.R")

trace.animate <- function () {
  lapply(plots, function(x) print(x))
}

saveGIF(trace.animate(), interval=1.0, movie.name="anim.gif")