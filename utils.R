
draw_circle <- function(r, center, color="black", fill=NA, ...) {
  xc <- center[1]
  yc <- center[2]
  x <- xc + r*cos(seq(0, pi, length.out=500))
  ymax <- yc + r*sin(seq(0, pi, length.out=500))
  ymin <- yc + r*sin(seq(0, -pi, length.out=500))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}

draw_point <- function(pt, color="red") {
  draw_circle(0.02, pt, color, color)
}

polar_point <- function(r, theta) {
  x <- r * cos(theta)
  y <- r * sin(theta)
  return(c(x, y))
}

draw_line <- function(start, end, color="black") {
  annotate("segment", x = start[1], xend = end[1], y = start[2], yend = end[2], color = color)
}