library(ggplot2)

source("utils.R")

profile_limits <- function(stage) {
  full_zoom <- 2
  if (stage <= 12) {
    zoom <- full_zoom
  } else {
    end_stage <- 48
    end_zoom <- 1
    delta <- (full_zoom - end_zoom) / (end_stage - 12)
    zoom <- full_zoom - delta * (stage - 12)
  }
  print(zoom)
  expand_limits(x = c(-zoom, zoom), y = c(-zoom, zoom))
}

g <- ggplot() + coord_fixed(1) +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()  
  )

plots <- list()
origin <- c(0, 0)
stage <- 0

# 1 plot a point at the origin
stage <- stage + 1
pts <- list(origin)
plots[[stage]] <- g + profile_limits(stage) + lapply(pts, FUN=function(x) draw_new_point(x))

# 2 circle centered at the origin, line to 0, 1
stage <- stage + 1
new_pts <- list(polar_point(1, pi / 2))
plots[[stage]] <- g + profile_limits(stage) + draw_line(origin, c(0, 1)) + draw_circle(1, origin) +
  lapply(pts, FUN=function(x) draw_point(x)) +
  lapply(new_pts, FUN=function(x) draw_new_point(x))
pts <- c(pts, new_pts)

# 3 another circle, centered at 0, 1
stage <- stage + 1
new_pts <- c(list(polar_point(1, 5*pi/6)), list(polar_point(1, pi/6)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, pts[[2]]) +
  lapply(pts, FUN=function(x) draw_point(x)) +
  lapply(new_pts, FUN=function(x) draw_new_point(x))
pts <- c(pts, new_pts)

# 4 two more circles centered at the intersections of the previous new circle
stage <- stage + 1
new_pts <- c(list(polar_point(1, 7*pi/6)), list(polar_point(1, -pi/6)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, pts[[3]]) + draw_circle(1, pts[[4]]) +
  lapply(pts, FUN=function(x) draw_point(x)) +
  lapply(new_pts, FUN=function(x) draw_new_point(x))
pts <- c(pts, new_pts)

# 5 continuing with two more circles, and new points at the intersection
stage <- stage + 1
new_pts <- list(polar_point(1, -pi/2))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, pts[[5]]) + draw_circle(1, pts[[6]]) +
  lapply(pts, FUN=function(x) draw_point(x)) +
  lapply(new_pts, FUN=function(x) draw_new_point(x))
pts <- c(pts, new_pts)

# Up to now, points have been added in somewhat random order.
# Recreate the points in order, counter-clockwise around the circle.
pts <- lapply(0:11 * pi/6, FUN = function(theta) polar_point(1, theta)) 

# 6 connect all the points, for a hexagon
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_line(pts[[2]], pts[[4]]) +
  draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[8]]) +
  draw_line(pts[[8]], pts[[10]]) +
  draw_line(pts[[10]], pts[[12]]) +
  draw_line(pts[[12]], pts[[2]]) +
  lapply(pts[seq(2, 12, 2)], FUN=function(x) draw_point(x))

# 7 circle of radius 0.6, at one of the points
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(.6, pts[[2]]) +
  draw_line(pts[[2]], pts[[4]]) +
  draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[8]]) +
  draw_line(pts[[8]], pts[[10]]) +
  draw_line(pts[[10]], pts[[12]]) +
  draw_line(pts[[12]], pts[[2]]) +
  lapply(pts[seq(2, 12, 2)], FUN=function(x) draw_point(x))

# 8 ... and another circle of radius 0.6, at an adjacent point
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(.6, pts[[2]]) + draw_circle(0.6, pts[[4]]) +
  draw_line(pts[[2]], pts[[4]]) +
  draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[8]]) +
  draw_line(pts[[8]], pts[[10]]) +
  draw_line(pts[[10]], pts[[12]]) +
  draw_line(pts[[12]], pts[[2]]) +
  lapply(pts[seq(2, 12, 2)], FUN=function(x) draw_point(x))

# 9 draw a line at the two 0.6 radius circles' intersections... and a point at the intersection of that line and the original circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(.6, pts[[2]]) + draw_circle(0.6, pts[[4]]) +
  draw_line(pts[[2]], pts[[4]]) +
  draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[8]]) +
  draw_line(pts[[8]], pts[[10]]) +
  draw_line(pts[[10]], pts[[12]]) +
  draw_line(pts[[12]], pts[[2]]) +
  draw_line(polar_point(0.25, pi/3), polar_point(1.75, pi/3)) +
  lapply(pts[seq(2, 12, 2)], FUN=function(x) draw_point(x)) +
  draw_new_point(pts[[3]])

# 10 new circle, radius 1, at the point of intersection created by the line in the previous step.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, pts[[3]]) +
  draw_line(pts[[2]], pts[[4]]) +
  draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[8]]) +
  draw_line(pts[[8]], pts[[10]]) +
  draw_line(pts[[10]], pts[[12]]) +
  draw_line(pts[[12]], pts[[2]]) +
  lapply(pts[seq(2, 12, 2)], FUN=function(x) draw_point(x)) +
  draw_point(pts[[3]]) +
  lapply(list(pts[[1]], pts[[5]]), FUN=function(x) draw_new_point(x))

# 11 two new circles, radius 1, centered at the two new points of intersection
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(1, pts[[1]]) +
  draw_circle(1, pts[[5]]) +
  draw_line(pts[[2]], pts[[4]]) +
  draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[8]]) +
  draw_line(pts[[8]], pts[[10]]) +
  draw_line(pts[[10]], pts[[12]]) +
  draw_line(pts[[12]], pts[[2]]) +
  lapply(pts[seq(2, 12, 2)], FUN=function(x) draw_point(x)) +
  draw_point(pts[[3]]) +
  draw_point(pts[[1]]) +
  draw_point(pts[[5]]) +
  lapply(list(pts[[11]], pts[[7]]), FUN=function(x) draw_new_point(x))

# 12 two more circles, centered at the points created in the previous step
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(1, pts[[11]]) +
  draw_circle(1, pts[[7]]) +
  draw_line(pts[[2]], pts[[4]]) +
  draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[8]]) +
  draw_line(pts[[8]], pts[[10]]) +
  draw_line(pts[[10]], pts[[12]]) +
  draw_line(pts[[12]], pts[[2]]) +
  lapply(pts[seq(2, 12, 2)], FUN=function(x) draw_point(x)) +
  draw_point(pts[[3]]) +
  draw_point(pts[[1]]) +
  draw_point(pts[[5]]) +
  draw_point(pts[[7]]) +
  draw_point(pts[[11]]) +
  draw_new_point(pts[[9]])

# 13 draw the dodecagon and the circle 
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(pts, FUN=function(x) draw_point(x))

# 14 lines from the origin to the outer circle points; intersections with a circle of radius 0.5 define a smaller dodecagon 
stage <- stage + 1
inner_pts <- lapply(0:11 * pi/6, FUN = function(theta) polar_point(0.5, theta))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(0:11, FUN=function(i) draw_line(origin, polar_point(1, i*pi/6))) +
  lapply(inner_pts, FUN=function(x) draw_new_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 15 two nested dodecagons
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# TODO start here
# 16 circle of radius 0.5 centered at one of the inner dodecagon points
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:1, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 17 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:2, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 18 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:3, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 19 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:4, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 20 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:5, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 21 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:6, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 22 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:7, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 23 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:8, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 24 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:9, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 25 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:10, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 26 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:11, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 27 and another circle
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

# 28 complete the inner circles
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(inner_pts, FUN=function(x) draw_point(x + c(0.5, 0))) +
  lapply(pts, FUN=function(x) draw_point(x))

# 29 points at each inner circle intersection
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(0:11,
    function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 30 drop the circles, draw a small dodecagon
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:0, 
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 31 and another small dodecagon
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:1,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 32 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:2,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 33 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:3,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 34 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:4,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 35 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:5,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 36 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:6,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 37 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:7,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 38 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:8,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 39 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:9,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 40 ... and another...
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:10,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 41 ... and the last.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 42 ... repeat last step
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 43 ... repeat last step.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 44 ... repeat last step.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 45 ... repeat last step.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 46 ... repeat last step.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 47 ... repeat last step.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

# 48 ... repeat last step.
stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11,
         function(i) 
           lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

