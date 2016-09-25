library(ggplot2)

source("utils.R")

g <- ggplot() + coord_fixed(1) + expand_limits(x = c(-2, 2), y = c(-2, 2)) +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()  
  )

origin <- c(0, 0)
# 1
pts <- list(origin)
g1 <- g + lapply(pts, FUN=function(x) draw_point(x))

# 2
pts <- c(pts, list(polar_point(1, pi / 2)))
g2 <- g + draw_line(origin, c(0, 1)) + draw_circle(1, origin) +
  lapply(pts, FUN=function(x) draw_point(x))

pts <- c(pts, list(polar_point(1, pi/6)))
pts <- c(pts, list(polar_point(1, 5*pi/6)))

g3 <- g + draw_circle(1, origin) + draw_circle(1, pts[[2]]) +
  lapply(pts, FUN=function(x) draw_point(x))

pts <- c(pts, list(polar_point(1, -pi/6)))
pts <- c(pts, list(polar_point(1, 7*pi/6)))
g4 <- g + draw_circle(1, origin) + draw_circle(1, pts[[3]]) + draw_circle(1, pts[[4]]) +
  lapply(pts, FUN=function(x) draw_point(x))

pts <- c(pts, list(polar_point(1, -pi/2)))
g5 <- g + draw_circle(1, origin) + draw_circle(1, pts[[5]]) + draw_circle(1, pts[[6]]) +
  lapply(pts, FUN=function(x) draw_point(x))

g6 <- g + draw_circle(1, origin) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

g7 <- g + draw_circle(1, origin) +
  draw_circle(.6, pts[[3]]) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

g8 <- g + draw_circle(1, origin) +
  draw_circle(.6, pts[[3]]) + draw_circle(0.6, pts[[2]]) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

pts <- c(pts, list(polar_point(1, pi/3)))
g9 <- g + draw_circle(1, origin) +
  draw_circle(.6, pts[[3]]) + draw_circle(0.6, pts[[2]]) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  draw_line(polar_point(0.25, pi/3), polar_point(1.75, pi/3)) +
  lapply(pts, FUN=function(x) draw_point(x))

pts <- c(pts, list(polar_point(1, 0)))
pts <- c(pts, list(polar_point(1, 2*pi/3)))
g10 <- g + draw_circle(1, origin) + draw_circle(1, polar_point(1, pi/3)) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

pts <- c(pts, list(polar_point(1, -pi/3)))
pts <- c(pts, list(polar_point(1, pi)))
g11 <- g + draw_circle(1, origin) + draw_circle(1, polar_point(1, 0)) + draw_circle(1, polar_point(1, 2*pi/3)) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

pts <- c(pts, list(polar_point(1, 4*pi/3)))
g12 <- g + draw_circle(1, origin) + draw_circle(1, polar_point(1, -pi/3)) + draw_circle(1, polar_point(1, pi)) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

# 12 points around the circle
pts <- lapply(0:11 * pi/6, FUN = function(theta) polar_point(1, theta)) 
g13 <- g + draw_circle(1, origin) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(pts, FUN=function(x) draw_point(x))

inner_pts <- lapply(0:11 * pi/6, FUN = function(theta) polar_point(0.5, theta))
g14 <- g + draw_circle(1, origin) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(0:11, FUN=function(i) draw_line(origin, polar_point(1, i*pi/6))) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

g15 <- g +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

g16 <- g +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:1, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

g17 <- g +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:2, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

g18 <- g +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

g19 <- g +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(inner_pts, FUN=function(x) draw_point(x + c(0.5, 0))) +
  lapply(pts, FUN=function(x) draw_point(x))

g20 <- g +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(0:11,
    function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

g21 <- g +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(1:12, function(i) {draw_line(rotate(inner_pts[[i]] + c(0.5, 0), 0), rotate(inner_pts[[(i %% 12) + 1]] + c(0.5, 0), 0)) } ) +
  lapply(pts, FUN=function(x) draw_point(x))

g22 <- g +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(1:12, function(i) {draw_line(rotate(inner_pts[[i]] + c(0.5, 0), 0), rotate(inner_pts[[(i %% 12) + 1]] + c(0.5, 0), 0)) } ) +
  lapply(1:12, function(i) {draw_line(rotate(inner_pts[[i]] + c(0.5, 0), pi/6), rotate(inner_pts[[(i %% 12) + 1]] + c(0.5, 0), pi/6)) } ) +
  lapply(pts, FUN=function(x) draw_point(x))

g23 <- g +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(0:11, 
    function(i) 
      lapply(1:12, function(j) {draw_line(rotate(inner_pts[[j]] + c(0.5, 0), i * pi/6), rotate(inner_pts[[(j %% 12) + 1]] + c(0.5, 0), i*pi/6)) } )
  ) +
  lapply(pts, FUN=function(x) draw_point(x))

