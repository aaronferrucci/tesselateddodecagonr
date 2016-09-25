library(ggplot2)

source("utils.R")

profile_limits <- function(stage) {
  full_zoom <- 2
  if (stage <= 12) {
    zoom <- full_zoom
  } else {
    end_stage <- 23
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

stage <- stage + 1
pts <- list(origin)
plots[[stage]] <- g + profile_limits(stage) + lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, pi / 2)))
plots[[stage]] <- g + profile_limits(stage) + draw_line(origin, c(0, 1)) + draw_circle(1, origin) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, pi/6)))
pts <- c(pts, list(polar_point(1, 5*pi/6)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, pts[[2]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, -pi/6)))
pts <- c(pts, list(polar_point(1, 7*pi/6)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, pts[[3]]) + draw_circle(1, pts[[4]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, -pi/2)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, pts[[5]]) + draw_circle(1, pts[[6]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(.6, pts[[3]]) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(.6, pts[[3]]) + draw_circle(0.6, pts[[2]]) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, pi/3)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_circle(.6, pts[[3]]) + draw_circle(0.6, pts[[2]]) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  draw_line(polar_point(0.25, pi/3), polar_point(1.75, pi/3)) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, 0)))
pts <- c(pts, list(polar_point(1, 2*pi/3)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, polar_point(1, pi/3)) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, -pi/3)))
pts <- c(pts, list(polar_point(1, pi)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, polar_point(1, 0)) + draw_circle(1, polar_point(1, 2*pi/3)) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
pts <- c(pts, list(polar_point(1, 4*pi/3)))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) + draw_circle(1, polar_point(1, -pi/3)) + draw_circle(1, polar_point(1, pi)) +
  draw_line(pts[[3]], pts[[2]]) + draw_line(pts[[2]], pts[[4]]) + draw_line(pts[[4]], pts[[6]]) +
  draw_line(pts[[6]], pts[[7]]) +
  draw_line(pts[[7]], pts[[5]]) +
  draw_line(pts[[5]], pts[[3]]) +
  lapply(pts, FUN=function(x) draw_point(x))

# 12 points around the circle
stage <- stage + 1
pts <- lapply(0:11 * pi/6, FUN = function(theta) polar_point(1, theta)) 
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
inner_pts <- lapply(0:11 * pi/6, FUN = function(theta) polar_point(0.5, theta))
plots[[stage]] <- g + profile_limits(stage) + draw_circle(1, origin) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(0:11, FUN=function(i) draw_line(origin, polar_point(1, i*pi/6))) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:1, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:2, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(1:12, FUN=function(i) draw_line(pts[[i]], pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_line(inner_pts[[i]], inner_pts[[(i %% 12) + 1]])) +
  lapply(1:12, FUN=function(i) draw_circle(0.5, inner_pts[[i]])) +
  lapply(inner_pts, FUN=function(x) draw_point(x)) +
  lapply(inner_pts, FUN=function(x) draw_point(x + c(0.5, 0))) +
  lapply(pts, FUN=function(x) draw_point(x))

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

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(1:12, function(i) {draw_line(rotate(inner_pts[[i]] + c(0.5, 0), 0), rotate(inner_pts[[(i %% 12) + 1]] + c(0.5, 0), 0)) } ) +
  lapply(pts, FUN=function(x) draw_point(x))

stage <- stage + 1
plots[[stage]] <- g + profile_limits(stage) +
  draw_point(origin) +
  lapply(0:11,
         function(i) lapply(inner_pts, FUN=function(x) draw_point(rotate(x + c(0.5, 0), i * pi/6)))
  ) +
  lapply(1:12, function(i) {draw_line(rotate(inner_pts[[i]] + c(0.5, 0), 0), rotate(inner_pts[[(i %% 12) + 1]] + c(0.5, 0), 0)) } ) +
  lapply(1:12, function(i) {draw_line(rotate(inner_pts[[i]] + c(0.5, 0), pi/6), rotate(inner_pts[[(i %% 12) + 1]] + c(0.5, 0), pi/6)) } ) +
  lapply(pts, FUN=function(x) draw_point(x))

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
