#' Add football pitch to ggplot
#'
#' Adds simple pitch to ggplot, requires ggplot and ggforce to work.
#' Dimensions are -5250 to 5250 in x, -3400 to 3400 in y, fits CH defintion of pitch.
#'
#'
#' @examples
#' ggplot()+addPitchCH()
#' @export

addPitchCH <- function() {
  list(  #Touch and goal lines
         geom_segment(aes(x=-5250,xend=5250,y=-3400,yend=-3400)),
         geom_segment(aes(x=-5250,xend=-5250,y=-3400,yend=3400)),
         geom_segment(aes(x=-5250,xend=5250,y=3400,yend=3400)),
         geom_segment(aes(x=5250,xend=5250,y=-3400,yend=3400)),
         #Halfway line and kickoff spot
         geom_segment(aes(x=0,xend=0,y=-3400,yend=3400)),
         geom_point(aes(0,0)),
         #Boxes
         geom_segment(aes(x=-5250,xend=-5250+1650,y=0-(4032/2),yend=0-(4032/2))),
         geom_segment(aes(x=-5250,xend=-5250+1650,y=0+(4032/2),yend=0+(4032/2))),
         geom_segment(aes(x=-5250+1650,xend=-5250+1650,y=0-(4032/2),yend=0+(4032/2))),
         geom_segment(aes(x=5250,xend=5250-1650,y=0-(4032/2),yend=0-(4032/2))),
         geom_segment(aes(x=5250,xend=5250-1650,y=0+(4032/2),yend=0+(4032/2))),
         geom_segment(aes(x=5250-1650,xend=5250-1650,y=0-(4032/2),yend=0+(4032/2))),
         #Small boxes
         geom_segment(aes(x=-5250,xend=-5250+550,y=0-(1832/2),yend=0-(1832/2))),
         geom_segment(aes(x=-5250,xend=-5250+550,y=0+(1832/2),yend=0+(1832/2))),
         geom_segment(aes(x=-5250+550,xend=-5250+550,y=0-(1832/2),yend=0+(1832/2))),
         geom_segment(aes(x=5250,xend=5250-550,y=0-(1832/2),yend=0-(1832/2))),
         geom_segment(aes(x=5250,xend=5250-550,y=0+(1832/2),yend=0+(1832/2))),
         geom_segment(aes(x=5250-550,xend=5250-550,y=0-(1832/2),yend=0+(1832/2))),
         #Goals
         geom_segment(aes(x=-5250-(244),xend=-5250-(244),y=0-(732/2)), yend=0+(732/2)),
         geom_segment(aes(x=-5250,xend=-5250-(244),y=0+(732/2)), yend=0+(732/2)),
         geom_segment(aes(x=-5250,xend=-5250-(244),y=0-(732/2)), yend=0-(732/2)),
         geom_segment(aes(x=5250+(244),xend=5250+(244),y=0-(732/2)), yend=0+(732/2)),
         geom_segment(aes(x=5250,xend=5250+(244),y=0+(732/2)), yend=0+(732/2)),
         geom_segment(aes(x=5250,xend=5250+(244),y=0-(732/2)), yend=0-(732/2)),
         #Penalty spots
         geom_point(aes(-5250+1100,0), col="grey20"),
         geom_point(aes(5250-1100,0), col="grey20"),
         #Circles
         ggforce::geom_circle(aes(x0=0,y0=0, r=915)),
         annotate("path",
                  x = (-5250 + 1100) + 915 * cos(seq(-0.295*pi, 0.295*pi, length.out = 300)),
                  y = 0+1100 * sin(seq(-0.295*pi, 0.295*pi, length.out = 300))) ,
         annotate("path",
                  x = (5250-1100) - 915 * cos(seq(-0.295*pi, 0.295*pi, length.out = 300)),
                  y = 0-1100 * sin(seq(-0.295*pi, 0.295*pi, length.out = 300))) ,
         #Void the theme & coord_fixed
         xlim(-5650,5650),
         ylim(-3500,3500),
         coord_fixed(),
         theme_void())
}


