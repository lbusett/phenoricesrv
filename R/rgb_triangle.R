# Author: Philip Kinlen, Aug 2014
library(grid) # required for grid.raster(.)

# numPix should be an integer,
# and gapX < (numPix / 2)
# where gapX is the minimum number of horizontal pixels from the triangle to the edge of the image
RGBTriangle <- function(numPix = 1000, gapX = 10, doSmoothing=FALSE) {
  # the verticle gap between the triangle and edge of image
  gapY            <- round(numPix * ( 0.5 - sqrt(3)/4) + gapX * sqrt(3)/2)

  xArr            <- 1:numPix
  yArr            <- numPix:1   # the function call grid.raster(..) below will put the element with
                                # highest row number at the bottom.
                                # For our array, the last shall be at the bottom.
  # The x's a constant in the matrix columns, y's are constant along the matrix rows.
  xMat            <- matrix(rep(xArr, numPix), numPix, numPix, F)
  yMat            <- matrix(rep(yArr, numPix), numPix, numPix, T)

  m1              <- sqrt(3)                        # slope
  c1              <- gapY - m1 * gapX               # intercept

  m2              <- -sqrt(3)                       # slope
  c2              <- gapY - m2 * (numPix - gapX)    # intercept

  height          <- numPix - 2 * gapY              # height of triangle in pixels

  red             <- matrix(mapply(getRed,   xMat, yMat, m1, c1, m2, c2, gapY, height), numPix, numPix, T)
  green           <- matrix(mapply(getGreen, xMat, yMat, m1, c1, m2, c2, gapY, height), numPix, numPix, T)
  blue            <- matrix(mapply(getBlue,  xMat, yMat, m1, c1, m2, c2, gapY, height), numPix, numPix, T)

  col             <- rgb(red, green, blue)
  dim(col)        <- dim(red)
  grid.raster(col, interpolate=doSmoothing)
  # One possible source of minor confusion is that when we write a cartesian co-ordinate (x,y)
  # the x comes first.
  # but in a matrix, we specify the row first, which corresponds to y
}

#################################################################
getRed <- function( x, y, m1, c1, m2, c2, gapY, height){
   if ( isInsideTriangle( x, y, m1, c1, m2, c2, gapY)){
     res <- (y - gapY) / height    # A rounding error may cause res to be outside the range [0,1]
     return (max(0, min(1, res)))  # If left untreated that could cause an error in grid.raster(.)
   } else
     return (1)
}
#################################################################
getBlue <- function( x, y, m1, c1, m2, c2, gapY, height){
  if ( isInsideTriangle( x, y, m1, c1, m2, c2, gapY)){
    res <- sqrt(3) * ((y - c2) / m2 - x) / ( 2 * height)
    return (max(0, min(1, res)))
  } else
    return (1)
}
#################################################################
getGreen <- function( x, y, m1, c1, m2, c2, gapY, height){
  if ( isInsideTriangle( x, y, m1, c1, m2, c2, gapY)){
    res <- 0.5 * (m1 * x + c1 - y) / height
    return (max(0, min(1, res)))
  } else
    return (1)
}
#################################################################
isInsideTriangle <- function(x,y, m1, c1, m2 ,c2, gapY){

  if (  ( y >  gapY)
      & ( y < ( m1 * x + c1 ) )
      & ( y < ( m2 * x + c2 ) ) )
    return(T)
  else
    return (F)
}
