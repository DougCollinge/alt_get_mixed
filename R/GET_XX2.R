#################
### Functions ###
#################

#Normalized Data
normalized <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

########################
## Load in libraries ###
########################
library(tidyverse)

## Two options for data structure
## Use same data as test_get_xx_norm.for
x <- c(1,2,3,3.5)
y <- c(10.0,20.0,10.0,5.0)

#approx(x, y,  xout=c(0,1,2,3,4,5), method= "linear", rule=2)

getxx <- function(x,y, x0) {

  yy <- rep(0,length(x0))
  j <- 1
  for (i in 1:length(x0) ) {
    while( x0[i] > x[j] & j < length(x) ) {
       j <- j+1
    }
    if( j == 1 ) {
      yy[i] <- y[1]
    } 
    else {
      yy[i] <- y[j-1]+(x0[i]-x[j-1])/(x[j]-x[j-1])*(y[j]-y[j-1])
    }
  }
  yy
}

x0 <- c(0,1,2,3,4,5)

yy <- getxx(x,y, x0)

cbind(x0,yy)


#c     Linear interpolation of the Input Data Series
#c
#c     Input:
#c         N - [INTEGER] number of points in the input array
#c         N1- [INTEGER] number of points in the output array
#c         X0- [REAL] start point along the x-axis
#c         DX- [REAL] data step along the x-axis
#c         X-  [REAL(N)] input x-axis array (should be in "chronological" order)
#c         Y-  [REAL(N)] input y-axis array
#c
#c     outputs:
#c         XX-  [REAL(N1)] output x-axis array (contains X0+dx(I-1))
#c         YY-  [REAL(N1)] output y-axis array
#
#      X1=X0
#      J=1
#      DO I=1,N1
#
#        XX(I)=X1
#        DO WHILE (X1.GT.X(J).AND.J.LT.N1)
#            J=J+1
#        END DO
#        IF (J.EQ.1) THEN
#            YY(I)=Y(1)
#          ELSE
#          YY(I)=Y(J-1)+(X1-X(J-1))/(X(J)-X(J-1))*(Y(J)-Y(J-1))
#        END IF
#          X1=X1+DX
#      END DO
#        RETURN
#      END
#
