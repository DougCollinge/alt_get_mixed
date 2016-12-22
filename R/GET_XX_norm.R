#####################
### Documentation ###
#####################

#getxxnorm(x,y, x0)
# Interpolate and normalize a vector of samples. 
# X and Y will be linearly interpolated to match the length of X0
# and will be normalized so that the first point is (0,0) and the 
# last point is (1,1).
# Input:
#   Vector x are the input x values, must be in ascending order.
#   Vector y are the input y values.
#   Vector x0 are the values of x at which the new interpolated samples
#   should be evaluated.
# Output is a data.frame with:
#   anormx, the normalization value used to make the last x == 1.
#   anormy, the normalization value used to make the last y == 1.
#   xx,yy  vectors of x,y values interpolated to be the same length as x0.
#


#################
### Functions ###
#################


getxxnorm <- function(x,y, x0) {
  y0 <- rep(0,length(x0))  # Reserve space for y values.
  j <- 1
  for (i in 1:length(x0) ) {
    while( (x0[i] >= x[j]) && (j < length(x)) ) {
      j <- j+1
    }
    if( j == 1 ) {
      y0[i] <- y[1]
    } 
    else {
      y0[i] <- y[j-1]+(x0[i]-x[j-1])/(x[j]-x[j-1])*(y[j]-y[j-1])
    }
  }
  anormx = x0[length(x0)] - x0[1]
  anormy = y0[length(y0)] - y0[1]
  x0 = (x0 - x0[1])/anormx
  y0 = (y0 - y0[1])/anormy
  
  data.frame(anormx,anormy,x0,y0)
}

###############
### Testing ###
###############

## Use same data as test_get_xx_norm.for
#x <- c(1,2,3,3.5)
#y <- c(10.0,20.0,10.0,5.0)

df <- read.csv("../data/testdata1.txt", header=FALSE, col.names = c("x","y"))


x0 <- c(0,1,2,3,4,5)

getxxnorm(df$x,df$y, x0)


##################################
### Corresponding FORTRAN CODE ###
##################################

#        SUBROUTINE GET_XX_norm(anormx,anormy,N,N1,X0,DX,X,Y,XX,YY)
#        REAL X(N),Y(N),XX(N1),YY(N1)
#c
#c     Linear interpolation with normalization (non-dimensionalization of the data series
#c     so that input values range from 0 to 1)
#c
#c     inputs:
#c         N - [INTEGER] number of points in the input array
#c         N1- [INTEGER] number of points in the output array
#c         X0- [REAL] X-axis start point
#c         DX- [REAL] X-axis data step
#c         X-  [REAL(N)] input x array (should be in "chronological" order)
#c         Y-  [REAL(N)] input y array (the start value is assumed to be different from the end value)
#c
#c     outputs:
#c      ANORMX - [REAL] x normalization coefficient for conversion back to real data
#c      ANORMY - [REAL] y normalization coefficient for conversion back to real data
#c         XX-  [REAL(N1)] output x array (contains values from 0 to 1)
#c         YY-  [REAL(N1)] output y array (contains values from 0 to 1 for monotonic input)
#      INTEGER I,J
#      X1=X0
#      J=1
#
#      DO I=1,N1
#
#        XX(I)=X1
#
#        DO WHILE (X1.Ge.X(J).AND.J.LT.N)
#            J=J+1
#        END DO
#        IF (J.EQ.1) THEN
#
#            YY(I)=Y(1)
#          ELSE
#          YY(I)=Y(J-1)+(X1-X(J-1))/(X(J)-X(J-1))*(Y(J)-Y(J-1))
#        END IF
#          X1=X1+DX
#      END DO
#        y0=yy(1)
#        anormx=xx(n1)-x0
#        anormy=yy(n1)-y0
#      DO I=1,N1
#        XX(I)=(XX(i)-x0)/anormx
#        YY(I)=(yy(i)-y0)/anormy
#      END DO
#
#        RETURN
#        END
#
