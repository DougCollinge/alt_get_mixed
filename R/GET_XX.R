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

approx(x, y,  xout=c(0,1,2,3,4,5), method= "linear", rule=2)


#    inputs:
#         N - [INTEGER] number of points in the input array
#         N1- [INTEGER] number of points in the output array
#         X0- [REAL] X-axis start point
#         DX- [REAL] X-axis data step
#         X-  [REAL(N)] input x array (should be in "chronological" order)
#         Y-  [REAL(N)] input y array (the start value is assumed to be different from the end value)

#     outputs:
#      ANORMX - [REAL] x normalization coefficient for conversion back to real data
#      ANORMY - [REAL] y normalization coefficient for conversion back to real data
#         XX-  [REAL(N1)] output x array (contains values from 0 to 1)
#         YY-  [REAL(N1)] output y array (contains values from 0 to 1 for monotonic input)

