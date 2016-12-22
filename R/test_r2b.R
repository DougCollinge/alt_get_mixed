#####################
### Documentation ###
#####################

#REAL function r2b(k1,k2,x,y,a,b)
#c      computing a norm value for the segment from point k1 to k2-1
#c     inputs:
#c         K1 -[INTEGER] start point
#c         K2 -[INTEGER] end point+1
#c         X -[REAL(?)] input x-axis array
#c         Y -[REAL(?)] input y-axis array
#c     outputs:
#c         A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B)
#c         B -[REAL] coefficient of linear regression
#c         R2B -[REAL] norm of the segment
#c                     (maximum "distance" measured perpendicular to the regression line)


#################
### Functions ###
#################
if(k2-k1<=2) {
  r2b=0
} else {
  sx=0
  sxx=0
  sy=0
  sxy=0
  eps=0
  for (i in k1:k2-1){
    sxx=sxx+df$x[i]^2
    sxy=sxy+df$x[i]*df$y[i]
    sy=sy+y[i]
    sx=sx+x[i]
  }
}
## Confused what happens from line 321 to 337 in exam_new.for


###############
### Testing ###
###############
df <- read.csv("../data/testdata1.txt", header=FALSE, col.names = c("x","y"))
## Using the same data as merge.R
ni = 42 + 0:(20-1)
i = 5

## Reflecting my naivety with FORTRAN, I'm not sure where nr comes in here.
nr = 10

k1=0
k2=10
