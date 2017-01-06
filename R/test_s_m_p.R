#####################
### Documentation ###
#####################

#SUBROUTINE s_m_p(n,eps,x,y,Nr,Ni)
#C     Main subroutine for determining LINEAR SEGMENTS for a SPECIFIED ERROR NORM VALUE
#c     (This subroutine determines the linear fit to the data for a specified error norm)
#C     Input:
#C          N   -[INTEGER] number of data points;
#C          EPS -[REAL]    error norm;
#C          X   -[REAL(N)] input x-axis data array, should be increasing function of index
#C          Y   -[REAL(N)] input y-axis data array
#C       Output:
#C          NR  -[INTEGER] final number of segments;
#C          NI  -[INTEGER] final array with segment start points
#C
#INTEGER NR, NI(n)
#REAL X(n),Y(n)
#INTEGER i,j,j1,k1,k2
#integer M
#REAL eps1,eps2,epsr,epsm,a,b
#LOGICAL change


#################
### Functions ###
#################
## Right now I am defining the functions by running each individual .R file so that the function is defined.
## Until they are stable it is best to keep them separate
## Not quite a function yet
## Lines 360-367



###############
### Testing ###
###############

## Data
df <- read.csv("../data/testdata1.txt", header=FALSE, col.names = c("x","y"))
##Slightly bigger dataset
#df <- read.csv("../data/testdata2.txt", header=FALSE, col.names = c("x","y"))

## Use same data as test_get_xx_norm.for
N <- length(df$x)
nr=2
eps=1

ni_l <- c()
m=round(N/nr)
for (i in 2:nr){
  u <- m*(i-1)+1
  ni <- c(ni_l, u)
}
ni <- c(1,ni, N+1)

m=0

## {step 1: if exceeds norma}
## lab1
i=1

#while( i <= nr ) {
  k1=ni[i]
  k2=ni[i+1]
  enorma=r2b(k1=k1,k2=k2,x=df$x, y=df$y)
  if( enorma$r2b>eps ) {
    spl(ni=ni, i=i, nr=nr)
  }   else {
    i=i+1
  }
#  }

#{step 2: try to merge}
## I'm stuck with this looping interval. 
## At nr=2 this results in an 2 then 1 to loop over
## Throws an error with r2b()
for(i in 2:(nr-1)){
  k1=ni[i-1]
  k2=ni[i+1]
  eps1=r2b(k1=k1,k2=k2,x=df$x, y=df$y)
  if (eps1$r2b<eps) {
    if (nr>2) {
      zerge(i=i, nr=nr, ni=ni)
    } else {
      ni[1]=1
      nr=2
    }
  }
}
##Stopped at line 410
  
##################################
### Corresponding FORTRAN CODE ###
##################################
#Nr=2
#m=NINT(FLOAT(N)/FLOAT(Nr))
#ni(1)=1
#DO i=2,Nr
#Ni(i)=m*(i-1)+1
#end DO
#Ni(Nr+1)=N+1 !{last interval}
#c      sss=orma2b(2,20,x,y,a,b)
#
#m=0
#100   change=.false.
#
#c       {step 1: if exceeds norma}
#
#C     lab1:
#  i=1
#101   CONTINUE
#DO while (i.le.Nr)
#  k1=Ni(i)
#k2=Ni(i+1)
#enorma=r2b(k1,k2,x,y,a,b)
#if (enorma.GT.eps) then
#CALL spl(i,Nr,Ni)
#change=.true.
#goto 104
#else
#  i=i+1
#end IF
#104   end DO
#
#C       {step 2: try to merge}
#
#102   CONTINUE
#DO i=2,Nr-1
#k1=Ni(i-1)
#k2=Ni(i+1)
#eps1=r2b(k1,k2,x,y,a,b)
#if (eps1.LE.eps) then
#if (Nr.GT.2) then
#CALL merge_(i,Nr,Ni)
#change=.true.
#goto 102
#else
#  Ni(1)=1
#Nr=2
#RETURN
#end IF
#end IF
#end DO
#c      to avoid couples
#do i=1,nr
#k1=ni(i)
#k2=ni(i+1)
#if (k2-k1.eq.1) then
#change=.true.
#ni(i+1)=ni(i+1)+1
#end if
#end do
#C         {"R" algorithm: adjusting the endpoint}
#DO i=2,Nr
#k1=Ni(i-1)
#k2=Ni(i+1)
#j1=Ni(i)
#eps1=r2b(k1,j1,x,y,a,b)
#eps2=r2b(j1,k2,x,y,a,b)
#if (eps1.GT.eps2) then
#epsm=eps1
#else
#  epsm=eps2
#END IF
#DO j=k1+2,k2-2
#eps1=r2b(k1,j,x,y,a,b)
#eps2=r2b(j,k2,x,y,a,b)
#
#if (eps1.GT.eps2) then
#epsr=eps1
#else
#  epsr=eps2
#END IF
#
#if (epsr.LT.epsm) then
#epsm=epsr
#j1=j
#end IF
#end DO
#if (j1.NE.Ni(i)) then
#Ni(i)=j1
#change=.true.
#end IF
#if (i.eq.2) epsm1=epsm
#end DO
#
#if (change) then
#change=.false.
#goto 102
#end if
#ni(nr+1)=N
#RETURN
#end