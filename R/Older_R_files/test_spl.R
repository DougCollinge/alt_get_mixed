#####################
### Documentation ###
#####################

#SUBROUTINE spl(i,Nr,Ni)
#INTEGER NR,NI(nr+2)
#
#C     {spliting interval i into 2 pieces
#  c     inputs:
#  c       I- [INTEGER] interval number, should be less than NR+1
#  c       NR- [INTEGER] current maximum of interval number
#  c       NI- [INTEGER(NR)] current array with interval start point number
#  c
#  c     outputs:
#  c       NR- [INTEGER] new maximum of interval number
#  c       NI- [INTEGER(NR)] new array with interval start point number


#################
### Functions ###
#################

#spl <- function(ni, i, nr) {
#  if( i >=nr+1 ) {"i needs to be less that nr+1"}
#  else {
#  k1=ni[i]
#  k2=ni[i+1]
#  
#  ## Feels a little silly to defining this explicitly
#  #jsplit_cond=floor((k1+k2)/2)
#  
#  ## Using double condition in an attempt to be defensive
#  jsplit <- ifelse(jsplit_cond >= k2-1, k2-2,
#                   ifelse(jsplit_cond <= k1+1, k1+2, 
#                          "Condition Not Satisfied")
#  )
#  
##  ## I think the following achieves the same result:
##  jsplit <- max(min(floor((k1 + k2) / 2), k2 - 2), k1 + 2) 
#  
#  nn = ni
#  nn[i:nr] = ni[(nr+1):(i+1)]
#  nn[i+1] =jsplit
#  nn
#}
#}

spl <- function(ni,i) {
  stopifnot(i<length(ni))
  k1 = ni[i]
  k2 = ni[i+1]
  jsplit = floor((k1+k2)/2) # Is an index, must be an integer.
  stopifnot(k1<jsplit & jsplit<k2) 
  c( ni[1:i], jsplit, ni[(i+1):length(ni)] )
}

###############
### Testing ###
###############

## Interval too small 
ni =  c(10, 20) 
i = 2

spl(ni, i)  

## i too small
ni =  c(10, 20, 30) 
i = 3

spl(ni, i) 
  

ni = c(1,5,10,15,20)
nr = 4
ni
ni = spl(ni,i=2)
ni
ni = spl(ni,i=5)
ni
ni = spl(ni,i=2)
ni
ni = spl(ni,i=1)
ni
ni = spl(ni,i=length(ni)-1)
ni
ni = spl(ni,i=length(ni)-1)
ni
ni = spl(ni,i=length(ni)-1)
ni

##################################
### Corresponding FORTRAN CODE ###
##################################

#INTEGER j,jsplit
#INTEGER k1,k2
#
#
#k1=Ni(i)
#k2=Ni(i+1)
#jsplit=(k1+k2)/2
#if (jsplit.ge.k2-1) jsplit=k2-2
#if (jsplit.le.k1+1) jsplit=k1+2
#C {splitting}
#
#Nr=Nr+1
#DO j=Nr+1,i+2,-1
#Ni(j)=Ni(j-1)
#END DO
#Ni(i+1)=jsplit
#RETURN
#END
