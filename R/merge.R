#####################
### Documentation ###
#####################

##      SUBROUTINE merge_(i,Nr,Ni)
#      INTEGER NI(nr+2)
#C{merge interval i and i+1}
#c     inputs:
#c       I- [INTEGER] interval number of the first segment to merge
#c       NR- [INTEGER] current maximum of the interval number
#c       NI- [INTEGER(NR)] current array with interval start point numbers
#c
#c     outputs:
#c       NR- [INTEGER] new maximum of interval number
#c       NI- [INTEGER(NR)] new array with interval start point numbers

#################
### Functions ###
#################

# See merge_.for - this now gets the same answers, though is a function, NB.

zerge = function(i,nr,ni) {
  nn = ni
  nn[i:nr] = ni[(i+1):(nr+1)]
  nn
}

###############
### Testing ###
###############

ni = 42 + 0:(20-1)
i = 5
nr = 10

ni
zerge(i,nr,ni)


##################################
### Corresponding FORTRAN CODE ###
##################################
#      Nr=Nr-1
#      DO J=I,Nr+1
#          Ni(j)=Ni(j+1)
#      END DO
#
#      RETURN
#      end
#
