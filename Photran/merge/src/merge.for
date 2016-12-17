      program merge
          implicit none
        INTEGER NR
        INTEGER NI(20)
        INTEGER I

        DO I=1,20
          NI(I) = 42+I-1
        END DO

        PRINT *,NI

        I = 5
        NR = 10
        CALL merge_(I,NR,NI)
        PRINT *,NI

      end program merge

      SUBROUTINE merge_(i,Nr,Ni)
      INTEGER NI(nr+2)
C{merge interval i and i+1}
c     inputs:
c       I- [INTEGER] interval number of the first segment to merge
c       NR- [INTEGER] current maximum of the interval number
c       NI- [INTEGER(NR)] current array with interval start point numbers
c
c     outputs:
c       NR- [INTEGER] new maximum of interval number
c       NI- [INTEGER(NR)] new array with interval start point numbers
      Nr=Nr-1
      DO J=I,Nr+1
          Ni(j)=Ni(j+1)
      END DO

      RETURN
      end
