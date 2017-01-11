      program test_spl
      implicit none

!      CHARACTER (len=32):: FNAME
!      INTEGER IXY
!      DATASIZE = 500
!
!      CALL get_command_argument(1, FNAME)
!
!      OPEN(1,FILE=FNAME)
!      IXY=1
!      DO WHILE(IXY<=DATASIZE)
!        READ(1,*,ERR=100,END=100) X(IXY),Y(IXY)
!        IXY = IXY+1
!      END DO
!100   CONTINUE
!      N = IXY-1
!
!      DO I=1,IXY
!        PRINT*,I,X(I),Y(I)
!      END DO

      INTEGER NR
      INTEGER NI(12)
      DATA NR /4/
      DATA NI /1,5,10,15,20,0,0,0,0,0,0,0/
      CALL printni(NR,NI)
      CALL spl(2,NR,NI)
      CALL printni(NR,NI)
      CALL spl(5,NR,NI)
      CALL printni(NR,NI)
      CALL spl(2,NR,NI)
      CALL printni(NR,NI)

      END PROGRAM test_spl

      SUBROUTINE printni(Nr,Ni)
      INTEGER Nr, Ni(Nr+2)
      INTEGER I
      PRINT*,"All segments:"
      DO I=1,NR
        PRINT*, I, Ni(I),ni(I+1)
      END DO
      PRINT*,""
      END

      SUBROUTINE spl(i,Nr,Ni)
      INTEGER NR,NI(nr+2)

C     {spliting interval i into 2 pieces
c     inputs:
c       I- [INTEGER] interval number, should be less than NR+1
c       NR- [INTEGER] current maximum of interval number
c       NI- [INTEGER(NR)] current array with interval start point number
c
c     outputs:
c       NR- [INTEGER] new maximum of interval number
c       NI- [INTEGER(NR)] new array with interval start point number

      INTEGER j,jsplit
      INTEGER k1,k2

      k1=Ni(i)
      k2=Ni(i+1)
      jsplit=(k1+k2)/2
      if (jsplit.ge.k2-1) jsplit=k2-2
      if (jsplit.le.k1+1) jsplit=k1+2
C {splitting}

      Nr=Nr+1
      DO j=Nr+1,i+2,-1
          Ni(j)=Ni(j-1)
      END DO
      Ni(i+1)=jsplit

      RETURN
      END
