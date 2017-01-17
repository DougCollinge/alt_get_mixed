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

      CALL test1
      CALL test2
      CALL test3
      CALL test4
      CALL test5
      CALL test6
      CALL test7
      CALL test8

      END PROGRAM test_spl

!test_that("spl divides the first and last intervals.", {
!})
!
!test_that("spl divides even,even intervals.", {
!})
!
!test_that("spl divides even,odd intervals.", {

!})
!
!test_that("spl divides odd,even intervals.", {

!})
!
!test_that("spl divides odd,odd intervals.", {

!})
      SUBROUTINE test1
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/1/
      DATA I/2/
      DATA NR/3/,NI/1,5,10,20,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE test2
!  expect_equal(spl(c(1,2,6,10),2),c(1,2,4,6,10))
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/2/
      DATA I/2/
      DATA NR/3/,NI/1,2,6,10,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE test3
!  expect_equal(spl(c(1,5,10),1),c(1,3,5,10))
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/3/
      DATA I/1/
      DATA NR/2/,NI/1,5,10,0,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE test4
!  expect_equal(spl(c(1,5,10),2),c(1,5,7,10))
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/4/
      DATA I/2/
      DATA NR/2/,NI/1,5,10,0,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE test5
!  expect_equal(spl(c(1,2,6,10),2),c(1,2,4,6,10))
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/5/
      DATA I/2/
      DATA NR/3/,NI/1,2,6,10,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE test6
!  expect_equal(spl(c(1,2,5,10),2),c(1,2,4,5,10))
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/6/
      DATA I/2/
      DATA NR/3/,NI/1,2,5,10,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE test7
!  expect_equal(spl(c(1,3,6,10),2),c(1,3,5,6,10))
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/7/
      DATA I/2/
      DATA NR/3/,NI/1,3,6,10,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE test8
!  expect_equal(spl(c(1,3,7,10),2),c(1,3,5,7,10))
      INTEGER TEST,I,NR,NI(10)
      DATA TEST/8/
      DATA I/2/
      DATA NR/3/,NI/1,3,7,10,0,0,0,0,0,0/

      CALL printin(TEST,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)
      END SUBROUTINE

      SUBROUTINE testlong
      INTEGER NR
      INTEGER NI(16)
      DATA NR /4/
      DATA NI /1,4,10,15,20,0,0,0,0,0,0,0,0,0,0,0/
      INTEGER I

      I=1
      CALL printin(1,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)

      I=5
      CALL printin(2,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)

      I=2
      CALL printin(3,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)

      I=1
      CALL printin(4,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)

      I=NR
      CALL printin(5,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)

      I=NR
      CALL printin(6,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)

      I=NR
      CALL printin(7,I,NR,NI)
      CALL spl(I,NR,NI)
      CALL printout(NR,NI)

      END SUBROUTINE

      SUBROUTINE printin(IT,I,Nr,Ni)
      INTEGER Nr, Ni(Nr+2)
      INTEGER I,IT,J
!      PRINT*,"All segments:"
!      DO I=1,NR
!        PRINT*, I, Ni(I),ni(I+1)
!      END DO
      PRINT*,"Test:",IT," I:",I
      PRINT*,(NI(J),J=1,NR+1)
      PRINT*,""
      END SUBROUTINE

      SUBROUTINE printout(NR,NI)
      INTEGER NR, NI(NR+1)
      PRINT*,(NI(J),J=1,NR+1)
      PRINT*,"========================================================"
      END SUBROUTINE

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
