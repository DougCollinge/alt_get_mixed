      program test_get_xx
          implicit none

c     Inputs:
      integer N, N1, I
      real X0, DX
      real X(4),Y(4)

c     Outputs:

      real XX(10),YY(10)

      DATA X /1.0, 2.0, 3.0, 3.5/
      DATA Y /10.0,20.0,10.0,5.0/
      X0 = 0.
      DX = 1.0
      N = 4
      N1 = 6

      call GET_XX(N,N1,X0,DX,X,Y,XX,YY)

      DO I=1,N1
        print *, XX(I),YY(I)
      END DO

      stop
      end program test_get_xx


      SUBROUTINE GET_XX(N,N1,X0,DX,X,Y,XX,YY)
      REAL X(N),Y(N),XX(N1),YY(N1)
c
c     Linear interpolation of the Input Data Series
c
c     Input:
c         N - [INTEGER] number of points in the input array
c         N1- [INTEGER] number of points in the output array
c         X0- [REAL] start point along the x-axis
c         DX- [REAL] data step along the x-axis
c         X-  [REAL(N)] input x-axis array (should be in "chronological" order)
c         Y-  [REAL(N)] input y-axis array
c
c     outputs:
c         XX-  [REAL(N1)] output x-axis array (contains X0+dx(I-1))
c         YY-  [REAL(N1)] output y-axis array

      X1=X0
      J=1
      DO I=1,N1

        XX(I)=X1
        DO WHILE (X1.GT.X(J).AND.J.LT.N1)
            J=J+1
        END DO
        IF (J.EQ.1) THEN
            YY(I)=Y(1)
          ELSE
          YY(I)=Y(J-1)+(X1-X(J-1))/(X(J)-X(J-1))*(Y(J)-Y(J-1))
        END IF
          X1=X1+DX
      END DO
        RETURN
      END

