      program test_get_xx_norm
          implicit none
c     Inputs:
      integer N, N1, I
      real X0, DX
      INTEGER DATASIZE
      real X(500),Y(500)

c     Outputs:
      real anormx, anormy
      real XX(500),YY(500)


      CHARACTER (len=32):: FNAME
      INTEGER IXY
      DATASIZE = 500

      CALL get_command_argument(1, FNAME)

      OPEN(1,FILE=FNAME)
      IXY=1
      DO WHILE(IXY<=DATASIZE)
        READ(1,*,ERR=100,END=100) X(IXY),Y(IXY)
        IXY = IXY+1
      END DO
100   CONTINUE
      IXY = IXY-1

!      DO I=1,IXY
!        PRINT*,I,X(I),Y(I)
!      END DO

      N = IXY
      X0 = 0.
      DX = 1.0
      N1 = 6

      call GET_XX_norm(anormx,anormy, N,N1,X0,DX,X,Y,XX,YY)

      DO I=1,N1
        print *, anormx,anormy,XX(I),YY(I)
      END DO

      stop
      end program test_get_xx_norm

        SUBROUTINE GET_XX_norm(anormx,anormy,N,N1,X0,DX,X,Y,XX,YY)
        REAL X(N),Y(N),XX(N1),YY(N1)
c
c     Linear interpolation with normalization (non-dimensionalization of the data series
c     so that input values range from 0 to 1)
c
c     inputs:
c         N - [INTEGER] number of points in the input array
c         N1- [INTEGER] number of points in the output array
c         X0- [REAL] X-axis start point
c         DX- [REAL] X-axis data step
c         X-  [REAL(N)] input x array (should be in "chronological" order)
c         Y-  [REAL(N)] input y array (the start value is assumed to be different from the end value)
c
c     outputs:
c      ANORMX - [REAL] x normalization coefficient for conversion back to real data
c      ANORMY - [REAL] y normalization coefficient for conversion back to real data
c         XX-  [REAL(N1)] output x array (contains values from 0 to 1)
c         YY-  [REAL(N1)] output y array (contains values from 0 to 1 for monotonic input)
      INTEGER I,J
      X1=X0
      J=1

      DO I=1,N1

        XX(I)=X1

        DO WHILE (X1.Ge.X(J).AND.J.LT.N)
            J=J+1
        END DO
        IF (J.EQ.1) THEN

            YY(I)=Y(1)
          ELSE
          YY(I)=Y(J-1)+(X1-X(J-1))/(X(J)-X(J-1))*(Y(J)-Y(J-1))
        END IF
          X1=X1+DX
      END DO
        y0=yy(1)
        anormx=xx(n1)-x0
        anormy=yy(n1)-y0
      DO I=1,N1
        XX(I)=(XX(i)-x0)/anormx
        YY(I)=(yy(i)-y0)/anormy
      END DO

        RETURN
        END
