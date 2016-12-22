      program test_r2b
          implicit none
c     Inputs:
      integer N

      INTEGER DATASIZE
      real X(500),Y(500)

c     Outputs:
      REAL r2b, r2bout, aout,bout

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
      N = IXY-1

!      DO I=1,IXY
!        PRINT*,I,X(I),Y(I)
!      END DO

      r2bout = r2b(1,N,x,y, aout,bout)
      print *, "r2b:",r2bout, "  a:",aout, "  b:",bout

      STOP
      end program test_r2b

      REAL function r2b(k1,k2,x,y,a,b)
c      computing a norm value for the segment from point k1 to k2-1
c     inputs:
c         K1 -[INTEGER] start point
c         K2 -[INTEGER] end point+1
c         X -[REAL(?)] input x-axis array
c         Y -[REAL(?)] input y-axis array
c     outputs:
c         A -[REAL] coefficient of linear regression (will zero if K1=0) (y=Ax+B)
c         B -[REAL] coefficient of linear regression
c         R2B -[REAL] norm of the segment
c                     (maximum "distance" measured perpendicular to the regression line)


        REAL X(k2),Y(k2)
      INTEGER i,n
C      {y=a*x+b}
      REAL sx,sxx,sxy,sy
C     begin
      if (k2-k1.Le.2) then
          r2b=0
        else
        sx=0
        sxx=0
        sy=0
        sxy=0
        eps=0
        DO i=k1,k2-1
          sxx=sxx+x(i)**2
          sxy=sxy+x(i)*y(i)
          sy=sy+y(i)
          sx=sx+x(i)
        end DO
        n=k2-k1
          a=0.0
c
c!!!    here, a=0 for first segment because we assume it is a constant (e.g. uniform salinity)!!!
c
        if (k1.gt.1) a=(n*sxy-sy*sx)/(n*sxx-sx*sx)
        b=(sy-a*sx)/n
        sx=0
        DO i=k1,k2-1
          s=abs(y(i)-a*x(i)-b)/sqrt(a**2+1)
            if(s.gt.sx) sx=s
        END DO
          r2b=sx
          end IF
        RETURN
        END
