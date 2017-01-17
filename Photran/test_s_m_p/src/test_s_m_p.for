      program test_s_m_p
          implicit none

      CALL TEST1
      CALL TEST2
      CALL TEST3
      end program test_s_m_p

      SUBROUTINE TEST1
      INTEGER N1
      REAL X1(3), Y1(3)
      INTEGER NR, NI(100)
      REAL eps

      DATA X1/0.0, 5.0, 10.0/
      DATA Y1/1.0, 10.0, 10.0/
      DATA N1/3/

      eps = .01
      CALL s_m_p(N1,eps,X1,Y1,NR,NI)
      CALL bigdump(1,N1,eps,X1,Y1,NR,NI)
      END SUBROUTINE

      SUBROUTINE TEST2
      INTEGER N
      REAL X(10), Y(10)
      INTEGER NR, NI(100)
      REAL eps

      DATA N/10/
      DATA X/0.0, 5.0, 10.0, 15.0, 20.0, 25.0,30.0,35.0,40.0,45.0/
      DATA Y/1.0, 2.0, 3.0,   4.0,  5.0,  5.0, 4.0, 4.0, 3.0, 2.0/

      eps = .01
      CALL s_m_p(N,eps,X,Y,NR,NI)
      CALL bigdump(2,N,eps,X,Y,NR,NI)
      END SUBROUTINE

      SUBROUTINE TEST3
      INTEGER N
      REAL X(100), Y(100)
      INTEGER NR, NI(100)
      REAL eps

      DATA N/100/

      DO I=1,N
        ANGLE = 2*3.141592653589793*(I-1)/N
        X(I) = ANGLE
        Y(I) = SIN(ANGLE)
      END DO

      eps = .01
      CALL s_m_p(N,eps,X,Y,NR,NI)
      CALL bigdump(3,N,eps,X,Y,NR,NI)
      END SUBROUTINE

      SUBROUTINE bigdump(seq,N,eps,X,Y,NR,NI)
        INTEGER seq,N,NR,NI(NR)
        REAL EPS,X(N),Y(N)
        PRINT*,"Test of s_m_p sequence:",seq
        PRINT*,"EPS:",eps
        PRINT*,"X:",(X(I),I=1,N)
        PRINT*,"Y:",(Y(I),I=1,N)
        PRINT*,"NI:",(NI(I),I=1,NR+1)
        PRINT*,""
        RETURN
      END SUBROUTINE

C The following code taken verbatim from exam_new.for
C Do not modify if you want an accurate test.


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
      PRINT*,"SPL() k1,k2,jsplit:",k1,k2,jsplit
        RETURN
        END


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



            SUBROUTINE s_m_p(n,eps,x,y,Nr,Ni)
C     Main subroutine for determining LINEAR SEGMENTS for a SPECIFIED ERROR NORM VALUE
c     (This subroutine determines the linear fit to the data for a specified error norm)
C     Input:
C          N   -[INTEGER] number of data points;
C          EPS -[REAL]    error norm;
C          X   -[REAL(N)] input x-axis data array, should be increasing function of index
C          Y   -[REAL(N)] input y-axis data array
C       Output:
C          NR  -[INTEGER] final number of segments;
C          NI  -[INTEGER] final array with segment start points
C
        INTEGER NR, NI(n)
        REAL X(n),Y(n)
        INTEGER i,j,j1,k1,k2
      integer M
      REAL eps1,eps2,epsr,epsm,a,b
      LOGICAL change
C      PRINT*,"X:",(X(III),III=1,n)
C      PRINT*,"Y:",(Y(III),III=1,n)

C       label lab0,lab1,lab2,lab3;

        Nr=2
      m=NINT(FLOAT(N)/FLOAT(Nr))
        ni(1)=1
      DO i=2,Nr
        Ni(i)=m*(i-1)+1
      end DO
      Ni(Nr+1)=N+1 !{last interval}
c      sss=orma2b(2,20,x,y,a,b)

        m=0
100   change=.false.

c       {step 1: if exceeds norma}

      PRINT*,"start. NI:",(NI(III),III=1,NR+1)
C     lab1:
      i=1
101   CONTINUE
      DO while (i.le.Nr)
        k1=Ni(i)
        k2=Ni(i+1)
        enorma=r2b(k1,k2,x,y,a,b)
        if (enorma.GT.eps) then
          CALL spl(i,Nr,Ni)
          change=.true.
      PRINT*,"Split at:",I," NI:",(NI(III),III=1,NR+1)
          goto 104
        else
            i=i+1
          end IF
104   end DO

C       {step 2: try to merge}

102   CONTINUE
      DO i=2,Nr-1
        k1=Ni(i-1)
        k2=Ni(i+1)
          eps1=r2b(k1,k2,x,y,a,b)
        if (eps1.LE.eps) then
          if (Nr.GT.2) then
            CALL merge_(i,Nr,Ni)
      PRINT*,"Merged at:",i," NI:",(NI(III),III=1,NR+1)
                 change=.true.
            goto 102
          else
            Ni(1)=1
            Nr=2
            RETURN
          end IF
        end IF
      end DO
c      to avoid couples
      do i=1,nr
          k1=ni(i)
          k2=ni(i+1)
        if (k2-k1.eq.1) then
            change=.true.
               ni(i+1)=ni(i+1)+1
      PRINT*,"Decoupled at:",i," NI:",(NI(III),III=1,NR+1)
          end if
        end do
C         {"R" algorithm: adjusting the endpoint}
      DO i=2,Nr
        k1=Ni(i-1)
        k2=Ni(i+1)
        j1=Ni(i)
        eps1=r2b(k1,j1,x,y,a,b)
        eps2=r2b(j1,k2,x,y,a,b)
        if (eps1.GT.eps2) then
            epsm=eps1
          else
            epsm=eps2
        END IF
      PRINT*,"eps1,2,m:",eps1,eps2,epsm
        DO j=k1+2,k2-2
          eps1=r2b(k1,j,x,y,a,b)
          eps2=r2b(j,k2,x,y,a,b)

          if (eps1.GT.eps2) then
                 epsr=eps1
               else
                 epsr=eps2
          END IF

          if (epsr.LT.epsm) then
            epsm=epsr
            j1=j
          end IF
        end DO
        if (j1.NE.Ni(i)) then
          Ni(i)=j1
          change=.true.
      PRINT*,"Ralged at:",i," NI:",(NI(III),III=1,NR+1)
        end IF
        if (i.eq.2) epsm1=epsm
        end DO

      if (change) then
          change=.false.
          goto 102
      end if
        ni(nr+1)=N
      PRINT*,"Done. NI:",(NI(III),III=1,NR+1)
      RETURN
      end
