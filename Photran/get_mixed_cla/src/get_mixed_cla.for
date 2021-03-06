C A version of get_mixed that accepts two command-line arguments
C which give the names of the input and output files.
C
      program get_mixed
      real t(2000),SAL(2000),sigm(2000),z(2000)
      real sms(100),smz(100)
      character (len=130):: froot,iname,oname,rname
!      CHARACTER (LEN=20):: density,temperature,salinity
!      DATA density    /"DENSITY             "/
!      DATA temperature/"TEMPERATURE         "/
!      DATA salinity   /"SALINITY            "/

!      CHARACTER (len=130):: FROOT

c Read the filenames from the command line
      CALL get_command_argument(1, froot)
      IF(trim(froot).EQ."") THEN
        print*,"get_mixed_cla <ifile>"
        print*,"  <ifile>: input data filename root, no extension."
        STOP
      END IF

c Compute the names of the input and output data files.
      iname=trim(FROOT)//'.t11'
      oname=trim(FROOT)//'-F.txt'
      rname=trim(FROOT)//'.R'
      PRINT*,"Input path: ",trim(iname),"  Report path: ",trim(oname)

c reading text file
      open(UNIT=1,FILE=iname)
      do i=1,1000
      read(1,*,err=1,end=1) z(i),t(i),sal(i),r,r,sigm(i)
      END DO
      close(1)
1     n=i-1

      IF(N.LT.1) THEN
        print*,"No data found at:",iname
        print*,"Data should be by rows:"
        print*,"z,temperature,salinity,r,r,sigma"
        STOP
      END IF


c     parameters
      z0=2.5
      zmax=140
      thres=0.005
C      !!  IMPORTANT !!
C     If you specify an error norm of 0.003-0.005, you will obtain the
c     mixing depth for a diurnal-type mixed layer. However, for
c     seasonal or interannual scale analyses, we advise you to use a larger
c     value of the error norm (up to 0.015)

      open(UNIT=2,FILE=oname)
      open(UNIT=3,FILE=rname)

      res1=BY_S_M(N,NIMAX,THRES,Z0,ZMAX,Z,SIGM,SMZ,SMS)
      CALL reporter(2,iname,"DENSITY",
     &              thres,res1,nimax,smz,sms)
      CALL rlist(3,iname,"Density","DensityBYSM",
     &           thres,res1,nimax,smz,sms)

      res2=BY_S_M(N,NIMAX,THRES,Z0,ZMAX,Z,T,SMZ,SMS)
      CALL reporter(2,iname,"TEMPERATURE",
     &              thres,res2,nimax,smz,sms)
      CALL rlist(3,iname,"Temperature","TemperatureBYSM",
     &           thres,res1,nimax,smz,sms)

      res3=BY_S_M(N,NIMAX,THRES,Z0,ZMAX,Z,SAL,SMZ,SMS)
      CALL reporter(2,iname,"SALINITY",
     &              thres,res3,nimax,smz,sms)
      CALL rlist(3,iname,"Salinity","SalinityBYSM",
     &           thres,res1,nimax,smz,sms)

C      THE THREE EXAMPLES BELOW SHOW ANOTHER VARIANT OF THE ALGORITHM
C      FOR THE CASE OF A PREDIFINED NUMBER OF SEGMENTS (IN THIS CASE IN
c      EQUAL TO 5). THE ERROR NORM IS AN OUTPUT PARAMETER.

      nimax=5
      res1=BY_s_m3(n,nimax,thres,z0,z,zmax,sigm,smz,sms)
      CALL reporter(2,iname,"DENSITY",thres,res1,nimax,smz,sms)
      CALL rlist(3,iname,"Density","DensityBYSM3",
     &           thres,res1,nimax,smz,sms)

      nimax=5
      res1=BY_s_m3(n,nimax,thres,z0,z,zmax,t,smz,sms)
      CALL reporter(2,iname,"TEMPERATURE",thres,res1,nimax,smz,sms)
      CALL rlist(3,iname,"Temperature","TemperatureBYSM3",
     &           thres,res1,nimax,smz,sms)


      nimax=5
      res1=BY_s_m3(n,nimax,thres,z0,z,zmax,sal,smz,sms)
      CALL reporter(2,iname,"SALINITY",thres,res1,nimax,smz,sms)
      CALL rlist(3,iname,"Salinity","SalinityBYSM3",
     &           thres,res1,nimax,smz,sms)

      CLOSE(2)
      CLOSE(3)
      STOP
      END PROGRAM

      SUBROUTINE reporter(unit,filepath,param,thres,res1,nimax,smz,sms)
      INTEGER unit
      character (len=*):: filepath,param
      CHARACTER (len=20):: ladjuster20
      REAL thres,res1,smz(nimax+1),sms(nimax+1)

      write(unit,*)
      write(unit,200 ) filepath
      ladjuster20 = param//" PROFILE,"
      ladjuster20 = ADJUSTL(ladjuster20)
      write(unit,2011) ladjuster20,thres
      write(unit,2020) res1,nimax
      write(unit,2041)
      ladjuster20 = param
      ladjuster20 = ADJUSTL(ladjuster20)
      write(unit,2031) ladjuster20
      write(unit,2041)
      do i=1,nimax+1
       write(unit,2030)i,smz(i),sms(i)
      end do
      write (unit,2040)

      RETURN

200   format('FILE NAME=  ',a20)
2011  format('RESULT FOR ',a20,'   ERR NORM=',f12.5)
!2012  format('RESULT FOR TEMPERATURE PROFILE,   ERR NORM=',f12.5)
!2013  format('RESULT FOR SALINITY PROFILE,      ERR NORM=',f12.5)
2020  format('MLD=',F12.2,10x,'MAX SEGMENTS=',i3)
2031  format('NUMBER     DEPTH    ',a20)
!2032  format('NUMBER     DEPTH    TEMPERATURE')
!2033  format('NUMBER     DEPTH    SALINITY')
!3011  format('RESULT FOR ',a23,i3,' SEGMENTS')
!3012  format('RESULT FOR TEMPERATURE PROFILE,   ',i3,' SEGMENTS')
!3013  format('RESULT FOR SALINITY PROFILE,      ',i3,' SEGMENTS')
!3020  format('MLD=',F12.2,10x,'ERROR NORM=',f12.5)

2030  format(i4,2f12.4)
2040  format('======================================================')
2041  format('------------------------------------------------------')

      END SUBROUTINE

      SUBROUTINE rlist(unit,filepath,param,varname,
     &                 thres,res1,nimax,smz,sms)
      INTEGER unit
      CHARACTER (len=*):: filepath,param,varname
      REAL thres,res1,smz(nimax+1),sms(nimax+1)

      write(unit,*) varname//" = list("
      write(unit,*)"  datapath="""//trim(filepath)//""","
      write(unit,*)"  variable="""//trim(param)//""","
      write(unit,*)"  thres=",thres,","
      write(unit,*)"  result=",res1,","
      write(unit,*)"  nimax=",nimax,","
      write(unit,*)"  smz=c("
      do i = 1,nimax
        write(unit,*)"    ",smz(i),","
      end do
      write(unit,*)"    ",smz(nimax+1)
      write(unit,*)"  ),"
      write(unit,*)"  sms=c("
      do i = 1,nimax
        write(unit,*)"    ",sms(i),","
      end do
      write(unit,*)"    ",sms(nimax+1)
      write(unit,*)"  )"
      write(unit,*)")"
      write(unit,*)
      END SUBROUTINE


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
      change=.false.

c       {step 1: if exceeds norma}

C     lab1:
      i=1
!101   CONTINUE
      DO while (i.le.Nr)
        k1=Ni(i)
        k2=Ni(i+1)
        enorma=r2b(k1,k2,x,y,a,b)
        if (enorma.GT.eps) then
          CALL spl(i,Nr,Ni)
          change=.true.
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
        end IF
        if (i.eq.2) epsm1=epsm
        end DO

      if (change) then
          change=.false.
          goto 102
      end if
        ni(nr+1)=N
      RETURN
      end

        REAL FUNCTION BY_S_M(N,NIMAX,THRES,Z0,ZMAX,Z,SIGMA,SMZ,SMS)
c     Service subroutine for determining Mixed Layer Depth for a SPECIFIED ERROR NORM VALUE
c     (This subroutine calls the S_M_P subroutine)
c       Input:
C          N     -[INTEGER] number of points;
C          THRES -[REAL]  error norm;
C          Z0    -[REAL]  initial depth: use to omit data above z0
C          ZMAX  -[REAL]  maximum depth: use to omit data below zmax
C
C          Z     -[REAL(N)] input x data array, should be increasing function of index
C          SIGMA -[REAL(N)] input y data array
C       Output:
C          NIMAX -[INTEGER] final number of segments;
C          SMZ   -[REAL(NIMAX)] final z array of segmented data
C          SMS   -[REAL(NIMAX)] final sigma array of segmented data
C          VY_S_M-[REAL] position of MLD (=SMZ(2)).
C                      return -99 if something is not right.
C

        real z(N),sigma(N),smz(N),sms(N)
        real xx(1000),yy(1000)
        integer ni(400)
      by_s_m=-99.0
      NN=800
c     finding initial s-level
        i=1
      do while (z(i).lt.z0.and.i.le.n)
          i=i+1
        end do
        if (i.eq.n) return
        i1=i
        sigma0=sigma(i)

c     finding second s-level
      do while (z(i).le.zmax.and.i.le.n)
          i=i+1
        end do
        i2=i-1

      dz=(z(i2)-z(i1))/nn
      call GET_XX_norm(ax,ay,i2-i1+1,nn,z(i1),dz,z(i1),sigma(i1),XX,YY)
      call s_m_p(nn,thres,xx,yy,Nr,Ni)
      k=ni(2)
        ss=0.5*(xx(k)+xx(k-1))*ax+z(i1)


      nimax=nr
      smz(1)=z(i1)
      sms(1)=sigma(i1)
        if (nimax.gt.100)nimax=100
        do i=2,nimax+1
          k=ni(i)
        smz(i)=0.5*(xx(k)+xx(k-1))*ax+z(i1)
        sms(i)=0.5*(yy(k)+yy(k-1))*ay+sigma(i1)
        end do
      by_s_m=ss
      return
        end


      SUBROUTINE s_mN(n,eps,x,y,Nr,Ni)
C     Subroutine to determine the Linear SEGMENTS for a PREDEFINED NUMBER OF SEGMENTS (NR)
c     (Use this program when you want to predefine the number of segments you want to fit
c      to the data)
C       Input:
C          N   -[INTEGER] number of points;
C          NR  -[INTEGER] number of segments (fixed);
C          X   -[REAL(N)] input x data array, should be increasing function of index
C          Y   -[REAL(N)] input y data array
C       Output:
C        EPS -[REAL] maximum error norm
C          NI  -[INTEGER] final array with segment start points
        INTEGER NR, NI(n)
        REAL X(n),Y(n)
      INTEGER i,j,j1,k1,k2 ! Remove warnings from unused variables
!        INTEGER i,j,j1,k,k1,k2,k3
      integer M
      REAL eps1,eps2,epsr,epsm,a,b
      LOGICAL change
!      REAL s  ! Remove warning from unused variable
C       label lab0,lab1,lab2,lab3;
      epsm = 0.0  ! Silence a compiler warning about unitialized use.
      m=NINT(FLOAT(N)/FLOAT(Nr))
        ni(1)=1
      DO i=2,Nr
        Ni(i)=m*(i-1)+1
      end DO
      Ni(Nr+1)=N+1 !{last interval}
c      sss=orma2b(2,20,x,y,a,b)

        m=0
102   CONTINUE
c      to avoid couples
      do i=1,nr
          k1=ni(i)
          k2=ni(i+1)
        if (k2-k1.eq.1) then
            change=.true.
               ni(i+1)=ni(i+1)+1
          end if
        end do
C         {"R" algorithm: adjusting of endpoint}
      DO i=2,Nr
        k1=Ni(i-1)
        k2=Ni(i+1)
        j1=Ni(i)
        eps1=r2b(k1,j1,x,y,a,b)
        eps2=r2b(j1,k2,x,y,a,b)
c         epsN0=eps1**2+eps2**2
        if (eps1.GT.eps2) then
            epsm=eps1
          else
            epsm=eps2
        END IF
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
        end IF
        if (i.eq.2) epsm1=epsm
        end DO

      if (change) then
          change=.false.
          goto 102
      end if
        ni(nr+1)=n
        eps=epsm
      RETURN
      end


        real FUNCTION BY_s_m3(n,nimax,thres,z0,z,zmax,sigma,smz,sms)
C     (This is the service subroutine for the case of a SPECIFIC ERROR NORM)
c     (Calls subroutine S_mN)
C       Input:
C          N   -[INTEGER] number of points;
C          NR  -[INTEGER] number of segments (fixed);
C          X   -[REAL(N)] input x data array, should be increasing function of index
C          Y   -[REAL(N)] input y data array
C       Output:
C          NI  -[INTEGER] final array with segment start points
        real z(n),sigma(n),smz(n),sms(n)
        real xx(1000),yy(1000)
        integer ni(400)
      by_s_m3=-99.0     ! Compiler is warning could be uninitialized.
!      by_s_m=-99.0
      nn=800
c     finding initial s-level
        i=1
      do while (z(i).lt.z0.and.i.lt.n)
          i=i+1
        end do
        if (i.eq.n) return
        i1=i
        sigma0=sigma(i)

c     finding second s-level

      do while (z(i).le.zmax.and.i.lt.n)
          i=i+1
      end do
        i2=i-1

      dz=(z(i2)-z(i1))/nn
      call GET_XX_norm(ax,ay,i2-i1+1,nn,z(i1),dz,z(i1),sigma(i1),XX,YY)
        Nr=nimax
      call s_mN(nn,thres,xx,yy,Nr,Ni)
      k=ni(2)
        ss=0.5*(xx(k)+xx(k-1))*ax+z(i1)
      smz(1)=z(i1)
      sms(1)=sigma(i1)
        do i=2,nimax+1
          k=ni(i)
        smz(i)=0.5*(xx(k)+xx(k-1))*ax+z(i1)
        sms(i)=0.5*(yy(k)+yy(k-1))*ay+sigma(i1)
        end do
      by_s_m3=ss
      return
        end

