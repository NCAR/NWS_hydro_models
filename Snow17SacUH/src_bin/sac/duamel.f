C===========================================================
C
C     THIS SUBROUTINE PERFORMS UNIT HYDROGRAPH ROUTING
C
c     A. Wood, 2016, modified so that function returns time of concentration length for the routing
c                    this is important for initializing a forecast; also documented & formatted
c
      SUBROUTINE DUAMEL(Q,NU1,U1,UN1,UT,DT,N,M,QB,K,NTAU,TOC_length)  ! AWW
      !SUBROUTINE DUAMEL(Q,NU1,U1,UN1,UT,DT,N,M,QB,K,NTAU)   ! orig
      IMPLICIT REAL (A-H,O-Z)
      INTEGER A,B
      REAL Q(N),QB(N),U1(1000)

      integer TOC_length  ! AWW-2016 added based on Tom Hopson routine for estimating TOC for routing

      !      PRINT *,'DUAMEL'
      !      PRINT *, UN1,UT,DT,Q(1),NU1,N,M,K,NTAU
      !      PRINT *,'DUAMEL?'
 
      ! AWW adding documentation for input variables
      ! Q      : TCI (total channel inflow) vector ... .ie unrouted
      ! NU1    : a constant=1
      ! U1     : unit hydrograph vector
      ! UN1    : unit hydrograph shape parameter (for gamma dist)
      ! UT     : unit hydrograph scale parameter (for gamma dist)
      ! DT     : timestep of the UH function (in days or fractions thereof)
      ! N      : sim_length + uh_length ...ie length of U1
      ! M      : max UH length?
      ! QB     : routed flow vector
      ! K      : 
      ! NTAU   : 
      ! TOC_length : time of concentration reflected in routing params, added functionality

      ! AWW adding documentation for local variables
      ! SP     : 

      TOC_length=0   ! AWW: initialize time of concentration (in timesteps)

      IF(UN1) 8,9,9
    8 U1(1)=1.
      NU1 = 1
      M = 1
      GOTO 6

    9 IF (K .EQ. 0) GOTO 6
      SP=0.
      TOC=GF(UN1)
      TOC=LOG(TOC*UT)
      DO 1 I=1,M
        TOP=I*DT/UT
        TOR=(UN1-1)*LOG(TOP)-TOP-TOC
        U1(I)=0.0
        IF(TOR.GT.-8.) THEN
          U1(I)=EXP(TOR)
        ELSE
          IF (I .GT. 1) THEN
            U1(I) = 0.0
            M = I
            GO TO 12
          END IF
        END IF
    1 SP=SP+U1(I)
   12 CONTINUE

      IF (SP .EQ. 0) SP=1.0E-5
      SP=1./SP

      DO 7 I=1,M
    7 U1(I)=U1(I)*SP

    6 CONTINUE
      IOC=N+NTAU

      IF(N.GT.M)GO TO 10

      DO 2 I=1,IOC
        QB(I)=0.
        A=1
        IF(I.GT.M)A=I-M+1
        B=I
        IF(I.GT.N)B=N
        DO 3 J=A,B
          IOR=I-J+1

          if (I-J .gt. TOC_length) TOC_length=I-J  ! AWW-2016 for TOC_length

    3   QB(I)=QB(I)+Q(J)*U1(IOR)
    2 CONTINUE

      GO TO 11

   10 DO 4 I=1,IOC
        QB(I)=0.
        A=1
        IF(I.GT.N)A=I-N+1
        B=I
        IF(I.GT.M)B=M

        DO 5 J=A,B
          IOR=I-J+1

          if (I-IOR .gt. TOC_length) TOC_length=I-IOR  ! AWW-2016 for TOC_length

 5      QB(I)=QB(I)+U1(J)*Q(IOR)
 4    CONTINUE

 11   RETURN

      END


C
C=================================================================
C
c     Gamma Function? 
      FUNCTION GF(Y)
      H=1
      X=Y
 38   IF(X.LE.0.)GO TO 39
      IF(X.EQ.2.)GO TO 42
      IF(X.GT.2.)GO TO 40
      H=H/X
      X=X+1
      GO TO 38
  40  IF(X.LE.3.)GO TO 44
      X=X-1
      H=H*X
      GO TO 38
  44  X=X-2
      H=(((((((.0016063118*X+0.0051589951)*X+0.0044511400)*X+.0721101567
     *)*X+.0821117404)*X+.4117741955)*X+.4227874605)*X+.9999999758)*H
 42   GF=H
  39  RETURN
      END
