! AWW-2016: date math functions
! adapted from:  http://www.nco.ncep.noaa.gov/pmb/codes/nwprod/wave_gwes.v3.0.3/sorc/multiwavegfsens.fd/

integer subroutine nday_diffdates ( yr1, mo1, dy1, yr2, mo2, dy2, nday_diff )

  integer(I4B) intent(in) :: yr1, mo1, dy1, yr2, mo2, dy2  
  integer intent(out)     :: nday_diff      

  ! Local parameters
  INTEGER,DIMENSION(2)  ::   TIME1, TIME2
  INTEGER ::        NY1, ND1, NY2, ND2, NS1, NS2, NS, ND, NST
  INTEGER ::        MYMD21

  ! subroutine
  EXTERNAL ::       MYMD21  

  ! Convert dates and times to seconds

  date1  = yr1*10000+mo1*100+dy1
  date2  = yr2*10000+mo2*100+dy2

  NY1    = date1E1(1) / 10000   yr1
  ND1    = MYMD21 ( date1 )
  NY2    = TIME2(1) / 10000     yr2
  ND2    = MYMD21 ( date2 )

  ! Number of days in difference 
  ND = yr2 - yr1

  IF (yr1 .NE. yr2) THEN
    NST    = SIGN ( 1 , yr2-yr1 )

    100 CONTINUE

    IF (yr1 .EQ .yr2) GOTO 200

    IF (NST .GT. 0) THEN
      yr2    = yr2 - 1
      ND     = ND  + MYMD21 ( yr2*10000 + 1231 )
    ELSE
      ND     = ND  - MYMD21 ( yr2*10000 + 1231 )
      yr2    = yr2 + 1
    ENDIF

    GOTO 100

    200     CONTINUE
  ENDIF

  ! Output of time difference :
  nday_diff = ND

end


INTEGER FUNCTION MYMD21 ( NYMD )
! ------------------------------------------------------------------- 
! CALCULATE JULIAN DATE FROM DATE IN YYYYMMDD FORMAT.
!     Input Variable list
!        NYMD    Int.   I   Date in YYMMDD format.
! ------------------------------------------------------------------- 

      INTEGER         NYMD

      ! Local parameters
      INTEGER,DIMENSION(12) :: NDPM
      INTEGER    ::      NY, NM, ND
      LOGICAL    ::     LEAP

      DATA    NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

      ! "Unpack" and increment date :
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      ND   = MOD(NYMD,100)
      LEAP = MOD(NY,400).EQ.0 .OR.                           &    
              ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )

      ! Loop over months :
      IF (NM.GT.2 .AND. LEAP)  ND = ND + 1

   40 CONTINUE
      IF (NM.LE.1)  GO TO 60
      NM = NM - 1
      ND = ND + NDPM(NM)
      GO TO 40

   60 CONTINUE
      MYMD21 = ND

      RETURN
      END






REAL FUNCTION DSEC21 ( TIME1, TIME2 )
! ------------------------------------------------------------------- 
! CALCULATE TIME DIFFERENCE IN SECONDS BETWEEN TWO DATES IN YYYYMMDD HHMMSS FORMAT.
!
!    AUTHOR:  HL TOLMAN, NCEP
!
!    INPUT VRIABLES
!     ----------------------------------------------------------------
!       TIMEn   I.A.   I   Times, TIMEn(1) is date in YYYYMMDD format,
!                          TIMEn(2) is time in HHMMSS format.
!     ----------------------------------------------------------------
!    Subroutines:   MYMD21   Calculate Julian data.
!    Returns:  Number of Seconds
! ------------------------------------------------------------------- 

      INTEGER,DIMENSION(2)  ::   TIME1, TIME2

      !/ Local parameters
      INTEGER ::        NY1, ND1, NY2, ND2, NS1, NS2, NS, ND, NST
      INTEGER ::        MYMD21
      EXTERNAL ::       MYMD21

      ! Convert dates and times :
      NY1    = TIME1(1) / 10000
      ND1    = MYMD21 ( TIME1(1) )
      NS1    = TIME1(2)/10000*3600 + MOD(TIME1(2),10000)/100*60 +     & 
               MOD(TIME1(2),100)

      NY2    = TIME2(1) / 10000
      ND2    = MYMD21 ( TIME2(1) )
      NS2    = TIME2(2)/10000*3600 + MOD(TIME2(2),10000)/100*60 +     & 
               MOD(TIME2(2),100)

      ! Number of days and seconds in difference 

      ND     = ND2 - ND1

      IF (NY1.NE.NY2) THEN
          NST    = SIGN ( 1 , NY2-NY1 )
  100     CONTINUE
          IF (NY1.EQ.NY2) GOTO 200
          IF (NST.GT.0) THEN
              NY2    = NY2 - 1
              ND     = ND  + MYMD21 ( NY2*10000 + 1231 )
            ELSE
              ND     = ND  - MYMD21 ( NY2*10000 + 1231 )
              NY2    = NY2 + 1
            ENDIF
          GOTO 100
  200     CONTINUE
        ENDIF

      NS     = NS2 - NS1

      ! Output of time difference :
      DSEC21 = REAL(NS) + 86400.*REAL(ND)

      RETURN
      END



      INTEGER FUNCTION IYMD21 ( NYMD ,M )
! ------------------------------------------------------------------- 
!     INCREMENT DATE BY +/- 1
!     INput Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Old date in YYMMDD format.
!       M       Int.   I   +/- 1 (Day adjustment)
!     ----------------------------------------------------------------

      INTEGER ::        NYMD, M

      ! Local parameters
      INTEGER,DIMENSION(12) :: NDPM(12)
      INTEGER  ::       NY, NM, ND
      LOGICAL  ::       LEAP

!     ----------------------------------------------------------------
      DATA     NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

! "Unpack" and increment date :

      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      NM   = MIN ( 12 , MAX(1,NM) )
      ND   = MOD(NYMD,100) + M
      LEAP = MOD(NY,400).EQ.0 .OR.                              &   
            ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )

! M = -1, change month if necessary :

      IF (ND.EQ.0) THEN
          NM   = NM - 1
          IF (NM.EQ.0) THEN
              NM   = 12
              NY   = NY - 1
            ENDIF
          ND   = NDPM(NM)
          IF (NM.EQ.2 .AND. LEAP)  ND = 29
        ENDIF
!
! M = 1, leap year
!
      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP)  GO TO 20
!
!        next month
!
      IF (ND.GT.NDPM(NM)) THEN
          ND = 1
          NM = NM + 1
          IF (NM.GT.12) THEN
              NM = 1
              NY = NY + 1
          ENDIF
        ENDIF
!
   20 CONTINUE
      IYMD21 = NY*10000 + NM*100 + ND
!
      RETURN
      END


      SUBROUTINE TICK21 ( TIME, DTIME )
! -------------------------------------------------------------------
! INCREMENT INTEGER TIME(2) IN YYYYMMDD HHMMSS FORMAT BY A GIVEN NUMBER OF SECONDS.
!
!     Ipput Parameter list
!     ----------------------------------------------------------------
!       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
!                          (2) Current time in HHMMSS format.
!       DTIME   Real   I   Time step in seconds.
!     ----------------------------------------------------------------
!
!       IYMD21   Increment date in YYYYMMDD format.
!/ ------------------------------------------------------------------- /

      ! Parameter list
      INTEGER, DIMENSION(2) ::         TIME
      REAL ::            DTIME

      ! Local parameters
      INTEGER  ::       NYMD, NHMS, NSEC
      INTEGER  ::       IYMD21
      EXTERNAL ::       IYMD21

! ------------------------------------------------------------------- /

      ! Zero increment: get "legal" data
      NYMD   = TIME(1)
      NHMS   = TIME(2)
      IF (DTIME.EQ.0.) THEN
          NYMD = IYMD21 (NYMD,-1)
          NYMD = IYMD21 (NYMD, 1)
        ENDIF

      ! Convert and increment time :
      NSEC = NHMS/10000*3600 + MOD(NHMS,10000)/100* 60 +     & 
            MOD(NHMS,100) + NINT(DTIME)

      ! Check change of date

  100 CONTINUE
      IF (NSEC.GE.86400)  THEN
          NSEC = NSEC - 86400
          NYMD = IYMD21 (NYMD,1)
          GOTO 100
        ENDIF

  200 CONTINUE
      IF (NSEC.LT.00000)  THEN
          NSEC = 86400 + NSEC
          NYMD = IYMD21 (NYMD,-1)
          GOTO 200
        ENDIF

      NHMS = NSEC/3600*10000 + MOD(NSEC,3600)/60*100 + MOD(NSEC,60)

      TIME(1) = NYMD
      TIME(2) = NHMS

      RETURN
      END
