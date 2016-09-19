!/ ------------------------------------------------------------------- /
      INTEGER FUNCTION IYMD21 ( NYMD ,M )
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    IYMD21      INCREMENT DATE BY +- 1
!   PRGMMR: H.S. CHEN        ORG: W/NP21     DATE: 1998-10-29
!
! ABSTRACT: INCREMENT DATE BY +/- 1
!
! PROGRAM HISTORY LOG:
!   98-10-18  H.L. TOLMAN ORIGINATION
!   98-10-29  H.S. CHEN   ADDING DOCBLOCK
!             H.L. TOLMAN
!   07-01-03  D CAO  TRANSFER FROM F77 TO F90 
!
! USAGE: SEE ORIGINAL DOCUMENTATION BELOW.
!
! REMARKS: SEE ORIGINAL DOCUMENTATION BELOW.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77-90
!   MACHINE:  CRAY C90 / IBM RS6000 SP
!
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 77 |
!/                  | Last update :         18-Oct-1998 |
!/                  +-----------------------------------+
!/                                Based on INCYMD of the GLA GCM.
!/
!  1. Purpose :
!
!     Increment date in YYYYMMDD format by +/- 1 day.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Old date in YYMMDD format.
!       M       Int.   I   +/- 1 (Day adjustment)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       STRACE
!
!  5. Called by :
!
!     Any subroutine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     C/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER ::        NYMD, M
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER,DIMENSION(12) :: NDPM(12)
      INTEGER  ::       NY, NM, ND
      LOGICAL  ::       LEAP
!/
!/ ------------------------------------------------------------------- /
!/
      DATA     NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
!
! "Unpack" and increment date :
!
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      NM   = MIN ( 12 , MAX(1,NM) )
      ND   = MOD(NYMD,100) + M
      LEAP = MOD(NY,400).EQ.0 .OR.                              &   
            ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
!
! M = -1, change month if necessary :
!
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
!/
!/ End of IYMD21 ----------------------------------------------------- /
!/
      END
