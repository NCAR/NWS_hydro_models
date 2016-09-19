!/ ------------------------------------------------------------------- /
      SUBROUTINE TICK21 ( TIME, DTIME )
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    TICK21      INCREMENT DATE AND TIME
!   PRGMMR: H.S. CHEN        ORG: W/NP21     DATE: 1998-10-29
!
! ABSTRACT: INCREMENT INTEGER TIME(2) IN YYYYMMDD HHMMSS FORMAT BY
!   A GIVEN NUMBER OF SECONDS.
!
! PROGRAM HISTORY LOG:
!   93-03-29  H.L. TOLMAN ORIGINATION
!   98-10-29  H.S. CHEN   ADDING DOCBLOCK
!             H.L. TOLMAN
!   06-01-03  D. CAO FROM F77 TO F90 
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
!/                  | Last update :         29-Mar-1993 |
!/                  +-----------------------------------+
!/                                Based on TICK of the GLA GCM.
!/
!  1. Purpose :
!
!     Updates time information, DTIME=0 converts to "legal" time.
!     Goes into the 21st century.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
!                          (2) Current time in HHMMSS format.
!       DTIME   Real   I   Time step in seconds.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       IYMD21   Increment date in YYYYMMDD format.
!       STRACE   Service routine
!
!  5. Called by :
!
!     Any other routine.
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
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, DIMENSION(2) ::         TIME
      REAL ::            DTIME
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER  ::       NYMD, NHMS, NSEC
      INTEGER  ::       IYMD21
      EXTERNAL ::       IYMD21
!/
!/ ------------------------------------------------------------------- /
!/
!
! Zero increment: get "legal" data
!
      NYMD   = TIME(1)
      NHMS   = TIME(2)
      IF (DTIME.EQ.0.) THEN
          NYMD = IYMD21 (NYMD,-1)
          NYMD = IYMD21 (NYMD, 1)
        ENDIF
!
! Convert and increment time :
!
      NSEC = NHMS/10000*3600 + MOD(NHMS,10000)/100* 60 +     & 
            MOD(NHMS,100) + NINT(DTIME)
!
! Check change of date :
!
  100 CONTINUE
      IF (NSEC.GE.86400)  THEN
          NSEC = NSEC - 86400
          NYMD = IYMD21 (NYMD,1)
          GOTO 100
        ENDIF
!
  200 CONTINUE
      IF (NSEC.LT.00000)  THEN
          NSEC = 86400 + NSEC
          NYMD = IYMD21 (NYMD,-1)
          GOTO 200
        ENDIF
!
      NHMS = NSEC/3600*10000 + MOD(NSEC,3600)/60*100 + MOD(NSEC,60)
!
      TIME(1) = NYMD
      TIME(2) = NHMS
!
      RETURN
!/
!/ End of TICK21 ----------------------------------------------------- /
!/
      END
