!/ ------------------------------------------------------------------- /
      REAL FUNCTION DSEC21 ( TIME1, TIME2 )
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    DSEC21      TIME DIFFERENCE IN SECONDS
!   PRGMMR: HENDRIK          ORG: W/NP21     DATE: 2000-12-28
!
! ABSTRACT: CALCULATE TIME DIFFERENCE IN SECONDS BETWEEN TWO DATES
!   IN YYYYMMDD HHMMSS FORMAT.
!
! PROGRAM HISTORY LOG:
!   93-03-29  H.L. TOLMAN ORIGINATION
!   98-10-29  H.S. CHEN   ADDING DOCBLOCK
!             H.L. TOLMAN
!   00-12-28  H.L. TOLMAN LEAP YEAR EOY FIX
!   06-01-02  D. CAO  TRANSFER FROM F77 TO F90 
!
! USAGE: SEE ORIGINAL DOCUMENTATION BELOW.
!
! REMARKS: SEE ORIGINAL DOCUMENTATION BELOW.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77-90
!   MACHINE:  CRAY C90-J90 / IBM RS6000 SP
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 77 |
!/                  | Last update :         29-Mar-1993 |
!/                  +-----------------------------------+
!/
!  1. Purpose :
!
!     Calculate the time difference in seconds between two times in
!     YYMMD HHMMMSS formats.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIMEn   I.A.   I   Times, TIMEn(1) is date in YYYYMMDD format,
!                          TIMEn(2) is time in HHMMSS format.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       MYMD21   Calculate Julian data.
!
!  5. Called by :
!
!     Any routine.
!
!  7. Remarks :
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
      INTEGER,DIMENSION(2)  ::   TIME1, TIME2
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER ::        NY1, ND1, NY2, ND2, NS1, NS2, NS, ND, NST
      INTEGER ::        MYMD21
      EXTERNAL ::       MYMD21
!/
!/ ------------------------------------------------------------------- /
!/
!
! Convert dates and times :
!
      NY1    = TIME1(1) / 10000
      ND1    = MYMD21 ( TIME1(1) )
      NS1    = TIME1(2)/10000*3600 + MOD(TIME1(2),10000)/100*60 +     & 
               MOD(TIME1(2),100)
!
      NY2    = TIME2(1) / 10000
      ND2    = MYMD21 ( TIME2(1) )
      NS2    = TIME2(2)/10000*3600 + MOD(TIME2(2),10000)/100*60 +     & 
               MOD(TIME2(2),100)
!
! Number of days and seconds in difference :
!
      ND     = ND2 - ND1
!
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
!
      NS     = NS2 - NS1
!
! Output of time difference :
!
      DSEC21 = REAL(NS) + 86400.*REAL(ND)
!
      RETURN
!/
!/ End of DSEC21 ----------------------------------------------------- /
!/
      END
