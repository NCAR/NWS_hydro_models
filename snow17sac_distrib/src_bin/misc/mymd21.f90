!/ ------------------------------------------------------------------- /
      INTEGER FUNCTION MYMD21 ( NYMD )
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    MYMD21      CALCULATE JULIAN DATE
!   PRGMMR: H.S. CHEN        ORG: W/NP21     DATE: 1998-10-29
!
! ABSTRACT: CALCULATE JULIAN DATE FROM DATE IN YYYYMMDD FORMAT.
!
! PROGRAM HISTORY LOG:
!   98-10-19  H.L. TOLMAN ORIGINATION
!   98-10-29  H.S. CHEN   ADDING DOCBLOCK
!             H.L. TOLMAN
!   06-02-03  D CAO TRANSFER FROM F77 TO F90 
!
! USAGE: SEE ORIGINAL DOCUMENTATION BELOW.
!
! REMARKS: SEE ORIGINAL DOCUMENTATION BELOW.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77-90
!   MACHINE:  CRAY C90 / IBM RS6000 SP
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 77 |
!/                  | Last update :         19-Oct-1998 |
!/                  +-----------------------------------+
!/                                Based on MODYMD of the GLA GCM.
!/
!  1. Purpose :
!
!     Convert date in YYMMDD format to julian date.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Date in YYMMDD format.
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
!/
!/ ------------------------------------------------------------------- /
!/ Parameer list
!/
      INTEGER         NYMD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER,DIMENSION(12) :: NDPM
      INTEGER    ::      NY, NM, ND
      LOGICAL    ::     LEAP
!/
!/ ------------------------------------------------------------------- /
!/
      DATA    NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
!
! "Unpack" and increment date :
!
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      ND   = MOD(NYMD,100)
      LEAP = MOD(NY,400).EQ.0 .OR.                           &    
              ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
!
! Loop over months :
!
      IF (NM.GT.2 .AND. LEAP)  ND = ND + 1
!
   40 CONTINUE
      IF (NM.LE.1)  GO TO 60
      NM = NM - 1
      ND = ND + NDPM(NM)
      GO TO 40
!
   60 CONTINUE
      MYMD21 = ND
!
      RETURN
!/
!/ End of MYMD21 ----------------------------------------------------- /
!/
      END
