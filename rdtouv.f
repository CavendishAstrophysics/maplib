




*+RDTOUV

       SUBROUTINE RDTOUV (RA, DEC, U, V, STATUS)
C      -----------------------------------------
C
C  Converts RA and Dec to U,V coordinates.
C
C  Given:
C      RA        real*8      right ascension (radians)
C      DEC       real*8      declination (radians)
C      STATUS    integer     status value
C
C  Returned:
C      U         real*8      U-coordinate (gridpoints)
C      V         real*8      V-coordinate (gridpoints)
C
C  Converts a position expressed as RA and Dec at projection date into
C  U and V coordinates, using the projection currently set up by routine
C  STPROJ.  Works for equatorial plane, sky coordinates or tangent plane
C  projection (but note that negative declinations have not been well
C  tested).
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (EMW, DAG, 18 November 86)
C  (DJT, 22 October 87)
C  (NMR, January 88)
C
*-
      REAL*8      RA,DEC,U,V
      INTEGER     STATUS


       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/maplib_proj.inc'
C
      REAL*8      CDEC, SDEC, RA_DIFF, CRA_DIFF, U0S, V0S, DENOM
C
      IF (STATUS.NE.0) RETURN

      RA_DIFF = RA - PRJ_RAMC

C     First find U and V at 1 radian/gridpoint scale and zero skew.
      IF (PRJ_CODE.EQ.3) THEN
C         Tangent plane projection
          SDEC     = DSIN( DEC )
          CDEC     = DCOS( DEC )
          CRA_DIFF = DCOS( RA_DIFF )

C         The following formula's basically are eqns. 4.16 and 4.18 in
C         'Positional Astronomy' by D. McNally (Muller 1974)
          DENOM = SDEC*SDECMC + CDEC*CDECMC*CRA_DIFF
          IF ( DENOM .LE. 0 ) THEN
              STATUS = UV_UNREAL
              GOTO 9999
          END IF

          U0S = CDEC*DSIN( RA_DIFF ) / DENOM
          V0S = (SDEC*CDECMC - CDEC*SDECMC*CRA_DIFF) / DENOM
      ELSE
C         Equatorial plane or sky coordinates projection
          CDEC= DCOS(DEC)
          U0S = CDEC*DSIN(RA_DIFF)
          V0S =-CDEC*DCOS(RA_DIFF) + CDECMC
      END IF

C     Scale to required sampling.
      U0S = U0S/USCALE
      V0S = V0S/VSCALE

C     Rotate to correct skew angle if necessary.
      IF (SSKEW .NE. 0.0D+0) THEN
          U = U0S*CSKEW - V0S*SSKEW
          V = V0S*CSKEW + U0S*SSKEW
      ELSE
          U = U0S
          V = V0S
      END IF

      IF (STATUS .EQ. 0) RETURN
 9999 CALL MAPERR( STATUS, 'in routine RDTOUV' )
      RETURN

      END
