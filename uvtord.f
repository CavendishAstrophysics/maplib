


*+UVTORD

       SUBROUTINE UVTORD (U, V, RA, DEC, STATUS)
C      -----------------------------------------
C
C  Converts U,V coordinates to RA and Dec.
C
C  Given:
C      U         real*8      U-coordinate (gridpoints)
C      V         real*8      V-coordinate (gridpoints)
C
C  Returned:
C      RA        real*8      right ascension (radians)
C      DEC       real*8      declination (radians)
C      STATUS    integer     status value
C
C  Converts U,V coordinates to RA and Dec at projection date, using
C  the projection currently set up by routine STPROJ.  Works for
C  equatorial plane, sky coordinates and tangent plane projections
C  (but note that negative declinations have not been well tested).
C
C  The STATUS value should be zero on entry.  The returned status
C  value is zero unless the given U,V corresponds to an unreal point.
C  In this case the local error code UV_UNREAL is returned (but note
C  that no error message is printed out: it is important to test and
C  reset the STATUS value on exit from this routine).
C
C  (EMW, DAG)
C  (NMR, January 88)
C
*-
       REAL*8     U, V, RA, DEC
       INTEGER    STATUS
C
       include '/mrao/include/maplib_proj.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/constants.inc'
C
      REAL*8      RA0, RAD, U0S, V0S, DENOM

      IF (STATUS.NE.0) RETURN

C     Rotate to zero skew angle if necessary.
      IF (SSKEW .NE. 0.0D+0) THEN
          U0S = U*CSKEW + V*SSKEW
          V0S = V*CSKEW - U*SSKEW
      ELSE
          U0S = U
          V0S = V
      END IF

C     Scale to sampling of 1 radian/gridpoint.
      U0S = U0S*USCALE
      V0S = V0S*VSCALE

C     Convert to RA and Dec.
      IF ( PRJ_CODE .EQ. 3 ) THEN
C         Tangent plane projection -
          DENOM = CDECMC - V0S*SDECMC
          IF ((U0S .EQ. 0.0D+0) .AND. (DENOM .EQ. 0.0D+0)) THEN
C             North or South Pole
              RA  = CONST_PI
              DEC = CONST_PIBY2
              IF (SDECMC .LT. 0.0D+0) DEC = -DEC
          ELSE
C             The following formula's basically are eqns. 4.22 and 4.20
C             in 'Positional Astronomy' by D. McNally (Muller 1974)
              RA0 = DATAN2( U0S, DENOM )
              RA  = RA0 + PRJ_RAMC
              IF (DENOM .GT. 0.0D+0) THEN
                  DEC = DATAN2( (DCOS(RA0)*(V0S*CDECMC+SDECMC)), DENOM )
              ELSE
                  DEC = DATAN2(-(DCOS(RA0)*(V0S*CDECMC+SDECMC)),-DENOM )
              END IF
          END IF
      ELSE
C         Equatorial or sky projection - convert to map centre at pole.
          V0S = V0S - CDECMC

          RAD = SQRT( V0S*V0S + U0S*U0S )
          IF (RAD .GT. 1.0D+0) THEN
C             Point lies outside the equatorial plane.
              STATUS = UV_UNREAL
              GOTO 9999
          ELSE IF (RAD .EQ. 0.0D+0) THEN
C             North or south pole.
              RA  = CONST_PI
              DEC = CONST_PIBY2
              IF (SDECMC .LT. 0.0D+0) DEC = -DEC
          ELSE
C             Normal polar to cartesian conversion.
              RA  = DATAN2( U0S, -V0S ) + PRJ_RAMC
              DEC = DACOS( RAD )
              IF (SDECMC .LT. 0.0D+0) DEC = -DEC
          END IF
      END IF

      IF (RA .LT. 0.0D+0)     RA = RA+CONST_2PI
      IF (RA .GE. CONST_2PI)  RA = RA-CONST_2PI

      IF (STATUS .EQ. 0) RETURN

 9999 IF (STATUS .NE. UV_UNREAL) THEN
          CALL MAPERR(STATUS, 'in routine UVTORD' )
          RETURN
      END IF
      END
