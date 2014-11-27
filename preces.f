




*+PRECES

       SUBROUTINE PRECES (RA1, DEC1, RA2, DEC2, YEARS)
C      -----------------------------------------------
C
C  Performs steady precession in RA and Dec.
C
C  Given:
C      RA1       real*8      initial right ascension (radians)
C      DEC1      real*8      initial declination (radians)
C      YEARS     real*8      time interval (years)
C
C  Returns
C      RA2       real*8      final right ascension (radians)
C      DEC2      real*8      final declination (radians)
C
C  Performs steady precession from position RA1, DEC1 to RA2, DEC2
C  during the time interval YEARS (+ve means forward precession).
C  No nutation or aberration is included.  The terms used are:
C
C    For general precession,  p = 50.2564 + 0.0222T  arcsecs
C
C    For obliquity of ecliptic,  epsilon = 23d 27' 8.26" - 46.845T
C
C    For T = 65/100,  p = 50.2708,  epsilon = 23d 26' 37.81"
C
C  Note that if abs(YEARS) < 0.01, precession is not performed, and
C  the final position is set equal to the initial one.
C
C  (EMW)
C
*-
       REAL*8     RA1,DEC1,RA2,DEC2,YEARS
       REAL*8     PSI,COSEP,SINEP,COSPSI,SINPSI
       REAL*8     X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3
       REAL*8     PI,TWOPI,EP,PSICON
C
       PARAMETER (PI=3.1415926535898D0)
       PARAMETER (TWOPI=2.D0*PI)
C
       PARAMETER (EP=(23.0D0*3600+26.0D0*60+37.81D0)*PI/648000.D0)
       PARAMETER (PSICON=50.2708D0*PI/648000.D0)
C
C  Check if YEARS is near zero
C
       IF (DABS(YEARS).LT.0.01) THEN
         RA2=RA1
         DEC2=DEC1
C
       ELSE
         PSI=YEARS*PSICON
         COSEP=DCOS(EP)
         SINEP=DSIN(EP)
         COSPSI=DCOS(PSI)
         SINPSI=DSIN(PSI)
C
C  Transform to rectangular celestial coordinates
C
         X=DCOS(DEC1)*DCOS(RA1)
         Y=DCOS(DEC1)*DSIN(RA1)
         Z=DSIN(DEC1)
C
C  Transform to rectangular ecliptic coordinates
C
         X1=X
         Y1=Y*COSEP+Z*SINEP
         Z1=Z*COSEP-Y*SINEP
C
C  Rotate the ecliptic plane
C
         X2=X1*COSPSI-Y1*SINPSI
         Y2=X1*SINPSI+Y1*COSPSI
         Z2=Z1
C
C  Transform back to celestial coordinates
C
         X3=X2
         Y3=Y2*COSEP-Z2*SINEP
         Z3=Y2*SINEP+Z2*COSEP
C
C  Transform back to RA and Dec
C
         RA2=0.D0
         DEC2=DASIN(Z3)
         IF (X3.NE.0.D0 .OR. Y3.NE.0.D0) THEN
           RA2=DATAN2(Y3,X3)
           IF (RA2.LT.0.D0) RA2=RA2+TWOPI
         ENDIF
       ENDIF
C
       END
