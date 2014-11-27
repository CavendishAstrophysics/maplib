C  (LIBRARY)MAPLIB-PROJ:INCL
C  -------------------------
C
C  $INCLUDE file for use with MAPLIB redtape routines
C
C  This file defines the map projection for use by routines RDTOUV and
C  UVTORD, which perform coordinate conversion between map grid points
C  and spherical coordinates.  The parameters defining the projection
C  are held in common /MAPROJ/ and are available to other MAPLIB routines
C  and to applications programs.
C
C  Note that the projection is always re-initialised by the RDREDT
C  routine using parameters read from map redtape.  The projection
C  may be reset explicitly by routine STPROJ, and displayed by PRPROJ.
C
      INTEGER    LENPRJ
      PARAMETER (LENPRJ=30)
      INTEGER*4  IPRALL(LENPRJ)
      COMMON /MAPROJ2/ IPRALL
C
C     PRJ_CODE      (I*4)   projection code
C     PRC_CODE      (I*4)   precession code
C     PRJ_USAMP     (R*8)   U-coordinate scale
C     PRJ_SKEW      (R*8)   Skew angle of U,V plane (radians)
C     PRJ_RAMAP     (R*8)   RA of map centre at ref. date (radians)
C     PRJ_DECMAP    (R*8)   Dec of map centre at ref. date (radians)
C     PRJ_REFDAT    (R*8)   Reference date (years)
C     PRJ_RAMC      (R*8)   RA of map centre at epoch of coordinate system
C     PRJ_DECMC     (R*8)   Dec of map centre at epoch of coordinate system
C     PRJ_EPOCH     (R*8)   Epoch of coordinate system (years)
C
C     Derived quantities - needed for RA, dec to U, V conversions.
C
C     USCALE        (R*8)   Scales U to 1 radian/gridpoint.
C     VSCALE        (R*8)   Scales V to 1 radian/gridpoint.
C     SSKEW         (R*8)   Sine of skew angle
C     CSKEW         (R*8)   Cosine of skew angle
C     SDECMC        (R*8)   Sine of map centre declination.
C     CDECMC        (R*8)   Cosine of map centre declination.
C     (N.B.   PRJ_RAMC and PRJ_CODE are the only other variables needed
C             for the conversions.)

      INTEGER*4   PRJ_CODE,  PRC_CODE
      REAL*8      PRJ_USAMP, PRJ_SKEW
      REAL*8      PRJ_RAMAP, PRJ_DECMAP, PRJ_REFDAT
      REAL*8      PRJ_RAMC,  PRJ_DECMC,  PRJ_EPOCH
      REAL*8      USCALE, VSCALE, SSKEW, CSKEW, SDECMC, CDECMC
C
      EQUIVALENCE  (IPRALL(1),  PRJ_CODE)
      EQUIVALENCE  (IPRALL(2),  PRC_CODE)
      EQUIVALENCE  (IPRALL(3),  PRJ_USAMP)
      EQUIVALENCE  (IPRALL(5),  PRJ_SKEW)
      EQUIVALENCE  (IPRALL(7),  PRJ_RAMAP)
      EQUIVALENCE  (IPRALL(9),  PRJ_DECMAP)
      EQUIVALENCE  (IPRALL(11), PRJ_REFDAT)
      EQUIVALENCE  (IPRALL(13), PRJ_RAMC)
      EQUIVALENCE  (IPRALL(15), PRJ_DECMC)
      EQUIVALENCE  (IPRALL(17), PRJ_EPOCH)
      EQUIVALENCE  (IPRALL(19), USCALE)
      EQUIVALENCE  (IPRALL(21), VSCALE)
      EQUIVALENCE  (IPRALL(23), SSKEW)
      EQUIVALENCE  (IPRALL(25), CSKEW)
      EQUIVALENCE  (IPRALL(27), SDECMC)
      EQUIVALENCE  (IPRALL(29), CDECMC)
C
