


*+STPROJ

       SUBROUTINE STPROJ (IPROJ, IPREC, USAMP, SKEW, RAMAP, DECMAP,
     :                                         REFDAT, EPOCH, STATUS)
C      --------------------------------------------------------------
C
C  Sets up current map projection parameters.
C
C  Given:
C      IPROJ     integer     projection code (1,2,3 for equatorial,
C                                        sky coordinates or tangent plane)
C      IPREC     integer     precession code (see below)
C      USAMP     real*8      U-coordinate sampling at map centre
C                                                       (arcsec/gridpoint)
C      SKEW      real*8      skew angle of U,V plane (radians)
C      RAMAP     real*8      RA of map centre at reference date (radians)
C      DECMAP    real*8      Dec of map centre at reference date (radians)
C      REFDAT    real*8      reference date (years)
C      IPREC     integer     precession code (see below)
C      EPOCH     real*8      epoch of projection (years)
C
C  Returned:
C      STATUS    integer     status value
C
C  Sets up parameters to specify the map projection to be used by the
C  coordinate conversion routines RDTOUV and UVTORD, converting between
C  map grid points and spherical coordinates.  Note that these parameters
C  will normally be initialised using the values from section 4 of the
C  redtape common blocks (STPROJ is called by routine RDREDT).
C
C  Details of the projection are made available to the conversion routines
C  via a common block (defined in (LIBRARY)MAPLIB-PROJ:INCL) which includes
C  the coordinates of the map centre precessed to the epoch of projection.
C  The precession code IPREC identifies the calculation used to produce
C  the precessed coordinates:
C
C      IPREC = 0,  simple steady precession (routine PRECES, valid only
C                  within the area of the map for differential precession
C                  with respect to the map centre).
C      IPREC = 1,  full precession with respect to B1950.0 (routine PRECRD)
C                  including aberration and nutation.  This is the usual
C                  precession calculation used for setting up map redtape.
C
C  The STATUS value should be zero on entry.  Possible error codes are:
C
C      - invalid projection parameters (ILL_PROJN)
C      - invalid precession code (ILL_PRECD)
C
C  (DJT, 28 October 87)
C
*-
       INTEGER  IPROJ, IPREC, STATUS
       REAL*8   USAMP, SKEW, RAMAP, DECMAP, REFDAT, EPOCH
C
       include '/mrao/include/constants.inc'
       include '/mrao/include/maplib_proj.inc'
       include '/mrao/include/maplib_errors.inc'
C
      IF (STATUS.NE.0) RETURN
C
C  Check validity : if map centre is North-Pole, reference date should
C  be set equal to projection epoch.
C
      IF (ABS(DECMAP).GE.(CONST_PIBY2-1.D-6)) THEN
          IF (REFDAT.NE.EPOCH) STATUS=ILL_PROJN
      ELSE IF (IPREC.EQ.1 .AND. REFDAT.NE.1950.D0) THEN
C         Reference date should be B1950.0 for IPREC=1
          STATUS = ILL_PROJN
      END IF
C
      IF (STATUS.EQ.0) THEN
          PRJ_CODE   = IPROJ
          PRC_CODE   = IPREC
          PRJ_USAMP  = USAMP
          PRJ_SKEW   = SKEW
          PRJ_RAMAP  = RAMAP
          PRJ_DECMAP = DECMAP
          PRJ_REFDAT = REFDAT
          PRJ_EPOCH  = EPOCH

C  Perform precession to provide map centre coordinates at epoch
C
          IF (EPOCH.EQ.REFDAT) THEN
              PRJ_RAMC  = RAMAP
              PRJ_DECMC = DECMAP
          ELSE IF (PRC_CODE.LE.0) THEN
              CALL PRECES(RAMAP,DECMAP,PRJ_RAMC,PRJ_DECMC,EPOCH-REFDAT)
          ELSE IF (PRC_CODE.EQ.1) THEN
              CALL PRECRD(REFDAT,RAMAP,DECMAP,EPOCH,PRJ_RAMC,PRJ_DECMC)
          ELSE
              STATUS=ILL_PRECD
          END IF

C         Set up calculation variables for coordinate conversion

C         U and V conversion factors to:
C         1.  Convert the map scale to 1 radian/gridpoint. (this is the
C             true scale at the Pole for sky and equatorial maps and at
C             the map centre for tangent plane maps.)
C         2.  Reflect the axes so that u axis points towards increasing
C             RA and the v axis points towards the nearest pole for
C             equatorial and sky coordinates (otherwise North is 'up')

          USCALE =-USAMP * CONST_SA2R
          VSCALE =-USCALE
          IF (IPROJ .EQ. 2) VSCALE = VSCALE*DSIN( PRJ_DECMC )
          IF (PRJ_DECMC .LT. 0.0D+0 .AND. IPROJ .NE. 3) VSCALE = -VSCALE

C         Sine and cosine of skew angle to rotate axes so RA is vertical
C           - signs are important because the definition of skew for sky
C             and equatorial projections is unconventional.
          IF (PRJ_CODE.EQ.3) THEN
              SSKEW =-DSIN( SKEW )
              CSKEW = DCOS( SKEW )
          ELSE
              SSKEW = DSIN( SKEW )
              CSKEW = DCOS( SKEW )
          END IF

C         Sine and cosine of the dec of the map centre.
          SDECMC = DSIN( PRJ_DECMC )
          CDECMC = DCOS( PRJ_DECMC )
      END IF
C
      IF (STATUS.NE.0) CALL MAPERR(STATUS, 'in routine STPROJ')
C
      END
