




*+ENPROJ

       SUBROUTINE ENPROJ (IPROJ, IPREC, USAMP, SKEW, RAMAP, DECMAP,
     :                                               REFDAT, EPOCH)
C      ------------------------------------------------------------
C
C  Returns current map projection parameters.
C
C  Returned:
C      IPROJ     integer     projection code (1,2,3 for equatorial,
C                                        sky coordinates or tangent plane)
C      IPREC     integer     precession code (see below)
C      USAMP     real*8      U-coordinate sampling at map centre
C                                                       (arcsec/gridpoint)
C      SKEW      real*8      skew angle of U,V plane (radians)
C      RAMAP     real*8      RA of map centre at reference date (radians)
C      DECMAP    real*8      Dec of map centre at reference date (radians)
C      REFDAT    real*8      reference date (years)
C      EPOCH     real*8      epoch of projection (years)
C
C  Returns parameters specifying the map projection used by the coordinate
C  conversion routines RDTOUV and UVTORD, converting between map grid points
C  and spherical coordinates.  Details of the projection are held in a
C  common block defined by the include file (LIBRARY)MAPLIB-PROJ:INCL.
C
C  The precession code IPREC is included to identify the calculation
C  used to produce map centre coordinates at the epoch of projection:
C
C      IPREC = 0,  simple steady precession (routine PRECES, valid only
C                  within the area of the map for differential precession
C                  with respect to the map centre).
C      IPREC = 1,  full precession with respect to B1950.0 (routine PRECRD)
C                  including aberration and nutation.  This is the usual
C                  precession calculation used for setting up map redtape.
C
C  (DJT, 26 October 87)
C
*-
       INTEGER  IPROJ, IPREC
       REAL*8   USAMP, SKEW, RAMAP, DECMAP, REFDAT, EPOCH
C
       include '/mrao/include/maplib_proj.inc'
C
       IPROJ=PRJ_CODE
       IPREC=PRC_CODE
       USAMP=PRJ_USAMP
       SKEW=PRJ_SKEW
       RAMAP=PRJ_RAMAP
       DECMAP=PRJ_DECMAP
       REFDAT=PRJ_REFDAT
       EPOCH=PRJ_EPOCH
C
       END
