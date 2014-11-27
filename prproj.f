



*+PRPROJ

       SUBROUTINE PRPROJ (IUNIT, STATUS)
C      ---------------------------------
C
C  Prints out map projection parameters.
C
C  Given:
C      IUNIT     integer     logical unit number
C      STATUS    integer     status value
C
C  Prints the parameters describing the current map projection set up
C  by routine STPROJ, for coordinate conversion between map grid points
C  and spherical coordinates, to the output unit IUNIT.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 15 October 86)
C
*-
       INTEGER    IUNIT, STATUS
       INTEGER    PRSEC, LR, LD
       REAL*8     USAMP, VSAMP
       CHARACTER  CHRA*12, CHDEC*12
C
       include '/mrao/include/constants.inc'
       include '/mrao/include/maplib_proj.inc'
C
       IF (STATUS.NE.0) RETURN
C
       PRSEC=1
       WRITE(IUNIT,*)
       CALL CHR_CHDTOS(PRJ_RAMAP/CONST_H2R,PRSEC,CHRA,LR)
       CALL CHR_CHDTOS(PRJ_DECMAP/CONST_D2R,PRSEC,CHDEC,LD)
       WRITE(IUNIT,'(X,A,F7.2,A,A,A)')
     :   'Map centre  (',PRJ_REFDAT,')  ',CHRA(1:LR),CHDEC(1:LD)
C
       USAMP=PRJ_USAMP
       VSAMP=PRJ_USAMP
       IF (PRJ_CODE.EQ.1) VSAMP=VSAMP/DSIN(PRJ_DECMC)
       WRITE(IUNIT,'(X,A,F9.4,F10.4)')
     :   'Sampling (arcsec/gridpoint)',USAMP,VSAMP
C
       IF (PRJ_CODE.EQ.1 .OR. PRJ_CODE.EQ.2) THEN
         WRITE(IUNIT,'(X,A,F10.4)')
     :     'Skew angle, hour angle of map centre',PRJ_SKEW
       ELSEIF (PRJ_CODE.EQ.3) THEN
         WRITE(IUNIT,'(X,A,F10.4)')
     :     'Skew angle, position angle of v-axis',PRJ_SKEW
       ENDIF
C
       WRITE(IUNIT,*)
       IF (PRJ_CODE.EQ.1) THEN
         WRITE(IUNIT,'(X,A)')
     :     'Equatorial plane projection (aerial coordinates)'
       ELSEIF (PRJ_CODE.EQ.2) THEN
         WRITE(IUNIT,'(X,A)')
     :     'Equatorial plane projection (sky coordinates)'
       ELSEIF (PRJ_CODE.EQ.3) THEN
         WRITE(IUNIT,'(X,A)')'Tangent plane projection'
       ENDIF
C
       WRITE(IUNIT,'(X,A,F8.2)')'Epoch of projection ',PRJ_EPOCH
C
       END
