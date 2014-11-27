


*+STMAPJ

       SUBROUTINE STMAPJ (ST_IPROJ, ST_USAMP, ST_SKEW,
     :                              ST_EPOCH, ST_PRANG, STATUS)
C      ----------------------------------------------------------
C
C  Sets map projection parameters in redtape.
C
C  Given:
C      ST_IPROJ    integer     projection code (1,2,3 for equatorial,
C                                      sky coordinates or tangent plane)
C      ST_USAMP    real*8      U-coordinate sampling at map centre
C                                                     (arcsec/gridpoint)
C      ST_SKEW     real*8      skew angle of U,V plane (radians)
C      ST_EPOCH    real*8      epoch of projection (years)
C      ST_PRANG    real*8      precession angle between epoch and 1950.0
C
C  Returned:
C      STATUS      integer     status value
C
C  The coordinates of the map projection are set up in 'astronomical'
C  redtape (section 4), using the values provided.  The map title
C  will also be updated.
C
C  The map centre will be updated if REFDAT is equal to EPOCH, but
C  not equal to 1950.0D+0 on entry.
C
C  The STATUS value should be zero on entry, but will be reset to
C  ILL_PROJN if the parameters are invalid.
C
C  (DJT, 10 November 87)
C  (SEGH, 20 July 98: correctly sets VSAMP for IPROJ=1, EPOCH=1950.0D0)
C
*-
       REAL*8         ST_USAMP, ST_SKEW, ST_EPOCH, ST_PRANG
       INTEGER        ST_IPROJ, STATUS
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/constants.inc'
C
       CHARACTER      STRING*40, TEXT*16
       INTEGER        I, LT, CHR_LENB
       LOGICAL        MCCHNG
C
       IF (STATUS .NE. 0) RETURN
C
C  See if the map centre has to be changed because of a change in epoch.
C
       MCCHNG=(ST_EPOCH.NE.EPOCH  .AND.
     *         DABS(DECMAP).GE.(CONST_PIBY2-1.0D-6) )

C
C  Check validity of given parameters
C
       IF (ST_IPROJ.EQ.0) THEN
         TEXT='Unknown'
       ELSEIF (ST_IPROJ.EQ.1) THEN
         TEXT='Ae coordinates'
       ELSEIF (ST_IPROJ.EQ.2) THEN
         TEXT='Sky coordinates'
       ELSEIF (ST_IPROJ.EQ.3) THEN
         TEXT='Tangent plane'
       ELSE
         STATUS=ILL_PROJN
       ENDIF
C
       IF (STATUS.EQ.0) THEN
C
C    Update redtape values
C
         IPROJ = ST_IPROJ
         USAMP = ST_USAMP
         VSAMP = ST_USAMP
         EPOCH = ST_EPOCH
         SKEW  = ST_SKEW
         PRANG = ST_PRANG
C    Set VSAMP for IPROJ=1, remembering to test for EPOCH 1950.0D0 case
         IF (IPROJ.EQ.1) THEN
           IF (EPOCH .EQ. 1950.0D+0) THEN
             IF (DECMAP.NE.0.0D+0) VSAMP = USAMP/DSIN(DECMAP)
           ELSE
             IF (DECOBS.NE.0.0D+0) VSAMP = USAMP/DSIN(DECOBS)
           ENDIF
         ENDIF
         IF (MCCHNG) CALL STMAPC(RAMAP,DECMAP,EPOCH,OBSDAT,' ',STATUS)
C
C    Update map title
C
         LT=CHR_LENB(TEXT)
         DO I=1,20
           STRING=RTITLE(I)
           CALL CHR_CHUCAS(STRING)
           IF (INDEX(STRING,'PROJECTION').NE.0) THEN
             WRITE(STRING,'(A,X,A,X,A,F6.1,A)')
     :                     'Projection   :',TEXT(1:LT),'(',EPOCH,')'
             RTITLE(I) = STRING
           ENDIF
         ENDDO
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine STMAPJ')
C
       END
