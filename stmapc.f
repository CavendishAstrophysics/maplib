

*+STMAPC

       SUBROUTINE STMAPC (ST_RAMAP, ST_DECMAP, ST_REFDAT,
     :                              ST_OBSDAT, ST_SOURCE, STATUS)
C      ----------------------------------------------------------
C
C  Sets map centre coordinates in redtape.
C
C  Given:
C      ST_RAMAP    real*8      RA of map centre (radians)
C      ST_DECMAP   real*8      Dec of map centre (radians)
C      ST_REFDAT   real*8      reference date (years)
C      ST_OBSDAT   real*8      date of observation (years)
C      ST_SOURCE   char*(*)    source name
C
C  Returned:
C      STATUS      integer     status value
C
C  The coordinates of the map centre are set up in 'astronomical' redtape,
C  (section 4), using the values of RA and Dec provided.  These should be
C  expressed in coordinates of reference date; normally 1950.0, unless
C  the map centre is the North or South pole (ie. abs(dec)=const_pi/2),
C  in which case the reference date should equal the epoch of the map
C  projection.
C
C  The RA and Dec are precessed to the observation date to give RAOBS
C  and DECOBS.
C
C  If the source name is non-blank then the source title is updated.
C
C  The STATUS value should be zero on entry, but will be reset to
C  ILL_PROJN if the reference date is invalid.
C
C  (DJT, NPR, 10 November 87)
C  (SEGH, 20 July 98: correctly sets VSAMP for IPROJ=1, EPOCH=1950.0D0)
C
*-
       REAL*8         ST_RAMAP, ST_DECMAP, ST_REFDAT, ST_OBSDAT
       CHARACTER*(*)  ST_SOURCE
       INTEGER        STATUS
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/constants.inc'
C
       CHARACTER      STRING*40, CHRA*12, CHDEC*12
       CHARACTER*3    MONTH(12)
       INTEGER        I, LD, LR, CHR_LENB, PRSEC, IDATE(3)
       DATA   MONTH / 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     *                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'  /
C
       IF (STATUS.NE.0) RETURN
C
C  Check validity of reference date
C
       IF (ABS(ST_DECMAP).GE.(CONST_PIBY2-1.D-6)) THEN
          IF (ST_REFDAT.NE.EPOCH) STATUS = ILL_PROJN
       ELSEIF (ST_REFDAT.NE.1950.0D+0) THEN
          IF (ST_REFDAT.EQ.EPOCH) THEN
             CALL PRECRD(ST_REFDAT,ST_RAMAP,ST_DECMAP,
     :                                             1950.D0,RAMAP,DECMAP)
             ST_REFDAT = 1950.D0
             ST_RAMAP  = RAMAP
             ST_DECMAP = DECMAP
          ELSE
             STATUS = ILL_PROJN
          ENDIF
       ENDIF
C
       IF (STATUS.NE.0) GOTO 1
C
C  Update redtape values
C
       RAMAP  = ST_RAMAP
       DECMAP = ST_DECMAP
       REFDAT = ST_REFDAT
       OBSDAT = ST_OBSDAT
C
C  Update coordinates at observation date
C
       IF (ABS(DECMAP).GE.(CONST_PIBY2-1.D-6)) THEN
          DECMAP = DSIGN( CONST_PIBY2, DECMAP )
          RAOBS  = RAMAP
          DECOBS = DECMAP
       ELSE
          CALL PRECRD(REFDAT,RAMAP,DECMAP,OBSDAT,RAOBS,DECOBS)
       ENDIF
C
C  Update sampling in V if necessary
C   (remembering to test for EPOCH 1950.0D+0 case...)
C
       IF (IPROJ.EQ.1) THEN
         IF (EPOCH.EQ.1950.0D+0) THEN
           IF (DECMAP.NE.0.0D+0) VSAMP = USAMP/DSIN(DECMAP)
         ELSE
           IF (DECOBS.NE.0.0D+0) VSAMP = USAMP/DSIN(DECOBS)
         ENDIF
       ENDIF

C
C  Update map title
C
C      Update map centre in title.
       PRSEC=0
       CALL CHR_CHDTOS(RAMAP/CONST_H2R,PRSEC,CHRA,LR)
       CALL CHR_CHDTOS(DECMAP/CONST_D2R,PRSEC,CHDEC,LD)
       CALL CHR_CHLJUS(CHRA(1:LR),LR)
       CALL CHR_CHLJUS(CHDEC(1:LD),LD)
C
       DO I=1,20
         STRING=RTITLE(I)
         CALL CHR_CHUCAS(STRING)
         IF (INDEX(STRING,'MAP CENT').NE.0) THEN
           WRITE(STRING,'(F6.1,A,2X,A,2X,A)')
     :                    REFDAT,' map centre :',CHRA(1:LR),CHDEC(1:LD)
           RTITLE(I) = STRING
         END IF
       END DO
C
C      Update observation centre in title.
       PRSEC=0
       CALL CHR_CHDTOS(RAOBS/CONST_H2R,PRSEC,CHRA,LR)
       CALL CHR_CHDTOS(DECOBS/CONST_D2R,PRSEC,CHDEC,LD)
       CALL CHR_CHLJUS(CHRA(1:LR),LR)
       CALL CHR_CHLJUS(CHDEC(1:LD),LD)
C
       DO I=1,20
         STRING=RTITLE(I)
         CALL CHR_CHUCAS(STRING)
         IF (INDEX(STRING,'OBS CENT').NE.0) THEN
           WRITE(STRING,'(F6.1,A,2X,A,2X,A)')
     :                    OBSDAT,' obs centre :',CHRA(1:LR),CHDEC(1:LD)
           RTITLE(I) = STRING
         END IF
       END DO

C      Update observation date
       CALL FRDDAT( OBSDAT, IDATE )
       WRITE(STRING,'(I2,X,A,X,I4)')IDATE(1),MONTH(IDATE(2)),IDATE(3)
       CALL ADREDT('Observation', STRING, STATUS)

C      Update source title
       IF (CHR_LENB(ST_SOURCE).NE.0) THEN
          CALL ADREDT('Source title', ST_SOURCE, STATUS)
       ENDIF
C
    1  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine STMAPC')
C
       END
