


*+PRMAPC

       SUBROUTINE PRMAPC (IUNIT, STATUS)
C      ---------------------------------
C
C  Prints map centre coordinates from redtape.
C
C  Given:
C      IUNIT     integer     logical unit number
C      STATUS    integer     status value
C
C  Prints out the map centre coordinates on the output unit IUNIT.  The
C  coordinates of the map centre at date of observation and at reference
C  date are read from the redtape common blocks and printed out using a
C  standard format.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 13 October 86)
C
*-
       INTEGER    IUNIT, STATUS
       INTEGER    PRSEC, LR, LD
       CHARACTER  CHRA*12, CHDEC*12
C
       include '/mrao/include/constants.inc'
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS.EQ.0) THEN
C
         PRSEC=1
         WRITE(IUNIT,*)
C
         CALL CHR_CHDTOS(RAMAP/CONST_H2R,PRSEC,CHRA,LR)
         CALL CHR_CHDTOS(DECMAP/CONST_D2R,PRSEC,CHDEC,LD)
         CALL CHR_CHLJUS(CHRA(1:LR),LR)
         CALL CHR_CHLJUS(CHDEC(1:LD),LD)
         WRITE(IUNIT,'(X,A,F6.1,A,2(2X,A))')
     :    'Map centre  (',REFDAT,')',CHRA(1:LR),CHDEC(1:LD)
C
         CALL CHR_CHDTOS(RAOBS/CONST_H2R,PRSEC,CHRA,LR)
         CALL CHR_CHDTOS(DECOBS/CONST_D2R,PRSEC,CHDEC,LD)
         CALL CHR_CHLJUS(CHRA(1:LR),LR)
         CALL CHR_CHLJUS(CHDEC(1:LD),LD)
         WRITE(IUNIT,'(13X,A,F6.1,A,2(2X,A))')
     :                '(',OBSDAT,')',CHRA(1:LR),CHDEC(1:LD)
C
       ENDIF
C
       END
