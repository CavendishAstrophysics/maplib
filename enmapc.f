


*+ENMAPC

       SUBROUTINE ENMAPC (EN_RAMAP, EN_DECMAP, EN_REFDAT,
     :                              EN_OBSDAT, EN_SOURCE, STATUS)
C      ----------------------------------------------------------
C
C  Returns map centre coordinates from redtape.
C
C  Returned:
C      EN_RAMAP    real*8      RA of map centre (radians)
C      EN_DECMAP   real*8      Dec of map centre (radians)
C      EN_REFDAT   real*8      reference date (years)
C      EN_OBSDAT   real*8      date of observation (years)
C      EN_SOURCE   char*(*)    source name
C      STATUS      integer     status value
C
C  The coordinates of the map centre are returned from 'astronomical'
C  redtape (section 4), expressed with respect to the reference date.
C  The reference date will normally be 1950.0, unless the map centre
C  is the North or South pole (ie. abs(dec)=const_pi/2), in which case
C  the reference date should equal the epoch of the map projection.
C  The source name is read from the map title.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 10 November 87)
C
*-
       CHARACTER  EN_SOURCE*(*), STRING*40
       REAL*8     EN_RAMAP, EN_DECMAP, EN_REFDAT, EN_OBSDAT
       INTEGER    I, I1, CHR_INTLC, INDEX, STATUS
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS .NE. 0) RETURN
C
       EN_RAMAP  = RAMAP
       EN_DECMAP = DECMAP
       EN_REFDAT = REFDAT
       EN_OBSDAT = OBSDAT
       EN_SOURCE = ' '
C
C  Extract source name from title
C
       DO I=1,20
         STRING=RTITLE(I)
         CALL CHR_CHUCAS(STRING)
         IF (INDEX(STRING,'SOURCE TITLE').NE.0) THEN
           I1 = INDEX(RTITLE(I),':')
           I1 = chr_INTLC(RTITLE(I)(I1+1:))+I1
           EN_SOURCE = RTITLE(I)(I1:)
         ENDIF
       ENDDO
C
       END
