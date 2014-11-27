


*+ENSRCN

       SUBROUTINE ENSRCN ( EN_SOURCE, STATUS )
C      ---------------------------------------
C
C  Returns source name from the redtape
C
C  Returned:
C      EN_SOURCE   char*(*)    source name
C      STATUS      integer     status value
C
C  P.Alexander  09/04/92
*-
       CHARACTER  EN_SOURCE*(*), STRING*40
       INTEGER    I, I1, CHR_INTLC, INDEX, STATUS
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS .NE. 0) RETURN
C
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
