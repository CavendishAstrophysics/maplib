*+ADREDT

       SUBROUTINE ADREDT (KEY, STRING, STATUS)
C      ---------------------------------------
C
C  Adds text field to map redtape.
C
C  Given:
C      KEY       char*(*)    redtape field keyword
C      STRING    char*(*)    input text
C      STATUS    integer     status value
C
C  Adds new text to the appropriate item of the redtape common block,
C  identified by the KEY string:
C
C    KEY = 'CREATED'  updates 'Map created' text in the map title
C    KEY = 'HISTORY'  updates the redtape history record
C    KEY = 'TITLE'    updates the map title
C    KEY = 'OWNER'    updates the 'map owner' user name
C    KEY = 'USER'     updates the 'last used by' user name
C
C  For the 'CREATED' and 'HISTORY' keys, STRING should contain the name of
C  the processing program.  The 'CREATED' key will also update the processing
C  history in section 6.  The 'TITLE' key will simply add a line of text to
C  the first non-blank line in the map title (maximum of 20 lines). Other
C  keys identify a field within the title and insert the text in that field.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 2 December 86)
C
*-
       CHARACTER*(*)  KEY, STRING
       CHARACTER  TKEY*16, PROGNAM*12, DATE*9, TIME*8
       INTEGER    ITIME(7), CHR_INTLC, CHR_LENB
       INTEGER    STATUS, I, J, K, L
       LOGICAL    CHR_CHSAME, CHR_MATCH
C
       CHARACTER  STOKES(7)*4
       DATA  STOKES /'BEAM', 'I', 'Q', 'U', 'V', 'I-Q', 'I+Q'/
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS.NE.0) RETURN
C
       L=CHR_LENB(STRING)
       CALL UTIL_ENQTIM(ITIME(2))
       CALL UTIL_ENQDAT(ITIME(5))
       WRITE(DATE,'(I2,2(''/'',I2))')
     :   ITIME(5),ITIME(6),MOD(ITIME(7),100)
C
       TKEY=KEY
       CALL CHR_CHUCAS(TKEY)
C
C  Update 'Map created' text and history
C
       IF (TKEY.EQ.'CREATED') THEN
         PROGNAM=STRING(1:L)
         CALL CHR_CHUCAS(PROGNAM)
         NHIST=MIN0(15,NHIST+1)
         HISTRY(NHIST)=DATE(1:8)//PROGNAM
         CALL CHR_CHTIME(ITIME(2),TIME,L)
         CALL CHR_CHDATE(ITIME(5),0,DATE,L)
         L=MIN0(8,CHR_LENB(PROGNAM))
         RTITLE(11)='Map created ('//PROGNAM(1:L)//')'
         RTITLE(11)(25:)=TIME(1:5)//' '//DATE
C
C  Update history
C
       ELSEIF (TKEY.EQ.'HISTORY') THEN
         PROGNAM=STRING(1:L)
         CALL CHR_CHUCAS(PROGNAM)
         NHIST=MIN0(15,NHIST+1)
         HISTRY(NHIST)=DATE(1:8)//PROGNAM
         CALL CHR_CHTIME(ITIME(2),TIME,L)
         CALL CHR_CHDATE(ITIME(5),0,DATE,L)
         L=MIN0(8,CHR_LENB(PROGNAM))
         I=0
         DO WHILE (I.LE.20)
           IF (CHR_LENB(RTITLE(I)).EQ.0) THEN
             RTITLE(I)='Processed   ('//PROGNAM(1:L)//')'
             RTITLE(I)(25:)=TIME(1:5)//' '//DATE
             I=20
           ENDIF
           I=I+1
         ENDDO
C
C  Update title
C
       ELSEIF (TKEY.EQ.'TITLE') THEN
         I=1
         DO WHILE (I.LE.20)
           IF (CHR_LENB(RTITLE(I)).EQ.0) THEN
             RTITLE(I)=STRING
             I=20
           ENDIF
           I=I+1
         ENDDO
C
C  Update polarisation
C
       ELSEIF (TKEY.EQ.'POLN' .OR.
     :         TKEY.EQ.'STOKES' .OR.
     :         TKEY.EQ.'POLARISATION' .OR.
     :         TKEY.EQ.'POLARIZATION') THEN
         POLN=STRING(1:L)
         CALL CHR_CHUCAS(POLN)
         DO I=1,7
           IF (CHR_CHSAME(POLN,STOKES(I))) IPOLN=I-1
         ENDDO
         DO I=1,20
           IF (CHR_MATCH('Polarisation',RTITLE(I))) THEN
             K=INDEX(RTITLE(I),':')
             IF (K.GT.0) RTITLE(I)(K+2:)=STRING(1:L)
           ENDIF
         ENDDO
C
C  Update name of measured quantity
C
       ELSEIF (TKEY.EQ.'NAME') THEN
         NAME=STRING(1:L)
C
C  Update name of external units
C
       ELSEIF (TKEY.EQ.'UNITS' .OR. TKEY.EQ.'EXUNIT') THEN
         EXUNIT=STRING(1:L)
C
C  Update owner identifier and last user fields
C
       ELSEIF (TKEY.EQ.'OWNER') THEN
         RTOWNR=STRING
         RTUSER=STRING
         CALL CHR_CHUCAS(RTOWNR)
         CALL CHR_CHUCAS(RTUSER)
       ELSEIF (TKEY.EQ.'USER') THEN
         RTUSER=STRING
         CALL CHR_CHUCAS(RTUSER)
C
C  Update main title fields
C
       ELSE
         DO I=1,20
           IF (CHR_MATCH(TKEY,RTITLE(I))) THEN
             J=CHR_INTLC(STRING)
             K=INDEX(RTITLE(I),':')
             IF ((K+L-J).GE.40) K=K-1
             IF (K.GT.0) RTITLE(I)(K+2:)=STRING(J:L)
           ENDIF
         ENDDO
       ENDIF
C
       END
