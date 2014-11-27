*+PRHIST

       SUBROUTINE PRHIST (IUNIT, STATUS)
C      ---------------------------------
C
C  Prints map processing history from redtape.
C
C  Given:
C      IUNIT     integer     logical unit number
C      STATUS    integer     status value
C
C  Prints out details of the map processing history from section 6
C  of the current map redtape, using a standard format.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 19 November 86)
C
*-
       INTEGER  IUNIT, STATUS
       INTEGER  IDATE(3), IHIST, LD, LP, CHR_LENB
       CHARACTER  PROGNAM*12, DATE*9
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS.NE.0) RETURN
C
       IF (NHIST.GT.0) THEN
C
         WRITE(IUNIT,*)
         DO IHIST=1,NHIST
           READ(HISTRY(IHIST),'(I2,X,I2,X,I2)') IDATE
           CALL CHR_CHDATE(IDATE,0,DATE,LD)
           PROGNAM='('//HISTRY(IHIST)(9:)
           LP=CHR_LENB(PROGNAM)
           PROGNAM(LP+1:)=')'
           IF (IHIST.EQ.1) THEN
             WRITE(IUNIT,*)'  Map created  ',PROGNAM,DATE(1:LD)
           ELSE
             WRITE(IUNIT,*)'  Map updated  ',PROGNAM,DATE(1:LD)
           ENDIF
         ENDDO
         WRITE(IUNIT,*)
C
       ENDIF
C
       END
