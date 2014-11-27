*+WRREDT

       SUBROUTINE WRREDT (IUNIT, IPRINT, STATUS)
C      -----------------------------------------
C
C  Writes redtape to a map file.
C
C  Given:
C      IUNIT     integer     logical unit number
C      IPRINT    integer     print control
C
C  Returned:
C      STATUS    integer     status value
C
C  Writes the current redtape common blocks to disc as redtape for the
C  map or aperture currently opened for 'WRITE' on unit IUNIT.
C  If IPRINT=1, the redtape title will be printed on the output device.
C  Any extra redtape pages are also written to the file.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 18 January 90)
C  (DJT, 1 April 92; Unix implementation)
C
*-
       INTEGER    IUNIT, IW, IPRINT, STATUS
       INTEGER    IBLK, NWD, MODE, TERMNO
       INTEGER    I, L, IOUT
       CHARACTER  USER*16
C
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_extrart.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL IO_ENQOUT(IOUT)
       CALL IO_ENQEXE(USER,MODE,TERMNO)
       IF (USER.EQ.'BATCH-USER' .OR.
     :     USER.EQ.'SYSTEM' .OR. USER.EQ.'RT') THEN
         USER=RTUSER
       ENDIF
C
C  Single valued maps only supported in Unix implementation
C
       IW=1
C
C  Check redtape and component number
C
c      IF (STATUS.EQ.0) THEN
c        DO I=1,20
c          L=CHR_LENB(RTITLE(I))
c          IF (L.GT.0) WRITE(IOUT,*) '  ',RTITLE(I)(1:L)
c        ENDDO
c        WRITE(IOUT,*)IW,IWMAX
c      ENDIF
       CALL CHREDT(STATUS)
       IF (STATUS.EQ.0) THEN
         IF (IW.LT.1 .OR. IW.GT.IWMAX) STATUS=ILL_COMPNO
       ENDIF
C
       IF (STATUS.EQ.0) THEN
C
C    Update current user
C
         RTOWNR=USER
         RTUSER=USER
C
C    Write redtape
C
         IWMAP=IW
         NWD=PAGE
         IBLK=(IW-1)*IPCOMP + 1
         CALL IO_WRFILE(IUNIT,IBLK,IRTALL,NWD,STATUS)
C
C    Write extra redtape pages, if any
C
         IF (IPEXRT.GT.0) THEN
           IF (IPEXRT.LE.MAX_EXTRA) THEN
             NWD=IPEXRT*PAGE
             IBLK=IWMAX*IPCOMP + 1
             CALL IO_WRFILE(IUNIT,IBLK,EXTRART,NWD,STATUS)
           ELSE
             STATUS=ILL_EXREDT
           ENDIF
         ENDIF
C
       ENDIF
C
C  Print redtape title
C
       IF (STATUS.EQ.0 .AND. IPRINT.GT.0) THEN
         DO I=1,20
           L=CHR_LENB(RTITLE(I))
           IF (L.GT.0) WRITE(IOUT,*) '  ',RTITLE(I)(1:L)
         ENDDO
         WRITE(IOUT,*)
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine WRREDT')
C
       END
