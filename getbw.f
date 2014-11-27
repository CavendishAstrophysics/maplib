

*+GETBW

       SUBROUTINE GETBW (PROMPT, DEFAULT, HPFBW, STATUS)
C      -------------------------------------------------
C
C  Prompts for beamwidths.
C
C  Given:
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C
C  Returned:
C      HPFBW     real*8(2)   half-power full beamwidths (arcsec)
C      STATUS    integer     status
C
C  Prompts on the input device for half-power full beamwidths in U and V,
C  specified in arcsecs, either as a single value in U, or two separate
C  values.  If the U beamwidth only is specified, the V beamwidth is
C  calculated according to the map projection and map centre defined in
C  the current redtape, to provide a circular beam.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.
C
C  (DJT, 22 May 87)
C
*-
       CHARACTER*(*)  PROMPT, DEFAULT
       CHARACTER*20   STRING
       REAL*8         HPFBW(2), HPFBW1, HPFBW2
       INTEGER        LS, STATUS
C
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/maplib_redtape.inc'
C
       CHARACTER*18  ERRMSG
       PARAMETER   ( ERRMSG = 'invalid beamwidths' )
C
    1  IF (STATUS.EQ.0) THEN
C
         WRITE(STRING,'(2F8.2)')HPFBW
C
C    Read beamwidths from command line
C
         CALL IO_GETWRD(PROMPT,DEFAULT,STRING,LS,STATUS)
         IF (LS.GT.0) THEN
           CALL CHR_CHCTOD(STRING(1:LS),HPFBW1,STATUS)
           CALL IO_NXTWRD(STRING,LS,STATUS)
           IF (LS.GT.0) THEN
             CALL CHR_CHCTOD(STRING(1:LS),HPFBW2,STATUS)
           ELSE
             HPFBW2=HPFBW1
             IF (IPROJ.EQ.2 .OR. IPROJ.EQ.3) THEN
               HPFBW2=HPFBW2/SIN(DECMAP)
             ENDIF
             LS=1
           ENDIF
C
C    Check for validity
C
           IF (STATUS.EQ.BAD_SYNTAX .OR.
     :         HPFBW1.LT.0.0 .OR. HPFBW2.LT.0.D0) THEN
             STATUS=BAD_SYNTAX
             CALL IO_WRMSG(STATUS,ERRMSG)
             CALL IO_SETCLI(' ')
             GOTO 1
           ENDIF
C
           HPFBW(1)=HPFBW1
           HPFBW(2)=HPFBW2
C
         ENDIF
       ENDIF
C
       END
