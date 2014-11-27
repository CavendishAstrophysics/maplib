

*+GETUV

       SUBROUTINE GETUV (PROMPT, DEFAULT, IUV, STATUS)
C      -----------------------------------------------
C
C  Prompts for a map U,V coordinate window.
C
C  Given:
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C
C  Returned:
C      IUV       integer(4)  U,V coordinate window
C      STATUS    integer     status
C
C  Prompts on the input device for a map area, specified as four integers,
C  (IU1,IU2,IV1,IV2).  If only one integer is entered then the square area
C  (-IU1,IU1,IU1,-IU1) is returned.  Note that the ordering convention
C   IU1<IU2 and IV1>IV2  is enforced if necessary.  If the redtape common
C  blocks contain parameters for a current map, then checks are also made
C  that the area lies within the map bounds.  If the input string is null,
C  the DEFAULT string is used as input.  If the default string is null or
C  '*', the current values of IUV are used as default.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C      - unexpected IO_SYSTEM I/O error, or user break
C
C  (DJT, 14 October 86)
C
*-
       CHARACTER*(*)  PROMPT, DEFAULT
       CHARACTER*20   STRING
       INTEGER        IUV(4), IU1, IU2, IV1, IV2, STATUS
       INTEGER        IU, IV, LS, ERRDEV
       LOGICAL        BATCH
C
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/maplib_redtape.inc'
C
    1  IF (STATUS.EQ.0) THEN
C
         WRITE(STRING,'(3(I4,'',''),I4)')IUV
C
C    Read U,V window from command line
C
         CALL IO_GETWRD(PROMPT,DEFAULT,STRING,LS,STATUS)
         IF (LS.GT.0) THEN
           CALL CHR_CHCTOI(STRING(1:LS),IU1,STATUS)
           CALL IO_NXTWRD(STRING,LS,STATUS)
           IF (LS.GT.0) THEN
             CALL CHR_CHCTOI(STRING(1:LS),IU2,STATUS)
             CALL IO_NXTWRD(STRING,LS,STATUS)
             CALL CHR_CHCTOI(STRING(1:LS),IV1,STATUS)
             CALL IO_NXTWRD(STRING,LS,STATUS)
             CALL CHR_CHCTOI(STRING(1:LS),IV2,STATUS)
           ELSE
             IU1=-IU1
             IU2=-IU1
             IV1=IU2
             IV2=IU1
             LS=1
           ENDIF
C
C    Check for validity
C
           IF (LS.EQ.0 .OR. STATUS.EQ.BAD_SYNTAX) THEN
             CALL IO_WROUT('*** bad syntax,  U1,U2,V1,V2  wanted')
             STATUS=ILL_UVWIND
           ELSE
C
C    Enforce IU1 < IU2 and IV1 > IV2
C
             IF (IU1.GT.IU2) THEN
               IU=IU1
               IU1=IU2
               IU2=IU
             ENDIF
             IF (IV1.LT.IV2) THEN
               IV=IV1
               IV1=IV2
               IV2=IV
             ENDIF
C
C    Check against map bounds
C
             IF (RTHDR(1:3).EQ.'MAP') THEN
               IF (IU1.LT.IUMAP1 .OR. IU2.GT.IUMAP2 .OR.
     :             IV1.GT.IVMAP1 .OR. IV2.LT.IVMAP2 ) THEN
                 CALL IO_ENQERR( ERRDEV )
                 WRITE(ERRDEV,'(X,A,4I6)')
     :             '*** area outside map, restrict to :',
     :                                  IUMAP1,IUMAP2,IVMAP1,IVMAP2
                 STATUS=UV_OUTMAP
               ENDIF
             ENDIF
           ENDIF
C
           IF (STATUS.EQ.ILL_UVWIND .OR. STATUS.EQ.UV_OUTMAP) THEN
             CALL IO_ENQBCH( BATCH )
             IF (.NOT.BATCH) STATUS=0
             CALL IO_SETCLI(' ')
             GOTO 1
           ENDIF
C
           IUV(1)=IU1
           IUV(2)=IU2
           IUV(3)=IV1
           IUV(4)=IV2
C
         ENDIF
       ENDIF
C
       END
