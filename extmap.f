


*+EXTMAP

       SUBROUTINE EXTMAP (DATA1, IUV, IEXTRA, DATA2, NX, NY, STATUS)
C      -------------------------------------------------------------
C
C  Extracts a map area.
C
C  Given:
C      DATA1     real*4()    input map data
C      IUV       integer(4)  U,V coordinate window
C      IEXTRA    integer     extra-point control
C
C  Returned:
C      DATA2     real*4()    output map data
C      NX        integer     output row length
C      NY        integer     number of rows in output data
C      STATUS    integer     status value
C
C  Windows the map data held by rows in array DATA1, and produces an
C  output data array DATA2 containing rows of the map area specified
C  by the window IUV (IU1,IU2,IV1,IV2).  The extra-point parameter may
C  be used to control whether extra points should be provided, if the
C  window extends beyond the input map area :
C
C      IEXTRA = 0,  extra points are not provided
C      IEXTRA = 1,  edge values are used to provide extra points
C      IEXTRA = 2,  the map zero level is used to provide extra points
C      IEXTRA = 3,  extra points are set to the 'undefined data' value
C
C  The redtape common blocks are assumed to be set up appropriately for
C  the input data array, and are not updated.
C
C  The STATUS value should be zero on entry,  but note that this will
C  be reset if the window extends beyond the map area.  Possible error
C  conditions are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C
C  (DJT, 2 December 86)
C
*-
       REAL*4     DATA1(1), DATA2(1)
       INTEGER    IUV(4), IEXTRA, NX, NY, STATUS
       INTEGER    IU, IU1, IU2, IUU, IV, IV1, IV2, IVV
       LOGICAL    UEXTRA, VEXTRA
       INTEGER*4  IX0, IX1, IX2
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Check the U,V window
C
       CALL CHCKUV(IUV,STATUS)
       IF (STATUS.EQ.0) THEN
C
         NX=0
         NY=0
         IU1=IUV(1)
         IU2=IUV(2)
         IV1=IUV(3)
         IV2=IUV(4)
C
C  Sample the U,V window, adding data to the output array
C
         IX2=0
         DO IV=IV1,IV2,-1
           IVV=IV
           VEXTRA=.FALSE.
C
C    Check for V-coordinate outside map
C
           IF (IV.GT.IVMAP1 .OR. IV.LT.IVMAP2) THEN
             IVV=MIN0(IVMAP1,MAX0(IVMAP2,IV))
             STATUS=UV_OUTMAP
             VEXTRA=.TRUE.
           ENDIF
           IX0=(IVMAP1-IVV)*IXMAX
C
           DO IU=IU1,IU2
             IUU=IU
             UEXTRA=.FALSE.
C
C      Check for U-coordinate outside map
C
             IF (IU.LT.IUMAP1 .OR. IU.GT.IUMAP2) THEN
               IUU=MAX0(IUMAP1,MIN0(IUMAP2,IU))
               STATUS=UV_OUTMAP
               UEXTRA=.TRUE.
             ENDIF
C
             IX2=IX2+1
             IX1=IX0+(IUU-IUMAP1)+1
             DATA2(IX2)=DATA1(IX1)
             IF (UEXTRA .OR. VEXTRA) THEN
               IF (IEXTRA.EQ.2) THEN
                 DATA2(IX2)=0.0
               ELSEIF (IEXTRA.EQ.3) THEN
                 DATA2(IX2)=BLANK
               ENDIF
             ENDIF
           ENDDO
         ENDDO
C
         NX=(IU2-IU1)+1
         NY=(IV1-IV2)+1
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine EXTMAP')
C
       END
