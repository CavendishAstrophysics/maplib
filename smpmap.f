


*+SMPMAP

       SUBROUTINE SMPMAP (DATA1, IUV, NS, DATA2, NX, NY, STATUS)
C      ---------------------------------------------------------
C
C  Samples and windows map data.
C
C  Given:
C      DATA1     real*4()    input map data
C      IUV       integer(4)  U,V coordinate window
C      NS        integer     sampling frequency
C
C  Returned:
C      DATA2     real*4()    output map data
C      NX        integer     output row length
C      NY        integer     number of rows in output data
C      STATUS    integer     status value
C
C  Samples the map data held by rows in array DATA1, and produces an
C  output data array DATA2 containing rows of the map area specified
C  by the window IUV (IU1,IU2,IV1,IV2), and sampled every NS points.
C  The redtape common blocks are assumed to be set up appropriately
C  for the input data array, and are not updated.
C
C  The STATUS value should be zero on entry, and is normally unchanged
C  on exit.  Possible error conditions are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C
C  (DJT, 13 October 86)
C
*-
       REAL*4     DATA1(1), DATA2(1)
       INTEGER    IUV(4), NS, NX, NY, STATUS
       INTEGER    IU, IU1, IU2, IV, IV1, IV2
       INTEGER*4  IX1, IX2
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
C  Adjust limits to sample u,v points modulo NS
C
         IU1=(IU1/NS)*NS
         IV1=(IV1/NS)*NS
         IF (IU1.LT.IUV(1)) IU1=IU1+NS
         IF (IV1.GT.IUV(3)) IV1=IV1-NS
C
C  Sample the U,V window, adding data to the output array
C
         IX2=0
         DO IV=IV1,IV2,-NS
           IX1=(IVMAP1-IV)*IXMAX+(IU1-IUMAP1)-NS+1
           DO IU=IU1,IU2,NS
             IX1=IX1+NS
             IX2=IX2+1
             DATA2(IX2)=DATA1(IX1)
           ENDDO
         ENDDO
C
         NX=(IU2-IU1)/NS+1
         NY=(IV1-IV2)/NS+1
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine SMPMAP')
C
       END
