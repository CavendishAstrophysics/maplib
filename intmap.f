


*+INTMAP

       SUBROUTINE INTMAP (DATA1, IUV, DATA2, NX, NY, STATUS)
C      -----------------------------------------------------
C
C  Interpolates map data to mid-points.
C
C  Given:
C      DATA1     real*4()    input map data
C      IUV       integer(4)  U,V coordinate window
C
C  Returned:
C      DATA2     real*4()    output map data
C      NX        integer     output row length
C      NY        integer     number of rows in output data
C      STATUS    integer     status value
C
C  Interpolates the map data held by rows in array DATA1, using a standard
C  sinT/T convolution, and produces an output data array DATA2 containing
C  rows of the map area specified by the window IUV (IU1,IU2,IV1,IV2),
C  interpolated to mid-points.  The redtape common blocks are assumed to
C  be set up appropriately for the input data array, and are not updated.
C
C  Deficiencies:
C    o  margins of windows are not extended properly before interpolation.
C    o  undefined data values encountered during interpolation will
C       cause the interpolated value also to be set undefined.
C
C  The STATUS value should be zero on entry, and is normally unchanged
C  on exit.  Possible error conditions are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C
C  (DJT, 5 September 88)
C
*-
       REAL*4     DATA1(1), DATA2(1)
       INTEGER    IUV(4), NX, NY, STATUS
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
C  Find the output row length and no.of rows
C
         NX=2*(IU2-IU1)+1
         NY=2*(IV1-IV2)+1
C
C  Spread the data from the U,V window to the output array,
C  leaving alternate points and alternate rows empty
C
         IX2=-1
         DO IV=IV1,IV2,-1
           IX1=(IVMAP1-IV)*IXMAX+(IU1-IUMAP1)
           DO IU=IU1,IU2
             IX1=IX1+1
             IX2=IX2+2
             DATA2(IX2)=DATA1(IX1)
           ENDDO
C
C    Interpolate intermediate points within each row
C
           CALL INTROW(DATA2(IX2-NX+1),1,NX)
           IX2=IX2+NX-1
         ENDDO
C
C    Interpolate intermediate rows
C
         CALL INTROW(DATA2,NX,NY)
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine INTMAP')
C
       END
