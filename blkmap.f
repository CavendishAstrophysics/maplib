

*+BLKMAP

       SUBROUTINE BLKMAP (DATA, IUV, VALUE, STATUS)
C      --------------------------------------------
C
C  Replaces undefined values within a map area.
C
C  Given:
C      DATA      real*4()    input map data
C      IUV       integer(4)  U,V coordinate window
C      VALUE     real        data value
C
C  Returned:
C      STATUS    integer     status value
C
C  Scans the map data held by rows in array DATA, and replaces all
C  undefined data values within the window IUV (IU1,IU2,IV1,IV2), by
C  the data value provided in parameter VALUE, in external units.
C  The redtape common blocks are assumed to be set up appropriately
C  for the input data array, and are not updated.
C
C  For aperture data, both the array and the value parameter should
C  be set up to provide complex values in the calling program.
C
C  The STATUS value should be zero on entry, and is normally unchanged
C  on exit.  Possible error conditions are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C
C  (DJT, 20 January 87)
C
*-
       REAL*4     DATA(1), VALUE(1)
       INTEGER    IUV(4), STATUS
       INTEGER    IU, IU1, IU2, IV, IV1, IV2
       INTEGER*4  IX, IW, NW
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
         IU1=IUV(1)
         IU2=IUV(2)
         IV1=IUV(3)
         IV2=IUV(4)
C
         NW=1
         IF (ISWD.EQ.4) NW=2
C
C  Scan the U,V window replacing undefined data values
C
         DO IV=IV1,IV2,-1
           IX=((IVMAP1-IV)*IXMAX+(IU1-IUMAP1))*NW-NW+1
           DO IU=IU1,IU2
             IX=IX+NW
             IF (DATA(IX).EQ.BLANK) THEN
               DO IW=1,NW
                 DATA(IX+IW-1)=VALUE(IW)
               ENDDO
             ENDIF
           ENDDO
         ENDDO
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine BLKMAP')
C
       END
