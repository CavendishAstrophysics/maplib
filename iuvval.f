

*+IUVVAL

       SUBROUTINE IUVVAL (DATA, IU, IV, VALUE, STATUS)
C      -----------------------------------------------
C
C  Returns the map value at given, integral u, v point.
C
C  Given:
C      DATA      real*4()    map data
C      IU        integer     U-coordinate
C      IV        integer     V-coordinate
C
C  Returned:
C      VALUE     real*4()    real or complex map value
C      STATUS    integer     status value - should be unchanged.
C
C  Returns the value in external units of the data point (IU,IV) from
C  map data held by rows in array DATA.  The redtape common blocks are
C  assumed to be set up appropriately for the data array.  If the point
C  (IU,IV) lies outside the data array, the 'undefined data' value is
C  returned. If the data type of the map is complex, then the value
C  returned is complex.
C
C  (DJT, 12 October 86)
C
*-
       REAL*4  DATA(1), VALUE(*)
       INTEGER  IU, IV, IUV, STATUS
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS .NE. 0) RETURN

       IF (IU.LT.IUMAP1 .OR. IU.GT.IUMAP2 .OR.
     :     IV.GT.IVMAP1 .OR. IV.LT.IVMAP2 ) THEN
         STATUS = UV_OUTMAP
         VALUE(1)=BLANK
         IF (ISWD .EQ. 4) VALUE(2) = BLANK
       ELSE
         IUV=(IVMAP1-IV)*IXMAX+(IU-IUMAP1+1)
         IF (ISWD .NE. 4) THEN
           VALUE(1)=DATA(IUV)
         ELSE
           VALUE(1)=DATA(2*IUV-1)
           VALUE(2)=DATA(2*IUV)
         END IF
       ENDIF
C
       END
