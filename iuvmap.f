


*+IUVMAP

       INTEGER FUNCTION IUVMAP (IU, IV)
C      --------------------------------
C
C  Finds the index of U,V point within a map.
C
C  Given:
C      IU        integer     U-coordinate
C      IV        integer     V-coordinate
C
C  Returns the index of the point (IU,IV) within the map currently
C  represented by the redtape common blocks.  If the point (IU,IV)
C  lies outside the current map, the returned value is zero.
C
C  (DJT, 20 January 87)
C
*-
       INTEGER  IU, IV
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (IU.LT.IUMAP1 .OR. IU.GT.IUMAP2 .OR.
     :     IV.GT.IVMAP1 .OR. IV.LT.IVMAP2) THEN
         IUVMAP=0
       ELSE
         IUVMAP=(IVMAP1-IV)*IXMAX+(IU-IUMAP1)+1
       ENDIF
C
       END
