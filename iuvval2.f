

*+IUVVAL2

       SUBROUTINE IUVVAL2 (REDTAPE, DATA, IUV, VALUE, STATUS)
C      ------------------------------------------------------
C
C  Returns the map value at a given, integral u, v point.
C
C  Given:
C      REDTAPE   integer(*)  Abbreviated version of map redtape.
C      DATA      real*4()    map data
C      IUV       integer(2)  U and V coordinate on map.
C
C  Returned:
C      VALUE     real*4()    real or complex map value
C      STATUS    integer     status value - should be unchanged.
C
C  Returns the value in external units of the data point IUV from
C  map data held by rows in array DATA.  The abbreviated redtape is
C  assumed to be set up appropriately for the data array.  If the point
C  IUV lies outside the data array, the 'undefined data' value is
C  returned. If the data type of the map is complex, then the value
C  returned is complex.
C
C  (NPR, 9 September 88)
C
*-
       include '/mrao/include/maplib_minirt.inc'
       include '/mrao/include/maplib_errors.inc'

       REAL*4  DATA(1), VALUE(*)
       INTEGER  REDTAPE(MINIRT_LEN), IUV(2), I, STATUS
C
C
       IF (STATUS .NE. 0) RETURN

       MINIRT(BLK_PTR)=REDTAPE(BLK_PTR)

       IF (IUV(1).LT.REDTAPE(U1_PTR).OR.IUV(1).GT.REDTAPE(U2_PTR) .OR.
     :     IUV(2).GT.REDTAPE(V1_PTR).OR.IUV(2).LT.REDTAPE(V2_PTR) ) THEN
         STATUS  =UV_OUTMAP
         VALUE(1)=BLK_MRT
         IF (REDTAPE(DTP_PTR) .EQ. 4) VALUE(2) = BLK_MRT
       ELSE
         I=(REDTAPE(V1_PTR)-IUV(2))*REDTAPE(NX_PTR)+
     :     (IUV(1)-REDTAPE(U1_PTR)+1)

         IF (REDTAPE(DTP_PTR) .NE. 4) THEN
           VALUE(1)=DATA(I)
         ELSE
           VALUE(1)=DATA(2*I-1)
           VALUE(2)=DATA(2*I)
         END IF
       ENDIF
C
       END
