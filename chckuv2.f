
*$(4b)  Map data handling routines, using explicit redtape.

*+CHCKUV2

       SUBROUTINE CHCKUV2 (UV_MAP, IUV, STATUS)
C      ----------------------------------------
C
C  Checks U,V window for validity.
C
C  Given:
C      UV_MAP    integer(4)  map U,V coordinate window
C      IUV       integer(4)  U,V coordinate window
C
C  Returned:
C      STATUS    integer     status value
C
C  Checks a given U,V coordinate window IUV(IU1,IU2,IV1,IV2) for validity,
C  with respect to the given map uv range.
C
C  The STATUS value should be zero on entry.  Possible error values are:
C
C      - invalid U,V window (ILL_UVWIND)
C      - window outside map (UV_OUTMAP)
C
C  (DJT, 24 November 87)
C
*-
       INTEGER  UV_MAP(4), IUV(4), STATUS
       INTEGER  IU1, IU2, IV1, IV2
C
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
       IU1=IUV(1)
       IU2=IUV(2)
       IV1=IUV(3)
       IV2=IUV(4)
       IF (IU1.GT.IU2 .OR. IV1.LT.IV2 .OR.
     :     IU2.LT.UV_MAP(1) .OR. IU1.GT.UV_MAP(2) .OR.
     :     IV2.GT.UV_MAP(3) .OR. IV1.LT.UV_MAP(4)) THEN
         STATUS=ILL_UVWIND
       ELSEIF (IU1.LT.UV_MAP(1) .OR. IU2.GT.UV_MAP(2) .OR.
     :         IV1.GT.UV_MAP(3) .OR. IV2.LT.UV_MAP(4)) THEN
         STATUS=UV_OUTMAP
       ENDIF
C
       END
