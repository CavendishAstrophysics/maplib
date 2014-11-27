

*+IUVMAP2

       INTEGER FUNCTION IUVMAP2( UV_MAP, UV_PIX )
C      ------------------------------------------
C
C  Finds the index of a U,V point within a map.
C
C  Given:
C      UV_MAP    integer(4)  U, V window of map.
C      UV_PIX    integer(2)  U, V coordinate of a pixel
C
C  Returns the index of the point UV_PIX within the map represented
C  by the given abbreviated U,V window.  If the point UV_PIX lies
C  outside the map, the returned value is zero.
C
C  (NPR, 9 September 1988)
C
*-
       INTEGER  UV_MAP(4), UV_PIX(2)
C
       IF (UV_PIX(1).LT.UV_MAP(1) .OR. UV_PIX(1).GT.UV_MAP(2) .OR.
     :     UV_PIX(2).GT.UV_MAP(3) .OR. UV_PIX(2).LT.UV_MAP(4)     ) THEN
         IUVMAP2=0
       ELSE
         IUVMAP2=(UV_MAP(3)-UV_PIX(2))*(UV_MAP(2)-UV_MAP(1)+1)+
     :           (UV_PIX(1)-UV_MAP(1))+1
       ENDIF
C
       END
