


*+ENMPAG

       SUBROUTINE ENMPAG (PAGES, STATUS)
C      ---------------------------------
C
C  Returns the size of the map in pages.
C
C  Given:
C      None
C
C  Returned:
C      PAGES      integer     Number of pages in the map.
C      STATUS     integer     Status value-must be 0 on entry and is
C                             not changed
C
C  Returns the total size of the map in pages from the current map
C  redtape.  This size includes map redtape plus data, and should
C  correspond to the size of the map file.
C
C  (NPR, 26 November 86)
C
*-
       INTEGER        PAGES, STATUS
C
       include '/mrao/include/maplib_redtape.inc'

       IF (STATUS .NE. 0) RETURN

       PAGES = NPTOT

       RETURN
       END
