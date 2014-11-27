

*+ENNULL

       SUBROUTINE ENNULL( NULL, STATUS )
C      ---------------------------------

C     Returns the undefined pixel value from current redtape.
C
C     Given:
C         None.
C
C     Returned:
C         The external value used to represent undefined pixels.
              real*4      null
C         Status value - must be zero on entry - otherwise unchanged
              integer     status
C
C     NPR  10 August 1988.
C
C-

             include    '/mrao/include/maplib_redtape.inc'

      if (status.ne.0) return

      null = blank

      return
      end
