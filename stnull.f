

*+STNULL

       SUBROUTINE STNULL( NULL, STATUS )
C      ---------------------------------

C     Sets the undefined pixel value in the current redtape.
C
C     Given:
C         The external value to be used to represent undefined pixels.
              real*4      null
C
C     Returned:
C         Status value - must be zero on entry - otherwise unchanged
              integer     status
C
C     NPR  10 August 1988.
C
C-

             include    '/mrao/include/maplib_redtape.inc'

      if (status.ne.0) return

      blank = null

      return
      end
