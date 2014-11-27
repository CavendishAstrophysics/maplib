
*+ENREDT

       SUBROUTINE ENREDT( UV, DATA_TYPE, STATUS )
C      ------------------------------------------

C     Returns details of the current 'computing' redtape.
C
C     Given:
C         None.
C
C     Returned:
C         UV map window in traditional format.
              integer     uv(4)
C         Map data type indicator - (ISWD in redtape).
              integer     data_type
C         Status value - must be zero on entry - otherwise unchanged
              integer     status
C
C     NPR  10 August 1988.
C
C-

             include    '/mrao/include/maplib_redtape.inc'

      if (status.ne.0) return

      uv(1) = iumap1
      uv(2) = iumap2
      uv(3) = ivmap1
      uv(4) = ivmap2
      data_type = iswd

      return
      end
