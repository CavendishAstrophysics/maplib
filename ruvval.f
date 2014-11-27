

*+RUVVAL

       SUBROUTINE RUVVAL (DATA, U, V, DEGRID_TYPE, RESULT, S)
C      ------------------------------------------------------
C
C     Returns the map value at a given, real u, v point.
C
C     Given:
C         Map data
              REAL        DATA(*)
C         U, V position
              REAL*8      U, V
C         Function to use for degridding.
              INTEGER     DEGRID_TYPE
C
C     Returned:
C         Returned value - complex if an aperture, real if a map.
              REAL        RESULT(*)
C         Status - must be zero on entry.
              INTEGER     S
C
C     Degrids the map data to a given, non-integral uv point in a map
C     or aperture.  At present, there are two types of degridding
C     function available :
C
C     degrid_type 1 - linear degridding from the four nearest pixels.
C     degrid_type 2 - degridding using a tabulated gaussian-sinc
C                     degridding function. The first zero of the
C                     function is at a radius of 1 and the standard
C                     deviation of the gaussian is sqrt(0.5/0.275). The
C                     tabulation is oversampled by a factor of 100 and
C                     has a halfwidth of 3 pixels. U, V near the edge
C                     of the map are interpolated linearly and any
C                     undefined pixels are ignored.
C
C     N.B. Uses only ISWD, UVMAPW and BLANK in the current map redtape.
C
C     Originally based on EMW's two routines LININT and DOINT.
C
C     NPR     20 January 1987.
C
*-
C     ******************************************************************
C
             include        '/mrao/include/maplib_minirt.inc'

C     U and V of given position.
          real*8          uv(2)

C     Check for non zero entry status
      if ( s .ne. 0 ) return

      uv(1) = u
      uv(2) = v
      call enminirt( minirt, s )
      if (s.ne.0) goto 9999
      call ruvval2( minirt, data, uv, degrid_type, result, s )
      return

 9999 continue
          call maperr( s, 'in routine RUVVAL' )
          return
      end
