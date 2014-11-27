
*+RUVDUV

       SUBROUTINE RUVDUV (DATA, U, V, PDU, PDV, S)
C      -------------------------------------------
C
C     Returns the map partial derivatives at a given, real u, v point.
C
C     Given:
C         Map data
              REAL        DATA(*)
C         U, V position
              REAL*8      U, V

C     Returned:
C         Returned values - complex if an aperture, real if a map.
              REAL        PDU(*), PDV(*)
C         Status - must be zero on entry.
              INTEGER     S
C
C     Degrids the map partial derivatives for a given, non-integral,
C     uv point in a map or aperture.  At present, the degridding is
C     done using a tabulated gaussian-sinc degridding function. The
C     first zero of the function is at a radius of 1 and the standard
C     deviation of the gaussian is sqrt(0.5/0.275). The tabulation is
C     oversampled by a factor of 128 (so the ideal of a continuous map
C     is approximated by a map with a grid size 128  times finer than
C     the real map) and has a halfwidth of 3 pixels. U, V values too
C     near the edge of the map return an error of UV_OUTMAP.
C
C     Uses the current map redtape.
C
C     NPR     16 March 1987.
C
*-
C     ******************************************************************

             include        '/mrao/include/maplib_minirt.inc'

C     U and V of given position.
          real*8          uv(2)
C     Partial derivatives at (u,v)
          real            pduv(4)

C     Check for non zero entry status
      if ( s .ne. 0 ) return

      uv(1) = u
      uv(2) = v
      call enminirt( minirt, s )
      if (s.ne.0) goto 9999
      call ruvduv2( minirt, data, uv, pduv, s )

      if (dtp_mrt.eq.4) then
C         Aperture - return complex numbers.
          pdu(1) = pduv(1)
          pdu(2) = pduv(2)
          pdv(1) = pduv(3)
          pdv(2) = pduv(4)
      else
          pdu(1) = pduv(1)
          pdv(1) = pduv(2)
      end if
      return

 9999 continue
          call maperr( s, 'in routine RUVDUV' )
          return
      end
