
*+SINCPI

       REAL FUNCTION SINCPI (X)
C      ------------------------
C
C     Returns the value of sinc( pi.x )
C
C     Given:
C         REAL            X          argument
C
C     Returned:
C         REAL            SINCPI     sin(pi.x)/(pi.x)
C
*-
      real  x

             include  '/mrao/include/constants.inc'

      if ( x .eq. 0 ) then
          sincpi = 1.0
      else
          sincpi = sin( real(const_pi)*x ) / ( real(const_pi)*x )
      end if

      return
      end
