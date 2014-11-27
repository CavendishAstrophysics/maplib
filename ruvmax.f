

*+RUVMAX

       SUBROUTINE RUVMAX (DATA, IU, IV, U, V, MAX, S )
C      -----------------------------------------------
C
C     Returns the real valued u and v of a local map maximum.
C
C     Given:
C         Map data
              REAL        DATA(*)
C         Integral value of a local map maximum
              INTEGER     IU, IV

C     Returned:
C         Position of map maximum.
              REAL*8      U, V
C         Value of map at this position
              REAL        MAX
C         Status - must be zero on entry.
              INTEGER     S
C
C     NPR     20 January 1987.
C
*-
C     ******************************************************************
C
             include        '/mrao/include/maplib_minirt.inc'

C     U and V of given position and final position.
          integer         iuv(2)
          real*8          ruv(2)

C     Check for non zero entry status
      if ( s .ne. 0 ) return

      iuv(1) = iu
      iuv(2) = iv
      call enminirt( minirt, s )
      if (s.ne.0) goto 9999
      call ruvmax2( minirt, data, iuv, ruv, max, s )
      u = ruv(1)
      v = ruv(2)
      return

 9999 continue
          call maperr( s, 'in routine RUVMAX' )
          return
      end
