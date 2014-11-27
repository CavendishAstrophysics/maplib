
*+GAM_0

       REAL*8 FUNCTION GAM_0( BIG_N, N )
C      ---------------------------------

C     Returns a term in the generalised PSWF recursion relationship.

C     Given:
C         Values of N and n
              integer     n, big_N
*-

      if (n.lt.0 .or.  big_N.lt.0) then
          gam_0 = 0.0D+0
      else if (n.eq.0 .and. big_N.eq.0) then
          gam_0 = 0.5D+0
      else
          gam_0 = 0.5D+0*(1.0D+0 +
     *              dble(big_N*big_N) / dble((2*n+big_N)*(2*n+big_N+2)))
      end if
      return
      end
