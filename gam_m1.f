
*+GAM_M1

       REAL*8 FUNCTION GAM_M1( BIG_N, N )
C      ----------------------------------

C     Returns a term in the generalised PSWF recursion relationship.

C     Given:
C         Values of N and n
              integer     big_N, n
*-
      if (n.le.0 .or.  big_N.lt.0) then
          gam_m1 = 0.0D+0
      else
          gam_m1 = -dble(n*n) / dble((2*n+big_N)*(2*n+big_N+1))
      end if
      return
      end
