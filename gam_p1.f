

*+GAM_P1

       REAL*8 FUNCTION GAM_P1( BIG_N, N )
C      ----------------------------------

C     Returns a term in the generalised PSWF recursion relationship.

C     Given:
C         Values of N and n
              integer     big_N, n
*-

      if (n.lt.0 .or.  big_N.lt.0) then
          gam_p1= 0.0D+0
      else
          gam_p1= -dble((n+big_N+1)*(n+big_N+1)) /
     *                                 dble((2*n+big_N+1)*(2*n+big_N+2))
      end if
      return
      end
