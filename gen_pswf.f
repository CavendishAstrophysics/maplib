


*+gen_pswf

       subroutine gen_pswf( c, n, big_N, max_x, num_elmts, pswf, s )
C      -------------------------------------------------------------

C     Returns tabulated values for the generalised PSWF.

C     Given
C         Value of c to use
              real        c
C         Eigenvalues of the pswf
              integer     n, big_N
C         Size of table in x (ie 0<=x<=max_x)
              real        max_x
C         Size of table
              integer     num_elmts

C     Returned
C         Tabulated values of pswf, normalised to unity at the origin.
              real        pswf(0:num_elmts)
C         Status value - must be zero on entry
              integer     s
C
C     Tabulates the generalised prolate spheroidal wave function between
C     zero and max_x and returns it in the table pswf. Evaluation is
C     performed by calculating the Bessel function expansion as in
C     Eqn. 44 in Slepian (1964) Bell System Tech. J. 43, pp 3009-3057.
C
C     NPR     September 1988
*-
C     Global constant declarations.
             include    '/mrao/include/maplib_grading.inc'

      integer         max_jn
      parameter     ( max_jn= 3*order+1 )

C     Local variable declarations
C         Loop counters.
              integer         i, j
C         Eigenvalue and eigenvector of the dij's for this value of c.
              real*8          chi, d_val(0:order)
C         The eigenvalue of the pswf for this value of c
              real*8          gamma
C         Values of current x, c*x and the value of the pswf at x
              real*8          x, cx, pswf_x
C         Tabulated values of the first n order Bessel functions at cx.
              real*8          jn(0:max_jn)
C         Combination coefficient (big_N+j)C(j)
              real*8          njcj
	      

C     Main Code:
      if (s.ne.0) return

C     Do the full evaluation
      call eig_pswf( c, n, big_N, chi, d_val, s )

      if (s.ne.0) then
          continue
      else if (c.ne.0) then
C         Evaluate the eigenvalue
          gamma  = 0.0D+0
          do 100, j = 0, order
              gamma  = gamma +real(d_val(j))
  100     continue
          gamma =(c**(big_N+0.5)/2**(big_N+1))*(real(d_val(0))/gamma)
          do 200, j = 1, (big_N+1)
              gamma = gamma/dfloat(j)
  200     continue

C         Tablulate the eigenfunction
          pswf(0) = 0.0
          do 400, i = 1, num_elmts
              x     = abs( max_x*real(i)/real(num_elmts) )
              cx    = c*x
              call bessjn( cx, int(big_N+2*order+1), jn, s )

              pswf_x = 0.0D+0
              do 300, j = 0, order
C                 Evaluate next (N+j)C(j) term
                  if (j.eq.0) then
                      njcj = 1
                  else
                      njcj = njcj * dble(big_N+j)/dble(j)
                  end if

                  pswf_x = pswf_x+d_val(j)*jn(big_N+2*j+1)/njcj
  300         continue

              pswf(i) = pswf_x/(gamma*cx**0.5)
  400     continue
      else
C         Just return zero
          do 600, i = 0, num_elmts
              pswf(i) = 0.0
  600     continue
      end if

      if (s.ne.0) goto 9999
      return

 9999 call maperr( s, 'in routine GEN_PSWF' )
      return
      end
