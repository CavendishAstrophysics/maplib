*$(5)  Routines for tabulating convolution and other functions.

*+BESSJN

       subroutine bessjn( x, n, jn, s )
C      --------------------------------

C     Returns the value of the first n order Bessel functions Jn(x)

C     Given
C         Value of x
              real*8      x
C         Number of orders required.
              integer     n

C     Returned
C         Values of Jn(x), n=0,1,..n
              real*8      jn(0:n)
C         Status value - must be zero on entry.
              integer     s
C
C     Returns values of the lowest n+1 positive order Bessel functions:
C
C             J0(x), J1(x), J2(x),...Jn(x).,
C
C     in the array jn. These are calculated by the negative recursion
C     method and should be correct to the machine accuracy.
C
C     NPR     September 1988
*-
C     Function declarations:
C         Naglib routine to return Bessel function of zero order.
              real*8          S17AEF

C     Global constant declarations:
             include    '/mrao/include/maplib_grading.inc'

C     Local constant declarations:
C         Approximate value of the machine maximum real value.
              real*8          max_real
              parameter     ( max_real = 1.0D+70 )

C     Local variable declarations
C         Naglib status value
              integer         ifail
C         Loop counter
              integer         i
C         Value of n of the Jn where the negative recursion starts.
              integer         max_iter
C         Values of Jn(x) for n = 0, i, i+1, i+2
              real*8          j0, ji, jip1, jip2
C         Miscellaneous temporary calculation variables
              real*8          factor, temp, twobyx

C     Main Code:

      if (s.ne.0) return

C     Initialise Jn array
      do 100, i = 1, n
          jn(i) = 0.0D+0
  100 continue

C     Evaluate J0(x) with NAGLIB call.
      ifail = 1
      j0 = S17AEF( x, ifail )

      if (ifail .ne. 0) then
          s = ERR_NAGLIB
      else if (x.eq.0) then
          jn(0) = j0
      else if (2.0D+0/x.gt.max_real*x/2.0D+0) then
C         X is as close to zero as makes no difference
          jn(0) = j0
      else
C         Find a reasonable place to start the negative recursion.
          twobyx   = 2.0D+0/x
          temp     = twobyx
          factor   = twobyx
          max_iter = 1
  200     continue
              temp     = factor*temp
              max_iter = max_iter+1
              factor   = dble(max_iter)*twobyx
          if ( max_real/factor.gt.temp) goto 200

          jip2 = 0.0D+0
          jip1 = 1.0D+0
          do 300, i = max_iter, 0, -1
              ji = (i+1)*twobyx*jip1 - jip2
              if (i.le.n) jn(i) = ji
              jip2 = jip1
              jip1 = ji
  300     continue

          temp = j0/ji
          do 400, i = 0, n
              jn(i) = jn(i)*temp
  400     continue
      end if

      if (s.ne.0) goto 9999
      return

 9999 call maperr( s, 'in routine BESSJN' )
      return
      end
