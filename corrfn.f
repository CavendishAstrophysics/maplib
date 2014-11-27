
*+CORRFN
C
       SUBROUTINE CORRFN( CONV_HW, CONV_OS, CONV_PTS, CONV,
     *                           CORR_TYPE, CORR_PTS, CORR, S )
C      --------------------------------------------------------
C
C     Computes the grid correction fn. for a particular convolution fn.
C
C     Given:
C         Number of convolution function points per uv point.
              integer         conv_os
C         Half width of convolution function in uv gridpoints.
              integer         conv_hw
C         Half width of convolution function table (for dimensioning)
              integer         conv_pts
C         Tabulated convolution function.
              real            conv( -conv_pts:conv_pts )
C         Correction function type (see below).
              integer         corr_type
C         Length of correction function array
              integer         corr_pts
C
C     Returned:
C         Tabulated correction function.
              real            corr( 1:corr_pts )
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     This subroutine computes the grid correction function for a
C     particular convolution function using the correction types
C     defined in (library)maplib-tabfns:incl.
C
C     1. corr_type = 0 = NO_CORR
C         No correction, h(t)=1.0.
C
C     2. corr_type = 1 = DFT_FFT_OPT
C         Minimises the misfit between the DFT and the FFT (Sze Tans
C         algorithm).
C
C              SUM[k] {INTEG( C(k-u) * cos (2*pi*(k-u)u*t) du) }
C  h(t) = ------------------------------------------------------------
C         SUM[k] SUM[l] {INTEG( C(k-u)*C(l-u) du) * cos(2*pi*(k-l)*t)}
C
C         The integrals are approximated using the trapezium rule for
C         each of the corr_pts positions in the CORR array.
C
C     3. corr_type = 2 = CONV_FFT
C         The correction function is inverse of the fourier transform of
C         the convolution function (ie. the inverse of the numerator of
C         the above). This is preferred when finding the true flux of
C         known sources near the edge of a map.
C
C     4. corr_type = 3 = PETER_FFT
C         Performs a fast fourier transform of the half integer points
C         of the convolution function. (as in the old CLFST mapper).
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
             include        '/mrao/include/maplib_tabfns.inc'
             include        '/mrao/include/maplib_errors.inc'
             include        '/mrao/include/constants.inc'

C     ****************************************************************
C
C     Local constants
C         Local work array size
              integer         work_size
              parameter     ( work_size = 1026 )

C     Variables, equivalences and commons
C         Work array
              real            work( work_size )
C         Loop control variables
              integer         i, j, k, l, idelta
C         k and l times conv_os respectively
              real            k_ptr, l_ptr
C         Temporary summation variables, _d and _n indicate numerator
C         and denominator respectively.
              real            temp, sum_n, sum_d
C         Half of conv_hw and corr_pts respectively
              integer         conv_os2, corr_pts2
C         Table of cosine differences
              real            cos_diff( 0:10 )
C         more precalculated values
              real            twopi_t, delta

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

C     Check parameters
      if (( mod( conv_os, 2 ) .ne. 0 ) .or.
     *    ( conv_pts .lt. (conv_hw*conv_os) ) )
     *    s = ILL_MAPFN

      if ( mod( corr_pts, 2 ) .ne. 0 ) s = ILL_MAPFN

      if ( s .ne. 0 ) goto 9999

      corr_pts2       = corr_pts/2
      conv_os2 = conv_os/2

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ( corr_type .eq. no_corr ) then
          do 100, i = 1, corr_pts
              corr(i) = 1.0
  100     continue
      else if (corr_type.eq.dft_fft_opt .or. corr_type.eq.conv_fft) then
C         Sze Tan's window Correction.
C         Step through half of the correction array on the outer loop
          do 700, j = 0, corr_pts2
              twopi_t = const_2pi*float(j) / float(corr_pts)

C             First set up a table of cosine differences
              do 200, i = 0, 2*conv_hw-1
                  cos_diff(i)=cos(twopi_t*i)
  200         continue

              sum_n = 0.0
              sum_d = 0.0

              do 600, k = -conv_hw+1, conv_hw
                  k_ptr = k*conv_os
                  if (corr_type .eq. dft_fft_opt) then
C                     Evaluate the denominator
                      do 400, l = -conv_hw+1, conv_hw
                          l_ptr = l*conv_os
                          temp = 0.5 *
     *                            ( conv(k_ptr)*conv(l_ptr) +
     *                              conv(k_ptr-conv_os2) *
     *                              conv(l_ptr-conv_os2)      )
                          do 300, idelta = 1, conv_os2-1
                              temp = temp + conv(k_ptr-idelta)*
     *                                  conv(l_ptr-idelta)
  300                     continue
                          sum_d = sum_d + cos_diff(abs(k-l))*temp
  400                 continue
                  end if

C                 Now evaluate the numerator.
                  temp = 0.5 *
     *                     (  conv(k_ptr)*cos(k*twopi_t) +
     *                        conv(k_ptr-conv_os2) *
     *                        cos((k-0.5)*twopi_t)                   )
                  do 500, idelta = 1, conv_os2-1
                      delta = float(idelta)/float(conv_os)
                      temp = temp + conv(k_ptr-idelta) *
     *                              cos((k-delta)*twopi_t)
  500             continue
                  sum_n = sum_n + temp
  600         continue

              if (corr_type.eq.dft_fft_opt) then
                  if ( sum_d.le.0.0 ) then
                      s = ILL_CORRFN
                      goto 9999
                  else
                      temp=sum_n/sum_d
                  end if
              else
                  if ( sum_n.eq.0.0 ) then
                      temp=1.0E+50
                  else
                      temp=1.0/sum_n
                  end if
              end if
              corr(corr_pts2-j+1)= temp
              if ( j .ne. corr_pts2 ) corr(corr_pts2+j+1)=temp
  700     continue

C         Normalise
          temp = corr( corr_pts2 + 1 )
          do 800, i = 1, corr_pts
              corr( i ) = corr( i ) / temp
  800     continue

      else if ( corr_type .eq. peter_fft ) then
C         Peter's old mapper algorithm.
          if ( work_size .lt. 2*(corr_pts+1) ) then
              s = ARR_TOOSMALL
              goto 9999
          end if

          do 1100, i = 1, work_size
              work(i) = 0.0
 1100     continue

          j = 0
          do 1200, i = 0, conv_hw*2
              work(2*i+1) = conv(j)
              j = j + conv_os2
 1200     continue

          call hermitian_fft( work, corr_pts+1, 1, s )

          do 1300, i = 0, corr_pts2
              if (work(i+1).eq.0.0) then
                  temp = 1.0E+50
              else
                  temp = work(1)/work(i+1)
              end if

              corr(corr_pts2-i+1) = temp
              if (i.ne.corr_pts2) corr(corr_pts2+i+1) = temp
 1300     continue
      else
          s = ILL_CORRFN
          goto 9999
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call maperr( s, 'in routine CORRFN' )
          return
      end
