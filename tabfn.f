

*+tabfn

       subroutine tabfn ( fn_type, fn_os, fn_hw, parameters,
     *                                    fn_pts, fn_table, s )
C      --------------------------------------------------------

C
C     General routine to tabulate a mapping function.
C
C     Given:
C         Mapping function type
              integer*4       fn_type
C         Number of mapping function points per uv point.
              integer*4       fn_os
C         Half width of mapping function in gridpoints.
              integer*4       fn_hw
C         Parameter list to control evaluation of each function type.
              real*4          parameters(*)
C         Half width of function table (for dimensioning)
              integer*4       fn_pts
C
C     Returned:
C         Tabulated mapping function.
              real*4          fn_table( -fn_pts:fn_pts )
C         Status variable - must be zero on entry - otherwise error
              integer         s

C
C     General subroutine to calculate most of the useful mapping
C     functions.
C
C     This version implements the following function types:
C         1 - Spheroidal function with the rational function
C             approximation given by Schwab (in Indirect Imaging (1983))
C             Supported values of alpha are 0, 1 and 2, with function
C             half widths of 2 and 3.
C                 parameter(1) = alpha
C         2 - Gaussian*sinc -
C                 parameter(1) = first zero of sinc fn
C                 parameter(2) = standard deviation of gaussian
C                 (both parameters are in u, v units)
C         3 - L2 optimal function (Sze Tan (1986)), with supported
C             values of X0 being 0.45, 0.40, 0.35, 0.30 and 0.25 -
C                 parameter(1) = X0
C         4 - Guassian*sinc derivative -
C                 parameters are the same as for gaussian*sinc but the
C                 derivative of the function is returned.
C         5 - Optimal grading function with the grading truncation
C             at a halfwidth of 1.
C                 parameter(1) = c.
C
C     If the function type is negative then the function is only
C     tabulated in the positive half plane, zero being the first element
C     in the tabulation array. In this case the above dimensioning of
C     the tabulation array should be ignored.
C
C     The following status's can be returned.
C         0           - Success
C         ILL_MAPFN   - Mapping function not implemented.
C         ARR_TOOSMALL- The mapping function array provided is
C                       too small.
C         other       - Unexpected io_system error.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      REAL*4      sincpi

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
             include    '/mrao/include/constants.inc'
             include    '/mrao/include/maplib_errors.inc'
             include    '/mrao/include/maplib_tabfns.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop control variables
              integer         i, j
C         True function type - absolute value of fn_type parameter
              integer         abs_fn_type
C         Pointer to zero element in tabulation array
              integer         zero
C         Current cell being evaluated in uv coords.
              real*8          u_val
C         Eta as defined by Schwartz (1983) = u_val*fn_hw
              real*8          eta
C         Next integer greater than u_val (but <= fn_hw)
              integer         nxt_int
C         Temporary summation values and multiplicative factor
              real*8          sum1, sum2, factor
C         Prolate spheroidal rational approximation values. psw1 and 2
C         are for function half widths of 2 whilst psw3 to 6 are for
C         half widths of 3.
              real            psw1( 5, 0:2 ), psw2( 3, 0:2 ),
     *                        psw3( 5, 0:2 ), psw4( 3, 0:2 ),
     *                        psw5( 5, 0:2 ), psw6( 3, 0:2 )
              integer         alpha
C         Gaussian*sinc parameters
              real            sinc_zero, gauss_sd
C         L2 optimal function rational approximation values.
              real            xnum2( 5, 2, 5:9 ), xden2( 3, 2, 5:9 ),
     *                        xnum3( 5, 3, 5:9 ), xden3( 3, 3, 5:9 )
              integer         x0_index
C         Optimal grading parameter c.
              real            c

C     ****************************************************************
C
C     Data statements to initialise approximation values.

C     Prolate spheroidal wave function (Schwab - 1983)

      data psw1 /
     * 1.584774E-2,-1.269612E-1, 2.333851E-1,-1.636744E-1, 5.014648E-2,
     * 5.007900E-2,-1.971357E-1, 2.363775E-1,-1.211569E-1, 2.853104E-2,
     * 9.585932E-2,-2.481381E-1, 2.194469E-1,-8.862132E-2, 1.672243E-2 /

      data psw2 /
     * 1.000000E+0, 4.845581E-1, 7.457381E-2,
     * 1.000000E+0, 4.228767E-1, 5.655715E-2,
     * 1.000000E+0, 3.756999E-1, 4.448800E-2 /

      data psw3 /
     * 5.613913E-2,-3.019847E-1, 6.256387E-1,-6.324887E-1, 3.303194E-1,
     * 8.203343E-2,-3.644705E-1, 6.278660E-1,-5.335581E-1, 2.312756E-1,
     * 1.124069E-1,-4.172349E-1, 6.069622E-1,-4.405326E-1, 1.618978E-1 /

      data psw4 /
     * 1.000000E+0, 9.077644E-1, 2.535284E-1,
     * 1.000000E+0, 8.212018E-1, 2.078043E-1,
     * 1.000000E+0, 7.481828E-1, 1.726085E-1 /

      data psw5 /
     * 8.531865E-4,-1.616105E-2, 6.888533E-2,-1.109391E-1, 7.747182E-2,
     * 4.028559E-3,-3.697768E-2, 1.021332E-1,-1.201436E-1, 6.412774E-2,
     * 1.071895E-2,-6.404749E-2, 1.297386E-1,-1.194208E-1, 5.112822E-2 /

      data psw6 /
     * 1.000000E+0, 1.101270E+0, 3.858544E-1,
     * 1.000000E+0, 9.599102E-1, 2.918724E-1,
     * 1.000000E+0, 8.517470E-1, 2.289667E-1 /
C
C     L2 optimal function (Sze Tan 1986)
C
C     Gridding radius = 2 pixels.

      data  xnum2 /
     *  3.4814309E-01,  4.5058902E-02, -1.3628925E-01,  1.5156485E-01,
     *                                                 -6.5760674E-03,
     *  5.8799508E-04, -1.3232130E-02, -3.4592703E-03, -4.2296550E-03,
     *                                                 -1.3400944E-03,
     *
     *  3.6832461E-01,  2.9529068E-02, -1.6377581E-01,  1.3450698E-01,
     *                                                 -3.0269345E-02,
     *  1.1405421E-03, -1.5315522E-02, -3.5029999E-03, -3.8985593E-03,
     *                                                 -1.5416539E-03,
     *
     *  3.9439222E-01,  1.8619442E-02, -1.9561402E-01,  1.2140544E-01,
     *                                                 -4.6617140E-02,
     *  2.2894970E-03, -2.2499212E-02,  3.5425417E-02, -9.7140573E-03,
     *                                                  8.0299424E-03,
     *
     *  4.2802973E-01,  5.8224796E-03, -2.3322864E-01,  1.1459510E-01,
     *                                                 -5.9696088E-02,
     *  4.7825855E-03, -2.4224579E-02,  4.7705197E-03, -3.3435488E-03,
     *                                                 -2.3917987E-04,
     *
     *  4.7280170E-01, -4.5861771E-01, -3.8460866E-01,  2.8517809E-01,
     *                                                 -2.2787340E-01,
     *  1.1457362E-02, -3.3010288E-02,  5.7609857E-03, -1.0600078E-03,
     *                                                 -7.0098702E-04 /

      data xden2 /
     *  1.0000000E+00,  1.1813122E+00,  1.8996621E-01,
     *  1.0000000E+00,  4.7758614E-01,  5.2800092E-02,
     *
     *  1.0000000E+00,  1.0934202E+00,  1.0366356E-01,
     *  1.0000000E+00,  5.0457817E-01,  5.5859044E-02,
     *
     *  1.0000000E+00,  1.0088398E+00,  2.0976009E-02,
     *  1.0000000E+00, -1.4391191E+00, -2.2371443E-01,
     *
     *  1.0000000E+00,  9.0799915E-01, -7.7313429E-02,
     *  1.0000000E+00,  2.4621727E-01,  2.1011680E-02,
     *
     *  1.0000000E+00, -1.5246028E-01, -1.1187010E+00,
     *  1.0000000E+00,  3.2024356E-01,  2.8091748E-02 /


C     Gridding radius = 3 pixels.
      data xnum3 /
     *  4.8914430E-01,  1.4292122E-01, -2.3993557E-01,  7.3792416E-02,
     *                                                 -2.8836175E-02,
     *  4.3719243E-02, -2.6297940E-02,  6.6246210E-03, -1.1280033E-05,
     *                                                  4.2892112E-06,
     *  1.2454903E-05, -2.4818061E-04,  3.5435914E-06, -7.7765324E-05,
     *                                                 -2.7973268E-06,
     *
     *  5.1006074E-01,  1.7719075E-01, -2.3778674E-01,  6.8586842E-02,
     *                                                 -2.1577950E-02,
     *  5.2176148E-02, -2.9427656E-02,  6.5252421E-03,  5.5823299E-05,
     *                                                 -2.9986344E-05,
     *  2.8390020E-05, -3.6229083E-04,  2.9081245E-05, -9.7011646E-05,
     *                                                 -3.0482903E-06,
     *
     *  5.3524989E-01,  2.2290318E-01, -2.3193441E-01,  6.0420423E-02,
     *                                                 -1.3813002E-02,
     *  6.4105379E-02, -3.3482110E-02,  6.2166540E-03,  9.4248802E-05,
     *                                                 -7.3996428E-05,
     *  6.7502171E-05, -5.5551353E-04,  9.3428900E-05, -1.2248208E-04,
     *                                                 -2.1074007E-06,
     *
     *  5.6604087E-01,  2.8037223E-01, -2.2144681E-01,  4.8950323E-02,
     *                                                 -6.9334363E-03,
     *  8.1834139E-02, -3.6348377E-02,  4.1601074E-03,  4.4350312E-04,
     *                                                 -1.6633996E-04,
     *  1.7945045E-04, -9.3539560E-04,  2.5379353E-04, -1.5791946E-04,
     *                                                  1.0680306E-06,
     *
     *  6.0527199E-01,  3.3587534E-01, -2.1497621E-01,  3.8904394E-02,
     *                                                 -5.3295409E-03,
     *  1.1049142E-01, -3.4967472E-02, -6.6916751E-04,  1.1027297E-03,
     *                                                 -2.5684433E-04,
     *  6.0855041E-04, -1.8814394E-03,  6.5014897E-04, -1.9612551E-04,
     *                                                  9.4834730E-07 /

      data xden3 /
     *  1.0000000E+00,  1.0303107E+00,  3.3969588E-02,
     *  1.0000000E+00,  2.8115729E-01,  2.4150192E-02,
     *  1.0000000E+00,  2.3025351E-01,  1.4573518E-02,
     *
     *  1.0000000E+00,  1.0418408E+00,  4.6759292E-02,
     *  1.0000000E+00,  2.7330452E-01,  2.2523453E-02,
     *  1.0000000E+00,  2.1539373E-01,  1.2881806E-02,
     *
     *  1.0000000E+00,  1.0604162E+00,  6.6595131E-02,
     *  1.0000000E+00,  2.6193532E-01,  2.0202934E-02,
     *  1.0000000E+00,  1.9326145E-01,  1.0619748E-02,
     *
     *  1.0000000E+00,  1.0807897E+00,  8.9127788E-02,
     *  1.0000000E+00,  2.7447136E-01,  2.0209178E-02,
     *  1.0000000E+00,  1.6358527E-01,  8.2021963E-03,
     *
     *  1.0000000E+00,  1.0711361E+00,  8.1322567E-02,
     *  1.0000000E+00,  3.1491945E-01,  2.3050743E-02,
     *  1.0000000E+00,  1.4856126E-01,  8.5105642E-03 /
C
C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

C     Check subroutine parameters
      if ( (fn_os*fn_hw) .gt. fn_pts ) s = ARR_TOOSMALL

C     Determine whether to tabulate both sides, and set up the zero ptr
      if (fn_type.ge.0) then
          abs_fn_type = fn_type
          zero = 0
      else
          abs_fn_type = -fn_type
          zero = -fn_pts
      end if

C     Set up free parameters for each function and check validity
      if ( abs_fn_type .eq. prol_spher ) then
          alpha = nint( parameters(1) )
          if (( alpha .lt. 0 .or. alpha .gt. 2 ) .or.
     *        ( fn_hw.lt.2 .or. fn_hw.gt.3 ) )
     *        s = ILL_MAPFN

      else if ( abs_fn_type .eq. gauss_sinc       .or.
     *          abs_fn_type .eq. gauss_sinc_deriv      ) then
          sinc_zero = parameters( 1 )
          gauss_sd  = parameters( 2 )
          if ( sinc_zero .le. 0.0 .or. gauss_sd .le. 0.0 )
     *        s = ILL_MAPFN

      else if ( abs_fn_type .eq. l2_optimal ) then
          x0_index = nint( parameters(1)*20.0 )
          if (( x0_index .lt. 5 .or. x0_index .gt. 9 ) .or.
     *        ( fn_hw.lt.2 .or. fn_hw.gt.3 ) )
     *        s = ILL_MAPFN
      else if ( abs_fn_type .eq. opt_grading ) then
          c = parameters(1)
          if (c.le.0.or.fn_hw.lt.1) s = ILL_MAPFN
      else
          s = ILL_MAPFN
      end if

      if ( s .ne. 0 ) goto 9999

C     ****************************************************************
C
C         Main Code
C         ---------
C

      if (abs_fn_type .eq. opt_grading) then
C         Generate function.
          call gen_pswf( c, 0, 0, real(fn_hw), fn_pts,
     *                   fn_table(zero), s )

C         Divide by sqrt(x)
          do 100, i = zero+1, zero+(fn_os*fn_hw)
              u_val= dfloat(i-zero)/dfloat(fn_os)
              fn_table(i) = fn_table(i)/sqrt(u_val)
  100     continue

C         Quadratic approximation for x = 0
          fn_table(zero) = fn_table(zero+1)+
     *                    (fn_table(zero+1)-fn_table(zero+2))/2.0

C         Normalise to unity at the origin
          do 110, i = zero+(fn_os*fn_hw), zero, -1
              fn_table(i) = fn_table(i)/fn_table(zero)
  110     continue
      else
C         Explicitly calculate each function
          do 1000, i = zero, zero+(fn_os*fn_hw)
              u_val= dfloat(i-zero)/dfloat(fn_os)

              if ( abs_fn_type .eq. prol_spher ) then
                  sum1 = 0.0
                  sum2 = 0.0
                  eta  = u_val / dfloat( fn_hw )
                  if ( fn_hw .eq. 2 ) then
                      factor = eta*eta - 1.0
                      do 200, j = 5, 1, -1
                          sum1=sum1*factor+psw1( j, alpha )
  200                 continue
                      do 210, j = 3, 1, -1
                          sum2=sum2*factor+psw2( j, alpha )
  210                 continue
                  else if ( eta .le. 0.75 ) then
C                     fn_hw = 3 and first approximation
                      factor = eta*eta - 0.75*0.75
                      do 220, j = 5, 1, -1
                          sum1=sum1*factor+psw3( j, alpha )
  220                 continue
                      do 230, j = 3, 1, -1
                          sum2=sum2*factor+psw4( j, alpha )
  230                 continue
                  else
C                     fn_hw = 3 and second approximation
                      factor = eta*eta - 1.0
                      do 240, j = 5, 1, -1
                          sum1=sum1*factor+psw5( j, alpha )
  240                 continue
                      do 250, j = 3, 1, -1
                          sum2=sum2*factor+psw6( j, alpha )
  250                 continue
                  end if

                  if (alpha .eq. 0) then
                      fn_table(i) = sum1/sum2
                  else
                      fn_table(i) = (sum1/sum2)*(1.0-eta*eta)**alpha
                  end if
              else if ( abs_fn_type .eq. gauss_sinc ) then
                  fn_table(i) = sincpi( real(u_val/sinc_zero) ) *
     *                          exp(-0.5*(u_val/gauss_sd)**2)
              else if ( abs_fn_type .eq. gauss_sinc_deriv ) then
                  if (u_val .eq. 0.0) then
                      fn_table(i) = 0.0
                  else
                      factor = real(u_val/sinc_zero)
                      fn_table(i) = (cos(const_pi*factor)/u_val-
     *                              sincpi(sngl(factor))*
     *                             (1/u_val+u_val/(gauss_sd*gauss_sd)))*
     *                              exp(-0.5*(u_val/gauss_sd)**2      )
                  end if
              else if ( abs_fn_type .eq. l2_optimal ) then
                  sum1    = 0.0
                  sum2    = 0.0
                  nxt_int = min0( fn_hw, (int( u_val ) + 1) )
                  factor  = u_val*u_val - dfloat( int(nxt_int*nxt_int) )

                  if ( fn_hw .eq. 2 ) then
                      do 300, j = 5, 1, -1
                          sum1=sum1*factor + xnum2(j,nxt_int,x0_index)
  300                 continue
                      do 310, j = 3, 1, -1
                          sum2=sum2*factor + xden2(j,nxt_int,x0_index)
  310                 continue
                  else if ( fn_hw .eq. 3 ) then
                      do 320, j = 5, 1, -1
                          sum1=sum1*factor + xnum3(j,nxt_int,x0_index)
  320                 continue
                      do 330, j = 3, 1, -1
                          sum2=sum2*factor + xden3(j,nxt_int,x0_index)
  330                 continue
                  end if
                  fn_table(i) = sum1/sum2
              end if
 1000     continue
      end if

C     Tabulate other half plane if required
      if (fn_type .gt. 0) then
          do 1100, i = 1, (fn_os*fn_hw)
              if ( abs_fn_type .eq. gauss_sinc_deriv ) then
C                 An odd function
                  fn_table(-i) =-fn_table(i)
              else
C                 Even functions.
                  fn_table(-i) = fn_table(i)
              end if
 1100     continue
      end if

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call maperr( s, 'in routine TABFN' )
          return
      end
