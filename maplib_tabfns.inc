C     ******************************************************************
C
C     Include file for MAPLIB tabulated function descriptions.
C
C     ******************************************************************

C         Number of mapper convolution function types
              integer             num_conv
              parameter         ( num_conv = 3 )

C         Total number of tabulated functions.
              integer             num_fns
              parameter         ( num_fns = 5 )


C         Valid function types.
              character*(30)      fn_types(num_fns)

              integer             prol_spher, gauss_sinc, l2_optimal
              integer             gauss_sinc_deriv, opt_grading
              parameter         ( prol_spher = 1 )
              parameter         ( gauss_sinc = 2 )
              parameter         ( l2_optimal = 3 )
              parameter         ( gauss_sinc_deriv = 4 )
              parameter         ( opt_grading = 5 )
              data      fn_types/ 'spheroidal',
     *                            'gaussian-sinc',
     *                            'L2-optimal',
     *                            'deriv-gaussian-sinc',
     *                            'optimal-grading'      /

C         Number of mapper correction function types
              integer             num_corr
              parameter         ( num_corr = 4 )

C         Valid correction function types
              character*(20)      corr_types(0:num_corr-1)

              integer             no_corr, dft_fft_opt
              integer             conv_fft, peter_fft
              parameter         ( no_corr     = 0 )
              parameter         ( dft_fft_opt = 1 )
              parameter         ( conv_fft    = 2 )
              parameter         ( peter_fft   = 3 )
              data    corr_types/ 'none',
     *                            'dft-fft-optimisation',
     *                            'simple-transform',
     *                            'Peters'            /

