C     *****************************************************************
C
C     Include file for MAPLIB degridding routines.
C
C     Defines the constants that the determine the MAPLIB degridding
C     function and a tabulation of the degridding function and its
C     derivative. These allow the map to be defined on an effectively
C     continuous subgrid which has a mesh 128 times finer than the
C     standard, integral u,v grid.
C
C     *****************************************************************

      include    '/mrao/include/maplib_tabfns.inc'

C     The power of two of the convolution function oversampling.
          integer         conv_osp2
C     Degridding function parameter definitions.
          integer         conv_tp, conv_hw, conv_os, max_pts, conv_pts
          real            conv_pa(2)

          parameter     ( conv_osp2 = 7 )
          parameter     ( conv_tp   = gauss_sinc )
          parameter     ( conv_os   = 2**conv_osp2 )
          parameter     ( conv_hw   = 3 )
          parameter     ( max_pts   = conv_os*conv_hw )
          parameter     ( conv_pts  = conv_os*(conv_hw*2-1) )
          data            conv_pa(1) / 1.0 /
C         conv_pa(2) is sqrt( 0.5/0.275 )
          data            conv_pa(2) / 1.348399725 /

C     Logicals indicating whether convolution function tables are setup
          logical         setup_fn, setup_deriv
C     Tabulated convolution functions
          real            conv( -max_pts:max_pts )
          real            conv_deriv( -max_pts:max_pts )

*      data                setup_fn    / .false. /
*      data                setup_deriv / .false. /
      common / map_grid / setup_fn, setup_deriv, conv, conv_deriv
