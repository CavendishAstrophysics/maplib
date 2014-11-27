
*+RUVDUV2

       SUBROUTINE RUVDUV2( REDTAPE, DATA, UV, PDUV, S )
C      ------------------------------------------------
C
C     Returns the map partial derivatives at a given, real u, v point.
C
C     Given:
C         Abbreviated map redtape - see (library)maplib-minirt:incl
              INTEGER     REDTAPE(*)
C         Map data
              REAL        DATA(*)
C         U, V position to find partial derivatives at.
              REAL*8      UV(2)

C     Returned:
C         Returned values - two C*8 if an aperture,  two R*4's if a map.
              REAL        PDUV(*)
C         Status - must be zero on entry.
              INTEGER     S
C
C     Degrids the map partial derivatives for a given, non-integral,
C     uv point in a map or aperture.  At present, the degridding is
C     done using a tabulated gaussian-sinc degridding function. The
C     first zero of the function is at a radius of 1 and the standard
C     deviation of the gaussian is sqrt(0.5/0.275). The tabulation is
C     oversampled by a factor of 128 (so the ideal of a continuous map
C     is approximated by a map with a grid size 128  times finer than
C     the real map) and has a halfwidth of 3 pixels. U, V values too
C     near the edge of the map return an error of UV_OUTMAP.
C
C     N.B. Does not use current map redtape.
C
C     NPR     12 August 1988.
C
*-
C     ******************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
             include        '/mrao/include/constants.inc'
             include        '/mrao/include/maplib_minirt.inc'
             include        '/mrao/include/maplib_errors.inc'
             include        '/mrao/include/maplib_subgrid.inc'

C     ****************************************************************
C
C     Local variables, equivilances and commons
C         Real and complex parts of final partial derivative values
              real            rupd_val, iupd_val, rvpd_val, ivpd_val
C         Complex variable flag.
              logical         comp_flg
C         Integral and fractional part of u and v
              integer         iu, iv
              real            deltu, deltv
C         Variables used for pointing into the map array,
              integer         data_ptr, elm_length, row_length,start_row
C         Variables for pointing into the convolution fn tables
              integer         start_conv_u, start_conv_v
              integer         end_conv_u, end_conv_v
              integer         conv_v_ptr, conv_u_ptr
C         Convolution and derivative function value for the current row
              real            conv_v, deriv_v
C         Values of the partial derivative of the convolution function.
              real            upd_conv_val, vpd_conv_val

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      do 10, iu = 1, minirt_len
          minirt(iu) = redtape(iu)
   10 continue

C     ****************************************************************
C
C         Main Code
C         ---------
C
      iu = int(uv(1))
      if (uv(1).lt.0.0D0) iu = iu-1
      deltu = uv(1) - real(iu)

      iv = int(uv(2))
      if (uv(2) .lt. 0.0D0) iv = iv-1
      deltv = uv(2) - real(iv)

      comp_flg = (dtp_mrt .eq. 4)
      if (comp_flg) then
          elm_length = 2
      else
          elm_length = 1
      end if
      row_length = (u2_mrt-u1_mrt+1)*elm_length

      if ( (iu-2).lt.u1_mrt .or. (iu+3).gt.u2_mrt .or.
     *     (iv-2).lt.v2_mrt .or. (iv+3).gt.v1_mrt      ) then
          s = UV_OUTMAP
          rupd_val = blk_mrt
          if (comp_flg) iupd_val = blk_mrt
      else
C         Setup the convolution function if not yet done so.
          if ( .not. setup_fn ) then
              call tabfn( gauss_sinc, conv_os, conv_hw, conv_pa,
     *                    max_pts, conv, s                          )
              setup_fn = .true.
          end if
          if ( .not. setup_deriv ) then
              call tabfn( gauss_sinc_deriv, conv_os, conv_hw, conv_pa,
     *                    max_pts, conv_deriv, s                      )
              setup_deriv = .true.
          end if

C         Set up some array pointer variables prior to the convolution.
          start_conv_u = nint((real(1-conv_hw)-deltu)*real(conv_os))
          start_conv_v = nint((real(1-conv_hw)-deltv)*real(conv_os))
          end_conv_u   = start_conv_u + conv_pts
          end_conv_v   = start_conv_v + conv_pts

          data_ptr = (v1_mrt-(1-conv_hw+iv))*row_length +
     *               ((1-conv_hw+iu)-u1_mrt)*elm_length + 1
          start_row = data_ptr

C         Do the convolution
          rupd_val = 0.0
          rvpd_val = 0.0
          iupd_val = 0.0
          ivpd_val = 0.0

          do 200, conv_v_ptr = start_conv_v, end_conv_v, conv_os
              conv_v  = conv( conv_v_ptr )
              deriv_v = conv_deriv( conv_v_ptr )
              do 100, conv_u_ptr = start_conv_u, end_conv_u, conv_os
                  upd_conv_val = conv_deriv(conv_u_ptr)*conv_v
                  vpd_conv_val = conv( conv_u_ptr )*deriv_v
                  if (data(data_ptr) .ne. blk_mrt) then
                      rupd_val = rupd_val + data(data_ptr)*upd_conv_val
                      rvpd_val = rvpd_val + data(data_ptr)*vpd_conv_val
                      data_ptr = data_ptr + 1
                      if (comp_flg) then
                          iupd_val= iupd_val+data(data_ptr)*upd_conv_val
                          ivpd_val= ivpd_val+data(data_ptr)*vpd_conv_val
                          data_ptr  = data_ptr + 1
                      end if
                  else
                      data_ptr = data_ptr + 1
                      if (comp_flg) data_ptr = data_ptr + 1
                  end if
  100         continue
              start_row = start_row - row_length
              data_ptr  = start_row
  200     continue
      end if

      if (comp_flg) then
          pduv(1) =-rupd_val
          pduv(2) =-iupd_val
          pduv(3) =-rvpd_val
          pduv(4) =-ivpd_val
      else
          pduv(1) =-rupd_val
          pduv(2) =-rvpd_val
      end if

      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call maperr( s, 'in routine RUVDUV2' )
          return
      end
