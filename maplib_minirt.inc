C     *****************************************************************
C
C     Include file for MAPLIB mini redtape record.
C
C     Defines a map data array so that it can be used with the various
C     array scanning and source finding routines in maplib. It is not
C     defined as in common but is passed as a parameter when needed.
C
C     *****************************************************************

C     Redtape length
          integer         minirt_len
          parameter     ( minirt_len = 8 )

C     Record declaration
          integer*4       minirt ( minirt_len )
          real*4          rminirt( minirt_len )
          equivalence   ( minirt, rminirt )

C     Declaration of pointers to values in the redtape
          integer         u1_ptr, u2_ptr, v1_ptr, v2_ptr, nx_ptr, ny_ptr
          integer         dtp_ptr, blk_ptr
          parameter     ( u1_ptr = 1 )
          parameter     ( u2_ptr = 2 )
          parameter     ( v1_ptr = 3 )
          parameter     ( v2_ptr = 4 )
          parameter     ( nx_ptr = 5 )
          parameter     ( ny_ptr = 6 )
          parameter     ( dtp_ptr = 7 )
          parameter     ( blk_ptr = 8 )

C     Declaration of the variables themselves
          integer         uv_mrt(4)
          integer         u1_mrt, u2_mrt, v1_mrt, v2_mrt, nx_mrt, ny_mrt
          integer         dtp_mrt
          real            blk_mrt
          equivalence   ( uv_mrt,  minirt(u1_ptr) )
          equivalence   ( u1_mrt,  minirt(u1_ptr) )
          equivalence   ( u2_mrt,  minirt(u2_ptr) )
          equivalence   ( v1_mrt,  minirt(v1_ptr) )
          equivalence   ( v2_mrt,  minirt(v2_ptr) )
          equivalence   ( nx_mrt,  minirt(nx_ptr) )
          equivalence   ( ny_mrt,  minirt(ny_ptr) )
          equivalence   ( dtp_mrt, minirt(dtp_ptr) )
          equivalence   ( blk_mrt, minirt(blk_ptr) )

