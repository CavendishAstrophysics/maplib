*+STSAMP

       subroutine stsamp(st_usamp,st_vsamp,status)
C      -------------------------------------------
C
C Set parameters for the map sampling
C
C Given
C  EN_USAMP       -      R8        -       sampling in U
C  EN_VSAMP       -      R8        -       sampling in V
C Returned
C  STATUS         -      I4        -       Error return
C
C STATUS should be zero on entry and is not changed by this routine.
C The map sampling in both U and V is set.
C
C [PA, 24 April 1993]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer    status
       real*8     st_usamp, st_vsamp

       if (status.ne.0) return

       usamp = st_usamp
       vsamp = st_vsamp
       end
