*+ENSAMP

       subroutine ensamp(en_usamp,en_vsamp,status)
C      -------------------------------------------
C
C Enquire parameters for the map sampling
C
C Returned
C  EN_USAMP       -      R8        -       sampling in U
C  EN_VSAMP       -      R8        -       sampling in V
C  STATUS         -      I4        -       Error return
C
C STATUS should be zero on entry and is not changed by this routine.
C The map sampling in both U and V is returned.
C
C [PA, 4 April 1991]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer    status
       real*8     en_usamp, en_vsamp

       if (status.ne.0) return

       en_usamp = usamp
       en_vsamp = vsamp
       end
