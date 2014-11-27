
*+ENBEAM

       subroutine enbeam(en_flxnrm,en_beam,en_posa,status)
C      ---------------------------------------------------
C
C Enquire parameters for the CLEAN beam shape from the redtape
C
C Returned
C  EN_FLXNRM      -      R4        -       flux normalization FLXNRM
C  EN_BEAM        -      R4(2)     -       beam widths U and V arcsec
C                                          HPFBWU, HPFBWV
C  EN_POSA        -      R4        -       beam position-angle, degrees
C                                          BEAMPA
C  STATUS         -      I4        -       Error return
C
C STATUS should be zero on entry and is not changed by this routine.
C The beam sizes are returned as an array for compatability with
C GETBW.
C
C [PA, 31 July 1988]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer    status
       real*4     en_flxnrm, en_beam(2), en_posa

       if (status.ne.0) return

       en_flxnrm  = flxnrm
       en_beam(1) = hpfbwu
       en_beam(2) = hpfbwv
       en_posa    = beampa

       end
