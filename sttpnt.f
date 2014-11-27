
*+STTPNT

       subroutine sttpnt(en_tscope,en_freq,en_rapnt,en_decpnt,status)
C      --------------------------------------------------------------
C
C Set information on the telescope, frequency and aerial pointing
C
C Given
C   EN_TSCOPE        -       I4       -      telescope code
C   EN_FREQ          -       R4       -      frequency (MHz) FREQ
C   EN_RAPNT         -       R8       -      aerial pointing ra
C   EN_DECPNT        -       R8       -      aerial pointing dec
C   STATUS           -       I4       -      error return
C
C STATUS should be zero on entry and is not changed by this routine.
C
C [PA, 17 January 1991]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer      status, en_tscope
       real*4       en_freq
       real*8       en_rapnt, en_decpnt

       if (status.ne.0) return

       rteles = en_tscope 
       freq = en_freq
       rapnt = en_rapnt
       decpnt = en_decpnt

       end

