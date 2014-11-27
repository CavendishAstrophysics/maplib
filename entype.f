

*+ENTYPE

       subroutine entype(en_freq,en_poln,en_name,en_unit,status)
C      ---------------------------------------------------------
C
C Enquire type of data measured in the image from the redtape
C
C Returned
C   EN_FREQ          -       R4       -      frequency (MHz) FREQ
C   EN_POLN          -       I4       -      polarization-code IPOLN
C   EN_NAME          -       C*16     -      name of quantity NAME
C   EN_UNIT          -       C*16     -      unit of quantity EXUNIT
C   STATUS           -       I4       -      error return
C
C STATUS should be zero on entry and is not changed by this routine.
C
C [PA, 31 July 1988]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer      status, en_poln
       real*4       en_freq
       character    en_name*16, en_unit*16

       if (status.ne.0) return

       en_poln = ipoln
       en_freq = freq
       en_name = name
       en_unit = exunit

       end
