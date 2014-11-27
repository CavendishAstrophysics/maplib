


*+STTYPE

       subroutine sttype(st_freq,st_poln,st_name,st_unit,status)
C      ---------------------------------------------------------
C
C Set type of data measured in the image in the redtape
C
C Input
C   ST_FREQ          -       R4       -      frequency (MHz) FREQ
C   ST_POLN          -       I4       -      polarization-code IPOLN
C   ST_NAME          -       C*(*)    -      name of quantity NAME
C   ST_UNIT          -       C*(*)    -      unit of quantity EXUNIT
C   STATUS           -       I4       -      error return
C
C STATUS should be zero on entry and will be returned as zero unless
C an invalid polariztion code is detected
C
C [PA, 31 July  1988, 13 January 1990]
*-

       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'

       integer        status, st_poln, i1, i2, chr_intlc, chr_lenb
       real*4         st_freq
       character*(*)  st_name, st_unit
       character*16   frequency_text

       if (status.ne.0) return

       if (st_poln.lt.-1 .or. st_poln.gt.13) then
         status = ill_polcode
         return
       end if

C define parameters as passed by the routine call
       ipoln  = st_poln
       freq   = st_freq
       name   = st_name
       exunit = st_unit

C set character polarization
       if (ipoln.eq.-1) then
         poln = 'UNDEFINED'
       else if (ipoln.eq.0) then
         poln = 'BEAM'
       else if (ipoln.eq.1) then
         poln = 'I'
       else if (ipoln.eq.2) then
         poln = 'Q'
       else if (ipoln.eq.3) then
         poln = 'U'
       else if (ipoln.eq.4) then
         poln = 'V'
       else if (ipoln.eq.5) then
         poln = 'I-Q'
       else if (ipoln.eq.6) then
         poln = 'I+Q'
       else if (ipoln.eq.10) then
         poln = 'mI'
       else if (ipoln.eq.11) then
         poln = 'Chi'
       else if (ipoln.eq.12) then
         poln = '%P'
       else if (ipoln.eq.13) then
         poln = 'Alpha'
       end if

C update the text of the redtape
       call adredt('Polarisation',poln,status)
       frequency_text = ' '
       write(frequency_text,'(f10.2,X,A)') st_freq,'MHz'
       i1 = chr_intlc(frequency_text)
       i2 = chr_lenb(frequency_text)
       call adredt('Frequency',frequency_text(i1:i2),status)

       end
