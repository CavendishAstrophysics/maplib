*+ENMFIL

       SUBROUTINE ENMFIL (MAP_NAME, FILE_NAME, STATUS)
C      -----------------------------------------------

C     Returns the full file name for a given map.
C
C     Given:
C         Abbreviated map name
              CHARACTER*(*)       MAP_NAME
C     Returned:
C         Full map file name
              CHARACTER*(*)       FILE_NAME
C         Status variable - must be zero on entry - otherwise error
              INTEGER             STATUS
C
C     Returns the full file name matching a given map name, using the
C     default map directory if not explicitly specified.  Case sensitivity
C     is relaxed when matching existing file names.  If no match is found
C     a new file name is constructed using the given name, and a NO_FILE
C     status is returned.
C     
C     The STATUS value should be zero on entry.
C
C     DJT, 28 July 1994.
C-
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/chrlib_functions.inc'
C
      character*64  temp_name, file
      character*32  def_dir
      integer       iobjx, count
      integer       ls, n
C
      if ( status .ne. 0 ) return
C
C  Add default directory to the given name
C
      temp_name = map_name
      call enmdir(def_dir, status)
      if ( map_name(1:1).ne.'/' .and. def_dir.ne.' ' ) then
         ls = chr_lenb(def_dir)
         if (def_dir(ls:ls).eq.'/') then
            temp_name = def_dir(1:ls) // map_name
         else
            temp_name = def_dir(1:ls) // '/' // map_name
         endif
      endif
      file_name = temp_name
C
C  Search the directory for a match to the given name
C
      n = 0
      count = 0
      ls = chr_ilstc(temp_name,'/')
      temp_name(ls+1:) = '*'
      do while (status.eq.0)
        call io_nxtfil(temp_name,file,iobjx,count,status)
        if ( status.eq.0 ) then
           if ( chr_fmatch(map_name, file)) then
             file_name = file
             n = n + 1
           endif
        endif
      enddo
      if ( n.eq.0 ) then
        call io_makfil(def_dir,map_name,'map',file_name,ls)
        status = NO_FILE
      elseif ( n.eq.1 ) then
        status = 0
      elseif ( n.gt.1 ) then
        status = AMB_FILE
      endif
C
      end
