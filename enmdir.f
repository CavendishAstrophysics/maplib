*+ENMDIR

       SUBROUTINE ENMDIR (DIR_NAME, STATUS)
C      ------------------------------------

C     Returns the default map directory for the current user.
C
C     Returned:
C         Default map directory
              CHARACTER*(*)       DIR_NAME
C         Status variable - must be zero on entry - otherwise error
              INTEGER             STATUS
C
C     Uses the value of the MAPSDIR environment variable if set,
C     otherwise the ~/images directory if it exists, and otherwise
C     the current directory.
C
C     The STATUS value should be zero on entry.
C
C     NPR     12 November 1987.
C     DJT     21 February 1994, Unix version.
C
C-
C
      include '/mrao/include/chrlib_functions.inc'
C
      character*24  home_dir, map_dir
      character*24  string
      integer       getcwd
      integer       system
      integer       istat
C
      if ( status .ne. 0 ) return

      dir_name = ' '

      call getenv ('MAPSDIR', dir_name)

      if (dir_name .eq. ' ') then
        call getenv ('HOME', home_dir)
        map_dir = home_dir(1:chr_lenb(home_dir))//'/images'
        string = 'test -d '//map_dir
        istat = system( string )
        if (istat.ne.0) then
          istat = getcwd (map_dir)
        endif
        dir_name = map_dir
      endif

      end
