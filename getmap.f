*+GETMAP

       SUBROUTINE GETMAP (PROMPT, DEFAULT, FILE, STATUS)
C      -------------------------------------------------
C
C  Prompts for a map name.
C
C  Given:
C      PROMPT    char*(*)    prompt string
C      DEFAULT   char*(*)    default input string
C
C  Returned:
C      FILE      char*(*)    map file name
C      STATUS    integer     status
C
C  Prompts on the input device for a map name, and returns the full file
C  name.  The default map directory (taken from the environment variable
C  MAPSDIR) is used if the directory is not explicitly specified.  Case
C  sensitivity is relaxed when matching existing file names.
C  If no match is found a new file name is constructed.
C
C  The STATUS value should be zero on entry.
C
C  (DJT, 28 July 1994)
C
*-
      CHARACTER*(*)  PROMPT, DEFAULT, FILE
      INTEGER        STATUS
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/include/iolib_functions.inc'
      include '/mrao/include/chrlib_functions.inc'
C
      character*64  temp_name, string, chstr
      integer       iobjx, count
      integer       ls
      logical       batch
C
      if ( status .ne. 0 ) return
C
      string = file
      call io_enqbch(batch)
      call io_getc(prompt,default,string,status)
      call enmfil(string,file,status)
C
C  If file not found, return new file name
C
      if ( status.eq.NO_FILE) then
        status = 0
C
C  Resolve ambiguities
C
      elseif ( status.eq.AMB_FILE .and. .not.batch ) then
C
C    Search the directory for a match to the given name
C
        count = 0
        status = 0
        ls = chr_ilstc(file,'/')
        temp_name = file(1:ls)//'*'
        do while (status.eq.0)
          call io_nxtfil(temp_name,file,iobjx,count,status)
          if ( status.eq.0 ) then
             if ( chr_fmatch(string, file)) then
               ls = chr_lenb(file)
               chstr = file(1:ls)//'?'
               if (io_yesno(chstr, 'no', status)) goto 1
             endif
          endif
        enddo
C
    1   continue
C
        if ( status.eq.usr_break ) file = ' '
C
      endif
C
      end
