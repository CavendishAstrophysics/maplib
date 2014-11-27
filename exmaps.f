C  Main program for the EXAMINE-MAPS utility
C
       character*80 parameters
       integer length,status
C
       status=0
       call io_initio
       call io_enqcli(parameters,length)
       call ex_maps(parameters(1:length),status)
C
       end


