C     (LIBRARY)MAPLIB-GRADING:INCL
C
C     $INCLUDE file containing constants defined in the optimal grading
C     calculation routines.
C

C     Global constant:
C         Order of evaluation of all summation expressions
              integer     order
              parameter ( order = 32 )

C     Errors defined in the optimal grading routines:
C         NAGLIB error.
              integer     ERR_NAGLIB
              parameter ( ERR_NAGLIB = -3701 )

C         NAGLIB returned complex eigenvalue when real was expected.
              integer     ILL_EIGENVALUE
              parameter ( ILL_EIGENVALUE = -3702 )

