C     *****************************************************************
C
C     Include file for MAPLIB telescope definitions.
C
C     Defines the current telescopes that are valid for the maplib
C     library. A data statement also initialises a tabulated symmetrical
C     primary beam - which can be accessed via the routine PBCORR. If
C     the primary beam is not defined, it is assumed to be unity.
C
C     A telescope has a different telescope code for each observing
C     frequency - so that a different primary beam can be tabulated
C     for each frequency.
C
C     *****************************************************************

C     Maximum number of telescopes.
          integer     max_tscope
          parameter ( max_tscope = 5 )

C     Maximum number of tabulated primary beams.
          integer     max_pbeam
          parameter ( max_pbeam = 3 )

C     Length of each primary beam table
          integer     max_pbtab
          parameter ( max_pbtab = 40 )

C     Constants defining the code for each telescope
          integer     t151, t38, tryle
          integer     tryle_5000, tryle_15000
          parameter ( t151       = 1 )
          parameter ( t38        = 2 )
          parameter ( tryle      = 4 )
          parameter ( tryle_5000 = 4 )
          parameter ( tryle_15000= 5 )

C     Index array defining which beam table to use for each telescope
          integer     pb_indx(max_tscope)
          data        pb_indx / 1,2,0,3,3 /

C     Table defining number of arcsec/table entry for each telescope
          real        pb_scale(max_tscope)

C     Array of tabulated primary beams
          real        pb(0:max_pbtab, max_pbeam)
          integer     dlist_ctr

C     151 MHz primary beam - tabulated in 0.5 degree increments
          data    pb_scale(T151) / 1800.0 /
          data   (pb(dlist_ctr,1), dlist_ctr=0, max_pbtab)  /
     *         1.000,.998,.992,.981,.967,.949,
     *          .927,.902,.873,.841,.807,.770,
     *          .730,.690,.647,.604,.559,.515,
     *          .470,.426,.383,.345,.308,.274,
     *          .242,.212,.179,.149,.122,.098,.076, 10*0.0 /

C     38 MHz primary beam - tabulated in 1 degree increments
          data    pb_scale(T38) / 3600.0 /
          data   (pb(dlist_ctr,2), dlist_ctr=0, max_pbtab) /
     *         1.0000, .9987, .9950, .9887, .9800, .9687, .9550, .9388,
     *          .9200, .8988, .8750, .8487, .8200, .7887, .7550, .7187,
     *          .6800, .6387, .5950, .5493, .5036, .4578, .4120, .3662,
     *          .3204, .2747, .2289, .1831, .1373, .0916, .0458, 10*0 /

C     Ryle primary beam - tabulated in 1 minute increments at 2.7 GHz.
          data    pb_scale(tryle_5000)  / 32.4 /
          data    pb_scale(tryle_15000) / 16.2 /
          data   (pb(dlist_ctr,3), dlist_ctr=0, max_pbtab) /
     *         1.000,.980,.975,.960,.935,.910,.880,.850,.815,.780,
     *          .740,.700,.657,.613,.570,.525,.480,.435,.390,.345,
     *          .300,.255,.210,.165,.120,.075,.030, 14*0.0 /
