      **  Copyright Progressive Data Solutions, Inc., 2003                                         *
      **********************************************************************************************
      **     Program Description:                                                                  *
      **                                                                                           *
      **     WCMS - Convert WMCMP claims reserve buckets for PDSQuest.                             *
      **                                                                                           *
      **                                                                                           *
      **     Programmer:  C15                                                                      *
      **     Created:    02/27/2003                                                                *
      **********************************************************************************************
      **     Revisions:                                                                            *
      **                                                                                           *
      **       /  /       C:                                                                       *
      **                                                                                           *
      **********************************************************************************************
     h
     fsmupl     if   e           k disk    Rename(pup01:pup00)                  claim file
     fwmfnl     if   e           k disk                                         group file
     fwmcml     uf   e           k disk                                         claim file
      **********************************************************************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     d                 DS
     drsvcd                    1     20    Dim(10)
     dcmrt01                   1      2
     dcmrt02                   3      4
     dcmrt03                   5      6
     dcmrt04                   7      8
     dcmrt05                   9     10
     dcmrt06                  11     12
     dcmrt07                  13     14
     dcmrt08                  15     16
     dcmrt09                  17     18
     dcmrt10                  19     20
      *
     d                 DS
     drsvamt                   1     90  2 Dim(10)
     dcmrr01                   1      9  2
     dcmrr02                  10     18  2
     dcmrr03                  19     27  2
     dcmrr04                  28     36  2
     dcmrr05                  37     45  2
     dcmrr06                  46     54  2
     dcmrr07                  55     63  2
     dcmrr08                  64     72  2
     dcmrr09                  73     81  2
     dcmrr10                  82     90  2
      *
     d                 DS
     dpayamt                   1     90  2 Dim(10)
     dcmar01                   1      9  2
     dcmar02                  10     18  2
     dcmar03                  19     27  2
     dcmar04                  28     36  2
     dcmar05                  37     45  2
     dcmar06                  46     54  2
     dcmar07                  55     63  2
     dcmar08                  64     72  2
     dcmar09                  73     81  2
     dcmar10                  82     90  2
      *
     d                 DS
     dnewrsvcd                 1     20    Dim(10)
     dnewrt01                  1      2
     dnewrt02                  3      4
     dnewrt03                  5      6
     dnewrt04                  7      8
     dnewrt05                  9     10
     dnewrt06                 11     12
     dnewrt07                 13     14
     dnewrt08                 15     16
     dnewrt09                 17     18
     dnewrt10                 19     20
      *
     d                 DS
     dnewrsvamt                1     90  2 Dim(10)
     dnewrr01                  1      9  2
     dnewrr02                 10     18  2
     dnewrr03                 19     27  2
     dnewrr04                 28     36  2
     dnewrr05                 37     45  2
     dnewrr06                 46     54  2
     dnewrr07                 55     63  2
     dnewrr08                 64     72  2
     dnewrr09                 73     81  2
     dnewrr10                 82     90  2
      *
     d                 DS
     dnewpayamt                1     90  2 Dim(10)
     dnewar01                  1      9  2
     dnewar02                 10     18  2
     dnewar03                 19     27  2
     dnewar04                 28     36  2
     dnewar05                 37     45  2
     dnewar06                 46     54  2
     dnewar07                 55     63  2
     dnewar08                 64     72  2
     dnewar09                 73     81  2
     dnewar10                 82     90  2
      *
     d svfnd           s              3  0
     d i               s              2  0                                      counter
     d x               s              2  0
     d company#        s                         Like(CMco#)
     d group#          s                         Like(CMfnd)
     d inGroup#        s                         Like(CMfnd)

     c     *entry        plist
     c                   parm                    inGroup#
      *
      *---------------------------------------------------------------------------------------------
      * Key list for the Claim Master file
      *
     c     cmky          klist
     c                   kfld                    company#
     c                   kfld                    group#
      * Key list for the Group Master file
      *
     c     fnky          klist
     c                   kfld                    cmco#
     c                   kfld                    cmfnd
      *
      *---------------------------------------------------------------------------------------------
      * initial read of file
      *

       // Are we runnng for one group or all?
       group# = 0;
       If q1Prms > 0;
          group# = inGroup#;
       EndIf;

       Chain Q1user pup00;
       If %Found(smupl);
          company# = UPco#;
       Else;
          company# = 777;
       Endif;

     c* Get the company#
     c

     c
     c                   if        group# = 0
     c                   read      wmcml                                  60
     c                   Else
     c     cmky          Chain     pcm01                              60
     c                   EndIf
     c                   dow       not *in60
      *
     c                   clear                   newrsvamt
     c                   clear                   newpayamt
      *
      * Load the new order of reserve codes, if the group # changed.
      *
     c                   if        svfnd <> cmfnd
     c                             or *in98 = *on
     c     fnky          chain     wmfnl                              60
      *
     c                   eval      newrt01 = fnrs01
     c                   eval      newrt02 = fnrs02
     c                   eval      newrt03 = fnrs03
     c                   eval      newrt04 = fnrs04
     c                   eval      newrt05 = fnrs05
     c                   eval      newrt06 = fnrs06
     c                   eval      newrt07 = fnrs07
     c                   eval      newrt08 = fnrs08
     c                   eval      newrt09 = fnrs09
     c                   eval      newrt10 = fnrs10
     c                   setoff                                           98
     c                   eval      svfnd = cmfnd
      *
     c                   endif
      *------
      *
     c                   do        10            i
      *
     c                   eval      x = 1
     c     rsvcd(i)      lookup    newrsvcd(x)                            99
      *
     c                   if        *in99 = *off
     c                   eval      x = 1
     c     *blanks       lookup    newrsvcd(x)                            99
     c   99              eval      newrsvcd(x) = rsvcd(i)
     c                   seton                                            98
     c                   endif
      *
     c                   if        *in99 = *on
     c                   eval      newrsvamt(x) = (newrsvamt(x) + rsvamt(i))
     c                   eval      newpayamt(x) = (newpayamt(x) + payamt(i))
     c                   endif
      *
     c                   enddo                                                  do 10
      *
     c                   exsr      updclaim
      *
      *----
     c                   If        group# = 0
     c                   read      wmcml                                  60    read next record
     c                   Else
     c     cmky          ReadE     pcm01                                  60
     c                   EndIf
     c                   enddo                                                  *in60 = *off
      *--------
     C                   call      'CVTRSVQST2'
     c                   parm                    company#
     c                   parm                    group#
      *--------
      *  End of pgm
     c                   eval      *inlr = *on                                  forced end of pgm
      *=============================================================================================
      *
     c     updclaim      begsr
      *
      *  Resequence the reserve buckets
      *
     c                   do        10            i
      *
     c                   eval      rsvcd(i) = newrsvcd(i)
     c                   eval      rsvamt(i) = newrsvamt(i)
     c                   eval      payamt(i) = newpayamt(i)
      *
     c                   enddo                                                  do 10
      *
     c                   update    pcm01
      *
     c                   endsr                                                  updclaim
      **********************************************************************************************
