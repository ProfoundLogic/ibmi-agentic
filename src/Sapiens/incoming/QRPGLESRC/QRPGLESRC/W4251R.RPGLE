      *****************************************************************
      * (C) Copyright Fiserv, Inc., 2006                              *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of Revisions.           *
     * ------------------------------------------------------------- *
      *                                                               *
      *  05/19/06 C:136  Project 136WCE                               *
      *                  Change format to match bank download file.   *
      *                                                               *
      *                                                               *
      *****************************************************************
     Fsmcol     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwmupl     if   e           k disk
     Fwxcrp1    if   e           k disk
     Fwdcrl1    uf   e           k disk

     D                 ds
     D  dsdttm                 1     12  0
     D  dstime                 1      6  0
     D  dsdate                 7     12  0

     D fids01        e ds                  extname(SZQ2P)
     D psds          esds                  extname(SZQ1P)

     D kyacct          s             20
     D kychk#          s              7p 0

     Dinrec            ds
     D xxacct                  1     10
     D xxchk#                 11     20s 0
     D xxtrdt                 21     26
     D xxamt                  27     36
      *** beg chg ***** 05/19/06 *******************************************
     D*********** xxfil1                 37     38
     D*********** xxtrcd                 39     39
     D*********** xxrccd                 40     40
     D xxpddt                 37     42s 0
     D xxtrcd                 43     44
     d xxrccd                 45     45
      *** end chg ***** 05/19/06 *******************************************
      *
     D/copy scopydtpl
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     cokey         klist
     C                   kfld                    upco#
      *
      *   KEY LIST FOR THE FUND MASTER FILE
      *
     C     fnkey         klist
     C                   kfld                    upco#
     C                   kfld                    upfnd
      *
      *  USER PARAMETER FILE
      *
     C     upkey         klist
     C                   kfld                    q1user
      *
      *  CHECK RECONCILIATION DETAIL FILE
      *
     C     crkey         klist
     C                   kfld                    upco#
     C                   kfld                    kyacct
     C                   kfld                    kychk#
      *
     C     dtmdy8        plist
     C                   parm                    srdti
     C                   parm      '*MDY'        srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
      *** beg add ***** 05/19/06 ***********************************
     C     dtydm         plist
     C                   parm                    srdti
     C                   parm      '*YMD'        srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *** beg add ***** 05/19/06 ***********************************
      *
      *  GET THE USER PARAMETER FILE
      *
     C     upkey         chain     pup01                              60
     C                   if        *in60 = *on
     C                   eval      upco# = *zero
     C                   endif
     C                   time                    dsdttm
      *
     C                   dow       *inlr = *off
     C                   read      crp01
     C                   if        %eof(wxcrp1)
     C                   leave
     C                   endif
     C                   move      crfld1        inrec
      *** beg chg ***** 05/19/06 *******************************************
     C***********        if        xxrccd = 'T'
     C                   if        xxrccd = '2'
      *** end chg ***** 05/19/06 *******************************************
     C                   movel(p)  xxacct        kyacct
     C                   move      xxchk#        kychk#
     C     crkey         chain     pcr01                              60
     C                   if        *in60 = *off
     C                   if        cractp = *blank
     C                   eval      cractp = 'X'
      *** beg chg ***** 05/19/06 *******************************************
     C***********        eval      srdti = dsdate
     C***********        call      'SRCVTDAT'    dtcymd
     C***********        movel     srdto         cracdt
     C                   eval      srdti = xxpddt
     C                   call      'SRCVTDAT'    dtydm
     C                   movel     srdto         cracdt
      *** end chg ***** 05/19/06 *******************************************
     C                   update    pcr01
     C                   endif                                                  if cractp = *blank
     C                   endif                                                  if *in60 = *on
     C                   endif                                                  if xxrccd = 'T'
     C                   enddo
      *
      *  END THE PROGRAM
      *
     C                   eval      *inlr = *on
