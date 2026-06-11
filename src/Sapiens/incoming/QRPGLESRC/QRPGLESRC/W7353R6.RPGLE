     hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/TITLE W7353R6 - Financial Data Files Extract program           \               *
     H dftactgrp(*no) actgrp(*caller)
     h*********************************************************************************
     h*  Description:  Transfer extraction data to WCSTAT file for tape transfer.     *
     h*                                                                               *
     h*  Called from: W7353C6                                                         *
     h*  Calls to:    W7351R                                                          *
     h*                                                                               *
     h*  Created: 11/23/10    Programmer C:190   Project:139WPQ                       *
     h*********************************************************************************
     h*  Revisions:                                                                   *
     H*                                                                               *
     H*  01/22/13  C:287 Seq#: SY266_OVR                                              *
     H*                  Added DFTACTGRP and ACTGRP compile options                   *
     h*                                                                               *
     h*********************************************************************************
     fwxgsl     if   e           k disk
     fwmssl     uf a e           k disk
     fwmfnl     if   e           k disk
     f                                     infds(fnfdb)
     f*
     d  rpmnth         s              5  0
     d**wrkdate        s              5  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8

     D psds          esds                  extname(szq1p)

     d fnfdb           ds
     d fnrrn                 397    400b 0

     d*  Date/time data structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     d                 ds
     d wrkdate                 1      5  0
     d  xxcy                   1      3  0
     d  xxmm                   4      5  0

     d*----------------------------
     d rptDate         s               d   datfmt(*iso)
     d #@wrkdt         s               d   datfmt(*iso)
     d*----------------------------

     c*---------------
     c*  Key list
     c*---------------
     c     fnky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0
     c
     c     ssky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkrpmy            5 0
     c*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     c     dtcym2        plist
     c                   parm                    srdti
     c                   parm      '*YYMD'       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     c     *entry        plist
     c                   parm                    @@frmy            5
     c                   parm                    @@trmy            5
     c                   parm                    @@rptl            1
     c                   parm                    @@mode            1
     c
     c*
     c                   time                    dsdttm
     c*
     c* Move the From month/year and To month/year values into numeric fields.
     c                   move      @@frmy        ##frmy            5 0
     c                   move      @@trmy        ##trmy            5 0
     c                   move      @@rptl        ##rptl            1 0

     c* Read over WXGSL and for each record where the include/exclude field is set to 'I'.
     c                   read      pgs01
     c                   dow       not(%eof)
     c                   if        gsiefl = 'I'
     c                   exsr      SrMain
     c                   endif
     c                   read      pgs01
     c                   enddo
     C*
     C                   seton                                        lr
     C*==========================================================================
     C*  Srmain   - subroutine
     C*==========================================================================
     c     SrMain        begsr
     c*
     c                   eval      wrkdate = ##frmy

     c                   dow       wrkdate <= ##trmy
     c                   move      *zeros        ##wrkdt           7 0
     c                   move      01            ##wrkdt
     c                   movel     wrkdate       ##wrkdt
     c                   eval      srdti1 = ##wrkdt
     C                   call      'SRCVTDAT'    dtcymd1
     C                   movel     srdto         @@wrkdt           8 0          Submission date
     C                   movel     @@wrkdt       #@wrkdt
     c     #@wrkdt       Adddur    18:*months    Rptdate

     c                   move      Rptdate       @@rpdt            8 0
     c                   eval      srdti  = @@rpdt
     C                   call      'SRCVTDAT'    dtcym2
     C                   movel     srdto         @#rtdt            7 0          Submission date
     c                   movel     @#rtdt        rpmnth
     c                   exsr      Writer

     C*  Add one to process next reporting month/year.
     C                   add       1             wrkdate
     c                   if        xxmm = 13
     c                   add       1             xxcy
     c                   eval      xxmm = 1
     c                   endif                                                  xxmm = 13

     c                   enddo                                                  wrkdate <= ##frmy

     c                   endsr                                                  Srmain
     C*==========================================================================
     C*  Writer   - subroutine
     C*==========================================================================
     c     Writer        begsr
      *
     c                   eval      kkco# = gsco#
     c                   eval      kkfnd = gsfnd
     c                   eval      kkrpmy= rpmnth
      *
     c     ssky          chain     pss01
     c                   if        not(%found)
     c                   eval      ssco#  = kkco#
     c                   eval      ssfnd  =  kkfnd
     c                   eval      ssrpmy =  rpmnth                             REP mnth/yr
     c                   eval      ssrptl = ##rptl                              Repoting levals
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         sssbdt                         Submission date
     c                   eval      ssxtrc = 'M'                                 Extract data flag
     c                   eval      ssuser = q1user
     c                   eval      sspgm  = q1pgm
     c                   eval      sstime = dstime
     C                   movel(p)  srdto         ssdt
     c                   eval      ssiusr = q1user
     C                   movel(p)  srdto         ssidt

     c                   write     pss01

     C*  Call to W7351R
     C     fnky          chain     pfn01                              99
     c                   move      fnrrn         qqrrn             9
     c                   move      ssrpmy        qqrpmy            5
     c                   move      ssrptl        qqrptl            2
     c                   move      @@mode        qqmode            1

     c                   call      'W7351R'
     c                   parm                    qqrrn
     c                   parm                    qqrpmy
     c                   parm                    qqrptl
     c                   parm                    qqmode
      *
     c                   endif                                                  *in60 = *on
      *
     c                   endsr                                                  Writer

