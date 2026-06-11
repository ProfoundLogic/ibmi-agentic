     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H/TITLE FIXWMWVP2 - Move Waiver Flat Charge
     H*****************************************************************
     H*
     H* Copy previouly entered Flat Charge Amounts to standard fields.
     H*
     H*
     H*****************************************************************
     H* Revised:
     H*
     H* 12/02/09 C:61 Project 132WMP
     H*               Created.
     H*
     H*****************************************************************
      *
     Fsmcol     if   e           k disk
      *
      * Waiver data
     Fwmwvl     uf a e           k disk
      *
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)

     D psds          esds                  extname(szq1p)


     c     dtcymd        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      '*CYMD'       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1
      *
      *===============================================
      * Main
      *===============================================

      * for each Waiver Record

     c     *loval        setll     pwv01
     c                   read      pwv01

     c                   dow       not %eof
     c                   eval      *in71 = *off

     c                   if        wvcfcg <> 0
     c                             and wvfchg = 0
     c                   eval      wvfchg = wvcfcg
     c                   eval      *in71 = *on
     c                   endif

     c                   if        wvcpct <> 0
     c                             and wvswpc = 0
     c                   eval      wvswpc = wvcpct
     c                   eval      *in71 = *on
     c                   endif

     c   71              exsr      srupdate

     c                   read      pwv01
     c                   enddo

     c                   eval      *inlr = *on
      *
      *   END OF PROGRAM
      *
      *=========================================================
      * srupdate - update record
      *=========================================================
     c     srupdate      begsr
      *
     C                   eval      wvuser = q1user
     C                   eval      wvpgm = q1pgm
      *
     C                   time                    dsdttm
     C                   eval      wvtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         wvdt
     C                   update    pwv01
      *
     c                   endsr
      *
