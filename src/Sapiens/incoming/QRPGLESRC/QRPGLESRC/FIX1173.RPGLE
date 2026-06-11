    H* (C) Copyright Progressive Data Solutions, Inc., 2004
    H*
    H/TITLE FIX1173 - Initialize WTCDP Table
    H*****************************************************************
      * Revised:
      *
      * 07/16/04  C:61  Project: WC1173
      *                 Changed Table values.
      *
      * 01/27/04  C:61  Project: WC1173
      *                 Created
    H*                                                               *
    H*****************************************************************
      *
     *
     * System company
     Fsmcol     if   e           k disk
     *
     * Contractor Credit Table
     Fwtcdl     uf a e           k disk
     *
     *
     d awul            s              5  2 dim(56) ctdata perrcd(1)
     d fact            s              5  2 dim(56) alt(awul)
     *
     *
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D psds          esds                  extname(szq1p)
     *
     *
     *
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D qqco#           s              3
      *
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0
     D xxjrst          s                   like(cdst)
     D xxawul          s                   like(cdawul)
     *
     *
     *
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     *  Contractor Credit Factor Table
     *
     C     cdky          klist
     C                   kfld                    coco#
     C                   kfld                    xxjrst
     C                   kfld                    xxawul
     *
     *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
      *
     c     *loval        setll     pco01
     c                   read      pco01
      *
     c                   dow       not %eof
     C                   move      'XX'          xxjrst
     c                   eval      x = 1
     *
     C                   dow       x <= 56
      *
     c                   if        x = 22
     c                   eval      xxjrst = 'NY'
     c                   endif
      *
     c                   eval      xxawul = awul(x)
     c     cdky          chain     pcd01
      *
     c                   if        %found
     c                   exsr      srupd
     c                   else
     c                   exsr      srwrt
     c                   endif
     *
     c                   eval      x = x + 1
     C                   enddo
      *
     c                   read      pco01
     C                   enddo
      *
     *
     *   END OF PROGRAM
     C                   move      *on           *inlr
      *
      *
      *
      *=========================================================
      * srupd - update existing record
      *=========================================================
     c     srupd         begsr
      *
     c                   eval      cdco#  = coco#
     c                   eval      cdst   = xxjrst
     c                   eval      cdawul = awul(x)
     c                   eval      cdfact = fact(x)
      *
     C                   eval      cduser = q1user
     C                   eval      cdpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      cdtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         cddt
      *
     C                   update    pcd01
     c                   endsr
      *
      *
      *
      *=========================================================
      * srwrt - write new record
      *=========================================================
     c     srwrt         begsr
     c                   clear                   pcd01
      *
     c                   eval      cdco#  = coco#
     c                   eval      cdst   = xxjrst
     c                   eval      cdawul = awul(x)
     c                   eval      cdfact = fact(x)
      *
     C                   eval      cduser = q1user
     C                   eval      cdiusr = q1user
     C                   eval      cdpgm = q1pgm
     C                   eval      cdipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      cdtime = dstime
     C                   eval      cditm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         cddt
     C                   movel(p)  srdto         cdidt
     C                   write     pcd01
      *
     c                   endsr
      *
      *
**
0099900500
0115000600
0117500700
0120000800
0122500900
0125001000
0127501100
0130001200
0132501300
0135001400
0137501500
0140001600
0142501700
0145001800
0147501900
0150002000
0155002100
0160002200
0165002300
0170002400
0175002500
0129900000
0130000300
0140000400
0150000500
0160000600
0170000700
0180000800
0190000900
0195001000
0200001100
0205001200
0210001300
0215001400
0220001500
0225001600
0230001700
0235001800
0240001900
0245002000
0250002100
0260002200
0270002300
0280002400
0290002500
0300002600
0310002700
0320002800
0330002900
0340003000
0350003100
0360003200
0370003300
0380003400
0390003500
9990003500
