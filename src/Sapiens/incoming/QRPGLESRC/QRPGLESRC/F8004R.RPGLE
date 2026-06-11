    H*****************************************************************
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
    H*****************************************************************
    H*  F8004R - RECONSIDERATION CREDIT REPORT       C:62 02/19/02   *
    H*                                               PROJ: 48MEZ     *
    H*  CALLED FROM: F8004C                                          *
    H*  CALLS TO:    NONE
    H*                                                               *
    H*****************************************************************
    H*  REVISIONS:                                                   *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:XXX  Project: SY215                               *
     H*           C:189  Retrieve brand Name
     H*                                                               *
    H*                                                               *
    H*****************************************************************
    H*  INDICATORS USED:                                             *
    H*                                                               *
    H*****************************************************************
    F*
    F*  Physical file definition (for maintaining record)
     Ffaeal1    if   e           k disk
     Fsmorl     if   e           k disk
     Ffmsml     if   e           k disk
     Ffmfnl     if   e           k disk
    F*
    F* Check requests created
    F*
     Fqsysprt   o    f  198        printer oflind(*inoa)
    F*
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D psds          esds                  extname(szq1p)
     D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
    D*
     D comapi          c                   const('FA013R')
     D title           c                   const('Reconsideration-
     D                                      Credit Report')
     D*** BEGIN CHG *********** 01/28/10 ************************************
     D*** pdsnam          c                const('Progressive Data So-
     D***                                  lutions, Inc.')
     D    pdsnam       S             40    INZ
     D*** END CHG   ********* 01/28/10 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##data          s             80
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fyr           s              3  0
     D ##leng          s              3  0
     D ##sub#          s              5  0
     D @@rtcd          s              1
     D hdfnd           s                   like(fnfnd)
     D hdsub#          s                   like(smsub#)
     D kkfil#          s                   like(orfil#)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxconm          s             40
     D xxfnd           s                   like(fnfnd)
     D xxfnnm          s             40
     D xxsbnm          s             40
     D xxsub#          s                   like(smsub#)
     D xxbamt          s             11  2
     D xxnamt          s             11  2
     D xxtotal         s             11  2
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@rtcd
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    kkfil#
    C*
     C     smky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
    C*
     C     fnky          klist
     C                   kfld                    eaco#
     C                   kfld                    easub#
     C                   kfld                    eafnd
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*
    C*****************************************************************
    C*
    C* EXECUTE MAIN SUBROUTINE
    C*
     C                   exsr      mainsr
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkfil# = '0';
       Chain Orky Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
    C*
    C*================================================================
    C*  CHKCHG - Check for change in subscriber and/or group
    C*================================================================
     C     chkchg        begsr
    C*
     C                   if        easub# <> hdsub#
     C                             or eafnd <> hdfnd
    C*
     C                   eval      hdsub# = easub#
     C                   eval      hdfnd = eafnd
    C*
     C                   exsr      getcof
     C                   except    head
     C                   eval      *inoa = *off
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C*  CHKOVF - Check the overflow of QSYSPRT.
    C*================================================================
     C     chkovf        begsr
    C*
     C                   if        *inoa = *on
    C*
     C                   except    head
     C                   eval      *inoa = *off
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*================================================================
     C     getcof        begsr
    C*
     C                   call      comapi        compl                  99
    C*
    C* GET THE COMPANY NUMBER AND COMPANY NAME
    C*
     C                   if        *in99 = *on
    C*
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval       ##data   = brandname
     C***  End   Chg  ***  01/28/10  **************************************
    C*
     C                   else
    C*
     C                   movel(p)  ##conm        ##data
    C*
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        xxconm
    C*
    C* GET THE SUBSCRIBER AND SUBSCRIBER NAME
    C*
     C     smky          chain     psm01                              99
    C*
     C                   if        *in99 = *off
    C*
     C                   eval      xxsub# = smsub#
     C                   movel(p)  smname        xxsbnm
    C*
     C                   else
    C*
     C                   eval      xxsub# = *zeros
     C                   clear                   xxsbnm
    C*
     C                   endif
    C*
    C* GET THE GROUP AND GROUP NAME
    C*
     C     fnky          chain     pfn01                              99
    C*
     C                   if        *in99 = *off
    C*
     C                   eval      xxfnd = fnfnd
     C                   movel(p)  fnname        xxfnnm
    C*
     C                   else
    C*
     C                   eval      xxfnd = *zeros
     C                   clear                   xxfnnm
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* LOADRP - LOAD REPORT FIELDS
    C*================================================================
     C     loadrp        begsr
    C*
     c                   if        eabamt < 0
     c                   setoff                                       72
     c                   add       eabamt        xxbamt
     c                   else
     c                   seton                                        72
     c                   endif
    C*
     c                   if        eanamt < 0
     c                   setoff                                       73
     c                   add       eanamt        xxnamt
     c                   else
     c                   seton                                        73
     c                   endif
    C*
     C                   except    det1
    C*
     C                   endsr
    C*================================================================
    C* MAINSR - MAIN SUBROUTINE
    C*================================================================
     C     mainsr        begsr
    C*
     C                   time                    dsdttm
     C                   eval      hdsub# = *zeros
     C                   eval      hdfnd = *zeros
    C*
     C                   movel     '0'           kkfil#
    C*
     C     orky          chain     por01                              99
    C*
     C                   if        *in99 = *off
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 *********************************************
     C                   endif
    C*
     c                   eval      xxbamt = 0
     c                   eval      xxnamt = 0
     c                   eval      xxtotal = 0
    C*
     C                   read      pea01                                  99
    C*
     C                   if        *in99 = *off
    C*
     C                   dow       *in99 = *off
    C*
     C                   exsr      chkchg
     C                   exsr      chkovf
     C                   exsr      loadrp
    C*
     C                   read      pea01                                  99
    C*
     C                   enddo
    C*
     c                   eval      xxtotal = xxbamt + xxnamt
     c                   except    tot
    C*
     C                   else
    C*
     C                   except    head
     C                   except    error
    C*
     C                   endif
    C*
     C                   exsr      pgmend
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    O*****************************************************************
     Oqsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                 13 'XXXXXXXX'
     O**********                                 25 'XXXXXX'
     O**********                                 39 'XX      XX'
     O**********                                 50 'XXXXXXXXXX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XXX     XX'
     O**********                                 82 'XXXXXX'
     O**********e            route          1
     O**********                                 14 'XXXXXXXXX'
     O**********                                 26 'XXXXXXXX'
     O**********                                 39 'XX      XX'
     O**********                                 50 'XXXXXXXXXX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XXXX    XX'
     O**********                                 83 'XXXXXXXX'
     O**********e            route          1
     O**********                                 15 'XX      XX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XXXX    XX'
     O**********                                 84 'XX      XX'
     O**********e            route          1
     O**********                                 15 'XX      XX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX XX   XX'
     O**********                                 76 'XX'
     O**********e            route          1
     O**********                                 14 'XXXXXXXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX XX   XX'
     O**********                                 76 'XX'
     O**********e            route          1
     O**********                                 13 'XXXXXXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX  XX  XX'
     O**********                                 76 'XX'
     O**********e            route          1
     O**********                                  9 'XXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX  XX  XX'
     O**********                                 84 'XX    XXXX'
     O**********e            route          1
     O**********                                 10 'XXXXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX   XX XX'
     O**********                                 84 'XX    XXXX'
     O**********e            route          1
     O**********                                 11 'XX XXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX   XX XX'
     O**********                                 84 'XX      XX'
     O**********e            route          1
     O**********                                 12 'XX  XXX'
     O**********                                 27 'XX      XX'
     O**********                                 39 'XX      XX'
     O**********                                 46 'XX'
     O**********                                 57 'XX'
     O**********                                 72 'XX    XXXX'
     O**********                                 84 'XX      XX'
     O**********e            route          1
     O**********                                 13 'XX   XXX'
     O**********                                 26 'XXXXXXXX'
     O**********                                 38 'XXXXXXXX'
     O**********                                 46 'XX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XX    XXXX'
     O**********                                 83 'XXXXXXXX'
     O**********e            route          3
     O**********                                 14 'XX    XXX'
     O**********                                 25 'XXXXXX'
     O**********                                 37 'XXXXXX'
     O**********                                 46 'XX'
     O**********                                 61 'XXXXXXXXXX'
     O**********                                 72 'XX     XXX'
     O**********                                 82 'XXXXXX'
     O**********e            route          1
     O**********                                 15 'ROUTE TO:'
     O**********             q1user              26
     O**********e            route          1
     O**********e            route          1
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          1
     O                                           15 'ROUTE TO:'
     O                       q1user              26
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          2
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       title               34
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********e            route          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             orpnam              30
     O**********                                 36 'FILE:'
     O**********             orfil#              38
     O**********                                 46 'COPIES:'
     O**********             orcopy        z     50
     O**********                                 56 'HOLD:'
     O**********             orhold              61
     O**********e            route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             oroutq              30
     O**********                                 37 'FORMS:'
     O**********             orform              48
     O**********                                 56 'SAVE:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 *********************************************
     O          e            route            60
     O                                           20 '(C) COPYRIGHT PROGRE'
     O                                           41 'SSIVE DATA SOLUTIONS,'
     O                                           58 'INC. 2000       '
     Oqsysprt   e            head           1 02
     O                       dstime               8 '  :  :  '
     O                       xxconm              97
     O                       dsdate             157 '0  /  /    '
     O          e            head           2
     O                                           10 'F8004R    '
     O                                           74 '    Reconsidera'
     O                                           95 'tion Credit Report   '
     O                                          152 'PAGE:'
     O                       page               157
     O          e            head           1
     O                                            5 'Sub#:'
     O                       xxsub#        4   +001
     O                       xxsbnm            +001
     O          e            head           2
     O                                            5 'Grp#:'
     O                       xxfnd         4   +003
     O                       xxfnnm            +001
     O          e            head           1
     O                                            3 'Yr'
     O                                           14 'Case'
     O                                           26 'EOMB#'
     O                                           37 'Rcn'
     O                                           57 'Charge'
     O                                           77 'MRA'
     O                                           97 'Other1'
     O                                          117 'Other2'
     O                                          137 'Paid'
     O                                          157 'BRS fee'
     O                                          177 'Network Fee'
     O          e            det1           1
     O                       eafyr         4      3
     O                       eacase        4     14
     O                       eaeob#        4     26
     O                       earcn         4     37
     O                       eachrg        n     57 '$'
     O                       eamra         n     77 '$'
     O                       eaothr        n     97 '$'
     O                       eaoth2        n    117 '$'
     O                       eapaid        n    137 '$'
     O              n72      eabamt        n    157 '$'
     O               72                         157 '----------'
     O              n73      eanamt        n    177 '$'
     O               73                         177 '----------'
     O          e            tot            1
     O                                           17 'Total BRS Credit:'
     O                       xxbamt        n   +001 '$'
     O                                         +002 'Total Network Credit:'
     O                       xxnamt        n   +001 '$'
     O                                         +002 'Total Fee Credit:'
     O                       xxtotal       n   +001 '$'
     O          e            error          1
     O                                           70 'No EOMBs with any Credit'
     O                                           72 's.'
