    H* (c) 2012 StoneRiver, Inc. All rights reserved.
    H* StoneRiver is a trademark of StoneRiver, Inc.
     H DFTACTGRP(*NO) ACTGRP(*caller)
    H*****************************************************************
    H*  F1492R - CLAIMCHECK PAY AND PURSUE REPORT                    *
    H*                                                               *
    H*  Created: 06/21/12  C:280 Project: 114MER                     *
    H*                                                               *
    H*****************************************************************
    H* REVISIONS:                                                    *
    H*                                                               *
    H*                                                               *
    H*****************************************************************
    F*
     FFDKHL4    UF   E           K DISK
    F*
    F* Claim Check History File by CO#/SUB#/FND/FYR/CASE/EOB#/RCN/LIN
    F*
    F*
     FFDCAL24   IF   E           K DISK
    F*
    F*  Detail Charge File by CO/SUB/FND/FYR/CASE/CPT4/DSEQ
    F*
     FQSYSPRT   O    F  132        printer oflind(*inof)
    D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D date            s              6  0
     D found           s              1
    D*
     DPSDS           ESDS                  Extname(SZQ1P)
    D*
    D*================================================================
    D*  User defined data structures and fields
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D                 ds
     D  kheob                  1      3
     D  denial                 1      1
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     key           klist
1    C                   kfld                    khco#
2    C                   kfld                    khsub#
3    C                   kfld                    khfnd
4    C                   kfld                    khfyr
5    C                   kfld                    khcase
6    C                   kfld                    kheob#
7    C                   kfld                    khdseq
    C*
    C*----------------------------------------------------------------------
    C*  Read through FDKHL1 file
    C*----------------------------------------------------------------------
    C*
    C* Force OverFlow
    C*
     C                   eval      *inof = *on
    C*
    C* Read data
    C*
     C                   Read      FDKHL4                                 99
    C*
b01  C                   Dow       Not *in99
    C*
    C* If History Indicator equal to 1 and Denial is D or P
    C*
b02  C                   if        khckhs = '1' and
     C                             denial = 'D' or
     C                             khckhs = '1' and
     C                             denial = 'P'
    C*
    C* Get Detail Charge File
    C*
     C     key           chain     FDCAL24                            98
    C*
b03  C                   Dow       Not *in98
    C*
    C* Convert Date
    C*
     C                   eval      srdti = CAFDAT
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hdos              6 0
    C*
     C                   eval      srdti = khdos
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         fdat              6 0
    C*
     C                   time                    dsdttm
    C*
    C* Print Heading
    C*
b04  C                   if        *inof = *on
     C                   except    header
     C                   eval      *inof = *off
e04  C                   endif
    C*
    C* print detail
    C*
     C                   Except    detail
    C*
    C* Read Next Record
    C*
     C     key           reade     FDCAL24                                98
    C*
e03  C                   Enddo
    C*
    C* Now Update
    C*
     C                   eval      KHSTAT = 'P'
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   move      srdto         khdt
     C                   eval      khtime = dstime
    C*
     C                   eval      khuser = q1user
     C                   eval      khpgm  = q1pgm
    C*
     C                   Update    PKH01
    C*
e02  C                   Endif
    C*
    C* Read Next Record
    C*
     C                   Read      FDKHL4                                 99
     C*
e01  C                   Enddo
     C*
     C                   eval      *inlr = *on
     C*
     C**********************************************************************
     O*
     Oqsysprt   e            header         1  2
     O                                            5 'Time:'
     O                       DSTIME              14 '  :  :  '
     O                                           76 'Claim Check Pay and Pursue'
     O                                           83 'Report'
     O                                          122 'Date:'
     O                       DSDATE             132 '  /  /    '
     O*
     O          E            HEADER         2
     O                                            4 'Pgm:'
     O                       Q1PGM               15
     O                                          122 'Page:'
     O                       PAGE          Z    132
     O*
     O          E            HEADER         1
     O                                            8 'Current'
     O                                           72 'History'
     O*
     O          E            HEADER         2
     O                                            6 'EOB#'
     O                                           20 'Line #'
     O                                           30 'DOS'
     O                                           46 'Proc Code'
     O                                           59 'Paid Amt'
     O                                           70 'EOB#'
     O                                           84 'Line #'
     O                                           94 'DOS'
     O                                          110 'Proc Code'
     O                                          122 'Paid Amt'
     O*
     O          E            DETAIL         1
     O                       KHMCEB        Z      9
     O                       KHMLIN        Z     21
     O                       HDOS          Y     33
     O                       KHMCOD              44
     O                       CAPAID        J     61
     O                       KHEOB#        Z     73
     O                       KHLIN#        Z     85
     O                       FDAT          Y     97
     O                       KHPRCD             108
     O                       KHPAID        J    124
