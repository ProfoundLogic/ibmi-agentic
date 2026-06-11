     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H/TITLE F8105R1 - PURGE EOMB FILES
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE FUND       *
     H*       MASTER FILES FOR THE SELECTED FUND                     *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    F8105C1 - EOMB PURGE OVERRIDE CL                          *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:61                                            *
     H*  CREATED:    02/03/98    PROJECT: 60APK                      *
     H****************************************************************
     H*                                                              *
     H* REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                              *
     H*  03/28/00 C:53 PROJECT MC228                                 *
     H*                ADD FILE FDOSP TO THE PROCESS.                *
     H*                RMV UDPOP FROM PGM. MOVED TO F8115R           *
     H*                                                              *
     H****************************************************************
     H*                                                              *
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H* 08 = BATCH MODE                                              *
     H* 09 = DEMAND MODE                                             *
     H* 30 = DETERMINES GLOBAL FUND DELETE                           *
     H* 60 = GENERAL CHAIN                                           *
     H*                                                              *
     H****************************************************************
      *
      * SYSTEM FILES
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
      *
      *
     FNAMBL     UF   E           K DISK
     F*** BEG DEL *** 03/28/00 **************************
     F***UDPOL   UF  E           K        DISK
     F*** END DEL *** 03/28/00 **************************
     FFATHL     UF   E           K DISK
     FFATHL1    UF   E           K DISK
     F                                     RENAME(PTH01:PTH02)
     F*** BEG ADD *** 03/28/00 **************************
     FFDOSL     UF   E           K DISK
     F*** END ADD *** 03/28/00 **************************
      *
      * zzz
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D DAFDS           DS
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *   KEY LIST FOR ALL FILES
      *

     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR ALL FILES (GLOBAL DELETE)
      *
     C     XXKEYG        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB
      *
      *   KEY LIST FOR FILES WITHOUT SUB# IN KEY (WC)
      *
     C     XXKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
      *
      *   KEY LIST FOR FILES WITHOUT SUB# IN KEY (WC)
      *
     C     XXKYG2        KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE REPORT OVERRIDE
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  FS BASE CONTROL FILE
      *
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
      *
     C     *LIKE         DEFINE    XXFND         XXWCF#
      *
     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@SUB             5
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
      *
     C     X@FND         COMP      '000'                                  30
      *
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@SUB         XXSUB             5 0
     C                   MOVE      X@QUE         QQQUE            10
     C  N30              MOVE      X@FND         XXFND             3 0
     C   30              Z-ADD     *ZEROS        XXFND
      *
      *   SET-UP AND INITIALIZE
      *
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *   CLEAR ACCUMULATION FIELDS
      *
     C                   Z-ADD     *ZERO         XX#REC            6 0
     C                   Z-ADD     *ZERO         XXTREC            8 0
      *
      *   CHAIN TO THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
     C                   MOVE      X@CO#         XXCO#
     C                   MOVE      UDATE         XXDATE            6 0
      *
      *   CHAIN TO THE COMPANY FILE
      *
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   CONTROL INFO
      *
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C*** END   DEL *** 08/15/00 **************************************
      *
      *   PICKUP WC INFO FOR CHAIN
      *
     C     XXKEY         CHAIN     PFN01                              40
     C                   Z-ADD     FNWCF#        XXWCF#
      *
      *   GET THE DISTRIBUTION INFORMATION
      *
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
      *
      *   PRINT THE DISTRIBUTION INFORMATION
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C                   MOVE      *ON           *INOF
      *
     C                   EXCEPT    HEAD
      *
      *  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
      ****************************************************************
      *
      *  NAMBP - Medical Management Bill Transfer File
      *
     C  N30XXKEY         CHAIN     PMB01                              60
     C   30XXKEYG        CHAIN     PMB01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PMB01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PMB01                                  60                R
     C   30XXKEYG        READE     PMB01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'NAMBP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG DEL *** 03/28/00 **************************
     C**********
     C***UDPOP*- Pre-cert Procedure Code File
     C**********
     C**N30***** XXKEY     CHAINPPO01                60
     C***30***** XXKEYG    CHAINPPO01                60
     C**********
     C********** *IN60     DOWEQ'0'
     C**********           DELETPPO01
     C**********           ADD  1         XX#REC
     C**********           ADD  1         XXTREC
     C**N30***** XXKEY     READEPPO01                    60            R
     C***30***** XXKEYG    READEPPO01                    60            R
     C**********           END
     C***OA*****           EXCPTHEAD
     C**********           MOVE '0'       *INOA
     C**********           MOVEL'UDPOP'   XXFILE 10
     C**********           EXCPTDET
     C*** END DEL *** 03/28/00 **************************
      *
      *  FATHP - WCMC Case Transfer History file (FROM records)
      *
     C  N30XXKEY         CHAIN     PTH01                              60
     C   30XXKEYG        CHAIN     PTH01                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PTH01                                  60                R
     C   30XXKEYG        READE     PTH01                                  60                R
     C                   END
      *
      *  FATHP - WCMC Case Transfer History file (TO records)
      *
     C  N30XXKEY         CHAIN     PTH02                              60
     C   30XXKEYG        CHAIN     PTH02                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTH02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     PTH02                                  60                R
     C   30XXKEYG        READE     PTH02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FATHP'       XXFILE           10
     C                   EXCEPT    DET
     C*** BEG ADD *** 03/28/00 **************************
     C*
     C*  FDOSP - WCMC Outcome/Savings Transaction File
     C*
     C  N30XXKEY         CHAIN     POS01                              60
     C   30XXKEYG        CHAIN     POS01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   DELETE    POS01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C  N30XXKEY         READE     POS01                                  60                R
     C   30XXKEYG        READE     POS01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'FDOSP'       XXFILE           10
     C                   EXCEPT    DET
     C*** END ADD *** 03/28/00 **************************
      *
      * zzz
      *
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTL
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           20 'CO#:'
     O                       XXCO#               25
     O          E            ROUTE          2
     O                                           20 'DATE:'
     O                       XXDATE              30 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1996'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ****************************************************************
      *     HEADING
      ****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       XXDATE        Y    130
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              94
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           2
     O                                           18 'FILE NAME'
     O                                           44 '# DELETED RECORDS'
      ****************************************************************
      *     DETAIL LINE WITH FILE NAME AND COUNT
      ****************************************************************
     O          E            DET            1
     O                       XXFILE              19
     O                       XX#REC        JB    40
      ****************************************************************
      *     TOTAL RECORD COUNT FOR ALL FILES
      ****************************************************************
     O          E            TOTL        2  2
     O                                           18 'ALL FILES'
     O                       XXTREC        J     40
