     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H/TITLE F9000R - UR REPORT FOR CASES > AMOUNT - PRINT PROGRAM
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                              **
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
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
     H*  01/15/92 LJD  For 003/JG: Move output to right 3 bytes       *
     H*                                                              **
     H*  12/22/93 C:20     REQ/PROJ: 8/SY008                         **
     H*                    1. DO NOT USE "UP" VALUES BECAUSE PROGRAM **
     H*                       IS SUBMITTED                           **
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING                           *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF CLAIM FILE                        *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*       80      ALL COMPANIES TO BE PROCESSED                   *
     H*      N80      ONE COMPANY TO BE PROCESSED                     *
     H*       81      ALL FUNDS TO BE PROCESSED                       *
     H*      N81      ONE FUND TO BE PROCESSED                        *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  FNNM                   1     60
     D                                     DIM(60) ASCEND                       FUND NAME
     D  FNNAME                 1     60
     D                 DS
     D  CASE                   1     10
     D  XXYR                   1      2  0
     D  XXHYP1                 3      3
     D  XXCASE                 4     10  0
     D                 DS
     D  SS#                    1     11
     D  X1SS#                  1      3
     D  XXHYP2                 4      4
     D  X2SS#                  5      6
     D  XXHYP3                 7      7
     D  X3SS#                  8     11
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*  FUND MASTER
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  SUB# MASTER
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*  CLAIM MASTER
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  OUTPUT ROUTING
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  USER PARMETER FILE
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C*** BEGIN CHG *** 12/22/39 **************************************
     C**********           KFLD           UPCO#
     C                   KFLD                    XXCO#
     C*** END CHG *** 12/22/93 ****************************************
     C                   KFLD                    XXKEY            10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQAMT             9
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQCO#         SLCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQSUB#        SLSUB#            5 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFND         SLFND             3 0
     C                   MOVE      QQAMT         XXAMT             9 0
     C                   MOVE      QQAMT         SLAMT             9 0
     C*
     C*** DETERMINE KEY TO CLAIM FILE:
     C     XXCO#         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN80                          ALL CO
     C                   ELSE
     C                   MOVE      '0'           *IN80                          ONE CO
     C                   END
     C     XXSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN81                          ALL SUBS
     C                   ELSE
     C                   MOVE      '0'           *IN81                          ONE SUBS
     C                   END
     C     XXFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN82                          ALL FND
     C                   ELSE
     C                   MOVE      '0'           *IN82                          ONE FND
     C                   END
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    FNNAME        XXFNME
     C                   MOVE      '-'           XXHYP1
     C                   MOVE      '-'           XXHYP2
     C                   MOVE      '-'           XXHYP3
     C                   Z-ADD     *ZEROS        XXTOTP           10 2
     C*
     C                   TIME                    DSDTTM
     C*** Get user parameter name
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*** Get output routing
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*
     C*** READ CLAIM FILE
     C*
     C     *LOVAL        SETLL     PCM01
     C   80
     CAN 81
     CAN 82              READ      PCM01                                  50     ALL CO/SUB/FND
     C  N80
     CAN 81
     CAN 82CMKEY         CHAIN     PCM01                              50         1 CO/1 SUB/ALL FND
     C  N80
     CANN81
     CAN 82CMKEY1        CHAIN     PCM01                              50         1 CO/ALL SUB/ALL FN
     C  N80
     CANN81
     CANN82CMKEY2        CHAIN     PCM01                              50         1 CO/1 SUB/1 FND
     C*
     C     *IN50         IFEQ      '0'
     C*** Get company name
     C                   MOVE      CMCO#         XXCO#
     C     XXCO#         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*** Get SUBSCRIBER NAME
     C                   MOVE      CMSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C*** Get fund name
     C                   MOVE      CMFND         XXFND
     C                   MOVEL     *BLANKS       XXFNME
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*** Remove leading blanks from Fund Name
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNME
     C                   END
     C                   EXSR      FIRST
     C                   ELSE
     C                   EXSR      FIRST
     C                   END                                                    END 0 *IN50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     CMCO#         IFNE      XXCO#
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    TOTCO#
     C                   ADD       TLCCAS        TLACAS           11 0
     C                   Z-ADD     *ZEROS        TLCCAS
     C                   MOVE      CMCO#         XXCO#
     C     XXCO#         CHAIN     PCO01                              60
     C                   EXCEPT    HEAD
     C                   END                                                    END NE XXCO#
     C*
     C     CMSUB#        IFNE      XXSUB#
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    TOTSUB
     C                   ADD       TLSCAS        TLCCAS           10 0
     C                   Z-ADD     *ZEROS        TLSCAS
     C                   MOVE      CMSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              60
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END
     C**********           END                             END NE XXFND
     C*
     C     CMFND         IFNE      XXFND
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    TOTFND
     C                   ADD       TLFCAS        TLSCAS           10 0
     C                   Z-ADD     *ZEROS        TLFCAS
     C                   MOVE      CMFND         XXFND
     C                   MOVEL     *BLANKS       XXFNME
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*** Remove leading blanks from Fund Name
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XXFNME
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END
     C                   END                                                    END NE XXFND
     C*
     C*** Get total payments
     C                   Z-ADD     CMAR01        XXTOTP
     C*
     C*** If total payments are greater than AMOUNT, then process case for rpt
     C     XXTOTP        IFGT      XXAMT
     C*
     C*** Build case number
     C                   Z-ADD     CMFYR         XXYR              2 0
     C                   Z-ADD     CMCASE        XXCASE
     C*
     C*** Build SS#
     C                   MOVEL     CMSS#         X1SS#
     C                   MOVE      CMSS#         XHSS#             6
     C                   MOVEL     XHSS#         X2SS#
     C                   MOVE      XHSS#         X3SS#
     C*
     C                   MOVE      CMSS#         XXSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*** Get claimant name
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C     CMSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         XXLAST
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANKS
     C                   MOVEL     XXLAST        XXNAME           40
     C                   END                                                    END 0 *IN60
     C*
     C*** Print Detail Line
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        SS# = MaskData('SSN':'SS#':SS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DETAIL
     C                   ADD       1             TLFCAS            9 0
     C*
     C                   END                                                    END GT AMOUNT
     C*
     C   80
     CAN 81
     CAN 82              READ      PCM01                                  50     ALL CO/SUB/FND
     C  N80
     CAN 81
     CAN 82CMKEY         READE     PCM01                                  50     1 CO/1 SUB/ALL FND
     C  N80
     CANN81
     CAN 82CMKEY1        READE     PCM01                                  50     1 CO/ALL SUB/ALL FN
     C  N80
     CANN81
     CANN82CMKEY2        READE     PCM01                                  50     1 CO/1 SUB/1 FND
     C*
     C                   END                                                    END DOWEQ 50
     C*** PRINT FINAL TOTALS
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    TOTFND
     C                   ADD       TLFCAS        TLSCAS
     C                   EXCEPT    TOTSUB
     C                   ADD       TLSCAS        TLCCAS
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    TOTCO#
     C                   ADD       TLCCAS        TLACAS
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END                                                    END 1 *INOF
     C                   EXCEPT    TOTALL
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*** If first time, Print Output Routing & Headings before detail
     C*  BEGIN SUBROUTINE
     C*
     C     FIRST         BEGSR
      ***** Beg Del ***** 08/26/14 *********************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   ENDSR
     C*
     C*
     C***********************************************************************
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
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
     O                                           30 'SELECTION:'
     O          E            ROUTE          2
     O                                           30 'COMPANY:'
     O               80                          35 'ALL'
     O              N80      SLCO#               35
     O          E            ROUTE          2
     O                                           30 'SUB#:'
     O               81                          35 'ALL'
     O              N81      SLSUB#              35
     O          E            ROUTE          2
     O                                           30 'FUND:'
     O               82                          35 'ALL'
     O              N82      SLFND               35
     O          E            ROUTE          2
     O                                           30 'AMOUNT:'
     O                       SLAMT         2     44 '$'
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 1993, 1994'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE        Y     11
     O                       CONAME              73
     O                       DSTIME             104 '  :  :  '
     O          E            HEAD           1
     O                                            9 'F9000R'
     O                                           62 'UTILIZATION REVIEW'
     O                                           99 'PAGE:'
     O                       PAGE               104
     O          E            HEAD           2
     O                                           59 'CASES WITH PAYMENTS >'
     O                       XXAMT         2     72 '$'
     O          E            HEAD1          2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              17
     O                                           19 '-'
     O                       XXSBNM              60
     O          E            HEAD1          1
     O                                           11 'GROUP.....:'
     O                       XXFND               17
     O                                           19 '-'
     O                       XXFNME              80
     O          E            HEAD1          1
     O                                           87 'ACCIDENT'
     O                                          102 'TOTAL'
     O          E            HEAD1          2
     O                                           11 'CASE #'
     O                                           26 'CLAIMANT'
     O                                           74 'SOC. SEC. #'
     O                                           85 'DATE'
     O                                          104 'PAYMENTS'
     O          E            DETAIL         1
     O                       CASE                13
     O                       XXNAME              58
     O                       SS#                 74
     O                       XXACDT              87 '  /  /  '
     O                       XXTOTP        K    104
     O          E            TOTFND      1  1
     O                                           75 'TOTAL FUND CASES:'
     O                       TLFCAS        J     91
     O          E            TOTSUB      1  1
     O                                           75 'TOTAL SUB# CASES:'
     O                       TLSCAS        J     91
     O          E            TOTCO#      1  1
     O                                           75 'TOTAL CO# CASES:'
     O                       TLCCAS        J     91
     O          E            TOTALL      1  1
     O                                           75 'TOTAL ALL CASES:'
     O                       TLACAS        J     91
