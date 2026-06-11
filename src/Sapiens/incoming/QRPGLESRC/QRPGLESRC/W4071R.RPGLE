     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4071R - AGENT ASSIGNMENT LIST
     H*****************************************************************
     H* REVISIONS:                                                    *
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
     H* 01/03/07 C:136 SUPPORT: WA139 61694                           *
     H*                ENLARGE THE PRODUCER ARRAY TO PREVENT          *
     H*                OUT OF BOUNDS ERROR                            *
     H*                                                               *
     H*   C:58 6/10/00  PROJECT WC993                                 *
     H*                 Select only non-test groups.                  *
     H*                                                               *
     H*  10/14/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                                                               *
     H* 09/22/92 KWK  PROJECT WC179                                   *
     H*               FIXED PROBLEM WITH PRODUCER FIRST NAME NOT      *
     H*               APPEARING ON THE REPORT AFTER A "LONG" PRODUCER *
     H*               NAME IS PROCESSED BY SRNAME.                    *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMAGL1    IF   E           K DISK
     FWMAAL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     F**** BEGIN ADD *** 06/10/00 ************************************
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
      *** BEG CHG ***** 01/03/07 *************************************
     D***********         PRD       999 35               PRODUCERS
     D***********         FA        999  8               GROUP & AGT
     D***********         FL        999  3               FLAGS
     D PRD             S             35    DIM(9999)                            PRODUCERS
     D FA              S              8    DIM(9999)                            GROUP & AGT
     D FL              S              3    DIM(9999)                            FLAGS
      *** END CHG ***** 01/03/07 *************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AGPZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQQUE            10
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AGFEIN
     C                   KFLD                    AGMOD
     C*
     C*   KEY LIST FOR AGENT PRODUCER FILE
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AGFEIN
     C                   KFLD                    AGMOD
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFND
     C*
     C     *LIKE         DEFINE    AACO#         SVCO#
     C     *LIKE         DEFINE    AAFND         SVFND
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '1'           *INOF
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C********           MOVE      '0'           XXPRT
     C********ORKEY         CHAIN     POR01                              60
     C*********IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C********           EXCEPT    ROUTE
     C********           END
      ***** End Del ***** 08/26/14 ************************************
     C*
     C*   START READ THE AGENT MASTER FILE
     C*
     C     AGKEY         CHAIN     PAG01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     AGSTAT        IFEQ      'A'
     C*
     C*  LOAD AGENT ADDRESS AND PHONE
     C*
     C                   MOVEL     AGPHN1        XXPHN1            3 0
     C                   MOVE      AGPHN1        XXPHN2            4 0
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGPST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGPCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C*  INITIALIZE ARRAYS
     C*
     C                   MOVE      *BLANKS       PRD
     C                   MOVE      *BLANKS       FA
     C                   MOVE      *BLANKS       FL
     C*
     C*  CHAIN TO AGENT ASSIGNMENT FILE, LOAD GROUP/AGENT & FLAG ARRAY
     C*
     C                   Z-ADD     0             X                 4 0
     C     AAKEY         CHAIN     PAA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*   Get group master.
     C     AACO#         IFNE      SVCO#
     C     AAFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF                                                  .63 = ON
     C                   Z-ADD     AACO#         SVCO#
     C                   Z-ADD     AAFND         SVFND
     C                   ENDIF                                                  .CO/FND <>
     C**** END ADD ***** 06/10/00 ************************************
     C*
     C     AASTAT        IFEQ      'A'
     C**** BEG CHANGE ** 06/10/00 ************************************
     C     FNTEST        ANDNE     'Y'
     C********** AAFND     ANDNE999
     C**** END CHANGE ** 06/10/00 ************************************
     C                   ADD       1             X
     C                   MOVEL     AAFND         FA(X)
     C                   MOVE      AAAGT#        FA(X)
     C                   MOVEL     AAMLFL        XXHOLD            2
     C                   MOVE      AACMFL        XXHOLD
     C                   MOVEL     XXHOLD        FL(X)
     C                   MOVE      AAPRFL        FL(X)
     C                   END
     C*
     C     AAKEY         READE     PAA01                                  60
     C*
     C*** BEGIN ADD *** 10/14/97 **************************************
     C*
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C*** END   ADD *** 10/14/97 **************************************
     C*
     C*
     C                   END
     C*
     C*  CHAIN TO AGENT PRODUCER FILE
     C*
     C                   Z-ADD     0             X
     C     APKEY         CHAIN     PAP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  LOAD PRODUCER NAME INTO PRODUCER ARRAY
     C*
     C     APSTAT        IFEQ      'A'
     C                   MOVE      *BLANKS       XXPNME           35
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C                   MOVEL     APLSN         QQLAST           56
     C                   MOVEL     APFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXPNME
     C                   ADD       1             X
     C                   MOVE      XXPNME        PRD(X)
     C                   END
     C*
     C     APKEY         READE     PAP01                                  60
     C*
     C*** BEGIN ADD *** 10/14/97 **************************************
     C*
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C*
     C*** END   ADD *** 10/14/97 **************************************
     C*
     C                   END
     C*
     C*  PRINT THE AGENT INFORMATION
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   Z-ADD     1             X
     C     X             DOUEQ     5
     C                   MOVEL     FA(X)         XXFND             3 0
     C                   MOVE      FA(X)         XXAGT#            5 0
     C                   MOVEL     FL(X)         XXMLFL            1
     C                   MOVEL     FL(X)         XXHOLD            2
     C                   MOVE      XXHOLD        XXCMFL            1
     C                   MOVE      FL(X)         XXPRFL            1
     C     X             IFEQ      1
     C                   EXCEPT    DET1
     C                   END
     C     X             IFEQ      2
     C                   EXCEPT    DET2
     C                   END
     C     X             IFEQ      3
     C                   EXCEPT    DET3
     C                   END
     C     X             IFEQ      4
     C                   EXCEPT    DET4
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C                   MOVE      '0'           *IN60
     C     *IN60         DOWEQ     '0'
     C*
     C     PRD(X)        IFEQ      *BLANKS
     C     FA(X)         ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN60
     C*
     C                   ELSE
     C*
     C                   MOVEL     PRD(X)        XXPNME
     C                   MOVEL     FA(X)         XXFND             3 0
     C                   MOVE      FA(X)         XXAGT#            5 0
     C                   MOVEL     FL(X)         XXMLFL            1
     C                   MOVEL     FL(X)         XXHOLD            2
     C                   MOVE      XXHOLD        XXCMFL            1
     C                   MOVE      FL(X)         XXPRFL            1
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DET5
     C                   ADD       1             X
     C*
     C                   END
     C                   END
     C*
     C                   END
     C     AGKEY         READE     PAG01                                  60
     C                   END
     C*
     C*** BEGIN ADD *** 10/14/97 **************************************
     C*
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C     *IN61         IFEQ      '0'
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
     C*** END   ADD *** 10/14/97 **************************************
     C*
     C*
      /Free
       // ***** Beg Add ***** 08/26/14 ********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %Found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          3
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O*
     O*  SELECTION CRITERIA ADDED TO ROUTING PAGE
     O*
     O          E            ROUTE          2
     O                                           25 'SELECTION CRITERIA:'
     O          E            ROUTE          1
     O                                           14 'COMPANY:'
     O                       XXCO#         Z     18
     O                       CONAME            +  2
     O*
     O*
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O                       ORTITL              91
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           83 'APPROVED'
     O                                           93 'AGENT'
     O                                          106 'MAIL'
     O                                          121 'COMMISSION'
     O                                          132 'PROSPECT'
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
     O          E            HEAD           1
     O                                           20 'AGENT NAME, ADDRESS,'
     O                                           30 'AND PHONE'
     O                                           50 'PRODUCERS'
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O                                           82 'GROUPS'
     O                                           93 'NUMBER'
     O                                          106 'FLAG'
     O                                          118 'FLAG'
     O                                          130 'FLAG'
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
     O          E            DET1        1  1
     O                       AGNAME              40
     O                       PRD(1)              76
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O                       XXFND         Z     82
     O                       XXAGT#        Z     93
     O                       XXMLFL             104
     O                       XXCMFL             116
     O                       XXPRFL             128
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
     O          E            DET2           1
     O                       AGPAD1              40
     O                       PRD(2)              76
     O                       XXFND         Z     82
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O                       XXAGT#        Z     93
     O                       XXMLFL             104
     O                       XXCMFL             116
     O                       XXPRFL             128
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
     O          E            DET3           1
     O                       XXCSZ               40
     O                       PRD(3)              76
     O                       XXFND         Z     82
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O                       XXAGT#        Z     93
     O                       XXMLFL             104
     O                       XXCMFL             116
     O                       XXPRFL             128
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
     O          E            DET4           1
     O                                            1 '('
     O                       AGARE1               4
     O                                            5 ')'
     O                       XXPHN1               9
     O                                           10 '-'
     O                       XXPHN2              14
     O                       PRD(4)              76
     O                       XXFND         Z     82
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O                       XXAGT#        Z     93
     O                       XXMLFL             104
     O                       XXCMFL             116
     O                       XXPRFL             128
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
     O          E            DET5           1
     O                       XXPNME              76
     O                       XXFND         Z     82
     O*
     O*** BEGIN ADD *** 10/14/97 **************************************
     O*
     O                       XXAGT#        Z     93
     O                       XXMLFL             104
     O                       XXCMFL             116
     O                       XXPRFL             128
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC          1
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
     O*
     O*** END   ADD *** 10/14/97 **************************************
     O*
