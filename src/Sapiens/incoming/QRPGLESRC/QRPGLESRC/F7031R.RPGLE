     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE F7031R - CPT-4 PROCEDURE CODE LISTING
     H*****************************************************************
     H**  PROGRAM DESCRIPTION:  CPT-4 PROCEDURE CODE LISTING         **
     H**                                                             **
     H*****************************************************************
     H**  REVISIONS:                                                 **
     H*** 09/18/91 *** WLD *** PER 9999/LD *** PROJ #: NONE ***********
     H**   1.  Added ASC MRA field to output report and readjusted   **
     H**       the existing output fields.                           **
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
     H*  10/27/00 C:62   PROJECT:MC205                                *
     H*                  CORRECT PLACEMENT OF RANGE                   *
     H*                                                               *
     H*  04/15/94 C:20   REQ/PROJ: 128/FS127                          *
     H*                  1. FOR MA ADD IN USE OF PR92N1 FOR ASC GROUP *
     H*                                                               *
     H*  07/20/93 C:20   PROJ: AL006                                  *
     H*                  1. ADD IN USE OF PR92N5 FOR DIA AMOUNT       *
     H*                  2. CHANGED HEADINGS IN FDCFP FILE FOR NEW    *
     H*                     SPLTY CODE AMOUNTS                        *
     H*                                                               *
     H*  06/30/93 C:20   PROJ: FSMNT                                  *
     H*                  1. FIX TO COPYRIGHT DATES ON REPORT          *
     H*                                                               *
     H*  04/26/93 C:20   PROJ: FS098                                  *
     H*                  1. ADDED CODE TO GET GENERIC HEADINGS FROM   *
     H*                     FDCFP DEPENDING UPON STATE                *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMPRL     IF   E           K DISK
     FFDCFL     IF   E           K DISK
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CPT-4 PROCEDURE CODE FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     PRKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCPT4            5
     C                   KFLD                    XFEFDT
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  STATE CPT FIELD HEADINGS
     C*
     C     CFKEY         KLIST
     C                   KFLD                    XXST
     C*
     C     *LIKE         DEFINE    CFDT          X2EFDT
     C     *LIKE         DEFINE    CFDT          XFEFDT
     C     *LIKE         DEFINE    CFDT          XXRCDS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQST              2
     C                   PARM                    QQFROM            5
     C                   PARM                    QQTO              5
     C                   PARM                    QQEFDT            8
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQST          XXST              2
     C                   MOVE      QQFROM        XXFROM            5
     C                   MOVE      QQTO          XXTO              5
     C     QQEFDT        IFNE      *BLANK
     C*
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XFEFDT
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   ELSE
     C                   Z-ADD     0             XFEFDT
     C                   END
     C                   Z-ADD     0             XXRCDS
     C                   MOVEA     '000000'      *IN(31)
     C                   MOVE      '1'           *IN37
     C                   MOVE      '0'           *IN38                          ADD 07/20/93
     C                   MOVE      '0'           *IN39                          ADD 07/20/93
     C     XXST          IFEQ      'AL'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN38                          ADD 07/20/93
     C                   ELSE
     C     XXST          IFEQ      'FL'
     C                   MOVE      '1'           *IN31
     C                   ELSE
     C     XXST          IFEQ      'GA'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '0'           *IN37
     C                   ELSE
     C     XXST          IFEQ      'LA'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN35
     C                   ELSE
     C     XXST          IFEQ      'MA'
     C                   MOVE      '1'           *IN31
     C                   ELSE
     C     XXST          IFEQ      'NY'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN33
     C                   END                                                    END = NY
     C                   END                                                    END = MA
     C                   END                                                    END = LA
     C                   END                                                    END = GA
     C                   END                                                    END = FL
     C                   END                                                    END = AL
     C*
     C     CFKEY         CHAIN     PCF01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CFSMRA        XXHMRA           10
     C                   MOVEL     CF92S1        XX92H1           10
     C                   MOVEL     CF92S2        XX92H2           10
     C                   MOVEL     CF92S3        XX92H3           10
     C                   MOVEL     CF92S4        XX92H4           10
     C                   MOVEL     CF92S5        XX92H5           10
     C                   MOVEL     CF1S3         XX1H3             8
     C                   MOVEL     CF1S4         XX1H4             8
     C                   MOVEL     CF1S5         XX1H5             8
     C                   ELSE
     C                   MOVE      *BLANKS       XXHMRA
     C                   MOVE      *BLANKS       XX92H1
     C                   MOVE      *BLANKS       XX92H2
     C                   MOVE      *BLANKS       XX92H3
     C                   MOVE      *BLANKS       XX92H4                         ADD 07/20/93
     C                   MOVE      *BLANKS       XX92H5                         ADD 07/20/93
     C                   MOVE      *BLANKS       XX1H3
     C                   MOVE      *BLANKS       XX1H4
     C                   MOVE      *BLANKS       XX1H5
     C                   END
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   DETERMINE LOOP CONDITIONS
     C*
     C     XXFROM        IFEQ      *BLANK
     C     XXTO          ANDEQ     *BLANK
     C                   MOVE      '99999'       XXTO
     C                   END
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   DETERMINE REPORT SELECTIONS
     C*
     C     XXFROM        IFEQ      *BLANK
     C                   MOVEL     '*ALL'        RSFCDE            5
     C                   ELSE
     C                   MOVEL     XXFROM        RSFCDE            5
     C                   END
     C     XXTO          IFEQ      *BLANK
     C                   MOVEL     '*ALL'        RSTCDE            5
     C                   ELSE
     C                   MOVEL     XXTO          RSTCDE            5
     C                   END
     C     QQEFDT        IFEQ      *BLANK
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RSEFDT            6 0
     C*
     C                   END
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*   PRINT HEADINGS
     C*
     C                   EXCEPT    HEAD
     C*
     C*  GET STARTING RECORD AND START LOOP
     C*
     C                   MOVEL     XXFROM        XXCPT4
     C     PRKEY1        SETLL     PPR01
     C     PRKEY         READE     PPR01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     PRCPT4        ANDLE     XXTO
     C*
     C*   IF EFFECTIVE DATE IS SELECTED, ONLY PRINT RECORDS FOR THAT DATE
     C     XFEFDT        IFNE      *ZERO
     C     PREFDT        ANDEQ     XFEFDT
     C     XFEFDT        OREQ      *ZERO
     C*
     C*   Change posting date from YYMMDD to MMDDYY
     C*
     C                   MOVE      PREFDT        X2EFDT
     C*
     C                   Z-ADD     X2EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C*   PRINT THE DETAIL
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DET
     C*
     C                   ADD       1             XXRCDS
     C                   END                                                    END NE *ZEROS/EQ EFD
     C*
     C     PRKEY         READE     PPR01                                  60
     C                   END
     C*
     C*   END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOT
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
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
     O**********     08      ORDIS3              60
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
     O               08      ORDIS3              60
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
     O**********E            ROUTE       2  2
     O                                           24 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           24 'STATE:'
     O                       XXST                28
     O          E            ROUTE          2
     O                                           24 'FROM CODE:'
     O                       RSFCDE              31
     O          E            ROUTE          2
     O                                           24 'TO CODE:'
     O                       RSTCDE              31
     O          E            ROUTE          2
     O                                           24 'EFFECTIVE DATE:'
     O               80                          30 '*ALL'
     O                       RSEFDT              34 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                63 'INC. 1997            '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           50 'STATE:'
     O                       XXST                53
     O                                           90 'RANGE:'
     O                       XXFROM              98
     O                                          103 'TO'
     O                       XXTO               112
     O*** BEGIN ADD *** 10/27/00 **************************************
     O          E            HEAD           1
     O*** END   ADD *** 10/27/00 **************************************
     O                                          105 'EFFECTIVE DATE:'
     O               80                         110 '*ALL'
     O                       RSEFDT             114 '  /  /  '
     O          E            HEAD           1
     O                                           16 'EFFECTIVE'
     O                                           35 'FU'
     O                                           49 'ANES/MI/HR'
     O               37                          55 'ANES'
     O               37                          77 'PROF/PT'
     O                                           83 'BR/NC'
     O                                           87 'AST'
     O                                           91 'FS'
     O                                           97 'SUP'
     O               34      XX1H4              173
     O               35      XX1H3              185
     O               36      XX1H5              194
     O          E            HEAD           2
     O                                            4 'CODE'
     O                                           13 'DATE'
     O                       XXHMRA              29
     O                                           36 'DAYS'
     O                                           49 'RNT AMOUNT'
     O               37                          55 'FLAG'
     O                                           69 'PROF/PT AMT.'
     O               37                          75 'FLAG'
     O                                           82 'ANE'
     O                                           87 'SUR'
     O                                           93 'REIMB'
     O                                           97 'INC'
     O               31      XX92H1             109
     O               32      XX92H2             123
     O               33      XX92H3             137
     O               38      XX92H4             151
     O               39      XX92H5             166
     O               34                         172 'FLAG'
     O               35                         181 'FLAG'
     O               36                         190 'FLAG'
     O                                          198 'ST'
     O          E            DET            1
     O                       PRCPT4               5
     O                       XXEFDT              15 '  /  /  '
     O                       PRMRA         J     30
     O                       PRFOLU        Z     35
     O                       PRANAM        J     50
     O                       PR1A2               53
     O                       PRPROF        J     69
     O                       PR1A1               73
     O                       PRRPT               80
     O                       PRSURG              86
     O                       PRFSR               90
     O                       PRSUPP              96
     O               31      PR92N1        K    110
     O               32      PR92N2        K    124
     O               33      PR92N3        K    138
     O               38      PR92N4        K    152
     O               39      PR92N5        K    166
     O               34      PR1A4              172
     O               35      PR1A3              181
     O               36      PR1A5              190
     O                       PRSTAT             197
     O          E            DET            2
     O                       PRDSC1              60
     O                       PRDSC2             110
     O          E            TOT         2  1
     O                                          101 '# OF RECORDS LISTED:'
     O                       XXRCDS        Z    110
