     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W8605R - CASE DISTRIBUTION REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 03/15/06 C:132 PROJECT: WA136 - 51691                         *
     H*                Add Check for test group and bypass all w/in   *
     H*                test groups.                                   *
     H*                                                               *
     H* 11/11/99 C:118 PROJECT: WC577                                 *
     H*                Add 'Report Selections' to routing page and    *
     H*                headings to detail pages.                      *
     H*                                                               *
     H* 12/09/98 C:15 Support: WC082/13725                            *
     H*               Removed code which selected 'Med Only' claims   *
     H*               based on the NCCI code.                         *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     F*** BEGIN DLT *** 12/09/98 **************************************
     F************ WTABL   IF  E           K        DISK
     F*** END DLT ***** 12/09/98 **************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D LET             S              1    DIM(26) CTDATA PERRCD(26)            ALPHABET
     D FL              S              1    DIM(26)                              LETTERS/STAF
     D OS              S              1    DIM(45)                              OUTPUT STRING
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CDLSN                  1     30
     D  XXFRST                 1      1
     D  XXFILL                 2     30
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETERS FILE.
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY1        KLIST
     C                   KFLD                    XXSTAF            3
     C*
     C*   KEY LIST FOR CLAIM MASTER FILE - FOR SAME COMPANY.
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR NCCI CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQTYPE            1
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDT8             8 0
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C***** BEGIN ADD **** 11/11/99 ***********************************
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C***** END   ADD **** 11/11/99 ***********************************
     C     QQTYPE        COMP      'S'                                    71
     C     QQTYPE        COMP      'A'                                    72
     C     QQTYPE        COMP      'M'                                    73
     C                   TIME                    DSDTTM
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C********   CMFND     IFNE 999
     C*
     C*   IF MEDICAL ONLY SELECTED, CHECK LOST TIME FIELD
     C*
     C                   MOVE      'Y'           XXPASS            1
     C*** BEGIN DLT *** 12/09/98 **************************************
     C********** *IN73     IFEQ '1'
     C**********           MOVE *BLANKS   XXKEY  10
     C**********           MOVELCMNCCI    XXKEY
     C********** TBKEY     CHAINPNC01                60
     C********** *IN60     IFEQ '1'
     C********** NCTYPE    ORNE 'M'
     C**********           MOVE 'N'       XXPASS
     C**********           END
     C**********           ENDIF                             *IN73=1
     C*** END DLT ***** 12/09/98 **************************************
     C*
     C     XXPASS        IFEQ      'Y'
     C*
     C  N11              EXSR      SR1ST
     C*
     C*   CHECK FOR FUND LEVEL BREAK
     C*
     C     CMFND         IFNE      XHFND
     C                   EXSR      SRFBRK
     C                   END
     C*
      ***** BEG ADD ***** 03/15/06 ***************************************
      * If current Group is a Test Group, bypass until group changes.
     C     FNTEST        CABEQ     'Y'           NXTRCD
      ***** END ADD ***** O3/15/06 ***************************************
     C*
     C*   CHECK FOR STAF CODE BREAK
     C*
     C   71              MOVE      CMSUPR        SVSTAF            3
     C  N71              MOVE      CMCLEX        SVSTAF
     C     SVSTAF        IFNE      XXSTAF
     C                   EXSR      SRSBRK
     C                   END
     C*
     C                   ADD       1             XXCNT
     C*
     C*   GET FIRST LETTER OF CLAIMANTS LAST NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 3 0
     C     XXFRST        LOOKUP    LET(X)                                 60
     C   60              MOVE      '1'           FL(X)
     C                   END
     C*
     C                   END
     C*
     C**********           END
     C*
      ***** BEG ADD ***** 03/15/06 ***************************************
     C     NXTRCD        TAG
      ***** END ADD ***** O3/15/06 ***************************************
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C*
     C     XXCNT         IFNE      0
     C                   EXSR      SRSBRK
     C                   EXCEPT    FTOT
     C                   END
     C*
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*=========================================================**
     C*   SET-UP / INITIALIZE          S R 1 S T                **
     C*=========================================================**
     CSR   SR1ST         BEGSR
     C*
     C*   GET THE USER PARMETER FILE
     C*
     C     UPKEY         CHAIN     PUP02                              60
     C*
     C*   GET THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*   GET THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         XHFND             3 0
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 11/11/99 ***********************************
     C*
     C*  LEFT JUSTIFY TITLE FOR ROUTING PAGE
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL           80
     C                   ENDIF
     C                   ENDIF
     C*
     C* CENTER TITLE FOR HEADING
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
     C*
     C*  CENTER COMPANY NAME FOR HEADING
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C***** END   ADD **** 11/11/99 ***********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C*  GET FIRST STAFF CODE NAME
     C*
     C   71              MOVE      CMSUPR        XXSTAF            3
     C  N71              MOVE      CMCLEX        XXSTAF
     C                   MOVE      *BLANKS       XXSTNM
     C     XXSTAF        IFNE      *BLANKS
     C     UPKEY1        CHAIN     PUP01                              60
     C                   MOVEL     UPUSNM        XXSTNM           25
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADF
     C                   MOVE      '1'           *IN11
     C*
     C*   INITIALIZE
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             XXFCNT            6 0
     C                   MOVE      *ALL'0'       FL
     C*
     C                   ENDSR
     C*=========================================================**
     C*   LEVEL BREAK ON FUND#         S R S F R K              **
     C*=========================================================**
     C     SRFBRK        BEGSR
     C*
     C                   EXSR      SRSBRK
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADF
     C                   MOVE      '0'           *INOF
     C                   END
     C*
      ***** BEG CHG ***** 03/15/06 ***************************************
     C***************      EXCPTFTOT
     C     XXFCNT        IFGT      *ZEROS
     C                   EXCEPT    FTOT
     C                   ENDIF
      ***** END CHG ***** O3/15/06 ***************************************
     C                   Z-ADD     0             XXFCNT
     C*
     C*   GET THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         XHFND             3 0
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
      ***** BEG CHG ***** 03/15/06 ***************************************
     C****************     EXCPTHEADF
     C     FNTEST        IFNE      'Y'
     C                   EXCEPT    HEADF
     C                   ENDIF
      ***** END CHG ***** O3/15/06 ***************************************
     C*
     C                   ENDSR
     C*=========================================================**
     C*   LEVEL BREAK ON STAFF CODE    S R S B R K              **
     C*=========================================================**
     C     SRSBRK        BEGSR
     C*
     C                   ADD       XXCNT         XXFCNT
     C*
     C*  GET OUTPUT STRING OF ALL LETTERS ASSIGNED TO STAFF CODE
     C*
     C                   MOVE      *BLANKS       XXSTRG           45
     C                   EXSR      SRSTRG
     C                   MOVEA     OS            XXSTRG
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADF
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET
     C*
     C   71              MOVE      CMSUPR        XXSTAF            3
     C  N71              MOVE      CMCLEX        XXSTAF
     C                   MOVE      *BLANKS       XXSTNM
     C     XXSTAF        IFNE      *BLANKS
     C     UPKEY1        CHAIN     PUP01                              60
     C  N60              MOVEL     UPUSNM        XXSTNM           25
     C   60              MOVEL     XXSTAF        XXSTNM
     C                   END
     C                   Z-ADD     0             XXCNT
     C                   MOVE      *ALL'0'       FL
     C*
     C                   ENDSR
     C*
     C*=========================================================**
     C*   LOAD LETTER RANGE STRING     S R S T R G              **
     C*=========================================================**
     C     SRSTRG        BEGSR
     C*
     C                   MOVE      *BLANKS       OS
     C                   Z-ADD     1             X
     C                   Z-ADD     2             I                 3 0
     C                   MOVE      '('           OS(1)
     C     '1'           LOOKUP    FL(X)                                  60
     C     *IN60         DOWEQ     '1'
     C*
     C                   MOVE      LET(X)        OS(I)
     C                   ADD       1             I
     C     X             ADD       1             Z                 3 0
     C     RNGE          TAG
     C     Z             IFLE      26
     C*
     C     '0'           LOOKUP    FL(Z)                                  60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '-'           OS(I)
     C                   ADD       1             I
     C                   MOVE      LET(26)       OS(I)
     C*
     C                   ELSE
     C*
     C                   SUB       1             Z
     C     X             IFEQ      Z
     C*
     C                   ADD       1             Z
     C     '1'           LOOKUP    FL(Z)                                  60
     C     *IN60         IFEQ      '1'
     C                   MOVE      ','           OS(I)
     C                   ADD       2             I
     C                   MOVE      LET(Z)        OS(I)
     C     Z             ADD       1             Y                 3 0
     C     Y             IFLT      27
     C     FL(Y)         IFEQ      '1'
     C                   Z-ADD     Z             X
     C                   ADD       1             I
     C                   GOTO      RNGE
     C                   END
     C                   ELSE
     C                   MOVE      '0'           *IN60
     C                   END
     C                   ELSE
     C                   SUB       1             I
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      '-'           OS(I)
     C                   ADD       1             I
     C                   MOVE      LET(Z)        OS(I)
     C*
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C                   SUB       1             I
     C                   MOVE      '0'           *IN60
     C*
     C                   END
     C*
     C     Z             ADD       1             X
     C                   ADD       1             I
     C   60'1'           LOOKUP    FL(X)                                  60
     C   60              MOVE      ','           OS(I)
     C   60              ADD       2             I
     C*
     C                   END
     C*
     C                   MOVE      ')'           OS(I)
     C*
     C                   ENDSR
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
     O**********             UPUSNM              67
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG **** 11/11/99 ***********************************
     O**********               ORTITL    80
     O                       XXTITL             100
     O***** END   CHG **** 11/11/99 ***********************************
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
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O***** BEGIN ADD **** 11/11/99 ***********************************
     O          E            ROUTE       2  2
     O                                           23 'REPORT SELECTIONS'
     O          E            ROUTE          2
     O                                           20 'RUN DATE :'
     O                       XXDATE        Y     30
     O          E            ROUTE          1
     O                                           20 'REPORT BY:'
     O                  71                       32 'SUPERVISOR'
     O                  72                       30 'ADJUSTOR'
     O                  73                       34 'MEDICAL ONLY'
     O***** END   ADD **** 11/11/99 ***********************************
     O          E            ROUTE            60
      ***** BEG CHG ***** 03/15/06 ***************************************
     O*****************                  23 '(c) Copyright Progressi'
     O******************                    've Data Solutions, Inc.,'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                23 '(c) Copyright Fiserv In'
     O***                                   'surance Solutions, Inc.,'
     O***                      *YEAR   +  1
      ****** END DLT ***** 01/28/10 **********************************
      ***** END CHG ***** O3/15/06 ***************************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              93
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           09 'RUN DATE:'
     O***** BEGIN CHG **** 11/11/99 ***********************************
     O**********               XXDATEY   17
     O                       XXDATE        Y     20
     O**********          71             39 'BY SUPERVISOR  '
     O                  71                       49 'BY SUPERVISOR (LOST TI'
     O                  71                     +000 'ME & POTENTIAL LOST TI'
     O                  71                     +000 'ME, OPEN CLAIMS ONLY)'
     O**********          72             39 'BY ADJUSTOR    '
     O                  72                       49 'BY ADJUSTOR (LOST TIME'
     O                  72                     +000 ' & POTENTIAL LOST TIME'
     O                  72                     +000 ', OPEN CLAIMS ONLY)'
     O**********          73             54 'MEDICAL ONLY   '
     O                  73                       49 'BY ADJUSTOR (MEDICAL '
     O                  73                     +000 '& RECORD ONLY, OPEN '
     O                  73                     +000 'CLAIMS)'
     O***** END   CHG **** 11/11/99 ***********************************
     O***** BEGIN ADD **** 11/11/99 ***********************************
     O          E            HEAD           1
     O                                           37 '# OF OPEN'
     O                                           47 'CLAIMANT'
     O          E            HEAD           2
     O                  71                       15 'SUPERVISOR'
     O                 N71                       13 'ADJUSTOR'
     O                                           37 'CLAIMS'
     O                                           50 'ALPHA RANGE'
     O***** END   ADD **** 11/11/99 ***********************************
     O          E            HEADF          2
     O                                              'GROUP:'
     O                       XHFND         Z   +  1
     O                       FNNAME            +  1
     O          E            DET            1
     O***** BEGIN CHG **** 11/11/99 ***********************************
     O**********               XXSTNM    25
     O**********               XXCNT J   33
     O**********               XXSTRG    79
     O                       XXSTNM              30
     O                       XXCNT         J     38
     O                       XXSTRG              84
     O***** END   CHG **** 11/11/99 ***********************************
     O          E            FTOT        1  3
     O                                           19 'TOTAL GROUP'
     O                       XHFND         Z     23
     O                                           24 ':'
     O***** BEGIN CHG **** 11/11/99 ***********************************
     O**********               XXFCNTJ   33
     O                       XXFCNT        J     38
     O***** END   CHG **** 11/11/99 ***********************************
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
