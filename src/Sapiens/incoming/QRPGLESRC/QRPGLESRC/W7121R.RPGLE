     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7121R - Loss Ratio Report
     H***************************************************************
     H* REVISIONS:                                                  *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 10/17/01 C:61  SUPPORT WC065 29333                            *
     H*                Make Loss Ratio 72N instead of 6.2N.           *
     H*                                                             *
     H* 11/07/00  C:35   PROJECT: SUPP WA065 #24893                 *
     H*                  USE THE CORRECT POLICY YEAR WHEN CALLING   *
     H*                  THE SRPRTD ROUTINE.                        *
     H*                                                             *
     H* 04/10/00  C:118 PROJECT: WC987                              *
     H*                 Load alternate company name from fund master*
     H*                 if available.                               *
     H*                                                             *
     H* 07/19/99  C:118 PROJECT: WC898                              *
     H*                 Added region to parms passed in.  Region can*
     H*                 be one or all. Added 'Report Selections' to *
     H*                 routing page, adjusted titles on routing    *
     H*                 page and report.                            *
     H*                                                             *
     H* 05/25/99  C:118 SUPPORT: WA048/18181                        *
     H*                 COMMENT OUT 'CHEFDT ORNE XXYMD' SO REPORT   *
     H*                 RESULTS ARE CONSISTENT WITH UNDERWRITERS AND*
     H*                 EMPLOYER'S EXPERIENCE REPORT RESULTS.       *
     H*                                                             *
     H* 02/12/99  C:90  PROJECT: WA048/16478                        *
     H*                 FIXED SO THAT REPORT WILL PRINT CURRENT FYR *
     H*                 LOADED WHEN PRINTING GROUP TOTALS.          *
     H*                                                             *
     H* 12/23/97  C:35  PROJECT:WC577                               *
     H*                 FIXED SO THAT REPORT WILL PROCESS MULTIPLE  *
     H*                 FUND YEARS IF NO POLICY YEAR IS ENTERED.    *
     H*                                                             *
     H***************************************************************
     F*
     FWMCML     IF   E           K DISK
     F* * * Claims File
     F*
     FWMEML2    IF   E           K DISK
     F* * * Employer File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     FWDF1L     IF   E           K DISK
     F* * * Fund Detail File
     F*
     FWMREL     IF   E           K DISK
     F* * * Region Master File
     F*
     FWMCHL     IF   E           K DISK
     F* * * Claim History File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     F*
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F* * * Output File
     F*
     FWDELL     IF   E           K DISK
     F* * * EMPLOYER DETAIL FILE
     F*
     F*

     D*
     D*
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    F1FYR
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRGN
     C***** END   ADD **** 07/19/99 ***********************************
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXYMD
     C*
     C*  FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C*
     C*   KEY LIST FOR THE REGION MASTER FILE
     C*
     C     RGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    HFND
     C                   KFLD                    HRGN#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMPR
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMEMP#        HEMP#
     C     *LIKE         DEFINE    EMDIV         HDIV
     C     *LIKE         DEFINE    EMDT          XXCADT
     C     *LIKE         DEFINE    EMDT          XXYMD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQFYR             3
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C                   PARM                    QQRGN             3
     C***** END   ADD **** 07/19/99 ***********************************
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDTE         X1DATE            8 0
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C                   MOVE      QQRGN         XXRGN             3 0
     C***** END   ADD **** 07/19/99 ***********************************
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  GET FUND RECORD
     C*
     C     XXFND         IFEQ      *ZERO
     C     XXCO#         CHAIN     PFN01                              60
     C                   ELSE
     C     FNKEY         CHAIN     PFN01                              60
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     FNNAME        XFNAME           60
     C                   Z-ADD     FNFND         XXFND
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C*  CENTER COMPANY NAME IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C     XXFYR         IFEQ      *ZERO
     C     F1KY1         CHAIN     PF101                              60
     C                   ELSE
     C     F1KEY         CHAIN     PF101                              60
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      F1FYR         XXFYR2            2 0
     C                   MOVE      F1FYR         XXFYR
     C*
     C*  READ EMPLOYER FILE
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C     XXRGN         IFNE      *ZERO
     C     EMKY1         CHAIN     PEM01                              60
     C                   ELSE
     C***** END   ADD **** 07/19/99 ***********************************
     C     EMKEY         CHAIN     PEM01                              60
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C                   ENDIF
     C***** END   ADD **** 07/19/99 ***********************************
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     HFND          IFNE      *ZERO
     C     HFND          ANDNE     EMFND
     C*
     C     HFYR          ORNE      *ZERO
     C     HFYR          ANDNE     F1FYR
     C*
     C                   MOVE      '1'           *INL1
     C                   MOVE      '1'           *INL2
     C                   MOVE      '1'           *INL3
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   EXSR      SUBL3
     C   22              EXCEPT    HEAD
     C                   SETOFF                                       22
     C                   EXCEPT    TOTALS
     C                   MOVE      '1'           *IN22
     C                   GOTO      DETCLC
     C                   END
     C*
     C     HRGN#         IFNE      *ZERO
     C     HRGN#         ANDNE     EMRGN#
     C                   MOVE      '1'           *INL1
     C                   MOVE      '1'           *INL2
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   MOVE      '1'           *IN22
     C                   GOTO      DETCLC
     C                   END
     C*
     C     HEMP#         IFNE      *ZERO
     C     HEMP#         ANDNE     EMEMP#
     C                   MOVE      '1'           *INL1
     C                   EXSR      SUBL1
     C                   GOTO      DETCLC
     C                   END
     C*
     C     HEMP#         IFNE      *ZERO
     C     HEMP#         ANDEQ     EMEMP#
     C     HDIV          ANDNE     EMDIV
     C                   MOVE      '1'           *INL1
     C                   EXSR      SUBL1
     C                   END
     C*
     C     DETCLC        TAG
     C                   ADD       1             EMPTOT            5 0
     C                   MOVEL     EMNAME        ENAME            28
     C                   MOVE      EMRGN#        HRGN#             3 0
     C                   MOVE      EMEMP#        HEMP#
     C                   MOVE      EMFND         HFND              3 0
     C                   MOVE      EMCO#         HCO#              3 0
     C                   MOVE      EMDIV         HDIV
     C                   MOVE      F1FYR         HFYR              3 0
     C*
     C* Check to find out if employer was cancelled on or before the
     C* run date.
     C*
     C                   MOVE      QQDTE         X1CADT            8 0          LJQ 8-4-88
     C*
     C                   Z-ADD     X1CADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCADT
     C*                                                          "
     C*   FIND THE EFFECTIVE DATE                                "
     C                   Z-ADD     1             X                 1 0                "
     C     *ZERO         LOOKUP    ED(X)                                  60          "
     C     *IN60         IFEQ      '0'                                                "
     C                   Z-ADD     5             X                                    "
     C                   ELSE                                                         "
     C                   SUB       1             X                                    "
     C                   END                                                          "
     C     X             DOWNE     0                                                  "
     C     XXCADT        COMP      ED(X)                              60  60          "
     C  N60              SUB       1             X                                    "
     C  N60              END                                                          "
     C*                                                          "
     C*   CHECK CANCELLED DATE                                   "
     C     X             IFEQ      0                                                  "
     C                   MOVE      *BLANKS       CANFL             2                  "
     C*                                                          "
     C                   ELSE                                                         "
     C*                                                          "
     C     CD(X)         IFLE      XXCADT                                             "
     C     CD(X)         ANDNE     0                                                  "
     C                   MOVE      'CX'          CANFL                                "
     C                   ELSE                                                         "
     C                   MOVE      *BLANKS       CANFL                                "
     C                   END                                                          "
     C*                                                          "
     C                   END                                                          "
     C*
     C*****************************************************************
     C*
     C*  READ CLAIM FILE
     C*
     C                   Z-ADD     HCO#          XXCO#
     C                   Z-ADD     HFND          XXFND
     C                   Z-ADD     HEMP#         XXEMPR
     C                   Z-ADD     HDIV          XXDIV
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C**
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '1'
     C     CHCO#         ORNE      CMCO#
     C     CHFND         ORNE      CMFND
     C     CHFYR         ORNE      CMFYR
     C     CHCASE        ORNE      CMCASE
     C***** BEGIN DEL **** 05/25/99 ***********************************
     C********** CHEFDT    ORNE XXYMD
     C***** END   DEL **** 05/25/99 ***********************************
     C                   MOVE      CMSTAT        CHOCST
     C                   END
     C**
     C     CHOCST        IFEQ      'C'
     C                   ADD       1             L1CLOS            5 0
     C                   ELSE
     C                   ADD       1             L1OPEN            5 0
     C                   END
     C**
     C                   Z-ADD     *ZERO         TOTPMT
     C                   Z-ADD     *ZERO         TOTRES
     C**
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C**
     C**  ACCUMULATE PAYMENT AMOUNTS
     C                   ADD       CMA(R)        TOTPMT            9 2
     C**
     C**  ACCUMULATE RESERVE AMOUNTS
     C                   ADD       CMR(R)        TOTRES            9 2
     C**
     C                   END
     C                   ADD       TOTPMT        L1ITOT           11 2
     C                   ADD       TOTRES        L1ITOT
     C                   ADD       TOTPMT        L1PTOT           11 2
     C**
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C**
     C                   END
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C     XXRGN         IFNE      *ZERO
     C     EMKY1         READE     PEM01                                  60
     C                   ELSE
     C***** END   ADD **** 07/19/99 ***********************************
     C*
     C     EMKEY         READE     PEM01                                  60
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C                   ENDIF
     C***** END   ADD **** 07/19/99 ***********************************
     C                   END
     C*
     C     QQFYR         IFEQ      *BLANK
      **** BEGIN ADD **** 12/23/97 *********************************************
     C     QQFYR         OREQ      '000'
      **** END   ADD **** 12/23/97 *********************************************
     C     F1KY1         READE     PF101                                  60
     C                   ELSE
     C     F1KEY         READE     PF101                                  60
     C                   END
     C*
     C                   END
     C*
     C     QQFND         IFEQ      *BLANK
     C     XXCO#         READE     PFN01                                  60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C*
     C     END           TAG
     C                   EXSR      SUBL1
     C                   MOVE      '0'           *IN22
     C                   EXSR      SUBL2
     C                   MOVE      '1'           *INLR                          LJQ 4-15-88
     C***** BEGIN DEL **** 07/19/99 ***********************************
     C**********           EXCPTHEAD                       LJQ 4-15-88
     C***** END   DEL **** 07/19/99 ***********************************
     C                   MOVE      '0'           *IN22
     C                   EXSR      SUBL3
     C                   EXCEPT    TOTALS
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain orKey POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     C     SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* * * OPEN PRINT FILE
     C                   OPEN      QSYSPRT
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C*
     C*  LEFT JUSTIFY TITLE ON ROUTING PAGE
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C                   ENDIF
     C*
     C*  PRINT 'ALL' ON ROUTING PAGE IF ALL POLICY YEARS OR REGIONS CHOSEN
     C*
     C     QQFYR         IFEQ      *ZERO
     C                   MOVE      *ON           *IN85
     C                   ENDIF
     C*
     C     QQRGN         IFEQ      *ZERO
     C                   MOVE      *ON           *IN84
     C                   ENDIF
     C***** END   ADD **** 07/19/99 ***********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C***** BEGIN ADD **** 07/19/99 ***********************************
     C*
     C*  CENTER TITLE IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C*  CENTER COMPANY NAME IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C***** END   ADD **** 07/19/99 ***********************************
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *IN22
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
     C                   ENDSR
     C*
     C**
      *==================================================================*
      *   Premium Calculations             ** Subroutine **   S U B P R  *
      *==================================================================*
     C*
     C     SUBPR         BEGSR
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM THIS PERIOD
     C**
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     HFND          SRFND             3
      **** BEGIN CHG **** 11/07/00 *********************************************
     C******************** MOVELXXFYR     SRFYR   3
     C                   MOVEL     HFYR          SRFYR             3
      **** END   CHG **** 11/07/00 *********************************************
     C                   MOVEL     HEMP#         SREMP#            9
     C                   MOVEL     HDIV          SRDIV             5
     C                   MOVEL     X1DATE        QQDTE
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRTD'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         PRMYTD           11 2
     C                   MOVE      SRAMT2        PRMCUR           11 2
     C*
     C**   CALCULATE LOSS RATIO
     C     PRMYTD        IFNE      *ZERO
     C     L1ITOT        MULT      100           XXITOT           13 2
      ***** BEG CHG ******* 10/17/01 **********************************
     C*********  XXITOT    DIV  PRMYTD    LOSS    62H
     C     XXITOT        DIV(H)    PRMYTD        LOSS              7 2
      ***** END CHG ******* 10/17/01 **********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     PRMYTD        DIV(H)    1000          THPREM            9 2
     C     L1CLOS        ADD       L1OPEN        L1TOTC            5 0          EMPLR CLAIMS
     C*
     C     THPREM        IFNE      *ZERO
     C     L1TOTC        DIV(H)    THPREM        FREQ              4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
     C                   END
     C*
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 1 Calculations             ** Subroutine **   S U B L 1  *
      *==================================================================*
     C*
     C     SUBL1         BEGSR
     C*
     C                   EXSR      SUBPR
     C                   Z-ADD     PRMYTD        L1PREM           11 2          *
     C                   Z-ADD     FREQ          L1FREQ            4 2
      ***** BEG CHG ******* 10/17/01 **********************************
     C*********            Z-ADDLOSS      L1LOSS  62
     C                   Z-ADD     LOSS          L1LOSS            7 2
      ***** END CHG ******* 10/17/01 **********************************
     C*
     C                   ADD       L1PTOT        L2PTOT           11 2
     C                   ADD       L1PREM        L2PREM           11 2
     C                   ADD       L1CLOS        L2CLOS            5 0
     C                   ADD       L1OPEN        L2OPEN            5 0
     C     L1CLOS        ADD       L1OPEN        L1TOTC            5 0
     C                   ADD       L1TOTC        L2TOTC            5 0
     C**
     C                   ADD       L1ITOT        L2ITOT           11 2
     C**
     C     *IN22         IFEQ      '1'
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
     C*
     C*  GET REGION DESCRIPTION
     C*
     C     RGKEY         CHAIN     WMREL                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     REDESC        RGDESC           31
     C                   ELSE
     C                   MOVE      *BLANK        RGDESC
     C                   END
     C                   EXCEPT    HEAD
     C                   END
     C*
     C                   SETOFF                                       22
     C                   EXCEPT    TOTL1
     C     *INOF         IFEQ      '1'
     C                   SETON                                        22
     C                   END
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 2 Calculations (REGION)    ** Subroutine **   S U B L 2  *
      *==================================================================*
     C*
     C     SUBL2         BEGSR
     C*
     C                   ADD       EMPTOT        EMPL3             5 0
     C                   ADD       L2PREM        L3PREM           11 2
     C                   ADD       L2CLOS        L3CLOS            5 0
     C                   ADD       L2OPEN        L3OPEN            5 0
     C                   ADD       L2TOTC        L3TOTC            5 0
     C**
     C                   ADD       L2ITOT        L3ITOT           11 2
     C                   ADD       L2PTOT        L3PTOT           11 2
     C**
     C**   CALCULATE LOSS RATIO
     C     L2PREM        IFNE      *ZERO
     C     L2ITOT        MULT      100           XXITOT
      ***** BEG CHG ******* 10/17/01 **********************************
     C*********  XXITOT    DIV  L2PREM    L2LOSS  62H
     C     XXITOT        DIV(H)    L2PREM        L2LOSS            7 2
      ***** END CHG ******* 10/17/01 **********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L2LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L2PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L2TOTC        DIV(H)    THPREM        L2FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L2FREQ
     C                   END
     C*
     C     *IN22         IFEQ      '1'
     C*  GET FUND NAME
     C                   MOVEL     FNNAME        XFNAME           60
     C*
     C*  GET REGION DESCRIPTION
     C*
     C     RGKEY         CHAIN     WMREL                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     REDESC        RGDESC           31
     C                   ELSE
     C                   MOVE      *BLANK        RGDESC
     C                   END
     C                   EXCEPT    HEAD
     C                   END
     C*
     C                   SETOFF                                       22
     C                   EXCEPT    TOTL2
     C   OF              SETON                                        22
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 3 Calculations  (FUND)     ** Subroutine **   S U B L 3  *
      *==================================================================*
     C*
     C     SUBL3         BEGSR
     C*
     C                   ADD       EMPL3         EMPLR             5 0
     C                   ADD       L3PREM        LRPREM           11 2
     C                   ADD       L3CLOS        LRCLOS            5 0
     C                   ADD       L3OPEN        LROPEN            5 0
     C                   ADD       L3TOTC        LRTOTC            5 0
     C**
     C                   ADD       L3ITOT        LRITOT           11 2
     C                   ADD       L3PTOT        LRPTOT           11 2
     C**
     C**   CALCULATE LOSS RATIO
     C     L3PREM        IFNE      *ZERO
     C     L3ITOT        MULT      100           XXITOT
      ***** BEG CHG ******* 10/17/01 **********************************
     C*********  XXITOT    DIV  L3PREM    L3LOSS  62H
     C     XXITOT        DIV(H)    L3PREM        L3LOSS            7 2
      ***** END CHG ******* 10/17/01 **********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L3LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L3PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L3TOTC        DIV(H)    THPREM        L3FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L3FREQ
     C                   END
     C*
     C     *IN22         IFEQ      '1'
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
     C*
     C*  GET REGION DESCRIPTION
     C*
     C     RGKEY         CHAIN     WMREL                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     REDESC        RGDESC           31
     C                   ELSE
     C                   MOVE      *BLANK        RGDESC
     C                   END
     C                   EXCEPT    HEAD
     C                   END
     C*
     C                   SETOFF                                       22
     C                   EXCEPT    TOTL3
     C   OF              SETON                                        22
     C                   ENDSR
     C**
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
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O***** BEGIN ADD **** 07/19/99 ***********************************
     O          E            ROUTE          2 34
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           14 'COMPANY:'
     O                       XXCO#         Z     30
     O          E            ROUTE          2
     O                                           12 'GROUP:'
     O                       XXFND         Z     30
     O          E            ROUTE          2
     O                                           18 'POLICY YEAR:'
     O***** BEGIN CHG **** 04/10/00 ***********************************
     O**********      N85      XXFYR     22
     O              N85      XXFYR         Z     30
     O***** END   CHG **** 04/10/00 ***********************************
     O               85                          30 'ALL'
     O          E            ROUTE          2
     O                                           13 'REGION:'
     O              N84      XXRGN               30
     O               84                          30 'ALL'
     O          E            ROUTE          2
     O                                           15 'RUN DATE:'
     O                       XXDATE        Y     30
     O***** END   ADD **** 07/19/99 ***********************************
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O***     E  160           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           72 'POLICY YEAR:'
     C**** BEGIN CHG **** 02/12/99 ************************************
     O************************ XXFYR2  +  1
     O                       HFYR          Z   +  1
     C**** END   CHG **** 02/12/99 ************************************
     O                                          123 'RUN DATE:'
     O                       XXDATE        Y    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       HFND          Z   +  1
     O                       XFNAME            +  1
     O              NLR                          78 'REGION:'
     O              NLR      HRGN#         Z     82
     O              NLR      RGDESC             114
     O          E            HEAD           1
     O                                           74 'OPEN  CLOSED'
     O                                           81 'TOTAL'
     O                                          128 'LOSS  FREQ'
     O          E            HEAD           1
     O              NLR                           9 'EMPLOYER'
     O              NLR                          15 'DIV'
     O              NLR                        +  1 'EMPLOYER NAME'
     O                                           59 'PREM TO DATE'
     O                                           81 'CLAIMS CLAIMS CLAIMS'
     O                                           96 'INCURRED'
     O                                          112 'PAID'
     O                                          128 'RATIO RATIO'
     O          E            TOTL1          1
     O                       HEMP#         Z      9
     O                       HDIV          Z     15
     O                       ENAME               44
     O                       L1PREM        JB    60
     O                       L1OPEN        3B    67
     O                       L1CLOS        3B    74
     O                       L1TOTC        3B    81
     O                       L1ITOT        JB    97
     O                       L1PTOT        JB   113
     O                       L1LOSS        JB   123
     O                       L1FREQ        JB   129
     O                       CANFL              132
     O          E            TOTL2       1  1
     O                                           15 'EMPLOYER COUNT:'
     O                       EMPTOT        3B    21
     O                                           44 'REGION TOTALS:'
     O                       L2PREM        JB    60
     O                       L2OPEN        3B    67
     O                       L2CLOS        3B    74
     O                       L2TOTC        3B    81
     O                       L2ITOT        JB    97
     O                       L2PTOT        JB   113
     O                       L2LOSS        JB   123
     O                       L2FREQ        JB   129
     O          E            TOTL3       1  1
     O                                           15 'EMPLOYER COUNT:'
     O                       EMPL3         3B    21
     O                                           44 ' GROUP TOTALS:'
     O                       L3PREM        JB    60
     O                       L3OPEN        3B    67
     O                       L3CLOS        3B    74
     O                       L3TOTC        3B    81
     O                       L3ITOT        JB    97
     O                       L3PTOT        JB   113
     O                       L3LOSS        JB   123
     O                       L3FREQ        JB   129
     O          E    LR      TOTALS      1  1
