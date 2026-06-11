     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE W7261R - Loss Ratio Report, Selection Percentage
      *****************************************************************
      *                                                               *
      * REVISIONS:                                                    *
      *                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
      * 08/26/14 C:298 Project: SY354                                 *
      *                -Implement Routing Page Changes.               *
      *                                                               *
      * 01/28/10  C:249  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      *                                                               *
      * 05/31/00 C:61  PROJECT WC987                                  *
      *                If entered, use alt company name (FNANAM)      *
      *                instead of company name.                       *
      *                                                               *
      * 12/15/99  C:15   Project: WC577                               *
      *                  Y2K - Chgd FYR parm to 3A and chgd the policy*
      *                  year output to show the correct year.        *
      *                                                               *
      * 08/18/99 C:118 PROJECT: WC577
      *                Modified loading of HEMP#2 and HDIV2 to happen
      *                before the chain to the claim file.
      *
      * 05/03/99 C:118 PROJECT: WC577
      *                ADDED 'REPORT SELECTIONS' TO ROUTING PAGE.
      *
      * 03/02/99 C:118 PROJECT: WC577
      *                CHANGED 'FUND TOTAL' TO 'GROUP TOTAL'
      *
      * 07/17/98 C:108 PROJECT: WC577
      *               LOSS RATIO OUT FIELD SIZE: 6,2 TO 7,2 WITH IMPACT
      *               CORRECTED FOR FOLLOWING FIELDS AND HEADINGS.
      *
      * 08/12/97 C:78 PROJECT: WC577
      *               EMP/DIV FORCED TO SMALLER FIELDS FOR OUTPUT
      *               PURPOSES
      *
      * 08/27/97 C:78 PROJECT: WC577
      *               OUTPUT CHANGES ACCORDING TO NEW STANDARDS.
      *
      * 03/17/98 C:78 PROJECT: WC577
      *               OUTPUT CHANGES ACCORDING TO NEW STANDARDS.
      *
      *****************************************************************
     FWMCML     IF   E           K DISK
     F* * * Claims File
     F*
     FWMEML2    IF   E           K DISK
     F* * * Employer File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
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
     F* * * Output File
     F*
     F*
     FWDELL     IF   E           K DISK
     F* * * Employer Detail File
     F*

     D TLT             S             62    DIM(3) CTDATA PERRCD(1)              REPORT TITLES
     D*
     D* Added array for cancel dates; in order to print cancelled flag
     D* for Employers that are cancelled on or before run date.
     D*
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
     D*
     D* Cancel Date Array                                 LJQ 8-5-88
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D* Effective Date Array
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
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMPR
     C                   KFLD                    XXDIV
     C                   KFLD                    XXYR              3 0
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*                                                    *LJQ 01-14-88
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
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    HFND
     C*
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
     C     *LIKE         DEFINE    CHEFDT        XXYMD
     C     *LIKE         DEFINE    CHEFDT        XXCADT
     C     *LIKE         DEFINE    EMEMP#        XXEMPR
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C***** END ADD ****** 05/03/99 ***********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** BEGIN CHG *** 12/15/99 **************************************
     C******************** PARM           QQFYR   2
     C                   PARM                    QQFYR             3
     C*** END CHG ***** 12/15/99 **************************************
     C                   PARM                    QQDTE             8
     C                   PARM                    QQRPCT            7
     C                   PARM                    QQLOSS            1
     C                   PARM                    QQFREQ            1
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR             3 0
     C*** BEGIN DLT *** 12/15/99 **************************************
     C******************** MOVE XXFYR     XXFYR2  20
     C*** END DLT ***** 12/15/99 **************************************
     C*** BEGIN DLT *** 12/15/99 **************************************
     C     XXFYR         ADD       1900          XXFYR2            4 0
     C*** END DLT ***** 12/15/99 **************************************
     C                   MOVE      QQDTE         X1DATE            8 0
     C                   MOVE      QQRPCT        XXRPCT            7 2
     C                   MOVE      QQRPCT        XXFPCT            7 2
     C                   MOVE      QQLOSS        XXLOSS            1
     C                   MOVE      QQFREQ        XXFREQ            1
     C                   MOVE      QQMODE        XXMODE            1
     C                   DIV       100           XXFPCT
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C     XXLOSS        IFEQ      'L'
     C     XXFREQ        ANDEQ     'F'
     C                   MOVE      '1'           *IN43
     C                   MOVE      TLT(3)        XXTLT3           62
     C                   ELSE
     C     XXLOSS        IFEQ      'L'
     C                   MOVE      '1'           *IN41
     C                   MOVE      TLT(1)        XXTLT1           62
     C                   ELSE
     C                   MOVE      '1'           *IN42
     C                   MOVE      TLT(2)        XXTLT2           62
     C                   END
     C                   END
     C*
     C*  READ EMPLOYER FILE
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C*                                                    *LJQ 01-14-88
     C     ELKEY         CHAIN     PEL01                              60        *
     C     *IN60         IFEQ      '0'                                          *
     C*
     C     HFND          IFNE      *ZERO
     C     HFND          ANDNE     EMFND
     C                   MOVE      '1'           *INL1
     C                   MOVE      '1'           *INL2
     C                   MOVE      '1'           *INL3
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   EXSR      SUBL3
     C                   MOVE      '1'           *INOF
     C                   GOTO      DETCLC
     C                   END
     C*
     C     HRGN#         IFNE      *ZERO
     C     HRGN#         ANDNE     EMRGN#
     C                   MOVE      '1'           *INL1
     C                   MOVE      '1'           *INL2
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   MOVE      '1'           *INOF
     C                   GOTO      DETCLC
     C                   END
     C*
     C     HEMP#         IFNE      *ZERO
     C     HEMP#         ANDNE     EMEMP#
     C                   MOVE      '1'           *INL1
     C                   EXSR      SUBL1
     C                   GOTO      DETCLC                                       *LJQ 03-28-88
     C                   END
     C*
     C     HEMP#         IFNE      *ZERO                                        *LJQ 03-28-88
     C     HEMP#         ANDEQ     EMEMP#                                       *
     C     HDIV          ANDNE     EMDIV                                        *
     C                   MOVE      '1'           *INL1                          *
     C                   EXSR      SUBL1                                        *
     C                   END                                                    *
     C*
     C     DETCLC        TAG
     C                   MOVEL     EMNAME        ENAME            28                  "
     C                   MOVE      EMRGN#        HRGN#             3 0
     C                   MOVE      EMEMP#        HEMP#             9 0
     C                   MOVE      EMFND         HFND              3 0
     C                   MOVE      EMCO#         HCO#              3 0
     C                   MOVE      EMDIV         HDIV              5 0
     C*
     C*
     C* Check to find out if employer was cancelled on or before the
     C* run date.
     C*
     C                   MOVE      QQDTE         X1CADT            8 0          LJQ 8-5-88
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
     C*
     C*****************************************************************
     C*****************************************************************
     C*
     C*  READ CLAIM FILE
     C*
     C                   Z-ADD     HCO#          XXCO#
     C                   Z-ADD     HFND          XXFND
     C                   Z-ADD     HEMP#         XXEMPR
     C                   Z-ADD     HDIV          XXDIV
     C                   Z-ADD     XXFYR         XXYR
     C**** BEGIN ADD ***** 08/18/99 ***********************************
     C                   Z-ADD     HEMP#         HEMP#2            5 0
     C                   Z-ADD     HDIV          HDIV2             3 0
     C**** END   ADD ***** 08/18/99 ***********************************
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C***** BEGIN DEL **** 08/18/99 ***********************************
      **** BEGIN ADD ***** 08/12/97 ***********************************
     C**********           Z-ADDHEMP#     HEMP#2  50
     C**********           Z-ADDHDIV      HDIV2   30
      **** END   ADD ***** 08/12/97 ***********************************
     C***** BEGIN DEL **** 08/18/99 ***********************************
     C*
     C*
     C****************************************************************
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C**
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60     BOF
     C     *IN60         IFEQ      '1'
     C                   MOVE      CMSTAT        CHOCST
     C                   END
     C     CHCO#         IFEQ      CMCO#
     C     CHFND         ANDEQ     CMFND
     C     CHFYR         ANDEQ     CMFYR
     C     CHCASE        ANDEQ     CMCASE
     C     CHEFDT        ANDLE     XXYMD
     C                   GOTO      ARND06
     C                   END
     C                   MOVE      CMSTAT        CHOCST
     C     ARND06        TAG
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
     C*
     C                   END
     C     EMKEY         READE     PEM01                                  60
     C                   END
     C**
     C     END           TAG
     C   OF              EXCEPT    HEAD
     C                   SETOFF                                       OF
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   EXSR      SUBL3
     C                   EXCEPT    TOTALS
      ***** Beg Add ***** 08/26/14 ***********************************
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
      ***** End Add ***** 08/26/14 ***********************************
     C*
     C                   MOVE      '1'           *INLR
     C*
      **** BEGIN ADD ***** 08/12/97 ***********************************
     C                   Z-ADD     HEMP#         HEMP#2            5 0
     C                   Z-ADD     HDIV          HDIV2             3 0
      **** END   ADD ***** 08/12/97 ***********************************
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      ***** BEG DEL ******* 05/31/00 **********************************
     C*****BEGIN*ADD***** 03/17/98 ************************************
     C****************     MOVELCONAME    SRDFLD    P
     C****************     CALL 'SRCENTER'
     C****************     PARM           SRDFLD 80
     C****************     PARM 40        SRDLEN  30
     C****************     MOVELSRDFLD    CONAME
     C*****BEGIN*ADD***** 03/17/98 ************************************
      ***** END DEL ******* 05/31/00 **********************************
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 05/03/99 ***********************************
     C*   LEFT JUSTIFIY TITLE ON ROUTING PAGE
     C*
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 05/03/99 ***********************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *INOF
     C*
     C                   ENDSR
     C*
      ***** BEG ADD ******* 05/31/00 **********************************
      *
      *****************************************************************
      *   SRANAM - Use alternate company name if entered
      *****************************************************************
     CSR   SRANAM        BEGSR
      *
     C     FNANAM        IFNE      *BLANKS
     C     *IN40         ANDEQ     *OFF
      *
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
      *
     C                   MOVEL(P)  ##DATA        XXNAME           50
      *
     C                   ELSE
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXNAME           50
     C                   ENDIF                                                  FNANAM
      *
     C                   ENDSR                                                  SRANAM
      *
      *
      ***** END ADD ******* 05/31/00 **********************************
     C**
      *==================================================================*
      *   Premium Calculations             ** Subroutine **   S U B P R  *
      *==================================================================*
     C*
     CSR   SUBPR         BEGSR
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM THIS PERIOD
     C**
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     HFND          SRFND             3
     C                   MOVEL     XXFYR         SRFYR             3
     C                   MOVEL     HEMP#         SREMP#            9
     C                   MOVEL     HDIV          SRDIV             5
     C                   MOVEL     X1DATE        QQDTE             8
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
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
     C**** BEGIN CHG ***** 07/17/98 *******************************
     C********** XXITOT    DIV  PRMYTD    LOSS    62H
     C     XXITOT        DIV(H)    PRMYTD        LOSS              7 2
     C**** END   CHG ***** 07/17/98 *******************************
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
     CSR   SUBL1         BEGSR
     C*
     C                   EXSR      SUBPR
     C                   Z-ADD     PRMYTD        L1PREM           11 2
     C                   Z-ADD     FREQ          L1FREQ            4 2
     C**** BEGIN CHG ***** 07/17/98 *******************************
     C******************** Z-ADDLOSS      L1LOSS  62
     C                   Z-ADD     LOSS          L1LOSS            7 2
     C**** END   CHG ***** 07/17/98 *******************************
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
     C     *INOF         IFEQ      '1'
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
      ***** BEG ADD ******* 05/31/00 **********************************
      *
     C                   EXSR      SRANAM
      *
      ***** END ADD ******* 05/31/00 **********************************
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
     C                   MOVE      '0'           *IN48
     C                   MOVE      '0'           *INOF
     C     *IN43         IFEQ      '1'
     C     L1LOSS        IFGE      XXRPCT
     C     L1FREQ        ORGE      XXFPCT
     C                   MOVE      '1'           *IN48
     C                   END
     C                   ELSE
     C     *IN41         IFEQ      '1'
     C     L1LOSS        ANDGE     XXRPCT
     C                   MOVE      '1'           *IN48
     C                   ELSE
     C     *IN42         IFEQ      '1'
     C     L1FREQ        ANDGE     XXFPCT
     C                   MOVE      '1'           *IN48
     C                   END
     C                   END
     C                   END
     C*
     C     *IN48         IFEQ      '1'
     C                   ADD       1             EMPTOT            5 0
     C                   EXCEPT    TOTL1
     C**
     C                   ELSE
     C                   SUB       PRMYTD        L1PREM
     C                   SUB       PRMYTD        L2PREM
     C                   SUB       L1OPEN        L2OPEN
     C                   SUB       L1CLOS        L2CLOS
     C                   SUB       L1TOTC        L2TOTC
     C                   SUB       L1ITOT        L2ITOT
     C                   SUB       L1PTOT        L2PTOT
     C                   END
     C**
     C                   Z-ADD     *ZERO         L1PREM
     C                   Z-ADD     *ZERO         L1OPEN
     C                   Z-ADD     *ZERO         L1CLOS
     C                   Z-ADD     *ZERO         L1TOTC
     C                   Z-ADD     *ZERO         L1ITOT
     C                   Z-ADD     *ZERO         L1PTOT
     C                   Z-ADD     *ZERO         L1LOSS
     C                   Z-ADD     *ZERO         L1FREQ
     C**
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 2 Calculations (REGION)    ** Subroutine **   S U B L 2  *
      *==================================================================*
     C*
     CSR   SUBL2         BEGSR
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
     C**** BEGIN CHG ***** 07/17/98 *******************************
     C********** XXITOT    DIV  L2PREM    L2LOSS  62H
     C     XXITOT        DIV(H)    L2PREM        L2LOSS            7 2
     C**** END   CHG ***** 07/17/98 *******************************
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
     C     *INOF         IFEQ      '1'
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
      ***** BEG ADD ******* 05/31/00 **********************************
      *
     C                   EXSR      SRANAM
      *
      ***** END ADD ******* 05/31/00 **********************************
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
     C                   SETOFF                                       OF
     C                   EXCEPT    TOTL2
     C**
     C                   Z-ADD     *ZERO         L2PREM
     C                   Z-ADD     *ZERO         L2OPEN
     C                   Z-ADD     *ZERO         L2CLOS
     C                   Z-ADD     *ZERO         L2TOTC
     C                   Z-ADD     *ZERO         L2ITOT
     C                   Z-ADD     *ZERO         L2PTOT
     C                   Z-ADD     *ZERO         L2LOSS
     C                   Z-ADD     *ZERO         L2FREQ
     C**
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 3 Calculations  (FUND)     ** Subroutine **   S U B L 3  *
      *==================================================================*
     C*
     CSR   SUBL3         BEGSR
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
     C**** BEGIN CHG ***** 07/17/98 *******************************
     C********** XXITOT    DIV  L3PREM    L3LOSS  62H
     C     XXITOT        DIV(H)    L3PREM        L3LOSS            7 2
     C**** END   CHG ***** 07/17/98 *******************************
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
     C     *INOF         IFEQ      '1'
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
      ***** BEG ADD ******* 05/31/00 **********************************
      *
     C                   EXSR      SRANAM
      *
      ***** END ADD ******* 05/31/00 **********************************
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
     C                   SETOFF                                       OF
     C                   EXCEPT    TOTL3
     C**
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
     O***** BEGIN CHG ******** 05/03/99 *******************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END CHG ********** 05/03/99 *******************************
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
     O***** BEGIN ADD ******** 05/03/99 *******************************
     O          E            ROUTE       3  2
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           34 'COMPANY               :'
     O                       XXCO#               46
     O          E            ROUTE          2
     O                                           34 'GROUP                 :'
     O                       XXFND               46
     O          E            ROUTE          2
     O                                           34 'POLICY YEAR           :'
     O                       XXFYR               46
     O          E            ROUTE          2
     O                                           34 'RUN DATE              :'
     O                       X1DATE        Y     46
     O          E            ROUTE          2
     O                                           34 'PERCENTAGE            :'
     O                       XXRPCT        4     46
     O          E            ROUTE          2
     O                                           34 'SELECT LOSS RATIO     :'
     O                       XXLOSS              46
     O          E            ROUTE          2
     O                                           34 'SELECT FREQUENCY RATIO:'
     O                       XXFREQ              46
     O***** END ADD ********** 05/03/99 *******************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEGIN CHG ****** 08/27/97 ********************************
     O***********************************46 'VE DATA SOLUTIONS. INC,'
     O***********************************51 '.1990'
      ****** BEG DLT ***** 01/28/10 **********************************
     O****                               47 'VE DATA SOLUTIONS. INC.,'
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      ****** END   CHG ****** 08/27/97 ********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
      ***** BEG DEL ******* 05/31/00 **********************************
     O****************         CONAME    87
      ***** END DEL ******* 05/31/00 **********************************
      ***** BEG ADD ******* 05/31/00 **********************************
     O                       XXNAME              91
      ***** END ADD ******* 05/31/00 **********************************
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O*** BEGIN DLT *** 12/15/99 **************************************
      ***** BEGIN CHG ***** 08/27/97 **********************************
     O***********************************72 'FUND YEAR 19'
     O************************           72 'POLICY YEAR 19'
      ***** END   CHG ***** 08/27/97 **********************************
     O************************ XXFYR2    74
     O*** END DLT ***** 12/15/99 **************************************
     O*** BEGIN ADD *** 12/15/99 **************************************
     O                                           70 'POLICY YEAR:'
     O                       XXFYR2              75
     O*** END ADD ***** 12/15/99 **************************************
     O***** BEGIN CHG ******** 05/03/99 *******************************
     O**********                        123 'RUN DATE:'
     O                                          120 'RUN DATE:'
     O**********               XXDATEY  132
     O                       X1DATE        Y    132
     O***** END CHG ********** 05/03/99 *******************************
     O          E    41      HEAD           1
     O                       XXTLT1              96
     O                       XXRPCT        3     90
     O          E    42      HEAD           1
     O                       XXTLT2              96
     O                       XXFPCT        3     92
     O          E    43      HEAD           1
     O                       XXTLT3              96
     O                       XXRPCT        3    101
     O                                          103 '/'
     O                       XXFPCT        3    112
      ******* BEGIN CHG ***** 03/17/98 ********************************
     O******* E  2             HEAD
     O          E            HEAD           1
      ******* END   CHG ***** 03/17/98 ********************************
      ******* BEGIN CHG ***** 08/27/97 ********************************
     O************************************5 'FUND:'
     O*************************HFND  Z    9
     O*************************XFNAME    70
     O***********************************78 'REGION:'
     O*************************HRGN# Z   82
     O*************************RGDESC   114
     O                                            6 'GROUP:'
     O                       HFND          Z     11
     O                       XFNAME              72
      ******* BEGIN CHG ***** 03/17/98 ********************************
     O********************     HRGN# Z   83
     O********************     RGDESC   115
     O          E            HEAD           2
     O                                            7 'REGION:'
     O                       HRGN#         Z     11
     O                       RGDESC              44
      ******* END   CHG ***** 03/17/98 ********************************
      ******* END   CHG ***** 08/27/97 ********************************
     O          E            HEAD           1
     O                                           74 'OPEN  CLOSED'
     O                                           81 'TOTAL'
      ***** BEGIN CHG ******* 07/17/98 **************************
     O********************************* 128 'LOSS   FREQ'
     O                                          129 'LOSS  FREQ'
      ***** END   CHG ******* 07/17/98 **************************
     O          E            HEAD           2
      ***** BEGIN CHG ***** 03/17/98 **********************************
     O*********************************** 9 'EMPLR DIV'
     O                                           15 'EMPLOYER   DIV'
      ***** END   CHG ***** 03/17/98 **********************************
      ***** BEGIN CHG ***** 08/12/97 **********************************
     O***********************************23 'EMPLOYER NAME'
     O                                           29 'EMPLOYER NAME'
      ***** END   CHG ***** 08/12/97 **********************************
     O                                           59 'PREM TO DATE'
     O                                           81 'CLAIMS CLAIMS CLAIMS'
     O                                           96 'INCURRED'
     O                                          111 'PAID'
      ***** BEGIN CHG ******* 07/17/98 **************************
     O********************************* 128 'RATIO  RATIO'
     O                                          129 'RATIO RATIO'
      ***** END   CHG ******* 07/17/98 **************************
     O          E            TOTL1          1
      ****** BEGIN CHG **** 08/12/97 **********************************
     O*************************HEMP# Z    9
     O*************************HDIV  Z   15
     O**********************   HEMP#2Z    5
     O**********************   HDIV2 Z    9
      ****** BEGIN CHG **** 03/17/98 **********************************
     O                       HEMP#2        Z      9
     O                       HDIV2         Z     15
      ****** END   CHG **** 03/17/98 **********************************
      ****** END   CHG **** 08/12/97 **********************************
     O                       ENAME               44
     O                       L1PREM        JB    60
     O                       L1OPEN        3B    67
     O                       L1CLOS        3B    74
     O                       L1TOTC        3B    81
     O                       L1ITOT        JB    97
     O                       L1PTOT        JB   113
     O**** BEGIN CHG ***** 07/17/98 *******************************
     O************************ L1LOSSJB 123
     O                       L1LOSS        JB   124
     O************************ L1FREQJB 129
     O                       L1FREQ        JB   130
     O**** END   CHG ***** 07/17/98 *******************************
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
     O**** BEGIN CHG ***** 07/17/98 *******************************
     O************************ L2LOSSJB 123
     O                       L2LOSS        JB   124
     O************************ L2FREQJB 129
     O                       L2FREQ        JB   130
     O**** END   CHG ***** 07/17/98 *******************************
     O          E            TOTL3       1  1
     O                                           15 'EMPLOYER COUNT:'
     O                       EMPL3         3B    21
     O*
     O****** BEGIN CHG *** 03/02/99 ***********************************
     O**********                         44 'FUND TOTALS:'
     O                                           44 'GROUP TOTALS:'
     O****** END CHG ***** 03/03/99 **********************************
     O*
     O                       L3PREM        JB    60
     O                       L3OPEN        3B    67
     O                       L3CLOS        3B    74
     O                       L3TOTC        3B    81
     O                       L3ITOT        JB    97
     O                       L3PTOT        JB   113
     O**** BEGIN CHG ***** 07/17/98 *******************************
     O************************ L3LOSSJB 123
     O                       L3LOSS        JB   124
     O************************ L3FREQJB 129
     O                       L3FREQ        JB   130
     O**** END   CHG ***** 07/17/98 *******************************
     O          E    LR      TOTALS      1  1
**      TLT      REPORT SUB-TITLE
             SELECTION BY LOSS RATIO PERCENTAGE
          SELECTION BY FREQUENCY RATIO PERCENTAGE
  SELECTION BY LOSS RATIO AND FREQUENCY RATIO PERCENTAGE
