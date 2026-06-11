     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W8606R - CLOSURE REPORT
     H*****************************************************************
     H*  DESCRIPTION: THIS REPORT IS AN APPROXIMATE SUMMARY OF AN     *
     H*               ADJUSTERS CASE LOAD.                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
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
     H* 04/26/02 C:15  Project: WC577                                 *
     H*                Remove letter range from report.               *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                                                               *
     H*  05/10/99 C:118 PROJECT: WC892                                *
     H*                 MODIFIED NEW CLAIMS COUNT TO INCLUDE RECORD   *
     H*                 ONLY CLAIMS (CMENTD CHANGED TO CMIDT).        *
     H*                 MODIFIED CLOSED, REOPEN AND PENDING CLAIM     *
     H*                 COUNTS SO BASED ON LAST VALUE IN TO DATE      *
     H*                 RANGE.  CHANGED 'L' TO 'I' FOR COMPARISON OF  *
     H*                 TRANSFER FROM LOST TIME TO MED ONLY.  ADDED   *
     H*                 'R' COMPARISON IN THFROM FIELD FOR TRANSFER   *
     H*                 FROM MED ONLY TO LOST TIME.                   *
     H*                                                               *
     H*  08/20/93 C:10 PROJECT: WC256                                 *
     H*                CHANGED CALC OF NEW LOST TIME CLAIMS; NEW MED  *
     H*                CLAIMS; AND TRANSFERS.                         *
     H*                                                               *
     H*  10/18/93 C:10 PROJECT: WC256                                 *
     H*                SET INDICATOR *IN71 OUTSIDE OF 'IF STMT'       *
     H*                WHICH CHECKS FOR CMENTD IN MONTH               *
     H*                                                               *
     H*  10/17/96 C:62 PROJECT: WC588                                 *
     H*                EXCLUDE FUND 999 FROM CLOSURE REPORT.          *
     H*                                                               *
     H* 05/13/97 C:62 PROJECT WC674                                   *
     H*               ENSURE ALL PROGRAMS USE CMLTFL = I/M/R NOT Y/N  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMCML1    IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMTHL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
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
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
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
     D  RT                     1     20
     D                                     DIM(10)
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
     D*
     D*
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
     C                   KFLD                    XXSTAF
     C*
     C*   KEY LIST FOR CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR CLAIM HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*   KEY LIST FOR TRANSFER LOST TIME - MED ONLY FILE
     C*
     C     THKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     THKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXFRDT
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR NCCI CODE TABLE FILE
     C*                AND THE RESERVE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    CHEFDT        SVOPDT
     C     *LIKE         DEFINE    CHEFDT        SVFRST
     C     *LIKE         DEFINE    FNFND         XHFND
     C     *LIKE         DEFINE    CMCLEX        XXSTAF
     C     *LIKE         DEFINE    CHEFDT        XXTODT
     C     *LIKE         DEFINE    CHEFDT        XXFRDT
     C***** BEGIN ADD **** 05/10/99 ***********************************
     C     *LIKE         DEFINE    CHEFDT        SVLTST
     C     *LIKE         DEFINE    CHOCST        SVOCST
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C***** END ADD ****** 05/10/99 ***********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   MOVE      QQDATE        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTODT        XXFRDT
     C                   MOVE      '01'          XXFRDT
     C                   TIME                    DSDTTM
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*** BEGIN DLT *****  06/10/00  **********************************
     C********** CMFND     IFNE 999
     C*** END DELETE ****  06/10/00  **********************************
     C*
     C  N11              EXSR      SR1ST
     C*
     C*   CHECK FOR FUND LEVEL BREAK
     C*
     C     CMFND         IFNE      XHFND
     C                   EXSR      SRFBRK
     C                   ENDIF                                                  NE XHFND
     C*
     C*** BEGIN ADD *****  06/10/00  **********************************
     C     FNTEST        IFNE      'Y'
     C*** END ADD *******  06/10/00  **********************************
     C*
     C*   CHECK FOR USER BREAK
     C*
     C     CMCLEX        IFNE      XXSTAF
     C                   EXSR      SRSBRK
     C                   ENDIF                                                  NE XXSTAF
     C*
     C*  IF LOSS TIME FLAG IS 'Y', SET ON *IN71, ELSE SET IT TO OFF
     C*
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** CMLTFL    IFEQ 'Y'
     C*** BEGIN DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     CMLTFL        IFEQ      'I'
     C*** BEGIN ADD *** 05/13/97 **************************************
     C                   MOVE      '1'           *IN71
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   ENDIF                                                  EQ 'I'
     C*
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C****ACCUMULATE "NEW" CLAIM TOTALS.  (CMENTD DURING MONTH)
     C*
     C*   ACCUMULATE "NEW" CLAIM TOTALS, INCLUDING RECORD ONLY CLAIMS**
     C*
     C********** CMENTD    IFGE XXFRDT
     C********** CMENTD    ANDLEXXTODT
     C     CMIDT         IFGE      XXFRDT
     C     CMIDT         ANDLE     XXTODT
     C***** END CHG ****** 05/10/99 ***********************************
     C*
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** CMLTFL    IFEQ 'Y'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     CMLTFL        IFEQ      'I'
     C*** END   ADD *** 05/13/97 **************************************
     C                   ADD       1             XXLNEW
     C                   ELSE
     C                   ADD       1             XXMNEW
     C                   ENDIF                                                  EQ 'I'
     C                   ENDIF                                                  GE FR,LE TO
     C*
     C*   READ ALL CLAIM HISTORY RECORDS FOR CASE & ACCUM ALL OPEN
     C*   RECORDS & SAVE THE LAST OPEN DATE.
     C*
     C                   Z-ADD     0             SVOPTL            3 0
     C                   Z-ADD     0             SVOPDT
     C                   Z-ADD     0             SVFRST
     C     CHKEY         CHAIN     PCH01                              60
     C     *IN60         DOWEQ     '0'
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C********** CHEFDT    IFLE XXTODT
     C     CHEFDT        ANDLE     XXTODT
     C***** END CHG ****** 05/10/99 ***********************************
     C     SVFRST        IFEQ      0
     C                   Z-ADD     CHEFDT        SVFRST
     C                   ENDIF                                                  EQ 0
     C     CHOCST        IFEQ      'O'
     C                   Z-ADD     CHEFDT        SVOPDT
     C                   ADD       1             SVOPTL
     C                   ENDIF                                                  EQ 'O'
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C**********           END
     C*
     C                   Z-ADD     CHEFDT        SVLTST
     C                   MOVE      CHOCST        SVOCST
     C***** END CHG ****** 05/10/99 ***********************************
     C*
     C     CHKEY         READE     PCH01                                  60
     C                   ENDDO                                                  =0&LEXXTODT
     C***** END CHG ****** 05/10/99 ***********************************
     C*
     C*   1.  IF CLOSED DURING THE MONTH, STAYED CLOSED & AND WAS NOT
     C*       A REOPEN/CLOSE, ACCUMULATE CLOSED TOTAL
     C*   2.  IF OPEN, ACCUMULATE PENDING TOTAL
     C*   3.  IF REOPENED DURING THE MONTH & STAYED OPEN, ACCUM REOPEN
     C*
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C********** CHOCST    IFEQ 'C'
     C     SVOCST        IFEQ      'C'
     C*
     C********** CHEFDT    IFGE XXFRDT
     C********** CHEFDT    ANDLEXXTODT
     C     SVLTST        IFGE      XXFRDT
     C     SVLTST        ANDLE     XXTODT
     C***** END CHG ****** 05/10/99 **********************************
     C*
     C     SVOPTL        IFEQ      1
     C     SVOPDT        ORLT      XXFRDT
     C     SVFRST        ORGE      XXFRDT
     C  N71              ADD       1             XXMCLS
     C   71              ADD       1             XXLCLS
     C                   ENDIF                                                  =1,<FR,>=FR
     C                   ENDIF                                                  >=FR,<=TO
     C*
     C                   ELSE
     C*
     C  N71              ADD       1             XXMPND
     C   71              ADD       1             XXLPND
     C*
     C*   CHECK FOR REOPEN
     C*
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C********** CHEFDT    IFGE XXFRDT
     C********** CHEFDT    ANDLEXXTODT
     C     SVLTST        IFGE      XXFRDT
     C     SVLTST        ANDLE     XXTODT
     C***** END CHG ****** 05/10/99 ***********************************
     C     SVOPTL        ANDGT     1
     C  N71              ADD       1             XXMRO
     C   71              ADD       1             XXLRO
     C                   ENDIF                                                  >=FR<=TO>1
     C*                                                    >1
     C                   ENDIF                                                  =C
     C*
     C*   TRANSFER FROM LOST TIME TO MED ONLY
     C*         IF CLAIM WAS INITIALLY ENTERED IN A PREVIOUS MONTH,
     C*         AND TRANSFERRED IN THE CURRENT MONTH, HAVING A LT FLAG
     C*         OF 'Y'.
     C*
     C*   TRANSFER FROM MED PAY TO LOST TIME
     C*         IF CLAIM WAS INITIALLY ENTERED IN A PREVIOUS MONTH,
     C*         AND TRANSFERRED IN THE CURRENT MONTH, WITH NO COMP
     C*         PAID, HAVING A LT FLAG OF 'N'.
     C*
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C********** CMENTD    IFLT XXFRDT
     C     CMIDT         IFLT      XXFRDT
     C***** END CHG ****** 05/10/99 ***********************************
     C*
     C     THKY1         SETLL     PTH01
     C     THKEY         READE     PTH01                                  60
     C     *IN60         IFEQ      '0'
     C     THTRDT        ANDLE     XXTODT
     C*
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C********** THFROM    IFEQ 'L'
     C     THFROM        IFEQ      'I'
     C***** END CHG ****** 05/10/99 ***********************************
     C*
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** CMLTFL    ANDEQ'N'
     C*** END   DEL *** 05/13/97 **************************************
     C*
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     CMLTFL        ANDEQ     'M'
     C***** BEGIN CHG **** 05/10/99 ***********************************
     C********** THFROM    OREQ 'L'
     C     THFROM        OREQ      'I'
     C***** END CHG ****** 05/10/99 ***********************************
     C*
     C     CMLTFL        ANDEQ     'R'
     C*** END   ADD *** 05/13/97 **************************************
     C*
     C*   CHECK FOR COMP PAYMENTS
     C*
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y
     C                   Z-ADD     *ZERO         XXCOMP           11 2
     C*
     C     *BLANK        LOOKUP    RT(X)                                  50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     11            X
     C                   ENDIF                                                  ='0'
     C*
     C     Y             DOWLT     X
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     RT(Y)         XXKEY
     C     TBKEY         CHAIN     PRT01                              50
     C     RTTYPE        IFEQ      'C'
     C                   ADD       AR(Y)         XXCOMP
     C                   ENDIF                                                  =C
     C                   ADD       1             Y
     C                   ENDDO                                                  <X
     C*
     C     XXCOMP        IFEQ      *ZERO
     C                   ADD       1             XXLTMO
     C                   ENDIF                                                  =0
     C                   ENDIF                                                  =I&M,=I&R
     C*
     C     THFROM        IFEQ      'M'
     C*** BEGIN DEL *** 05/13/97 **************************************
     C********** CMLTFL    ANDEQ'Y'
     C*** END   DEL *** 05/13/97 **************************************
     C*** BEGIN ADD *** 05/13/97 **************************************
     C     CMLTFL        ANDEQ     'I'
     C*** END   ADD *** 05/13/97 **************************************
     C*
     C***** BEGIN ADD **** 05/10/99 ***********************************
     C     THFROM        OREQ      'R'
     C     CMLTFL        ANDEQ     'I'
     C***** END ADD ****** 05/10/99 ***********************************
     C                   ADD       1             XXMOLT
     C                   ENDIF                                                  =M OR R
     C*
     C                   ENDIF                                                  =0 & LE TODT
     C                   ENDIF                                                  LT FRDT
     C*
     C*   GET FIRST LETTER OF CLAIMANTS LAST NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 3 0
     C     XXFRST        LOOKUP    LET(X)                                 60
     C   60              MOVE      '1'           FL(X)
     C                   ENDIF                                                  =0
     C*
     C                   ENDIF                                                  IFNE 999
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   ENDDO                                                  EQ 0
     C*
     C                   MOVE      '1'           *INLR
     C                   EXSR      SRFBRK
     C     XGMPND        ADD       XGLPND        XXOPEN            6 0
     C     XGMCLS        ADD       XGLCLS        XXCLOS            6 0
     C                   EXCEPT    GTOT
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
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
     C*
     C*   GET THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         XHFND
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C***** BEGIN ADD **** 05/10/99 ***********************************
     C*   LEFT JUSTIFIY TITLE ON ROUTING PAGE
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 05/10/99 ***********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF                                                  =0
     C*
     C*  GET FIRST STAFF CODE NAME
     C*
     C                   MOVE      CMCLEX        XXSTAF
     C                   MOVE      *BLANKS       XXSTNM           20
     C     XXSTAF        IFNE      *BLANKS
     C     UPKEY1        CHAIN     PUP01                              60
     C  N60              MOVEL     UPUSNM        XXSTNM
     C   60              MOVEL     XXSTAF        XXSTNM
     C                   ENDIF                                                  NE*BLANKS
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADF
     C                   MOVE      '1'           *IN11
     C*
     C*   INITIALIZE  USER TOTALS
     C*
     C                   Z-ADD     0             XXMNEW            5 0          NEW MED ONLY
     C                   Z-ADD     0             XXLNEW            5 0           " LOST TIME
     C                   Z-ADD     0             XXLTMO            5 0          TRANS TO MED
     C                   Z-ADD     0             XXMOLT            5 0          TRANS TO LT
     C                   Z-ADD     0             XXMRO             5 0          MED REOPENS
     C                   Z-ADD     0             XXLRO             5 0          LT REOPENS
     C                   Z-ADD     0             XXMCLS            5 0          MED CLOSURES
     C                   Z-ADD     0             XXLCLS            5 0          LT CLOSURES
     C                   Z-ADD     0             XXMPND            5 0          MED PENDING
     C                   Z-ADD     0             XXLPND            5 0          LT PENDING
     C*
     C*   INITIALIZE FUND TOTALS
     C*
     C                   Z-ADD     0             XFMNEW            6 0          FUND MED
     C                   Z-ADD     0             XFLNEW            6 0          FND LOST TIM
     C                   Z-ADD     0             XFLTMO            6 0          FND MED TRAN
     C                   Z-ADD     0             XFMOLT            6 0          FND LT TRAN
     C                   Z-ADD     0             XFMRO             6 0          FND MED RO
     C                   Z-ADD     0             XFLRO             6 0          FND LT RO
     C                   Z-ADD     0             XFMCLS            6 0          FND MED CLS
     C                   Z-ADD     0             XFLCLS            6 0          FND LT CLS
     C                   Z-ADD     0             XFMPND            6 0          FND MED PNDG
     C                   Z-ADD     0             XFLPND            6 0          FND LT PNDG
     C*
     C*   INITIALIZE GRAND TOTALS
     C*
     C                   Z-ADD     0             XGMNEW            6 0          TL NEW MED
     C                   Z-ADD     0             XGLNEW            6 0          TL NEW LT
     C                   Z-ADD     0             XGLTMO            6 0          TL MED TRAN
     C                   Z-ADD     0             XGMOLT            6 0          TL LT TRAN
     C                   Z-ADD     0             XGMRO             6 0          TL MED RO
     C                   Z-ADD     0             XGLRO             6 0          TL LT RO
     C                   Z-ADD     0             XGMCLS            6 0          TL MED CLS
     C                   Z-ADD     0             XGLCLS            6 0          TL LT CLS
     C                   Z-ADD     0             XGMPND            6 0          TL MED PNDG
     C                   Z-ADD     0             XGLPND            6 0          TL LT PNDG
     C                   Z-ADD     0             XXOPEN            6 0          GRAND TL OPN
     C                   Z-ADD     0             XXCLOS            6 0          GRAND TL CLS
     C*
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
     C                   ENDIF                                                  ='1'
     C                   EXCEPT    FTOT
     C*
     C*  ACCUMULATE GRAND TOTALS
     C*
     C                   ADD       XFMNEW        XGMNEW
     C                   ADD       XFLNEW        XGLNEW
     C                   ADD       XFLTMO        XGLTMO
     C                   ADD       XFMOLT        XGMOLT
     C                   ADD       XFMRO         XGMRO
     C                   ADD       XFLRO         XGLRO
     C                   ADD       XFMCLS        XGMCLS
     C                   ADD       XFLCLS        XGLCLS
     C                   ADD       XFMPND        XGMPND
     C                   ADD       XFLPND        XGLPND
     C*
     C                   Z-ADD     0             XFMNEW            6 0          FUND MED
     C                   Z-ADD     0             XFLNEW            6 0          FND LOST TIM
     C                   Z-ADD     0             XFLTMO            6 0          FND MED TRAN
     C                   Z-ADD     0             XFMOLT            6 0          FND LT TRAN
     C                   Z-ADD     0             XFMRO             6 0          FND MED RO
     C                   Z-ADD     0             XFLRO             6 0          FND LT RO
     C                   Z-ADD     0             XFMCLS            6 0          FND MED CLS
     C                   Z-ADD     0             XFLCLS            6 0          FND LT CLS
     C                   Z-ADD     0             XFMPND            6 0          FND MED PNDG
     C                   Z-ADD     0             XFLPND            6 0          FND LT PNDG
     C*
     C*   GET THE FUND FILE
     C*
     C     *INLR         IFNE      '1'
     C     FNKEY         CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         XHFND
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   ENDIF                                                  ='1'
     C                   EXCEPT    HEADF
     C                   ENDIF                                                  NE'1'
     C*
     C                   ENDSR
     C*=========================================================**
     C*   LEVEL BREAK ON STAFF CODE    S R S B R K              **
     C*=========================================================**
     C     SRSBRK        BEGSR
     C*
     C*  ACCUMULATE FUND TOTALS
     C*
     C                   ADD       XXMNEW        XFMNEW
     C                   ADD       XXLNEW        XFLNEW
     C                   ADD       XXLTMO        XFLTMO
     C                   ADD       XXMOLT        XFMOLT
     C                   ADD       XXMRO         XFMRO
     C                   ADD       XXLRO         XFLRO
     C                   ADD       XXMCLS        XFMCLS
     C                   ADD       XXLCLS        XFLCLS
     C                   ADD       XXMPND        XFMPND
     C                   ADD       XXLPND        XFLPND
     C*
     C*  PRINT DETAIL ONLY IF AT LEAST ONE AMOUNT IS NOT ZERO
     C*
     C     XXMNEW        COMP      0                                      35
     C   35XXLNEW        COMP      0                                      35
     C   35XXLTMO        COMP      0                                      35
     C   35XXMOLT        COMP      0                                      35
     C   35XXMRO         COMP      0                                      35
     C   35XXLRO         COMP      0                                      35
     C   35XXMCLS        COMP      0                                      35
     C   35XXLCLS        COMP      0                                      35
     C   35XXMPND        COMP      0                                      35
     C   35XXLPND        COMP      0                                      35
     C*
     C     *IN35         IFEQ      '0'
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
     C                   ENDIF                                                  ='1'
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  ='0'
     C*
     C     *INLR         IFNE      '1'
     C                   MOVE      CMCLEX        XXSTAF
     C                   MOVE      *BLANKS       XXSTNM
     C     XXSTAF        IFNE      *BLANKS
     C     UPKEY1        CHAIN     PUP01                              60
     C                   MOVEL     UPUSNM        XXSTNM
     C                   ENDIF                                                  NE*BLANKS
     C                   ENDIF                                                  NE '1'
     C*
     C*  INITIALIZE DETAIL
     C*
     C                   Z-ADD     0             XXMNEW            5 0          NEW MED ONLY
     C                   Z-ADD     0             XXLNEW            5 0           " LOST TIME
     C                   Z-ADD     0             XXLTMO            5 0          TRANS TO MED
     C                   Z-ADD     0             XXMOLT            5 0          TRANS TO LT
     C                   Z-ADD     0             XXMRO             5 0          MED REOPENS
     C                   Z-ADD     0             XXLRO             5 0          LT REOPENS
     C                   Z-ADD     0             XXMCLS            5 0          MED CLOSURES
     C                   Z-ADD     0             XXLCLS            5 0          LT CLOSURES
     C                   Z-ADD     0             XXMPND            5 0          MED PENDING
     C                   Z-ADD     0             XXLPND            5 0          LT PENDING
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
     C                   ENDIF                                                  ='1'
     C                   ELSE
     C                   MOVE      '0'           *IN60
     C                   ENDIF                                                  <27
     C                   ELSE
     C                   SUB       1             I
     C                   ENDIF                                                  ='1'
     C*
     C                   ELSE
     C*
     C                   MOVE      '-'           OS(I)
     C                   ADD       1             I
     C                   MOVE      LET(Z)        OS(I)
     C*
     C                   ENDIF                                                  =Z
     C*
     C                   ENDIF                                                  ='0'
     C*
     C                   ELSE
     C*
     C                   SUB       1             I
     C                   MOVE      '0'           *IN60
     C*
     C                   ENDIF                                                  LE 26
     C*
     C     Z             ADD       1             X
     C                   ADD       1             I
     C   60'1'           LOOKUP    FL(X)                                  60
     C   60              MOVE      ','           OS(I)
     C   60              ADD       2             I
     C*
     C                   ENDDO                                                  ='1'
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
     O***** BEGIN CHG ******** 05/10/99 *******************************
     O                       ORTITL              80
     O                       XXTITL              80
     O***** END CHG ********** 05/10/99 *******************************
      ***** Beg Del ***** 08/26/14 ************************************
     O**********             ROUTE          2
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
     O***** BEGIN ADD ******** 05/10/99 *******************************
     O          E            ROUTE       3  2
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           21 'COMPANY :'
     O                       XXCO#               33
     O*       E  2             ROUTE
     O*                                  21 'GROUP   :'
     O*                        XHFND     33
     O          E            ROUTE          2
     O                                           21 'RUN DATE:'
     O                       X1TODT        Y     33
     O***** END   ADD ******** 05/10/99 *******************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
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
     O***** BEGIN CHG ******** 05/10/99 *******************************
     O**********               XXDATEY   18
     O                       X1TODT        Y     21
     O***** END CHG ********** 05/10/99 *******************************
     O          E            HEAD           0
     O                                           58 'M E D I C A L    O N L Y'
     O                                          113 'L O S T    T I M E'
     O          E            HEAD           1
     O                                           45 '________________________'
     O                                           69 '________________________'
     O                                           73 '____'
     O                                          104 '________________________'
     O                                          128 '________________________'
     O                                          132 '____'
     O          E            HEAD           1
     O                                           43 'TRANSFERRED'
     O                                          101 'TRANSFERRED'
     O          E            HEAD           2
     O                                           09 'USER NAME'
     O                                           45 'NEW   FROM LT   TO LT'
     O                                           63 'REOPEN   CLOSED'
     O                                           85 'PENDING         NEW'
     O                                          112 'FROM MO   TO MO   REOPEN'
     O                                          131 'CLOSED   PENDING'
     O          E            HEADF          2
     O                                              'GROUP:'
     O                       XHFND         Z   +  1
     O                       FNNAME            +  1
     O          E            DET            1
     O                       XXSTNM              20
     O                       XXMNEW        K     28
     O                       XXLTMO        K     37
     O                       XXMOLT        K     45
     O                       XXMRO         K     54
     O                       XXMCLS        K     63
     O                       XXMPND        K     72
     O*
     O                       XXLNEW        K     86
     O                       XXMOLT        K     95
     O                       XXLTMO        K    103
     O                       XXLRO         K    112
     O                       XXLCLS        K    121
     O                       XXLPND        K    130
     O*** Begin add *** 04/26/02 **************************************
     O          E            DET            1
     O*** End add ***** 04/26/02 **************************************
     O*** Begin dlt *** 04/26/02 **************************************
     O********E  2             DET
     O********                 XXSTRG    45
     O*** End dlt ***** 04/26/02 **************************************
     O          E            FTOT        1  3
     O                                           13 'GROUP TOTALS:'
     O                       XFMNEW        K     28
     O                       XFLTMO        K     37
     O                       XFMOLT        K     45
     O                       XFMRO         K     54
     O                       XFMCLS        K     63
     O                       XFMPND        K     72
     O*
     O                       XFLNEW        K     86
     O                       XFMOLT        K     95
     O                       XFLTMO        K    103
     O                       XFLRO         K    112
     O                       XFLCLS        K    121
     O                       XFLPND        K    130
     O          E            GTOT           1
     O                                           13 'GRAND TOTALS:'
     O                       XGMNEW        K     28
     O                       XGLTMO        K     37
     O                       XGMOLT        K     45
     O                       XGMRO         K     54
     O                       XGMCLS        K     63
     O                       XGMPND        K     72
     O*
     O                       XGLNEW        K     86
     O                       XGMOLT        K     95
     O                       XGLTMO        K    103
     O                       XGLRO         K    112
     O                       XGLCLS        K    121
     O                       XGLPND        K    130
     O          E            GTOT           1
     O                                           14 'TOTAL PENDING:'
     O                       XXOPEN        K     28
     O          E            GTOT           1
     O                                           13 'TOTAL CLOSED:'
     O                       XXCLOS        K     28
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
