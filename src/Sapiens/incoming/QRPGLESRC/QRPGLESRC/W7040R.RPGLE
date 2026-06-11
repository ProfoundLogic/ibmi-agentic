     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7040R - RESERVE HISTORY FILE EDIT LISTING
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  05/05/00  C:15  Support: WC096/22751                        *
     H*                  Recompiled pgm due to chg to WMRHL2.        *
     H*                                                              *
     H*  12/10/99  C:90 PROJECT: WC577                               *
     H*                 Y2K CHANGE - DISPLAY YR AS 3 CHAR, NOT 2.    *
     H*                                                              *
     H*  10/19/95  C:33  PROJECT# WC455                              *
     H*                 -ADJUSTED OUTPUT FOR READABILITY             *
     H*                 -CORRECTED SUPERIMPOSED AMOUNTS              *
     H*                 -ADD CLAIMANT NAME TO REPORT                 *
     H*                                                              *
     H*  01/20/98  C:77  PROJECT  SUPPORT (WC033 #9755)              *
     H*                  CHANGED QSYSPRT WIDTH TO BE 198 INSTEAD OF  *
     H*                  200.                                        *
     H****************************************************************
     F*
     F* * * Reserve History File
     FWMRHL2    IF   E           K DISK
     F*
     F* * * Batch Routing File
     FSMORL     IF   E           K DISK
     F*
     F* * * Company File
     FSMCOL     IF   E           K DISK
     F*
     F* * * Fund File
     FWMFNL     IF   E           K DISK
     F*
     F* * * Reserve Type Table File
     FWTABL     IF   E           K DISK
     F*
     F* * * User Parameter File
     FWMUPL     IF   E           K DISK
     F*
     F* * * CLAIMANT DEMOGRAPHIC FILE
     FWMCDL     IF   E           K DISK
     F*
     F* * * CLAIM MASTER FILE
     FWMCML1    IF   E           K DISK
     F*
     F* * * Output File
     F*** BEGIN CHG *** 01/20/98 **************************************
     F*****QSYSPRT O   F     200     OF     PRINTER                  UC
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*** END CHG ***** 01/20/98 **************************************
     F*

     D* * * DETAIL ARRAYS
     D DRT             S              1    DIM(10)                              RESERVE TYP 1-10
     D*
     D* * * FUND TOTAL ARRAYS
     D FRC             S              2    DIM(999)                             RSRV COD FND TOT
     D FRT             S              1    DIM(999)                             RSRV TYP FND TOT
     D FRA             S             10  2 DIM(999)                             RSRV AMT FND TOT
     D*
     D* * * SORT ARRAYS
     D #RC             S              2    DIM(999) ASCEND                      RSRV COD SRT ARY
     D #RT             S              1    DIM(999)                             RSRV TYP SRT ARY
     D #RA             S             11  2 DIM(999)                             RSRV AMT SRT ARY
     D*
     D* * * PRINT ARRAYS
     D PRTC            S              2    DIM(4)                               RSRV COD TTL PRT
     D PRTT            S              1    DIM(4)                               RSRV TYP TTL PRT
     D PRTA            S             11  2 DIM(4)                               RSRV AMT TTL PRT
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  DRC                    1     20
     D                                     DIM(10)                              RESERVE COD 1-10
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  DRA                    1     90  2
     D                                     DIM(10)                              RESERVE AMT 1-10
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RHFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    RHUSER
     C*
     C*  RESERVE HISTORY FILE KEY
     C*
     C     RHKEY         KLIST
     C                   KFLD                    XXDATE
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE RESERVE TYPE TABLE FILE
     C*
     C     RTKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    RHCO#
     C                   KFLD                    RHFND
     C                   KFLD                    RHFYR
     C                   KFLD                    RHCASE
     C*
     C/EJECT
     C*
     C     *LIKE         DEFINE    CMDT          XXDATE
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
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   Z-ADD     X1DATE        ZZDATE            6 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C* * * INITIALIZE ALL ARRAYS TO *ZEROS/*BLANKS
     C                   MOVE      *BLANKS       DRC
     C                   MOVE      *BLANKS       DRT
     C                   MOVE      *ZEROS        DRA
     C*
     C                   MOVE      *BLANKS       FRC
     C                   MOVE      *BLANKS       FRT
     C                   MOVE      *ZEROS        FRA
     C*
     C                   MOVEL     *BLANK        PRTC
     C                   MOVEL     *BLANK        PRTT
     C                   Z-ADD     *ZEROS        PRTA
     C*
     C*  GET THE COMPANY RECORD.
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
     C*
     C*
     C                   TIME                    DSDTTM
     C/EJECT
     C     RHKEY         CHAIN     PRH01                              40
     C*
     C     *IN40         DOWEQ     '0'
     C*
     C  N11              EXSR      SR1ST
     C*
     C     RHFND         CASNE     XHFND         SRFBRK
     C     RHUSER        CASNE     XHUSER        SRUBRK
     C                   END
     C*
     C* * *  DETAIL ROUTINE
     C*
     C* * * INITIALIZE THE DRT ARRAY
     C                   MOVE      *BLANKS       DRT
     C*
     C* * *  LOOP THROUGH DETAIL ARRAY AND ADD TO FUND TOTAL ARRAYS
     C                   DO        10            X                 5 0
     C*
     C     DRC(X)        IFGT      *BLANK
     C*
     C* * * GET RESERVE TYPE FROM TABLE FILE
     C                   MOVEL     DRC(X)        XXKEY            10
     C     RTKY1         CHAIN     PRT01                              41
     C*
     C     *IN41         IFEQ      '0'
     C                   MOVE      RTTYPE        DRT(X)
     C                   END
     C*
     C* * * SEE IF THE RESERVE CODE HAS ALREADY BEEN INCLUDED IN FUND
     C* * * TOTAL ARRAYS
     C                   Z-ADD     1             Y                 5 0
     C     DRC(X)        LOOKUP    FRC(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       DRA(X)        FRA(Y)
     C                   ELSE
     C*
     C* * * GET THE 1ST AVAILABLE TOTAL BUCKET AND INSERT THIS NEW
     C* * * RESERVE CODE/TYPE/AND AMOUNT
     C                   Z-ADD     1             Y                 5 0
     C     *BLANK        LOOKUP    FRC(Y)                                 61
     C   61              DO
     C                   MOVEL     DRC(X)        FRC(Y)
     C                   MOVEL     DRT(X)        FRT(Y)
     C                   Z-ADD     DRA(X)        FRA(Y)
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C* * * CHECK ARRAY ELEMENTS 1-4,5-8 & 9-10 TO SEE IF THERE IS DATA
     C* * * TO PRINT
     C                   MOVEA     DRC           XXDRC            10
     C     XXDRC         COMP      *BLANK                             20
     C*
     C                   MOVEA     DRC(5)        XXDRC            10
     C     XXDRC         COMP      *BLANK                             21
     C*
     C                   MOVEA     DRC(9)        XXDRC            10
     C     XXDRC         COMP      *BLANK                             22
     C*
     C* * * CHECK TO SEE IF OVERFLOW OCCURED ON LAST OUTPUT
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    UHEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
      **** BEGIN CHG **** 12/10/99 ************************************
     C******************** MOVE RHFYR     XXYR    20
     C                   MOVE      RHFYR         XXYR              3 0
      **** END   CHG **** 12/10/99 ************************************
     C                   Z-ADD     RHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C*   GET THE CLAIMANT'S NAME.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XXFRLS           19
     C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXFRLS
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *BLANK        XXFRLS
     C                   END
     C*
     C* * * PRINT DETAIL LINES
     C                   EXCEPT    DETL
     C*
     C     RHKEY         READE     PRH01                                  40
     C*
     C                   END
     C*
     C   11              EXSR      SRFPRT
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
     C                   MOVE      '1'           *INLR
     C/EJECT
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C                   MOVE      RHFND         XHFND             3 0
     C                   MOVE      RHUSER        XHUSER           10
     C*
     C* * * GET FUND NAME FROM FUND MASTER FILE
     C     FNKEY         CHAIN     PFN01                              42
     C     *IN42         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVEL     *BLANKS       XFNAME
     C                   END
     C*
     C* * * GET USER NAME FROM USER PARM FILE
     C     UPKEY         CHAIN     PUP01                              43
     C     *IN43         IFEQ      '0'
     C                   MOVEL     UPUSNM        XUNAME           50
     C                   ELSE
     C                   MOVEL     *BLANKS       XUNAME
     C                   END
     C*
     C* * * OPEN PRINT FILE
     C                   OPEN      QSYSPRT
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    UHEAD
     C                   EXCEPT    HEAD1
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Level Break on Fund #             ** Subroutine **  S R F B R K *
      *==================================================================*
     C*
     CSR   SRFBRK        BEGSR
     C*
     C* * * PRINT FUND TOTALS
     C                   EXSR      SRFPRT
     C*
     C* * * CLEAR FUND TOTALS
     C                   MOVE      *BLANKS       FRC
     C                   MOVE      *BLANKS       FRT
     C                   MOVE      *ZEROS        FRA
     C*
     C* * * GET NEW FUND INFORMATION
     C     FNKEY         CHAIN     PFN01                              42
     C     *IN42         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME
     C                   ELSE
     C                   MOVEL     *BLANKS       XFNAME
     C                   END
     C*
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C                   MOVE      RHFND         XHFND
     C*
     C* * * FORCE USER BREAK
     C                   MOVE      *BLANKS       XHUSER
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Print Fund Totals                 ** Subroutine **  S R F P R T *
      *==================================================================*
     C*
     CSR   SRFPRT        BEGSR
     C                   MOVE      '1'           *IN12
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *IN12
     C*
     C* * * TAG ALONG SORT FOR THE THREE TOTAL ARRAYS
     C                   MOVEA     FRC           #RC
     C                   MOVE      *BLANK        #RT
     C                   MOVE      *ZEROS        #RA
     C                   Z-ADD     1             Z                 5 0
     C     *BLANKS       LOOKUP    #RC(Z)                                 60
     C                   MOVEA     *ALL'9'       #RC(Z)
     C                   SORTA     #RC
     C                   DO        999           X
     C     #RC(X)        IFNE      '99'
     C                   Z-ADD     1             Y
     C     #RC(X)        LOOKUP    FRC(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     FRT(Y)        #RT(X)
     C                   Z-ADD     FRA(Y)        #RA(X)
     C                   END
     C                   ELSE
     C                   Z-ADD     999           X
     C                   END
     C                   END
     C*
     C* * * PRINT FUND TOTALS
     C                   Z-ADD     1             Y
     C                   DO        999           X
     C*
     C     #RC(X)        IFNE      '99'
     C                   MOVEL     #RC(X)        PRTC(Y)
     C                   MOVEL     #RT(X)        PRTT(Y)
     C                   Z-ADD     #RA(X)        PRTA(Y)
     C*
     C     Y             IFGE      4
     C                   EXCEPT    FTOTL
     C                   Z-ADD     *ZERO         Y
     C                   MOVEL     *BLANK        PRTC
     C                   MOVEL     *BLANK        PRTT
     C                   Z-ADD     *ZEROS        PRTA
     C                   END
     C*
     C* * * INCREMENT PRINTING ARRAY INDEX
     C                   ADD       1             Y
     C                   ELSE
     C*
     C* * * END OF DATA REACHED IN ARRAY, FORCE END OF LOOP
     C                   Z-ADD     999           X
     C                   END
     C                   END
     C*
     C* * * CHECK TO SEE IF ANY VALUES WERE LEFT TO PRINT
     C     PRTC(1)       IFGT      *BLANK
     C                   EXCEPT    FTOTL
     C                   END
     C*
     C                   XFOOT     #RA           FTOTAL           12 2
     C                   EXCEPT    FTOTL1
     C*
     C                   Z-ADD     *ZERO         Y
     C                   MOVEL     *BLANK        PRTC
     C                   MOVEL     *BLANK        PRTT
     C                   Z-ADD     *ZEROS        PRTA
     C*
     C                   ENDSR
     C/EJECT
      *==================================================================*
      *  Level Break on User               ** Subroutine **  S R U B R K *
      *==================================================================*
     C*
     CSR   SRUBRK        BEGSR
     C*
     C* * * GET USER NAME FROM USER PARM FILE
     C     UPKEY         CHAIN     PUP01                              43
     C     *IN43         IFEQ      '0'
     C                   MOVEL     UPUSNM        XUNAME           50
     C                   ELSE
     C                   MOVEL     *BLANKS       XUNAME
     C                   END
     C*
     C* * * MOVE USER NAME TO HOLD FIELD
     C                   MOVEL     RHUSER        XHUSER
     C*
     C* * * PRINT NEW USER HEADING
     C                   EXCEPT    UHEAD
     C                   EXCEPT    HEAD1
     C*
     C                   ENDSR
     C*
     O*
     O/EJECT
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
     O**********             ORDIS1              60
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
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O*** BEGIN CHG *** 01/20/98 **************************************
     O*************************          46 'VE DATA SOLUTIONS. INC,'
     O*************************          57 '.1996      '
     O***                                47 'VE DATA SOLUTIONS, INC.,'
     O****                     *YEAR   +  1
     O*** END DLT ***** 01/28/10 **************************************
     O*** END CHG ***** 01/20/98 **************************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             117
     O*** BEGIN CHG *** 01/20/98 **************************************
     O*************************X1DATEY  200
     O                       X1DATE        Y    198
     O*** END CHG ***** 01/20/98 **************************************
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             118
     O*** BEGIN CHG *** 01/20/98 **************************************
     O*************************         196 'PAGE:'
     O*************************PAGE  Z  200
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*** END CHG ***** 01/20/98 **************************************
     O          E    12      HEAD           2
     O                                          101 '* FUND TOTALS *'
     O          E            HEAD           2
     O                                           05 'FUND:'
     O                 N12   RHFND         Z     09
     O                  12   XHFND         Z     09
     O                       XFNAME              70
     O          E   N12      HEAD1          2
      **** BEGIN CHG **** 12/10/99 ************************************
     O*********************************** 2 'YR'
     O                                            3 'YR'
      **** END   CHG **** 12/10/99 ************************************
     O                                            9 'CASE'
     O                                           23 'CLAIMANT NAME'
     O                                           39 'EFF DATE'
     O                                           62 'EXPLANATION FOR CHANGE'
     O                                          118 'RESERVE CODES AND'
     O                                          133 'AMOUNT CHANGED'
     O          E    12      HEAD           2
     O                                            5 'CODE'
     O                                           11 'TYPE'
     O                                           25 'AMOUNT'
     O                                           35 'CODE'
     O                                           41 'TYPE'
     O                                           55 'AMOUNT'
     O                                           65 'CODE'
     O                                           71 'TYPE'
     O                                           85 'AMOUNT'
     O                                           95 'CODE'
     O                                          101 'TYPE'
     O                                          115 'AMOUNT'
     O          E            UHEAD          1
     O                       RHUSER              10
     O                       XUNAME              62
     O          E            DETL           0
      **** BEGIN CHG **** 12/10/99 ************************************
     O************************ XXYR       2
     O                       XXYR                 3
      **** END   CHG **** 12/10/99 ************************************
     O                       RHCASE        Z     10
     O                       XXFRLS              30
     O                       XXEFDT        Y     39
     O                       RHEXPL             100
     O          E    20      DETL           1
     O                       DRC(1)             103
     O                       DRT(1)             105
     O                       DRA(1)        K    119
     O*** BEGIN CHG *** 01/20/98 **************************************
     O*************************DRC,2    130
     O*************************DRT,2    132
     O*************************DRA,2 K  146
     O*************************DRC,3    157
     O*************************DRT,3    159
     O*************************DRA,3 K  173
     O*************************DRC,4    184
     O*************************DRT,4    186
     O*************************DRA,4 K  200
     O                       DRC(2)             129
     O                       DRT(2)             131
     O                       DRA(2)        K    145
     O                       DRC(3)             155
     O                       DRT(3)             157
     O                       DRA(3)        K    171
     O                       DRC(4)             181
     O                       DRT(4)             183
     O                       DRA(4)        K    197
     O*** END CHG ***** 01/20/98 **************************************
     O          E   N21      DETL           1
     O          E    21      DETL           2
     O                       DRC(5)             103
     O                       DRT(5)             105
     O                       DRA(5)        K    119
     O*** BEGIN CHG *** 01/20/98 **************************************
     O*************************DRC,6    130
     O*************************DRT,6    132
     O*************************DRA,6 K  146
     O*************************DRC,7    157
     O*************************DRT,7    159
     O*************************DRA,7 K  173
     O*************************DRC,8    184
     O*************************DRT,8    186
     O*************************DRA,8 K  200
     O                       DRC(6)             129
     O                       DRT(6)             131
     O                       DRA(6)        K    145
     O                       DRC(7)             155
     O                       DRT(7)             157
     O                       DRA(7)        K    171
     O                       DRC(8)             181
     O                       DRT(8)             183
     O                       DRA(8)        K    197
     O*** END CHG ***** 01/20/98 **************************************
     O          E   N22N21   DETL           0
     O          E   N22      DETL           1
     O          E    22      DETL           2
     O                       DRC(9)             103
     O                       DRT(9)             105
     O                       DRA(9)        K    119
     O*** BEGIN CHG *** 01/20/98 **************************************
     O*************************DRC,10   130
     O*************************DRT,10   132
     O*************************DRA,10K  146
     O                       DRC(10)            129
     O                       DRT(10)            131
     O                       DRA(10)       K    145
     O*** END CHG ***** 01/20/98 **************************************
     O          E            FTOTL          2
     O                       PRTC(1)              4
     O                       PRTT(1)              9
     O                       PRTA(1)       K     26
     O                       PRTC(2)             34
     O                       PRTT(2)             39
     O                       PRTA(2)       K     56
     O                       PRTC(3)             64
     O                       PRTT(3)             69
     O                       PRTA(3)       K     86
     O                       PRTC(4)             94
     O                       PRTT(4)             99
     O                       PRTA(4)       K    116
     O          E            FTOTL1
     O                                              'TOTAL FOR ALL CODES:'
     O                       FTOTAL        K     40
