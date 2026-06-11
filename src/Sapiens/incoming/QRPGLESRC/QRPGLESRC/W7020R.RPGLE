     H* (C) 2015 STONERIVER, INC. ALL RIGHTS RESERVED.                *
     H* STONERIVER IS A TRADEMARK OF STONERIVER, INC.                 *
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W7020R - DAILY CHECK REGISTER BY FUND YEAR
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                -Add E-pay logic                               *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                   Use EFT flag instead of EFT Bank code.      *
     H*                                                               *
     H* 02/10/10 C:246 PROJECT: C133WIJ                               *
     H*               ADD Jurisdiction State in the report.           *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 06/24/08 C:15 Project: WC1453 PRISS #08                       *
     H*               NOTIFY EFT TRANSACTIONS AND PRE-NOTIFICATIONS IN*
     H*               REPORT.                                         *
     H*                                                               *
     H* 11/19/01 C:15 Project: WC1020                                 *
     H*               Chgd code to use WACPL instead of WACPL4 so the *
     H*               report prints by group#.                        *
     H*                                                               *
     H* 05/10/01 C:15 Project: WC1020                                 *
     H*               Added code to read the check schedule records   *
     H*               by CO#/PRDT/ACCT/CKTP/FND.  And read the rqst   *
     H*               pmt records by CO#/CKTP/FND/YR/CASE/SEQ#.       *
     H*                                                               *
     H*  04/30/01 C:15  Support: WC082/27202                          *
     H*                 Added code to reset the company name printed  *
     H*                 on the report if the Alternate company name   *
     H*                 is used for a specific group.                 *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                 IF AVAILABLE.                                 *
     H*                                                               *
     H*  03/02/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  09/11/95 C:15 PROJECT: WC---                                 *
     H*                FIXED THE FUND HEADING.  IF NO CHECKS WERE     *
     H*                PRINTED FOR A FUND, THE HEADING WAS INCORRECT. *
     H*                                                               *
     H*  09/08/94 C:15 PROJECT: WC361                                 *
     H*                ADDED TYPE PARM TO DIFFERENTIATE BETWEEN       *
     H*                GROUP CHECKS AND NORMAL CHECKS.                *
     H*                                                               *
     H*  05/31/94 C:25 PROJECT WC327                                  *
     H*                ADDED CLAIMANT FIRST NAME AND MIDDLE INITIAL.  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FWARPLV    IF   E           K DISK
     FWACPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     F*QSYSPRT O   F     132     OA     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D OPTNM1          C                   CONST('              GROUP -
     D                                     CHECK REGISTER BY PO-
     D                                     LICY YEAR           ')
     D OPTNM2          C                   CONST('              DAILY -
     D                                     CHECK REGISTER BY PO-
     D                                     LICY YEAR           ')
     d*** Beg Add *** 07/20/20 *********************************************
     d isCheck         s               n
     d isEFT           s               n
     d isEPay          s               n
     d*** End Add *** 07/20/20 *********************************************
     D*
     D*
     D*
     D*                                                               *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQTYPE            1
     C*
     C     DT6MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C*
     C*   KEY LIST FOR THE CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT              7 0
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*   REQUEST PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPCKTP
     C                   KFLD                    CPFND
     C*
     C*   CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DT6MDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SVCONM           40
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*   START READING THE CHECK PROCESSING FILE
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C** PROCESSING EFT TRANSACTIONS
     C                   SETOFF                                       59
      *******Beg Chg********04/17/12**********************************
     C*****CPCKTP        IFEQ      'EFT'
      /Free
       //*** Beg Del *** 07/20/20 ************************************
       //****If CPeftc = 'E';
       //*** End Del *** 07/20/20 ************************************
      /End-Free
      *******End Chg********04/17/12**********************************
       //*** Beg Del *** 07/20/20 ************************************
     C*************      SETON                                        59
     C*************      ENDIF
       //*** End Del *** 07/20/20 ************************************
       //*** Beg Add *** 07/20/20 ************************************
                         Select;
                         When CPeftC = ' ';
                            isCheck = *on;
                            isEft   = *off;
                            isEpay  = *off;
                         When CPeftC = 'E';
                            isCheck = *off;
                            isEft   = *on;
                            isEpay  = *off;
                         When CPeftC = 'V';
                            isCheck = *off;
                            isEft   = *off;
                            isEpay  = *on;
                         When CPeftC = 'B';
                            isCheck = *on;
                            isEft   = *on;
                            isEpay  = *off;
                         When CPeftC = 'A';
                            isCheck = *on;
                            isEft   = *on;
                            isEpay  = *on;
                         Endsl;
       //*** End Add *** 07/20/20 ************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE CORRECT TITLE
     C*
     C     QQTYPE        IFEQ      'G'
     C                   MOVE      OPTNM1        XXTITL           60
     C                   ELSE
     C                   MOVE      OPTNM2        XXTITL
     C                   END
     C*
     C*  CENTER PROGRAM TITLE
     C*
     C                   MOVEL(P)  XXTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        XXTITL
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *INOA
     C                   END
     C*
     C*   GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C   60              MOVE      *BLANK        FNNAME
     C*
     C     *IN60         IFEQ      *OFF
     C     FNANAM        ANDNE     *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ELSE
     C                   MOVEL(P)  SVCONM        CONAME
     C                   ENDIF
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C*****************************************************************
     C*   1st Pass: Read request for payment file and check for Checks
     C*****************************************************************
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   CHECK FOR ACTIVE PAYMENTS FOR CHECKS FIRST
     C*   If we find any checks turn on indicator for checks
     C*   and turn off indicators for EFT and E-Pay
      *******Beg Chg********04/17/12**********************************
     C*****RPSTAT        IFEQ      'Y'
      /Free
       //*** Beg Chg *** 07/20/20 ************************************
       //***If RPstat = 'Y' And ((RPeftc = 'E' And *In59 = *On) Or
       //*** (RPeftc = ' ' And *In59 = *Off));
       If RPstat = 'Y' And RPeftc = ' ' And isCheck = *On;
          *in56 = *on;
          *in57 = *off;
          *in59 = *off;
       //*** End Chg *** 07/20/20 ************************************
      /End-Free
      *******End Chg********04/17/12**********************************
     C*
     C*   FIRST TIME THROUGH, SAVE THE FUND YEAR
     C*
     C     *IN80         IFEQ      '0'
     C*
     C                   MOVE      RPFYR         XXFYR             3 0
     C*
     C                   MOVE      '1'           *IN80
     C                   END
     C*
     C*   IF THE FUND YEAR HAS CHANGED, THEN PRINT THE FUND YEAR
     C*   TOTALS
     C*
     C     XXFYR         IFNE      RPFYR
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    FYRT
     C                   MOVE      RPFYR         XXFYR
     C                   MOVE      *ZERO         XXFYRT
     C                   MOVE      *ZERO         XXFYCT
     C*
     C                   END
     C*
     C*   CHAIN TO THE CLAIM HEADER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   CHAIN TO THE DEMOGRAPHICS FILE
     C*
     C                   MOVE      CMJRST        XXST              2
     C                   MOVE      *BLANKS       XCNAME           20
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       QQFNAM           15
     C                   MOVEL     QQMID         QQFNAM
     C*
     C                   MOVE      *BLANK        QQLNAM           56
     C                   MOVEL     CDLSN         QQLNAM
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XCNAME
     C                   END
     C                   END
     C*** PRE-NOTIFICATION TRANSACTION
     C                   SETOFF                                       58
     C     RPAMT         IFEQ      0
     C                   SETON                                        58
     C                   ENDIF
     C*
     C*   PRINT THE LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C*
     C*   ADD TO THE TOTALS
     C*
     C                   ADD       RPAMT         XXFYRT            9 2
     C                   ADD       RPAMT         XXFNDT            9 2
     C                   ADD       1             XXFYCT            5 0
     C                   ADD       1             XXFNCT            5 0
     C                   END
     C*
     C     RPKEY         READE     PRP01                                  60
      *******Beg Add********04/17/12**********************************
      /Free
       //*** Beg Del *** 07/20/20 ************************************
       //***If *In60 = *On And CPeftc = 'B' And *In59 = *Off;
       //***   *In59 = *On;
       //***   Chain  RPkey prp01;
       //***   If %Found;
       //***      *In60 = *Off;
       //***   EndIf;
       //***EndIf;
       //*** End Del *** 07/20/20 ************************************
      /End-Free
      *******End Add********04/17/12**********************************
     C                   END
       //*** Beg Add *** 07/20/20 ******************************************
       //*** Beg Add *** 07/20/20 ******************************************
       if xxFyrT > 0;
          If *inoa = *on;
             Except Head;
             *inoa = *off;
          Endif;
          Except FyrT;
          xxFyrT = 0;
          xxFyCt = 0;
       Endif;
       //*** End Add *** 07/20/20 ******************************************
     C*
     C*  PRINT THE TOTAL FOR THE FUND
     C*
     C     xxFnCt        Ifgt      0
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    FNDTT
     C                   Z-ADD     0             XXFNCT
     C                   Z-ADD     0             XXFNDT
     C                   Z-ADD     0             PAGE
     C                   MOVE      '0'           *IN80
     C                   Endif
     C*****************************************************************
     C*   2nd Pass: Read request for payment file and check for EFT
     C*****************************************************************
     C*
       *inoa = *on;
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   If we find any EFT turn on indicator for EFT
     C*   and turn off indicators for Checks and E-Pay
      /Free
       If RPstat = 'Y' And RPeftc = 'E' And isEFT = *On;
          *in56 = *off;
          *in57 = *off;
          *in59 = *on;
      /End-Free
     C*
     C*   FIRST TIME THROUGH, SAVE THE FUND YEAR
     C*
     C     *IN80         IFEQ      '0'
     C*
     C                   MOVE      RPFYR         XXFYR             3 0
     C*
     C                   MOVE      '1'           *IN80
     C                   END
     C*
     C*   IF THE FUND YEAR HAS CHANGED, THEN PRINT THE FUND YEAR
     C*   TOTALS
     C*
     C     XXFYR         IFNE      RPFYR
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    FYRT
     C                   MOVE      RPFYR         XXFYR
     C                   MOVE      *ZERO         XXFYRT
     C                   MOVE      *ZERO         XXFYCT
     C*
     C                   END
     C*
     C*   CHAIN TO THE CLAIM HEADER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   CHAIN TO THE DEMOGRAPHICS FILE
     C*
     C                   MOVE      CMJRST        XXST              2
     C                   MOVE      *BLANKS       XCNAME           20
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       QQFNAM           15
     C                   MOVEL     QQMID         QQFNAM
     C*
     C                   MOVE      *BLANK        QQLNAM           56
     C                   MOVEL     CDLSN         QQLNAM
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XCNAME
     C                   END
     C                   END
     C*** PRE-NOTIFICATION TRANSACTION
     C                   SETOFF                                       58
     C     RPAMT         IFEQ      0
     C                   SETON                                        58
     C                   ENDIF
     C*
     C*   PRINT THE LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C*
     C*   ADD TO THE TOTALS
     C*
     C                   ADD       RPAMT         XXFYRT            9 2
     C                   ADD       RPAMT         XXFNDT            9 2
     C                   ADD       1             XXFYCT            5 0
     C                   ADD       1             XXFNCT            5 0
     C                   END
     C*
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*
     C*  PRINT THE TOTAL FOR THE FUND
     C*
     C     xxFnCt        Ifgt      0
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    FNDTT
     C                   Z-ADD     0             XXFNCT
     C                   Z-ADD     0             XXFNDT
     C                   Z-ADD     0             PAGE
     C                   MOVE      '0'           *IN80
     C                   Endif
     C*****************************************************************
     C*   3rd Pass: Read request for payment file and check for E-Pay
     C*****************************************************************
     C*
       *inoa = *on;
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   If we find any E-Pay turn on indicator for E-Pay
     C*   and turn off indicators for Checks and EFT
      /Free
       If RPstat = 'Y' And RPeftc = 'V' And isEPay = *On;
          *in56 = *off;
          *in57 = *on;
          *in59 = *off;
      /End-Free
     C*
     C*   FIRST TIME THROUGH, SAVE THE FUND YEAR
     C*
     C     *IN80         IFEQ      '0'
     C*
     C                   MOVE      RPFYR         XXFYR             3 0
     C*
     C                   MOVE      '1'           *IN80
     C                   END
     C*
     C*   IF THE FUND YEAR HAS CHANGED, THEN PRINT THE FUND YEAR
     C*   TOTALS
     C*
     C     XXFYR         IFNE      RPFYR
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    FYRT
     C                   MOVE      RPFYR         XXFYR
     C                   MOVE      *ZERO         XXFYRT
     C                   MOVE      *ZERO         XXFYCT
     C*
     C                   END
     C*
     C*   CHAIN TO THE CLAIM HEADER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   CHAIN TO THE DEMOGRAPHICS FILE
     C*
     C                   MOVE      CMJRST        XXST              2
     C                   MOVE      *BLANKS       XCNAME           20
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       QQFNAM           15
     C                   MOVEL     QQMID         QQFNAM
     C*
     C                   MOVE      *BLANK        QQLNAM           56
     C                   MOVEL     CDLSN         QQLNAM
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XCNAME
     C                   END
     C                   END
     C*** PRE-NOTIFICATION TRANSACTION
     C                   SETOFF                                       58
     C     RPAMT         IFEQ      0
     C                   SETON                                        58
     C                   ENDIF
     C*
     C*   PRINT THE LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C*
     C*   ADD TO THE TOTALS
     C*
     C                   ADD       RPAMT         XXFYRT            9 2
     C                   ADD       RPAMT         XXFNDT            9 2
     C                   ADD       1             XXFYCT            5 0
     C                   ADD       1             XXFNCT            5 0
     C                   END
     C*
     C     RPKEY         READE     PRP01                                  60
     C                   END
       //*** End Add *** 07/20/20 ******************************************
     C*
     C*  PRINT THE TOTAL FOR THE FUND YEAR
     C*
       //*** Beg Add *** 07/20/20 ******************************************
       if xxFyrT > 0;
       //*** End Add *** 07/20/20 ******************************************
     C
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    FYRT
     C                   MOVE      *ZERO         XXFYRT
     C                   MOVE      *ZERO         XXFYCT
       //*** Beg Add *** 07/20/20 ******************************************
       Endif;
       //*** End Add *** 07/20/20 ******************************************
     C*
     C*  PRINT THE TOTAL FOR THE FUND
     C*
     C     xxFnCt        Ifgt      0
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    FNDTT
     C                   Z-ADD     0             XXFNCT
     C                   Z-ADD     0             XXFNDT
     C                   Z-ADD     0             PAGE
     C                   MOVE      '0'           *IN80
     C                   Endif
     C*
     C     CPKEY         READE     PCP01                                  60
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
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
     O                       XXTITL              80
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
     O*
     O*
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*
     O                       CONAME              87
     O*
     O*                        XXDATEY  132
     O                       XXDATE        Y    195
     O          E            HEAD           2
     O                       Q1PGM               10
     O*
     O                       XXTITL             106
     O*
     O**                                126 'PAGE:'
     O**                       PAGE  Z  132
     O                                          192 'PAGE:'
     O                       PAGE          Z    195
     O          E            HEAD           2
     O*
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O               59                          88 'EFT Transactions'
     O*** Beg Chg *** 07/20/20 ****************************************
     O**********    N59                          86 'Check Payments'
     O               56                          86 'Check Payments'
     O               57                          86 'E-Pay Payments'
     O*** End Chg *** 07/20/20 ****************************************
     O          E            HEAD           1
     O                                           23 'JURISDICTION'
     O*
     O          E            HEAD           2
     O                                           03 'YR'
     O                                           10 'CASE'
     O**                                 20 'CLAIMANT'
     O**                                 37 'PMT CD'
     O**                                 44 'PAYEE'
     O**                                 52 'MOD'
     O**                                 65 'PAYABLE TO:'
     O**              N59               105 'CHK DATE'
     O**               59               105 'EFT DATE'
     O**              N59               115 'CHECK #'
     O**                                131 'AMOUNT'
     O                                           20 'STATE'
     O                                           33 'CLAIMANT'
     O                                           60 'PMT CD'
     O                                           70 'PAYEE'
     O                                           95 'MOD'
     O                                          121 'PAYABLE TO:'
     O*** Beg Chg *** 07/20/20 ****************************************
     O***********   N59                         160 'CHK DATE'
     O               56                         160 'CHK DATE'
     O               57                         160 'EPAY DATE'
     O*** End Chg *** 07/20/20 ****************************************
     O               59                         172 'EFT DATE'
     O*** Beg Chg *** 07/20/20 ****************************************
     O************  N59                         182 'CHECK #'
     O               56                         182 'CHECK #'
     O               57                         182 'E-PAY #'
     O               59                         182 'EFT #'
     O*** End Chg *** 07/20/20 ****************************************
     O                                          193 'AMOUNT'
     O          E            DET            1
     O                       RPFYR         Z     03
     O** BEG CHG 02/10/10 **********************
     O**                       RPCASEZ   11
     O**                       XCNAME    32
     O**                       RPTRCD    35
     O**                       RPFEINZ   48
     O**                       RPMOD Z   52
     O**                       RPPNAM    94
     O**                       XXDATEY  105
     O**              N59      RPCHK#Z  115
     O                       RPCASE        Z     10
     O                       XXST                18
     O                       XCNAME              44
     O                       RPTRCD              56
     O                       RPFEIN        Z     74
     O                       RPMOD         Z     97
     O                       RPPNAM             150
     O                       XXDATE        Y    160
     O*** Beg Chg *** 07/20/20 *********************************************
     O**********    N59      RPCHK#        Z    183
     O                       RPCHK#        Z    183
     O*** End Chg *** 07/20/20 *********************************************
     O                       RPAMT         J    196
     O          E            FYRT        1  2
     O**                                 97 'COUNT:'
     O**                       XXFYCTZ  103
     O**                                118 'TOTAL:'
     O**                       XXFYRTJ  131
     O                                          159 'COUNT:'
     O                       XXFYCT        Z    163
     O                                          182 'TOTAL:'
     O                       XXFYRT        J    196
     O          E            FNDTT       1  2
     O**                                 97 'FINAL COUNT:'
     O**                       XXFNCTZ  103
     O**                                118 'FINAL TOTAL:'
     O**                       XXFNDTJ  131
     O                                          162 'FINAL COUNT:'
     O                       XXFNCT        Z    166
     O                                          180 'FINAL TOTAL:'
     O                       XXFNDT        J    196
