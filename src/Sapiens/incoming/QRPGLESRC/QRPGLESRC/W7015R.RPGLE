     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H*****************************************************************
     H/TITLE W7015R - DAILY CHECK REGISTER
     H
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 07/27/22 C:079 Support: 102973                                *
     H*                - Monitor for errors on date conversions       *
     H*                                                               *
     H* 09/22/21 C:079 Support: 129/98306                             *
     H*                - Fix service date conversion routines         *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                - Add processing for E-pay. Headings, etc.     *
     H*                                                               *
     H* 11/15/16 C:180 Support: 42910                                 *
     H*                - Print summary page for processed groups only.*
     H*                                                               *
     H* 08/03/16 C:310 Support: 42672                                 *
     H*                -Modified code to print correct header.        *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 07/06/15 C:136 Support: 34572 (CLT133)                        *
     H*                Correct report so that it prints for the EFT   *
     H*                portion of the report                          *
     H*                                                               *
     H* 12/11/14 C:53  Redmine: 4851
     H*               -Fix title being off center and incomplete
     H*                                                               *
     H* 02/19/13 C:277 Support: WA133 81758                           *
     H*               -Fix fund info in header on group break         *
     H*               -Adjust WC2044 so that check totals print       *
     H*                before fund totals when switching from         *
     H*                MICR to EFT processing.                        *
     H*                xref 81945 & 81946                             *
     H*                                                               *
     H*  01/25/13 C:189  SUPPORT: WA133  81514                        *
     H*                  FUND NUMBER IS PRINTING INCORRECTLY          *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                   Use EFT flag instead of EFT Bank code.      *
     H*                                                               *
     H*  02/10/10 C:246  PROJECT: C133WIJ                             *
     H*                  ADD JURISDICTION STATE IN QSYSPRT            *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 07/01/08 C:190 Project: WC1638 Priss#6                        *
     H*                Change the group check register report to
     H*                sort based on control flag.                    *
     H*                                                               *
     H* 06/24/08 C:191 PROJECT: WC1453 PRISS#08                       *
     H*                ADD HEADER LABEL FOR EFT PAYMENTS AND NOTATE   *
     H*                PRE-NOTIFICATION TRANSACTIONS                  *
     H*                                                               *
     H* 05/12/08 C:136 PROJECT: 114WQQ                                *
     H*                PAGE BREAK ON GROUP TOTALS FOR CSI             *
     H*                                                               *
     H* 11/19/01 C:15 Project: WC1020                                 *
     H*               Chgd code to use WACPL instead of WACPL4 so the *
     H*               report prints by group#.                        *
     H*                                                               *
     H* 05/10/01 C:15 Project: WC1020                                 *
     H*               Added code to read the check schedule records   *
     H*               by CO#/PRDT/ACCT/CKTP/FND.  And read the rqst   *
     H*               pmt records by CO#/CKTP/FND.                    *
     H*                                                               *
     H*  04/30/01 C:15  Support: WC082/27202                          *
     H*                 Added code to reset the company name printed  *
     H*                 on the report if the Alternate company name   *
     H*                 is used for a specific group.                 *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC577                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                 IF AVAILABLE.                                 *
     H*                                                               *
     H*  02/26/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 STANDARDS.                           *
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
     FWARPLU    IF   E           K DISK    USROPN
     FWACPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FSMBUP     IF   E             DISK
     FWARPLX    IF   E           K DISK    USROPN
     F                                     RENAME(PRP01:PRP02)
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*                                                               *
     D OPTNM1          C                   CONST('Group Check Register-
     D                                       ')
     D OPTNM2          C                   CONST('Daily Check Register-
     D                                       ')
     D*
     D*                                                               *
     D*                                                               *
     D*                                                               *
     D PrintRoute      s              1  0 Inz(1)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d prtRouting      s               n   inz(*on)
     d xxprt           s              1    inz('0')
     d xxTitl          s             60
     d hasEft          s               n
     d isEft           s               n
     d isCheck         s               n
     d isEPay          s               n
     d*=================================================================
     d* Procedure Prototypes
     d*=================================================================
     d prcFrLs         pr                  ExtPgm('SRFRLS')
     d  inMid                        56
     d  inFsn                        15
     d  inTerm                        1
     d prcName         pr                  ExtPgm('SRNAME')
     d  inLsn                        56
     d  inFsn                        15
     d  inTerm                        1
     d prcCvtDat       pr                  ExtPgm('SRCVTDAT')
     d  inDtI                         8  0
     d  inFfmt                        8
     d  inTfmt                        8
     d  outDt                        10
     d  outErr                        1

     c/copy sprstring
     C*
     C     *LIKE         DEFINE    CMDT          XXDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQTYP             1
     C*
     C     DT6MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
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
     C     FNKEY#        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C*
     C*   KEY LIST FOR THE CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
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
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
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
     C                   Z-ADD     0             XXCCNT            5 0
     C                   Z-ADD     0             XXCTOT            9 2
     C                   Z-ADD     0             SVCHK#            7 0
      *
     C     1             CHAIN     SMBUP                              60
     C     BUGCKS        IFEQ      'Y'
     C                   OPEN      WARPLX
     C                   ELSE
     C                   OPEN      WARPLU
     C                   ENDIF
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SVCONM           40
     C*
     C*   CENTER COMPANY NAME
     C*
     C*
     C*
     C*
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
      /Free

       // Retrieve fund and alternate company name

        chain fnkey pfn01;
        COname = FNanam;
        if COname = *blanks;
         COname = svConm;
        endif;
        COname = centerFld(COname : 40);

       // Set up EFT or MICR run

        Select;
        When CPeftC = 'E';
           isCheck = *Off;
           isEFT   = *On;
           isEPay  = *Off;
        When CpeftC = ' ';
           isCheck = *On;
           isEFT   = *Off;
           isEPay  = *Off;
        When CPeftC = 'V';
           isCheck = *Off;
           isEFT   = *Off;
           isEPay  = *On;
        When CpeftC = 'B';
           isCheck = *On;
           isEFT   = *On;
           isEPay  = *Off;
        When CpeftC = 'A';
           isCheck = *On;
           isEFT   = *On;
           isEPay  = *On;
        Endsl;

       // Default report title

        xxTitl = optnm2;
        if qqTyp = 'G';
         xxTitl = optnm1;
        endif;

       // Print routing if routing record


       hasEFT = *off;
      /End-Free
     C*
     C*   1st Pass: Process Checks
     C*
     C     BUGCKS        IFEQ      'Y'
     C     RPKEY         CHAIN     PRP02                              60
     C                   ELSE
     C     RPKEY         CHAIN     PRP01                              60
     C                   ENDIF
     C                   MOVE      '1'           *INOA
     C     *IN60         DOWEQ     '0'
     C*
      /free
      /end-free
     C*   CHECK FOR ACTIVE PAYMENTS
     C*
      /Free
       If RPstat = 'Y' and RPeftC = ' ' and isCheck = *on;
          *in56 = *on;
          *in57 = *off;
          *in59 = *off;
      /End-Free
     C*
     C*   PRINT CHECK # TOTALS FOR GROUP CHECKS
     C*
     C     QQTYP         IFEQ      'G'
     C     *IN80         ANDEQ     '1'
     C     RPCHK#        ANDNE     SVCHK#
     C                   EXCEPT    CHKTT
     C     XXCLT#        IFEQ      '114'
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   Z-ADD     0             XXCCNT
     C                   Z-ADD     0             XXCTOT
     C                   END
     C*
     C                   ADD       1             XXCCNT
     C                   ADD       RPAMT         XXCTOT
     C                   Z-ADD     RPCHK#        SVCHK#
     C*
     C*   FIRST TIME THROUGH, SAVE THE FUND YEAR
     C*
     C     *IN80         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN80
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
     C                   MOVE      *BLANKS       XCNAME           30
     C     CDKEY         CHAIN     PCD01                              60
     C*
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
     C*
     C                   END
     C*
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   MOVEL     RPPNAM        XXPNAM           30
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
     C                   ADD       RPAMT         XXFNDT            9 2
     C                   ADD       1             XXFNCT            5 0
     C                   END
     C*
     C     BUGCKS        IFEQ      'Y'
     C     RPKEY         READE     PRP02                                  60
     C                   ELSE
     C     RPKEY         READE     PRP01                                  60
     C                   ENDIF

       // If printing both EFT and MICR, set up for EFT
       // after MICR is done


     C                   Enddo
       If QQtyp = 'G' and xxccnt > 0;
          Except CHKTT;
          xxccnt = 0;
          xxctot = 0;
       Endif;
       If xxFnCt > 0;
          Except FNDTT;
       Endif;
       xxFnCt = 0;
       xxFndT = 0;
       Page = 0;
       *in80 = *off;
     C**********************************************************************
     C*   2nd Pass: Process EFT
     C**********************************************************************
       If BUgcKs = 'Y';
         Chain rpkey prp02;
         *in60 = Not %Found;
       Else;
         Chain rpkey prp01;
         *in60 = Not %Found;
       Endif;
       *inoa = *on;
       Dow *in60 = *off;
       // CHECK FOR ACTIVE PAYMENTS
          If RPstat = 'Y' and RPeftC = 'E' and isEFT = *on;
             *in56 = *off;
             *in57 = *off;
             *in59 = *on;

             // PRINT CHECK # TOTALS FOR GROUP CHECKS
             If qqTyp = 'G' and *in80 = *on and RPchk# <> svChk#;
                Except ChkTT;
                If xxClt# = '114';
                   Except Head;
                Endif;
                xxCcnt = 0;
                xxCtot = 0;
             Endif;
             xxCcnt = xxCcnt + 1;
             xxCtot = xxCtot + RPamt;
             svChk# = RPchk#;

             // FIRST TIME THROUGH, SAVE THE FUND YEAR
             If *in80 = *off;
                *in80 = *on;
             Endif;

             // CHAIN TO THE CLAIM HEADER FILE
             Chain cmkey pcm01;
             *in60 = Not %Found;
             If *in60 = '0';

                // CHAIN TO THE DEMOGRAPHICS FILE
                xxSt = CMjrSt;
                xcName = *blanks;

                Chain cdkey pcd01;
                *in60 = Not %Found;
                If *in60 = '0';
                   qqFsN = *blanks;
                   qqMid = *Blanks;
                   qqTerm = *on;
                   qqFsn = CDfsN;
                   qqMid = CDmid;

                   Callp PrcFrLs(qqMid:qqfSn:qqterm);
                   qqFnam = qqMid;

                   qqLnam = *blanks;
                   qqLnam = CDlsN;
                   qqTerm = *on;

                   Callp prcName(qqLnam:qqFnam:qqTerm);
                   xcName = qqLnam;
                Endif;
             Endif;

             // Convert Dates
             srDtI = RPfrDt;
             srFfmt = '*CYMD';
             srTfmt = '*MDY';
             callp prcCvtDat(srDtI:srFfmt:srTfmt:srDtO:srErr);
             //*** Beg Add *** 07/27/22 ***************************************
             If srErr <> '1';
             //*** End Add *** 07/27/22 ***************************************
                xxFrDt = %Dec(%Subst(srDtO:1:6):6:0);
             //*** Beg Add *** 07/27/22 ***************************************
             Else;
                xxFrDt = *zeros;
             Endif;
             //*** End Add *** 07/27/22 ***************************************

             srDtI = RPtoDt;
             srFfmt = '*CYMD';
             srTfmt = '*MDY';
             callp prcCvtDat(srDtI:srFfmt:srTfmt:srDtO:srErr);
             //*** Beg Add *** 07/27/22 ***************************************
             If srErr <> '1';
             //*** End Add *** 07/27/22 ***************************************
                xxToDt = %Dec(%Subst(srDtO:1:6):6:0);
             //*** Beg Add *** 07/27/22 ***************************************
             Else;
                xxToDt = *zeros;
             Endif;
             //*** End Add *** 07/27/22 ***************************************

             xxPnam = RPpNam;
             // PRE-NOTIFICATION TRANSACTION
             *in58 = *off;
             If RPamt = 0;
                *in58 = *on;
             Endif;

             // PRINT THE LINE
             If *inoa = *on;
                Except Head;
                *inoa = *off;
             Endif;
             Except Det;

             // ADD TO THE TOTALS
             xxFndT = xxFndT + RPamt;
             xxFnCt = xxFnCt + 1;
          Endif;

          If BUgcKs = 'Y';
             Reade rpkey prp02;
            *in60 = %Eof;
          Else;
             Reade rpkey prp01;
             *in60 = %Eof;
          Endif;
       Enddo;

       If QQtyp = 'G' and xxccnt > 0;
          Except chkTT;
          xxccnt = 0;
          xxctot = 0;
       Endif;
       If xxFnCt > 0;
          Except fndTT;
       Endif;
       xxFnCt = 0;
       xxFndT = 0;
       Page = 0;
       *in80 = *off;
     C**********************************************************************
     C*   3rd Pass: Process E-Pay
     C**********************************************************************
       If BUgcKs = 'Y';
         Chain rpkey prp02;
         *in60 = Not %Found;
       Else;
         Chain rpkey prp01;
         *in60 = Not %Found;
       Endif;
       *inoa = *on;
       Dow *in60 = *off;
          // CHECK FOR ACTIVE PAYMENTS
          If RPstat = 'Y' and RPeftC = 'V' and isEPay = *on;
             *in56 = *off;
             *in57 = *on;
             *in59 = *off;

             // PRINT CHECK # TOTALS FOR GROUP CHECKS
             If qqTyp = 'G' and *in80 = *on and RPchk# <> svChk#;
                Except ChkTT;
                If xxClt# = '114';
                   Except Head;
                Endif;
                xxCcnt = 0;
                xxCtot = 0;
             Endif;
             xxCcnt = xxCcnt + 1;
             xxCtot = xxCtot + RPamt;
             svChk# = RPchk#;

             // FIRST TIME THROUGH, SAVE THE FUND YEAR
             If *in80 = *off;
                *in80 = *on;
             Endif;

             // CHAIN TO THE CLAIM HEADER FILE
             Chain cmkey pcm01;
             *in60 = Not %Found;
             If *in60 = '0';

                // CHAIN TO THE DEMOGRAPHICS FILE
                xxSt = CMjrSt;
                xcName = *blanks;

                Chain cdkey pcd01;
                *in60 = Not %Found;
                If *in60 = '0';
                   qqFsN = *blanks;
                   qqMid = *Blanks;
                   qqTerm = *on;
                   qqFsn = CDfsN;
                   qqMid = CDmid;

                   Callp PrcFrLs(qqMid:qqfSn:qqterm);
                   qqFnam = qqMid;

                   qqLnam = *blanks;
                   qqLnam = CDlsN;
                   qqTerm = *on;

                   Callp prcName(qqLnam:qqFnam:qqTerm);
                   xcName = qqLnam;
                Endif;
             Endif;

             // Convert Dates
             srDtI = RPfrDt;
             srFfmt = '*CYMD';
             srTfmt = '*MDY';
             callp prcCvtDat(srDtI:srFfmt:srTfmt:srDtO:srErr);
             //*** Beg Add *** 07/27/22 ***************************************
             If srErr <> '1';
             //*** End Add *** 07/27/22 ***************************************
             xxFrDt = %Dec(%Subst(srDtO:1:6):6:0);
             //*** Beg Add *** 07/27/22 ***************************************
             Else;
                xxFrDt = *zeros;
             Endif;
             //*** End Add *** 07/27/22 ***************************************

             srDtI = RPtoDt;
             srFfmt = '*CYMD';
             srTfmt = '*MDY';
             callp prcCvtDat(srDtI:srFfmt:srTfmt:srDtO:srErr);
             //*** Beg Add *** 07/27/22 ***************************************
             If srErr <> '1';
             //*** End Add *** 07/27/22 ***************************************
             xxToDt = %Dec(%Subst(srDtO:1:6):6:0);
             //*** Beg Add *** 07/27/22 ***************************************
             Else;
                xxToDt = *zeros;
             Endif;
             //*** End Add *** 07/27/22 ***************************************

             xxPnam = RPpNam;
             // PRE-NOTIFICATION TRANSACTION
             *in58 = *off;
             If RPamt = 0;
                *in58 = *on;
             Endif;

             // PRINT THE LINE
             If *inoa = *on;
                 Except Head;
                *inoa = *off;
             Endif;
             Except Det;

             // ADD TO THE TOTALS
             xxFndT = xxFndT + RPamt;
             xxFnCt = xxFnCt + 1;
          Endif;

          If BUgcKs = 'Y';
             Reade rpkey prp02;
            *in60 = %Eof;
          Else;
             Reade rpkey prp01;
             *in60 = %Eof;
          Endif;
       Enddo;
       If QQtyp = 'G' and xxccnt > 0;
          Except CHKTT;
          xxccnt = 0;
          xxctot = 0;
       Endif;
       If xxFnCt > *Zero;
          Except fndTT;
       Endif;
     C                   Z-ADD     0             XXFNCT
     C                   Z-ADD     0             XXFNDT
     C                   Z-ADD     0             PAGE
     C                   MOVE      '0'           *IN80
     C*
     C     CPKEY         READE     PCP01                                  60
     C                   END
     C*
     C     BUGCKS        IFEQ      'Y'
     C                   CLOSE     WARPLX
     C                   ELSE
     C                   CLOSE     WARPLU
     C                   ENDIF
      /Free
       if prtRouting = *on;
        prtRouting = *off;
        chain orkey por01;
         if %found and qqTyp <> 'G';    // Pull routing title for the
          xxTitl = ORtitl;              // regular Check Register
         endif;
         if %found;
          except route;
          xxTitl = centerFld(xxTitl : 60);
          *inoa = *on;
         endif;
       else;
          xxTitl = centerFld(xxTitl : 60);
        *inoa = *on;
       endif;
      /End-Free
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O*
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       XXDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O               59                          88 'EFT Transactions'
     O               56                          86 'Check Payments'
     O               57                          86 'EPay  Payments'
     O          E            HEAD           1
     O                                           24 'JURISDICTION'
     O**                                 74 'SERVICE'
     O                                           87 'SERVICE'
     O**                                100 'PAYMENT'
     O                                          112 'PAYMENT'
     O**                                109 'RESERVE'
     O                                          121 'RESERVE'
     O          E            HEAD           2
     O                                           03 'YR'
     O                                           11 'CASE'
     O                                           21 'STATE'
     O                                           33 'EMPLOYER'
     O                                           39 'DIV'
     O                                           49 'CLAIMANT'
     O                                           79 'FROM'
     O                                           89 'TO'
     O                                           98 'WEEKS'
     O                                          103 'DAYS'
     O                                          110 'CODE'
     O                                          119 'CODE'
     O                                          126 'NCCI'
     O                                          134 'PAYEE'
     O                                          143 'MOD'
     O                                          154 'PAYABLE TO:'
     O               56                         180 'CHECK #'
     O               57                         180 'EPAY Seq#'
     O               59                         180 'EFT Seq#'
     O                                          194 'A M O U N T'
     O          E            DET            1
     O                       RPFYR         Z     03
     O                       RPCASE        Z     11
     O**                       CMEMP#Z   21
     O**                       CMDIV Z   27
     O**                       XCNAME    59
     O**                       XXFRDTY   69
     O**                       XXTODTY   80
     O**                       RPWKS Z   85
     O**                       RPDAYSZ   90
     O**                       RPTRCD    97
     O**                       RPRECD   106
     O**                       RPNCCI   114
     O**                       RPFEINZ  128
     O**                       RPMOD Z  133
     O**                       XXPNAM   165
     O**              N59      RPCHK#Z  173
     O**                       RPAMT J  186
     O                       XXST                19
     O                       CMEMP#        Z     32
     O                       CMDIV         Z     38
     O                       XCNAME              70
     O                       XXFRDT        Y     79
     O                       XXTODT        Y     92
     O                       RPWKS         Z     98
     O                       RPDAYS        Z    102
     O                       RPTRCD             110
     O                       RPRECD             119
     O                       RPNCCI             125
     O                       RPFEIN        Z    138
     O                       RPMOD         Z    145
     O                       XXPNAM             171
     O                       RPCHK#        Z    181
     O                       RPAMT         J    195
     O               58 59                      198 '*PRE-NOTE*'
     O          E            FNDTT       1  2
     O                                          162 'FINAL COUNT:'
     O                       XXFNCT        Z    166
     O                                          181 'FINAL TOTAL:'
     O                       XXFNDT        J    195
     O          E            CHKTT       1  2
     O               59                         124 '  TOTALS FOR EFT #:'
     O               56                         124 'TOTALS FOR CHECK #:'
     O               57                         124 'TOTALS FOR E-Pay #:'
     O                       SVCHK#        Z    130
     O                                          147 'COUNT:'
     O                       XXCCNT        Z    153
     O                                          172 'AMOUNT:'
     O                       XXCTOT        J    186
