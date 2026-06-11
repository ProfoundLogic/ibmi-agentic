     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE WA023R1 - API to obtain Bank Acct Code
     H* ***************************************************************
     H*  Description:                                                 *
     H*    This pgm will be used to:                                  *
     H*      1) validate Bank Codes (@@MODE = 1), and                 *
     H*      2) determine which Bank Code to use based on various     *
     H*         pgm defined criteria (@@MODE = 2).                    *
     H*                                                               *
     H*    To validate a bank code, the pgm will simply use the       *
     H*    application type and bank code passed into the pgm and     *
     H*    compare them to the bank code array.  If no match, the     *
     H*    pgm will return an error indicator.  If a match is found,  *
     H*    the error indicator will not be turned on and the calling  *
     H*    pgm will continue.                                         *
     H*                                                               *
     H*    To determine a bank code, the pgm will use data passed     *
     H*    into the pgm in the DATA parm and compare it to pgm        *
     H*    pgm defined criteria (which will be hardcoded).            *
     H*                                                               *
     H*    The base version of this pgm will always pass a blank bank *
     H*    code to the calling pgm.  Any client versions will have to *
     H*    be specific for their needs.                               *
     H*                                                               *
     H*  CREATED: 04/20/01  C:15  Project:  WC1020                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 12/15/22 C:053 Project: 114WAC
     H*                 Added new decision date 12/31/2018
    H*                                                               *
     H* 09/14/22 C:291 Project: 129WXH                                *
     H*                Bypass overide to E_Pay if Override E-Pay with *
     H*                EFT flag = 'Y'.                                *
     H*                                                               *
    H* 04/19/21 C:079 Support: 90/95537                              *
    H*                Add Bank Code to API                           *
     H*                                                               *
     H* 07/20/20 C:079 Project: WC2749                                *
     H*                Added E-Pay logic with new parm (Q1PRMS)       *
     H*                                                               *
     H* 11/29/18 C:079 Project: 114WNL                                *
     H*                 Added new decision date 12/31/2014            *
     H*                 for group 650.                                *
     H*                                                               *
     H* 12/13/17 C:053 Project: 114WJN                                *
     H*                 Edit decision date for CLT114 for group 375   *
     H*                 bank code assignment                          *
     H*                                                               *
     H* 06/22/17 C:053 Project: 114WGG                                *
     H*                 Added new decision date 12/31/2013            *
     H*                 for group 650.                                *
     H*                                                               *
     H* 11/15/16 C:053 Project: 114WCR                                *
     H*                 Edit decision date for CLT114 for group 375   *
     H*                 bank code assignment                          *
     H*                                                               *
     H* 08/04/16 C:298 Project: 114WZS                                *
     H*                 Added new decision date 12/31/2012            *
     H*                 for group 650.                                *
     H*                                                               *
     H* 01/11/16 C:302 Project: 114WWS                                *
     H*                 Edit decision date for CLT114 for group 375   *
     H*                 bank code assignment                          *
     H*                                                               *
     H* 09/21/15 C:295 Project: 114WVJ                                *
     H*                 Added new decision date 12/31/2011            *
     H*                 for group 650.                                *
     H*                                                               *
     H* 01/05/15 C:212 Project: 114WQV                                *
     H*                 Edit decision date for CLT114 for group 375   *
     H*                 bank code assignment                          *
     H*                                                               *
     H* 09/17/14 C:282 Support: WA114 86370                           *
     H*                 Added new decision date for group 650.        *
     H*                                                               *
     H* 01/15/14 C:279 Project: 114WLB                                *
     H*                 Added group 375 processing for Claims & Bank  *
     H*                 Account Selection.                            *
     H*                                                               *
     H* 11/06/13 C:191 Support: 83836                                 *
     H*                 Project 114WJZ is cancelled.
     H*                 Added new date decision for group 650.
     H*                                                               *
     H* 10/14/13 C:282 PROJECT 114WJZ                                 *
     H*                 Added new procssing for group 640.            *
     H*                                                               *
     H* 08/09/13 C:53  PROJECT 147WDB                                 *
     H*                Bypass CSI processing for OPTACOMP groups.     *
     H*                                                               *
     H* 02/07/13 C:136 Support: WA102 81657                           *
     H*                Correct logic implemented by WC2044 for        *
     H*                'WP' application type.                         *
     H*                                                               *
     H* 10/30/12 C:062 Project: WC2044                                *
     H*                Uncomment change that affects Med/Exp and Leg  *
     H*                                                               *
     H* 09/27/12 C:53  PROJECT 114WHS                                 *
     H*                 Added new date decision for group 650.
     H*
     H* 08/10/12 C:53  Support: 80540
     H*                Correct system flag usage in WC2044
     H*                                                               *
     H* 04/17/12 C:282 Project: WC2044                                *
     H*                Added new parameter EFT Flag and removing      *
     H*                EFT type from compile time array.
     H*                                                               *
     H* 01/26/12 C:53  PROJECT 114WEK                                 *
     H*                 Added new procssing for group 650.            *
     H*                                                               *
     H* 05/26/11 C:136 SUPPORT: WA114 76862                           *
     H*                DEFAULT EFT FOR CSI                            *
     H*                                                               *
     H* 11/18/10 C:190 PROJECT 133WKD                                 *
     H*                Change API for determining Bank Code to        *
     H*                recognize indemnity payments  are              *
     H*                EFT if CLTID=133.                              *
     H*                                                               *
     H* 03/31/10 C:61  PROJECT 133WIK                                 *
     H*                When a comp payment is processed for clt133    *
     H*                assign 'INDEMNITY' bank code.
     H*
     H*  09/30/09 C:53  PROJECT: 114WVH
     H*                 Added new procssing for group 460.
     H*
     H*  06/29/09 C:191 Project: WC1806                               *
     H*                 Add Processing for new app type 'WP'          *
     H*                                                               *
     H*  01/22/09 C:53  Project: 114WAW                               *
     H*                 Added specific bank code processing for fund  *
     H*                 700.                                          *
     H*                                                               *
     H*  04/01/08 C:189 PROJECT: WC997                                *
     H*                 IMPLEMENT EFT CONTROL FLAG EDITS.             *
     H*                                                               *
     H*  09/20/07 CL136 PROJECT: WC997                                *
     H*                 IMPLEMENT EFT - AGENT COMMISSION PAYMENTS     *
     H*                                                               *
     H*  08/09/07 C:191 Project: WC1453                               *
     H*                 EFT Payment to claimants - Expand DATA field  *
     H*                 Process FEIN/MOD/DSQ#                         *
     H*                                                               *
     H*  08/24/07 C:15  Project: 114WMF                               *
     H*                 Added CSI modified code.                      *
     H*                                                               *
     H*  07/18/07 C:136 PROJECT: WC1491 (IND CASH DIST)               *
     H*                -ADD 'IC' AS A VALID APPLICATION TYPE          *
     H*                 IF IND CASH MODULE IS INSTALLED               *
     H*                -QHANGE 'RP' SO THAT IT IS CHECKED ONLY        *
     H*                 IF RETURN FUND MODULE IS INSTALLED            *
     H*                                                               *
     H*  01/20/04 C:136 PROJECT: WC1033                               *
     H*                 Add return premium code.                      *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*
     H*****************************************************************
     F*
     FSWCPF     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCRL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     RENAME(PPA01:PPA02)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     FWMEFL2    IF   E           K DISK
     F                                     RENAME(PEF01:PEF02)
     FWMEFL3    IF   E           K DISK
     F                                     RENAME(PEF01:PEF03)
     FWMFNL     IF   E           K DISK
      *** Beg Add ****** 09/14/22 ********************************
     FWSCKL     IF   E           K DISK
      *** End Add ****** 09/14/22 ********************************
     D*
     D WCBC            S             10    DIM(3) CTDATA PERRCD(1)              othr WC Bnk Cd
     D W400            S             10    DIM(4) CTDATA PERRCD(1)              Bank Codes 400
     D W410            S             10    DIM(4) CTDATA PERRCD(1)              Bank Codes 410
     D W420            S             10    DIM(2) CTDATA PERRCD(1)              Bank Codes 420
     D W430            S             10    DIM(4) CTDATA PERRCD(1)              Bank Codes 430
     D W440            S             10    DIM(9) CTDATA PERRCD(1)              Bank Codes 440
     D AGBC            S             10    DIM(1) CTDATA PERRCD(1)              AG Bank Codes
     D RPBC            S             10    DIM(1) CTDATA PERRCD(1)              RP Bank Codes
     D IPBC            S             10    DIM(1) CTDATA PERRCD(1)              RP Bank Codes
     D W700            S             10    DIM(2) CTDATA PERRCD(1)              Bank Codes 700
     D WPBC            S             10    DIM(1) CTDATA PERRCD(1)              Bank Codes Premium A
     D W460            S             10    DIM(3) CTDATA PERRCD(1)              Bank Codes 460
     D W650            S             10    DIM(4) CTDATA PERRCD(1)              Bank Codes 650
     d w375            s             10    dim(4) ctData perRcd(1)              Bank Codes 640
     d flgStatus       s              1
     d NewYr           s              1
     d Execute         s              1
     d inCo#           s              3
     d inFnd           s              3
     d inAppl          s              2
     d inSrDt          s              8
     d outAcct         s             20
     d inCall          s              1
     d outHdCy         s              1
     d outChkQ         s             10
     d outFtyp         s             10
     d outEfDt         s              7
     d outCpyQ         s             10
     d outCkTp         s             10
     d outEfTc         s              1
     d outCoid         s             20
     d outRtCd         s              1
     d inEfDt          s              7  0
     d intCo#          s              3  0
     d intFnd          s              3  0
     d*--------------------------------------------------------
     d* Procedure Prototypes
     d*--------------------------------------------------------
     d cvtDate         pr                  ExtPgm('SRCVTDAT')
     d  inDate                        8  0
     d  inFfmt                        8
     d  inTfmt                        8
     d  outDate                      10
     d  outErr                        1
     d  inDate         s              8  0
     d  inFfmt         s              8
     d  inTfmt         s              8
     d  outDate        s             10
     d  outErr         s              1
     d  inEftC         s              1
     d  inPtyp         s              1
      *** End Add ***** 07/20/20 ******************************
      *** Beg Add ***** 04/19/21 ******************************
     d  inCkTp         s             10
      *** End Add ***** 04/19/21 ******************************
      *** Beg Add ***** 09/14/22 *******************************
     d  @@RecCmp       s              1
      *** End Add ***** 09/14/22 *******************************
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*  DATA array will hold selection criteria.
     D*
     D                 DS
     D  DATA                   1     75
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D  QXSEQ#                30     34  0
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D                 DS
     D  CMCTG2                 1     10
     D  XXCTG2                 1      2
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d*** Beg Add *** 07/20/20 ****************************************
     d/Copy wprBank
     d/Copy wprEPay
     d/Copy sprModule
     d*** End Add *** 07/20/20 ****************************************
     C*================================================================
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    QXSEQ#
     C*
     C     EFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    QXDSQ#
     C*
     C     EFKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    QXFEIN
     C                   KFLD                    QXMOD
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*
     C     *LIKE         DEFINE    RTCO#         KKCO#
     C     *LIKE         DEFINE    RTKEY         KKKEY
     C     *LIKE         DEFINE    RTTYPE        XXTYPE
     C     *LIKE         DEFINE    PCTRTP        XXTRTP
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1            Mode
     C                   PARM                    @@CO#             3            Company
     C                   PARM                    @@FND             3            Group #
     C                   PARM                    @@FYR             3            Year
     C                   PARM                    @@CASE            7            Claim #
     C                   PARM                    @@APPL            2            Applic Code
     C     DATA          PARM                    @@DATA           75            Selection criteria
     C                   PARM                    @@CKTP           10            Bank Acct Code
     C                   PARM                    @@RTCD            1            Return Code
     C                   PARM                    @@EFTC            1            EFT Flag
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   TIME                    DSDTTM
      //*** Beg Add ***** 09/14/22 *******************************
      /Free
       If @@Rtcd = 'R';
          @@RecCmp = @@Rtcd;
       Endif;
      /End-Free
      //*** End Add ***** 09/14/22 *******************************
     C*
     C                   MOVE      *BLANK        @@RTCD
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
      /Free
       if q1prms > 9 ;
          flgStatus = '1';
       endif;
      /End-Free
     C*
     C     QXEOB#        IFNE      *BLANKS
     C                   MOVE      QXEOB#        XXEOB#            9 0
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXEOB#
     C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*
     C     FNKY          CHAIN(N)  PFN01                              60
     C*
     C*__________________________________________________________________
     C*  If in edit mode (@@MODE = 1), validate the Bank Code using the
     C*    array.  If there is no match based on 'Application code' and
     C*    'Bank Code', set the return code to reflect an error.
     C*
     C     @@MODE        IFEQ      '1'
     C*
     C                   READ      SWCPF                                  60
     C     @@APPL        IFEQ      'WC'
     C     CLTID         IFEQ      '114'
     C     @@CKTP        ANDNE     'EFT'
     C     FNgrpAsc      ANDNE     'OPTACOMP'
     C                   EXSR      EDTWC
     C                   ELSE
     C     @@CKTP        LOOKUP    WCBC                                   21
     C                   ENDIF
     C                   ENDIF
     C*
     C     @@APPL        IFEQ      'AG'
     C     @@CKTP        LOOKUP    AGBC                                   21
     C                   ENDIF
     C*
     C     @@APPL        IFEQ      'RP'
     C     WCFL25        ANDEQ     'Y'
     C     @@CKTP        LOOKUP    RPBC                                   21
     C                   ENDIF
     C*
     C     @@APPL        IFEQ      'IC'
     C     WCFL36        ANDEQ     'Y'
     C     @@CKTP        LOOKUP    IPBC                                   21
     C                   ENDIF

     C     @@APPL        IFEQ      'WP'
     C     @@CKTP        LOOKUP    WPBC                                   21
     C                   ENDIF                                                  @@APPL = 'WP'
     C*
     C     *IN21         IFEQ      *OFF
     C                   MOVE      '1'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  If in "retrieval" mode (@@MODE = 2), use the parms to determine
     C*  which bank code to use.  (This section should be unique for
     C*  each client, if needed.)
     C*
     C     @@MODE        IFEQ      '2'
     C*
     C                   MOVE      *BLANKS       @@CKTP
     C* Load 'EFT' to @@CKTP, if (a) Payment Value 1 code EFT flag is 'Y'
     C*
     C                   MOVEL(P)  QXPV1         KKKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      *OFF
     C     B2EFT         ANDEQ     'Y'
     C******
     C     QXFEIN        IFEQ      0
     C     EFKY          CHAIN     PEF03                              60
     C                   ELSE
     C     EFKY1         CHAIN     PEF02                              60
     C                   ENDIF                                                  KKFEIN
     C******
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
     C                   MOVEL     SRDTO         RUNDAT            7 0
     C*****
     C     *IN60         DOWEQ     *OFF
     C     XXEFDT        IFGE      EFEFDT
     C     XXEFDT        ANDLT     EFEXDT
     C     XXEFDT        ORGE      EFEFDT
     C     EFEXDT        ANDEQ     0
      /free
       If flgStatus = '1';
          @@EFTC = 'E';
       Else;
          @@CKTP = 'EFT';
       EndIf;
      /end-free
     C                   LEAVE
     C                   ENDIF                                                  XXEFDT
     C*****
     C     QXFEIN        IFEQ      0
     C     EFKY          READE     PEF03                                  60
     C                   ELSE
     C     EFKY1         READE     PEF02                                  60
     C                   ENDIF                                                  KKFEIN
     C                   ENDDO                                                  *IN60=*OFF
     C                   ENDIF                                                             *IN60
     C*
     C*
      *
     C                   SELECT
      *
     C     CLTID         WHENEQ    '114'
     C     @@CKTP        ANDNE     'EFT'
     C     FNgrpAsc      ANDNE     'OPTACOMP'
     C                   EXSR      RTVWC
      *
     C     CLTID         WHENEQ    '133'
     C     @@CKTP        ANDNE     'EFT'
     C                   EXSR      SR133
      *
     C                   ENDSL
      *
       //*** Beg Add *** 07/20/20 ***********************************
       // Determine if the payment qualifies for E-pay.  This will
       // Override EFT = E.
      /Free
       If isModEnabled('E-Pay') and Q1prms > 9;
          // Load parms to get the Bank Account number
          inCo#  = @@Co#;
          inFnd  = @@Fnd;
          inAppl = @@Appl;
          inDate = DSdate;
          inFfmt = *blanks;
          inTfmt = '*MDYY';
          Callp cvtDate(Indate:inFfmt:inTfmt:outDate:outErr);
          inSrDt = outDate;
          inCall = ' ';
          inEftC = 'V';
       //*** Beg Add *** 04/19/21 ******************************************
          inCkTp = @@CkTp;
       //*** End Add *** 04/19/21 ******************************************

          // Get the Bank Account Number
       //*** Beg Chg *** 04/19/21 ******************************************
       //***outAcct = getBankAcc(inCo#:inFnd:inAppl:inSrDt:inCall:outHdCy:
       //***              outChkQ:outFtyp:outEfDt:outCpyQ:outCkTp:inEfTc:
       //***              outCoid:outRtCd);
          outAcct = getBankAcc(inCo#:inFnd:inAppl:inSrDt:inCall:outHdCy:
                     outChkQ:outFtyp:outEfDt:outCpyQ:inCkTp:inEfTc:
                     outCoid:outRtCd);
       //*** End Chg *** 04/19/21 ******************************************
          inTfmt = '*CYMD';
          Callp cvtDate(Indate:inFfmt:inTfmt:outDate:outErr);
          inEfDt = %Dec(%Subst(outDate:1:7):7:0);
          intCo# = %Dec(inCo#:3:0);
          intFnd = %Dec(inFnd:3:0);

          // Retrieve the reserve type.
          kkKey = %Trim(qxReCd);
          Chain tbky prt01;
          If %Found(wtabl);
             inPtyp = RTtype;
          Else;
             inPtyp = *blanks;
          Endif;

          // Get the E-Pay Vendor for EOB's
       //*** Beg Chg *** 04/19/21 ******************************************
       //*** If getWcEPay(intCo#:inAppl:intFnd:outAcct:inEfDt:inPtyp) <>  *blanks;
          //*** Beg Add ***** 09/14/22 *************************
          Chain (kkCo#:outAcct) pck01;
          If CKOVREPAY = 'Y' and @@RecCmp = 'R'
           and (@@EFTC = 'E' or @@CKTP = 'EFT');
             @@EFTC = 'E';
          Else;
          //*** End Add ***** 09/14/22 *************************
             If getWcEPay(intCo#:inAppl:intFnd:outAcct:inEfDt:inPtyp:inCkTp)
                 <> *blanks;
             //*** End Chg *** 04/19/21 ******************************************
                @@EftC = 'V';
             Endif;
          //*** Beg Add ***** 09/14/22 *************************
          Endif;
          //*** End Add ***** 09/14/22 *************************
       Endif;
      /End-free
      //*** End Add *** 07/20/20 ***********************************
     C*
     C                   ENDIF
     C*__________________________________________________________________
     C*  Program End
     C*
     C                   SETON                                        LR
      * ZZZ
      *
      *==================================================================
      * SR133  - Handle clt133 bank code assignment
      *==================================================================
     C     SR133         BEGSR
      *
      *  Retrieve the transaction type.
      *
     C                   MOVEL(P)  QXTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C  N99              MOVEL     PCTRTP        XXTRTP
     C   99              MOVE      *BLANK        XXTRTP
      *
      *  Retrieve the reserve type.
      *
     C                   MOVEL(P)  QXRECD        KKKEY
     C     TBKY          CHAIN     PRT01                              99
     C  N99              MOVEL     RTTYPE        XXTYPE
     C   99              MOVE      *BLANK        XXTYPE
      *
     C                   SELECT
      *
     C     XXTYPE        WHENEQ    'C'
     C                   MOVEL(P)  WCBC(2)       @@CKTP
     C                   OTHER
     C                   MOVEL(P)  *BLANKS       @@CKTP
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  SR133
      *
     C*==================================================================
     C* EDTWC  - This subroutine edits the WC bank code for clt 114.
     C*==================================================================
     C     EDTWC         BEGSR
     C*
     C                   SELECT
     C     @@FND         WHENEQ    '400'
     C     @@FND         OREQ      '405'
     C     @@CKTP        LOOKUP    W400                                   21
     C*
     C     @@FND         WHENEQ    '410'
     C     @@CKTP        LOOKUP    W410                                   21
     C*
     C     @@FND         WHENEQ    '420'
     C     @@CKTP        LOOKUP    W420                                   21
     C*
     C     @@FND         WHENEQ    '430'
     C     @@CKTP        LOOKUP    W430                                   21
     C*
     C     @@FND         WHENEQ    '440'
     C     @@CKTP        LOOKUP    W440                                   21
     C*
     C*
     C     @@FND         WHENEQ    '460'
     C     @@CKTP        LOOKUP    W460                                   21
     C*
     C*
     C     @@FND         WHENEQ    '650'
     C     @@CKTP        LOOKUP    W650                                   21
     C*
      /free
       when @@fnd  = '375';
          If %lookup(@@cktp:w375) > 0;
             *in21 = *on;
          Endif;
      /end-free
     C*
     C*
     C*          @@FND     WHEQ '700'
     C   99
     CANN99@@CKTP        LOOKUP    W700                                   21
     C*
     C*
     C                   OTHER
     C     @@CKTP        LOOKUP    WCBC                                   21
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*==================================================================
     C* RTVWC - This subroutine retrieves the WC bank code for clt 114.
     C*==================================================================
     C     RTVWC         BEGSR
     C*
     C*  Retrieve the transaction type.
     C*
     C                   MOVEL(P)  QXTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C  N99              MOVEL     PCTRTP        XXTRTP
     C   99              MOVE      *BLANK        XXTRTP
     C*
     C*  Retrieve the reserve type.
     C*
     C                   MOVEL(P)  QXRECD        KKKEY
     C     TBKY          CHAIN     PRT01                              99
     C  N99              MOVEL     RTTYPE        XXTYPE
     C   99              MOVE      *BLANK        XXTYPE
     C*
     C                   SELECT
     C*
     C*----------
     C     @@FND         WHENEQ    '400'
     C     @@FND         OREQ      '405'
     C*
     C                   SELECT
     C*
     C     XXTYPE        WHENEQ    'C'
     C                   MOVEL(P)  W400(1)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'E'
     C     XXTYPE        OREQ      'L'
     C                   MOVEL(P)  W400(2)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'M'
     C     QXFRDT        IFLE      0931231
     C                   MOVEL(P)  W400(3)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W400(4)       @@CKTP
     C                   ENDIF
     C                   ENDSL
     C*
     C*----------
     C     @@FND         WHENEQ    '410'
     C*
     C                   SELECT
     C*
     C     XXTYPE        WHENEQ    'C'
     C                   MOVEL(P)  W410(1)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'E'
     C     XXTYPE        OREQ      'L'
     C                   MOVEL(P)  W410(2)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'M'
     C                   MOVEL(P)  W410(4)       @@CKTP
     C                   ENDSL
     C*
     C*----------
     C     @@FND         WHENEQ    '420'
     C*
     C                   SELECT
     C*
     C     XXTYPE        WHENEQ    'C'
     C     XXTRTP        ANDNE     'M'
     C                   MOVEL(P)  W420(1)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'E'
     C     XXTYPE        OREQ      'L'
     C     XXTYPE        OREQ      'M'
     C     XXTYPE        OREQ      'C'
     C     XXTRTP        ANDEQ     'M'
     C                   MOVEL(P)  W420(2)       @@CKTP
     C                   ENDSL
     C*
     C*----------
     C     @@FND         WHENEQ    '430'
     C*
     C                   SELECT
     C*
     C     XXTYPE        WHENEQ    'C'
     C                   MOVEL(P)  W430(1)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'E'
     C     XXTYPE        OREQ      'L'
     C                   MOVEL(P)  W430(2)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'M'
     C     QXFRDT        IFLE      0990930
     C                   MOVEL(P)  W430(3)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W430(4)       @@CKTP
     C                   ENDIF
     C                   ENDSL
     C*
     C*----------
     C     @@FND         WHENEQ    '440'
     C*
     C                   SELECT
     C*
     C     XXTYPE        WHENEQ    'M'
     C                   MOVEL(P)  W440(1)       @@CKTP
     C*
     C     XXEOB#        IFNE      *ZEROS
     C*
     C                   CALL      'FA033R'
     C                   PARM      KKCO#         ##CO#             3 0          Company #
     C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C                   PARM      KKFYR         ##FYR             3 0          Policy Year
     C                   PARM      KKCASE        ##CASE            7 0          Claim #
     C                   PARM      XXEOB#        ##EOB#            9 0          EOMB #
     C                   PARM      *ZEROS        ##RCN             3 0          RECON #
     C                   PARM      *ZEROS        ##FEIN            9 0          FEIN
     C                   PARM      *ZEROS        ##MOD             3 0          MOD
     C                   PARM      *ZEROS        ##CHK#            9 0          CHECK NUMBER
     C                   PARM      *ZEROS        ##OTHR            9 2          EOMB billing amt
     C                   PARM      *BLANKS       ##AST             2            ADJUDICATION ST
     C                   PARM      *ZEROS        ##BINV            9 0          BRS INV#
     C                   PARM      *ZEROS        ##NINV            9 0          NWS INV#
     C                   PARM      *ZEROS        #@SUB#            5 0          SUBSCRIBER
     C                   PARM      *ZEROS        #@FND             3 0          GROUP
     C                   PARM      *ZEROS        ##RRN             9 0          RRN OF FAEOP
     C                   PARM      *BLANKS       ##RTCD            1            Return Code
     C                   PARM      *BLANKS       ##ACCT           30            Pat Account#
     C                   PARM      *BLANKS       ##BTYP            1            Bill type
     C*
     C     ##BTYP        IFEQ      'X'
     C                   MOVEL(P)  W440(9)       @@CKTP
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXTYPE        WHENEQ    'E'
     C     XXTYPE        OREQ      'L'
     C                   MOVEL(P)  W440(2)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'C'
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     XXCTG2        IFEQ      '02'
     C     CMEMP#        ANDNE     14
     C     XXCTG2        OREQ      '03'
     C     CMEMP#        ANDNE     14
     C*
     C* Transfers
     C     QXPV1         IFEQ      'TT'
     C     QXPV2         ANDEQ     '01'
     C     QXPV1         OREQ      'TT'
     C     QXPV2         ANDEQ     '97'
     C     QXPV1         OREQ      'TP'
     C     QXPV2         ANDEQ     '01'
     C     QXPV1         OREQ      'TP'
     C     QXPV2         ANDEQ     '97'
     C     QXPV1         OREQ      '32'
     C     QXPV2         ANDEQ     '01'
     C     QXPV1         OREQ      '32'
     C     QXPV2         ANDEQ     '97'
     C     QXPV1         OREQ      'PT'
     C     QXPV2         ANDEQ     '01'
     C     QXPV1         OREQ      'PT'
     C     QXPV2         ANDEQ     '97'
     C*
     C     CMCMPS        IFEQ      '093'
     C                   MOVEL(P)  W440(7)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W440(8)       @@CKTP
     C                   ENDIF
     C*
     C                   ELSE
     C                   GOTO      TRSRY
     C                   ENDIF
     C*
     C                   ELSE                                                   CMCTG2 <> 02 or 03
     C     TRSRY         TAG
     C*
     C* Advances
     C     CRKY          CHAIN     PCR01                              99
     C     *IN99         IFEQ      *OFF
     C     CRLPDT        ANDLT     CRBGDT
     C     QXPV2         ANDEQ     '01'
     C     *IN99         OREQ      *OFF
     C     CRLPDT        ANDLT     CRBGDT
     C     QXPV2         ANDEQ     '97'
     C*
     C     CMCMPS        IFEQ      '093'
     C                   MOVEL(P)  W440(3)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W440(4)       @@CKTP
     C                   ENDIF
     C*
     C* Treasuries
     C                   ELSE
     C*
     C     CMCMPS        IFEQ      '093'
     C                   MOVEL(P)  W440(5)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W440(6)       @@CKTP
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C*----------
     C     @@FND         WHENEQ    '460'
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   SELECT
     C*
     C     XXTYPE        WHENEQ    'C'
     C*
     C     CMCMPS        IFEQ      '100'
     C                   MOVEL(P)  W460(1)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W460(2)       @@CKTP
     C                   ENDIF
     C*
     C                   OTHER
     C                   MOVEL(P)  W460(3)       @@CKTP
     C                   ENDSL
     C*----------
     C     @@FND         WHENEQ    '650'
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*** Beg Chg *** 12/15/22 *************************************
     C*****CMACDT********IFLE      1141231
     C     CMACDT        IFLE      1181231
     C*** End Chg *** 12/15/22 *************************************
     C*
     C     XXTYPE        IFEQ      'C'
     C                   MOVEL(P)  W650(1)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W650(2)       @@CKTP
     C                   ENDIF
     C*
     C                   ELSE
     C     XXTYPE        IFEQ      'C'
     C                   MOVEL(P)  W650(3)       @@CKTP
     C                   ELSE
     C                   MOVEL(P)  W650(4)       @@CKTP
     C                   ENDIF
     C                   ENDIF
      /FREE
      /END-FREE
      /free
       When @@fnd = '375';
          Chain cmKy pcm01;
          If %found (Wmcml1);
             If RunDat >= 1180101;
                NewYr = 'Y';
             Else;
                NewYr = 'N';
             Endif;
             Execute = 'N';
             If NewYr = 'Y' and CMacdt <= 1140701;
                Execute = 'Y';
             Else;
                If CMacdt <= 1130701;
                   Execute = 'Y';
                Endif;
             Endif;

             If Execute = 'Y';
                If xxType = 'M' or xxType = 'E' or xxType = 'L';
                   @@cktp = w375(1);
                Endif;

                If xxType = 'C';
                   @@cktp = w375(2);
                EndIf;
             Else;
                If xxType = 'M' or xxType = 'E' or xxType = 'L';
                   @@cktp = w375(3);
                Endif;

                If xxType = 'C';
                   @@cktp = w375(4);
                EndIf;
             Endif;
          Endif;
      /end-free
     C*----------
     C                   OTHER
     C*
     C                   SELECT
     C*
     C     XXTYPE        WHENEQ    'C'
     C                   MOVEL(P)  WCBC(2)       @@CKTP
     C*
     C     XXTYPE        WHENEQ    'M'
     C     XXTYPE        OREQ      'E'
     C     XXTYPE        OREQ      'L'
     C                   MOVEL(P)  WCBC(3)       @@CKTP
     C*
     C                   ENDSL
     C*--------
     C                   ENDSL
     C*
     C*
     C                   ENDSR
     C*==================================================================
**  Work Comp Bank Codes (WCBC)

INDEMNITY
MEDEXP
**  Work Comp Bank Codes - Group 400  (W400)
COPIND
COPEXP
COPPRE
COPPGM
**  Work Comp Bank Codes - Group 410  (W410)
PGWIND
PGWEXP
PGWPRE
PGWPGM
**  Work Comp Bank Codes - Group 420  (W420)
PSDIND
PSDPMT
**  Work Comp Bank Codes - Group 430  (W430)
SEPTAIND
SEPTAEXP
SEPTAPRE
SEPTAPGM
**  Work Comp Bank Codes - Group 440  (W440)
CPAMED
CPAEXP
CPAADV093
CPAADV094
CPATRSRY93
CPATRSRY94
CPATRSFR93
CPATRSFR94
CPARX
**  Agent Commission Bank Codes  (AGBC)

**  Return Premium Bank Codes    (RPBC)

**  Independent Cash Distribution Codes (IPBC)

**  Work Comp Bank Codes - Group 700  (W700)
NCCIND
MEDEXP
**  Premium ACH Bank Codes  (WPBC)

**  Work Comp Bank Codes - Group 460 (W460)
SF100
SF101
MEDEXP
**  Work Comp Bank Codes - Group 650 (W650)
651IND
651ME
INDEMNITY
MEDEXP
**  Work Comp Bank Codes - Group 375 (W375)
376ME
376IND
MEDEXP
INDEMNITY
