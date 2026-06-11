     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE WA023R - API to obtain Bank Acct # for given Group/Date
     H* ***************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program will access the Bank Info file for a given    *
     H*    Group and Date and return the Bank Acct # in effect at     *
     H*    that time.                                                 *
     H*    PARM Description:
     H*         Input:
     H*           @@CO#   3        Company
     H*           @@FND   3        Group #
     H*           @@APPL  2        Applic Code
     H*           @@SRDT  8        Search Date MMDDCCYY
     H*           @@CALL  1        (V)oucher/(C)heck
     H*         Output:
     H*           @@ACCT 20        WMBIP Bank Acct #
     H*           @@HDCY  1        WMBIP Hardcopy Y/N
     H*           @@CHKQ 10        WMBIP Check/Voucher OUTQ
     H*           @@FTYP 10        WMBIP Form Type
     H*           @@RTCD  1        Return Code
     H*           @@EFDT  7        WMBIP Eff. DT CYYMMDD
     H*                                                               *
     H*  CREATED: 09/16/97  C:30  Project:  WC659                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
    H*                                                               *
    H*  04/19/21 C:079 Support: 90/95537                             *
    H*                 Add Bank Code to API                          *
     H*                                                               *
     H*  07/20/20 C:079 Project: WC2749                               *
     H*                 Add E-Pay logic                               *
     H*                                                               *
     H*  07/30/15 C:136 Project: WC2337                               *
     H*                 Ensure that correct bank account is selected  *
     H*                 for MICR or EFT accounts                      *
     H*                                                               *
     H*  03/06/14 C:136 Support: RED3966                              *
     H*                 Correct logic searching with new EFT flag     *
     H*                                                               *
     H*  08/05/13 SUPPORT WA114 82969                                 *
     H*           Fix Invalid EFT Company Ids reported in record      *
     H*           types 5 and 8 of EFT extract                        *
     H*                                                               *
     H*  04/17/12 C:282 Project: WC2044                               *
     H*                 Added new parameter of EFT Flag to determine  *
     H*                 if bank is eligible for EFT or not.           *
     H*                                                               *
     H*  03/12/12 C:255 Project: 114WDE                               *
     H*                 Added new parameter in *Entry list. Program   *
     H*                 will return Debit Comp. Id and Credit Comp.   *
     H*                 Id in it.                                     *
     H*                                                               *
     H*  07/27/09 C:136 PROJECT: 114WUK                               *
     H*                 ADDED CODE TO PROCESS BANK ACCOUNTS WITH      *
     H*                 XML FILE TRANSACTION TYPES                    *
     H*                                                               *
     H*  01/11/02 C:15 Support: WC039/30729                           *
     H*                Added code to correct Bank Info record         *
     H*                retrieval.                                     *
     H*                                                               *
     H*  04/20/01 C:15 Project: WC1020                                *
     H*                Added code for multi bank acct processing.     *
     H*                                                               *
     H*  02/19/99 C:15 Support: WC051/16747                           *
     H*                Added a parm to send the filecopy outque to    *
     H*                the agent commission check override pgm.       *
     H*                                                               *
     H*  10/22/97 C:30 Project:  60ANN                                *
     H*           Add PARMs @@CALL, @@HDCY, @@CHKQ, @@FTYP, & @@RTCD. *
     H*           Based on @@CALL value (Voucher or Check), determine *
     H*           the "Current" Bank Info record & assign @@RTCD  to  *
     H*           blank or if no "current" Bank Info, assign @@RTCD   *
     H*           to '1' and return.                                  *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*
     H*****************************************************************
     f*****************************************************************
     f* File Specifications
     f*****************************************************************
      * Bank Information
     FWMBIL5    IF   E           K DISK
      *__________________________________________________________________
      * Bank Account Master
     FWSCKL     IF   E           K DISK
      *__________________________________________________________________
     f*** Beg Add ***** 07/20/20 **************************************
     Fwdb2l2    if   e           k Disk
     f*** End Add ***** 07/20/20 **************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d*****************************************************************
     d* Standalone fields
     d*****************************************************************
     d flgStatus       s              1
     d NOTFND          s              1
     d inEFTC          s              1
     d*** Beg Add ***** 07/20/20 **************************************
     d strEPay         s              1
     d*** End Add ***** 07/20/20 **************************************
     d*****************************************************************
     d* Key Lists
     d*****************************************************************
      *
      *  Bank Account Master key
      *
     C     CKKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    BIACCT
      *
      *  Bank Info key
      *
     C     BIKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKAPPL
     C                   KFLD                    KKCKTP
     C                   KFLD                    KKFND
     C                   KFLD                    KKEFDT
      *
     C     BIKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKAPPL
     C                   KFLD                    KKCKTP
     C                   KFLD                    KKFND
      *
     C     keyWMBILA     KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKAPPL
     C                   KFLD                    KKCKTP
     C                   KFLD                    KKFND
     C                   KFLD                    KKEFTC
     C                   KFLD                    KKEFDT
      *
     C     key1WMBILA    KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKAPPL
     C                   KFLD                    KKCKTP
     C                   KFLD                    KKFND
     C                   KFLD                    KKEFTC
      *  *LIKE Definitions
      *
     C     *LIKE         DEFINE    BICO#         KKCO#
     C     *LIKE         DEFINE    BIAPPL        KKAPPL
     C     *LIKE         DEFINE    BICKTP        KKCKTP
     C     *LIKE         DEFINE    BIFND         KKFND
     C     *LIKE         DEFINE    BIEFDT        KKEFDT
     C     *LIKE         DEFINE    BIEFTC        KKEFTC
      *
      *  Date Conversion
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *__________________________________________________________________
      *  Program Start
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3            Company
     C                   PARM                    @@FND             3            Group #
     C                   PARM                    @@APPL            2            Applic Code
     C                   PARM                    @@SRDT            8            MMDDCCYY
     C                   PARM                    @@ACCT           20            Bank Acct #
     C                   PARM                    @@CALL            1            (V)oucher/(C)heck
     C                   PARM                    @@HDCY            1            Hardcopy Y/N
     C                   PARM                    @@CHKQ           10            Check/Voucher OUTQ
     C                   PARM                    @@FTYP           10            Form Type
     C                   PARM                    @@RTCD            1            Return Code
     C                   PARM                    @@EFDT            7            WMBIP Eff. DT CYYMMD
     C                   PARM                    @@CPYQ           10
     C                   PARM                    @@CKTP           10
     C                   PARM                    @@COID           20
     C                   PARM                    @@EFTC            1
      *
     C                   MOVE      @@CO#         KKCO#                          Company
     C                   MOVE      @@FND         KKFND                          Group #
     C                   MOVE      @@APPL        KKAPPL                         Applic Code
      *
     C                   MOVE      @@SRDT        SRDTI                          MMDDCCYY
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT                         CYYMMDD
     C*
     C                   CLEAR                   KKCKTP
     C*
     C     Q1PRMS        IFGE      13                                           Nbr of Parms
     C                   MOVEL     @@CKTP        KKCKTP
     C                   ELSE
     C                   MOVE      *BLANKS       KKCKTP
     C                   ENDIF                                                  Q1PRMS>=13
      *

      /free
       If Q1prms > 14;
          inEftc = @@Eftc;
       Else;
          inEftc = *blank;
       Endif;
       notFnd = *on;
       //*** Beg Add *** 07/20/20 *****************************************
       Select;
       When inEftC <> 'V';
       //*** End Add *** 07/20/20 *****************************************
       // Check for EFT and E-Pay (inEftc = V)
          Setll biKy1 pbi01;
          Reade biKy2 pbi01;
          Dow Not %eof(wmbil5);
             If inEftc = *blank and BIeftc = 'E'
              or inEftc = 'E' and BIeftc = ' ';
                Reade biKy2 pbi01;
                Iter;
             Endif;
             Exsr CurAccRec;
             If notFnd = *off;
                Leave;
             Endif;
             Reade biKy2 pbi01;
          Enddo;
       //*** Beg Add *** 07/20/20 *****************************************
       When inEftC = 'V';
          Setll biKy1 pbi01;
          Reade biKy2 pbi01;
          Dow Not %eof(wmbil5);
       //*** Beg Chg *** 04/19/21 ******************************************
       //*** Chain (kkCo#:kkAppl:kkFnd:BIEfDt) pb201;
             Chain (kkCo#:kkAppl:kkFnd:BIckTp:BIEfDt) pb201;
       //*** End Chg *** 04/19/21 ******************************************
             If %Found(WDB2L2) and B2epVend <> *blanks;
                strEPay = 'V';
             Else;
                strEPay = *blank;
             Endif;
             Exsr CurAccRec;
             If notFnd = *off;
                Leave;
             Endif;
             Reade biKy2 pbi01;
          Enddo;
       Endsl;
       //*** End Add *** 07/20/20 *****************************************
       //*** Beg Chg *** 07/20/20 *****************************************
       //***If notFnd = *on;
       If notFnd = *on or (inEftC = 'V' and strEPay = ' ');
       //*** End Chg *** 07/20/20 *****************************************
          @@acct = *blank;
          @@rtcd = '1';
       Else;
          @@acct = BIacct;
          @@hdcy = BIhdcy;
          @@chkq = BIoutq;
          @@ftyp = BIftyp;
          @@efdt = %editc(BIefdt:'X');
          @@rtcd = *blank;
          If Q1prms > 11;
             @@cpyq = BIcpyq;
          Endif;
          If Q1prms > 13;
             @@coid = BIccid + BIdcid;
          Endif;
          If Q1prms > 14;
          //*** Beg Add *** 07/20/20 **************************************
             If inEftC = 'V';
                @@EftC = strEPay;
             Else;
          //*** End Add *** 07/20/20 **************************************
                @@eftc = BIeftc;
          //*** Beg Add *** 07/20/20 **************************************
             Endif;
          //*** End Add *** 07/20/20 **************************************
          Endif;
       Endif;
      /end-free
      *
     C                   RETURN

      /Free
       //************************************************************
       // CurAccRec - Access Bank Info file to obtain current Account record
       //************************************************************
       BegSr CurAccRec;
      /End-Free
      *  Access Bank Account Master
      *
     C     CKKY          CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   SELECT
      *
     C     @@CALL        WHENEQ    'C'
     C     CKVCHR        IFEQ      'N'
     C                   MOVE      *OFF          NOTFND                         Found/Not Found flag
     C                   LEAVESR                                                Exit Loop
     C                   ENDIF                                                  CKVCHR = 'C'
      *
     C     @@CALL        WHENEQ    'V'
     C     CKVCHR        IFEQ      'Y'
     C                   MOVE      *OFF          NOTFND                         Found/Not Found flag
     C                   LEAVESR                                                Exit Loop
     C                   ENDIF                                                  CKVCHR = 'V'
      *
     C     @@CALL        WHENEQ    'X'
     C     CKVCHR        IFEQ      'X'
     C                   MOVE      *OFF          NOTFND                         Found/Not Found flag
     C                   LEAVESR                                                Exit Loop
     C                   ENDIF                                                  CKVCHR = 'X'
      /free
                         Other;
                         NOTFND = *Off;
                         LeaveSr;
                         EndSl;
      /end-free
      *
      *
     C                   ENDIF                                                  *IN60
      /free
       Endsr;
      /end-free
