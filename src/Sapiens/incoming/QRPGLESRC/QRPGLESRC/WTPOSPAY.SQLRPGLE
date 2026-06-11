     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H BndDir('STBNDDIR')
     H/define CONVERT
      //
     H/TITLE WTPOSPAY - Positive Pay Download Processing
      //****************************************************************
      //                                                               *
      // Called From:                                                  *
      //      WTPOSPAYC - Positive Pay Download Override               *
      //                                                               *
      // Calls To:                                                     *
      //      None                                                     *
      //                                                               *
      // Created  09/24/21  C:079 Project: 111WBY                      *
      //****************************************************************
      // Revisions:                                                    *
      //                                                               *
      // mm/dd/yy C:cc   Project: xxxxxx                               *
      //                 xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx    *
      //****************************************************************
      //
      // WCMS Payment Detail File  (OPNQRYF from W7038C - CO#/ACCT/CHK#)
     FWDPALI    IF   E           K DISK
     FWDRPL3    IF   E           K DISK
      //
      //
      //
     Fwxpospay  o    e             Disk

     d xxTamt          s             15s 2
     d xxTcnt          s              7s 0
     d xx1st           s              1
     d xxAmt           s                   Like(PAamt)
     d intCo#          s                   Like(PAco#)
     d strName         s                   Like(RPpNam)
     D BANKID          C                   CONST('6301538124509')
      //
     D TESTXT          C                   CONST('TEST CLARENDON BANK-
     D                                      DOWNLOAD')
      //
     D LIVMSG          C                   CONST('CLARENDON BANK DOWN-
     D                                     LOAD')
      //
      //
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D/Copy sprDate
     D/Copy sprUsrPrms
     D/Copy sprString
     D
     d WtPostPay       PR                  Extpgm('WTPOSPAY')
     d                                8a
     d                                8a
     d                               20a

     d WtPosPay        PI
     d @@frDt                         8a
     d @@toDt                         8a
     d
      //
      //==================================================================
      // KLISTS
      //==================================================================
      //
      //  Payment Detail
      //

     C     PAKY1         KLIST
     C                   KFLD                    intCo#
      //
      //=================================================================
      // *LIKE DEFN
      //=================================================================
     C     *LIKE         DEFINE    PAACCT        XXACCT
     C     *LIKE         DEFINE    PATRDT        XXFRDT
     C     *LIKE         DEFINE    PATRDT        XXTODT
     C     *LIKE         DEFINE    PACHK#        SVCHK#
     C     *LIKE         DEFINE    PACHK#        XXCHK#
     C     *LIKE         DEFINE    PATYPE        xxType
     C     *LIKE         DEFINE    PPisDate      xxChDt
       //===============================================================
       // Initialize
       //===============================================================
       Monitor;
       xxFrDt = dtCyMd(%Dec(@@frDt:8:0));
       xxToDt = dtCyMd(%Dec(@@toDt:8:0));
       On-Error;
       Endmon;

       xxAmt  = 0;
       intCo# = GetUserInfo(Q1user);


          // Process WCMS payments
          Setll paky1 ppa01;
          Reade paky1 ppa01;
          Dow not %Eof(WDPALI);
             If PApGtp = 'G';
                Exsr grpCk1;
             Else;
                If PAtrDt >= xxFrdt And PAtrDt <= xxToDt;
                   xxAcct = PAacct;
                   xxChk# = PAchk#;
                   xxamt  = PAamt;
                   Monitor;
                   xxChDt = %Editc(dtMdYy(PAtrDt):'X');
                   On-Error;
                   Endmon;
                   xxChDt = %Subst(xxChDt:1:2) + '/' + %Subst(xxChDt:3:2) +
                    '/' + %Subst(xxChDt:5:4);

                   If PAtype = 'V' OR PAtype = 'S' Or PAtype = 'Z';
                      xxType = 'V';
                   Else;
                      xxType = 'I';
                   Endif;                                                         //PATYPE = M, P, Z
                   Chain (PAco#:PAfnd:PAfYr:PAcase:PAseq#) prp01;
                   If %Found(wdrpl3);
                      strName = RPpNam;
                   Else;
                      strName = *blanks;
                   Endif;
                   Exsr WrtWxPosPay;
                Endif;                                                           //TRDT
                Reade paky1 ppa01;
             Endif;                                                             //PGTP = G
          Enddo;


       // End the program
       *inlr = *on;
       Return;
      //
      //=============================================================
      //  GrpCk1 - Process Group Check
      //=============================================================
       Begsr GrpCk1;
        xxAmt = 0;
        svChk# = PAchk#;
        xx1st = 'Y';
        If PAtype = 'V' Or PATYPE = 'S' or PAtype = 'Z';
           xxType = 'V';
        Else;
           xxType = 'I';
        Endif;                                                               //PATYPE = M, P, Z

        Dow not %Eof(WDPALI) and PAchk# = SVchk#;
           If PAtrDt >= xxFrDt And PAtrDt <= xxToDt;
              If xx1st = 'Y';
                 xx1st = 'N';
                 xxAcct = PAacct;
                 xxChk# = PAchk#;
                 Monitor;
                 xxChDt = %Editc(dtMdYy(PAtrDt):'X');
                 On-Error;
                 Endmon;
                 xxChDt = %Subst(xxChDt:1:2) + '/' + %Subst(xxChDt:3:2) +
                  '/' + %Subst(xxChDt:5:4);
                 Chain (PAco#:PAfnd:PAfYr:PAcase:PAseq#) prp01;
                 If %Found(wdrpl3);
                    strName = RPpNam;
                 Else;
                    strName = *blanks;
                 Endif;
              Endif;
              xxAmt = xxAmt + PAamt;                                //XX1ST = 'Y'
           Endif;                                                             //TRDT
           Reade paky1 ppa01;
         Enddo;
         IF xxAmt <> *zero;
            Exsr WrtWxPosPay;
         Endif;
         //
       Endsr;                                                                 //GRPCK1
       //
       //=============================================================
       //  WrtWxPosPay - Write record to WXPOSPAY
       //=============================================================
       Begsr WrtWxPosPay;
        Clear ppp01;

        // Account number must be at least 10 characters and retain leading zeros
        PPacct = xxAcct;
        Monitor;
        PPckAmt = %EditC(xxAmt:'C');
        PPckNbr = %char(xxChk#);
        On-Error;
        Endmon;
        PPisDate = xxChDt;
        PPtype = xxType;
        PPname = Upper(strName);

        Write ppp01;
       Endsr;
