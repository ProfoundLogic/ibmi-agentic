     hcopyright('(c) 2023 StoneRiver, Inc. All rights reserved.')
     h/TITLE WPIPMTPOP - Payment Pop-Up Processing
     h/DEFINE   PROFOUNDUI
     h BNDDIR('WTBNDDIR')
     h nomain
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:
     h*
     h*  This program is used to determine the Type of Check Processing
     h*  based on entries via the Type of Check Processing display
     h*  which executes upon completion of the check requests.
     h*
     h*  Called from: Pround UI
     h*
    h*  Created   : 06/08/23    C:344  Project: WC2430
     h**********************************************************************
     h* Revisions:
     h*
     h* 02/17/25 C:342 Support: 118400 Cove Risk
     h*                - Corrected security process for Correcting adjustments,
     h*                Recovery, Multiple Adjustments as it was using Void,Stop
     h*                Reversal security type and level.
     h*
     h* 09/08/23 C:344 Support: 114292
     h*                - Change the outgoing mode parm based on the
     h*                Adjustment code. Remove the Offsetting Adj option.
     h*
     h* 09/08/23 C:344 Support: 113706
     h*                - Revised Javascript in display file so that
     h*                Reserve Code is used to produce the specific Payment
     h*                Type Code in dropdown list.
     h*
     h* 08/23/23 C:344 Support: 113634
     h*                Included Reversal, Recovery, Offsetting Adjustment
     h*                and Multiple Adjustment options for user authority.
     h*
     h* 08/22/23 C:344 Support: 113627
     h*                - Revised PayDetailPrc proc to get Reserve Code
     h*                from the Claims checks payment file.
     h*
     h**********************************************************************
     fwpiPmtPop cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('WPIPMTPOP')
     f                                     UsrOpn
      * Request payment transaction file
     fwdpap     uf   e             Disk    Rename(ppa01:ppa05)
     fwdpal     uf a e           k Disk    Infds(DBFDS2)
     f                                     Rename(ppa01:ppa02)
     fwdrpl     if a e           k Disk    Rename(prp01:prp02)
     fwdrpl3    uf   e           k Disk    Rename(prp01:prp03)
     fwarpl     if   e           k Disk    Rename(prp01:prp00)
     fwmcxl     if   e           k Disk    Rename(pcx01:pcx00)
     fwmupl     if   e           k Disk    Rename(pup01:pup00)
     fwmcmp     if   e             Disk    Rename(pcm01:pcm02)
     fwmcml1    uf   e           k Disk    Infds(DBFDSC)
     f                                     Rename(pcm01:pcm00)
     fwtabl     if   e           k Disk    Rename(ppc01:ppc00)
     f                                     Rename(plr01:plr00)
     fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
     fwdf2l     if   e           k Disk    Rename(pf201:pf200)
     fwmpdl     if   e           k Disk    Rename(ppd01:ppd00)
     fwmcdl     if   e           k Disk    Rename(pcd01:pcd00)
     fwdell     uf   e           k Disk    Rename(pel01:pel00)
     fstctl1    if   e           k Disk    Rename(pct01:pct00)
      /copy wPrPmtPop
      /copy wPrGenPop
      /copy wPrIdc
      /copy wPrBank
      /copy sPrModule
      /copy sPrDate
      /copy sPrSecure
      /copy sPrSql

     d ClmPayPrc       pr                  extpgm('WTCLMPMT')
     d  outMode                       1
     d  outClmRrn                     9  0
     d  outPayRrn                     9  0
     d  outTaxId                      9  0
     d  outMod                        3  0
     d  outFromDt                     7  0
     d  outToDt                       7  0
     d  outEOB#                       9
     d  outRecon#                     3  0
     d  outSysType                    1
     d  outBillType                   1
     d  outPayment                    9  2
     d  outTransCode                  2
     d  outPayStatCod                 2
     d  outPayValue1                  2
     d  outPayValue2                  2
     d  outRecvDt                     7  0
     d  inReturnCode                  1

     d CheckAuth       pr                  extpgm('SRCCSC')
     d inUTYP                         1    const
     d inULVL                         3    const
     d inCTYP                         1    const
     d inCLVL                         3    const
     d inEXEC                         1    const
     d outRes                         1
     d inCTP2                         1    const
     d inCTP3                         1    const
     d inCTP4                         1    const
     d inCTP5                         1    const
     d inCTP6                         1    const

     d PayRelCC        pr                  extpgm('SRPRCC')
     d PAco#                               like(PAco#)
     d PAfnd                               like(PAfnd)
     d PAfyr                               like(PAfyr)
     d PAcase                              like(PAcase)
     d PArecd                              like(PArecd)
     d @payupd2                       1
     d PAamt                               like(PAamt)

     d DartsRemark     pr                  extpgm('DA700R')
     d outKey                        40
     d outComm                      256
     d outCdat                        8
     d outDcid                       12
     d outPnam                       10
     d outSecl                        3
     d outUser                       10
     d outPgm                        10
     d outFold                       80
     d outDifl                        1
     d outIpgm                       10
     d outFl01                        1
     d outFl02                        1
     d outRmcd                        5
     d outSeq#                        5
     d outSbyl                        1
     d outCom2                       19
     d outApid                        2
     d outIdoc                       12
     d outIque                       10
     d outImgf                        1
     d outIpth                       10
     d outPidx                        5
     d outQpty                        1
     d outDctp                        5
     d outCsid                        2

       // Program Status Data Structure
       Dcl-Ds SZQ1P ext psds;
       End-Ds;

       // Array DS For Payment Against Reserve
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

       // Array RR For Remaining Reserve
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
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

       // Array RT For Reserve Type
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

     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDSC          DS
     D  DBRRNC               397    400B 0

       Dcl-S outMode       Char(1) inz;
       Dcl-S outClmRrn     Packed(9:0) inz;
       Dcl-S outPayRrn     Packed(9:0) inz;
       Dcl-S outTaxId      Packed(9:0) inz;
       Dcl-S outMod        Packed(3:0) inz;
       Dcl-S outFromDt     Packed(7:0) inz;
       Dcl-S outToDt       Packed(7:0) inz;
       Dcl-S outEOB#       Char(9) inz;
       Dcl-S outRecon#     Packed(3:0) inz;
       Dcl-S outSysType    Char(1) inz;
       Dcl-S outBillType   Char(1) inz;
       Dcl-S outPayment    Packed(9:2) inz;
       Dcl-S outTransCode  Char(2) inz;
       Dcl-S outPayStatCod Char(2) inz;
       Dcl-S outPayValue1  Char(2) inz;
       Dcl-S outPayValue2  Char(2) inz;
       Dcl-S outRecvDt     Packed(7:0) inz;
       Dcl-S inReturnCode  Char(1) inz;
       Dcl-S @usrAut Ind inz;
       Dcl-S x Zoned(2:0) inz;
       Dcl-S ErrMsg char(78) dim(5) ctdata;
       //*************************************************************
       // Procedure:
       // Payment Detail Window
       // Update the Payment Information grid to allow void, stop and
       // reversal processing
       //*************************************************************
       Dcl-proc PayDetailPrc Export;
       Dcl-Pi *N;
        inRRN Zoned(9:0) const;
       End-Pi;

       Dcl-S @errTyp Char(1) inz;

       If Not %Open(wpiPmtPop);
          Open wpiPmtPop;
       Endif;

       // Retrieve Request Payment Transaction rec
       Chain(n) inRRN ppa05;

       // Retrieve Claim File Record
       Chain(n) (PAco#:PAfnd:PAfyr:PAcase) pcm00;

       Clear scPayUpd; // Clear radio button group
       Dou btnCancel = *on;
          Clear scPayTyp;
          //*** beg add ***** 08/22/23 ************************************
          scReCd = PArecd;
          //*** end add ***** 08/22/23 ************************************
          Exfmt rcdPayUpd;
          If btnSubmit = *on;
             Clear @errTyp;
             ValPayUpPrc(scPayUpd:inRRN:@errTyp);

             // Error encountered (R = Redisplay window)
             Select;
             When @errTyp = 'R';
                Iter; // Redisplay
             When @errTyp = 'X';
                Return; // Exit to calling pgm
             Endsl;

             //************************************************************
             // Process Payment Update Based On Option Selected M/R/V/Z/S
             //************************************************************
             Select;

             //---------------------------------------------------
             // Selection of Multiple Adjustments or Recovery option
             //---------------------------------------------------
             //*** Beg Chg ***** 10/12/23 *** 114292 ***************************
             //***When scPayUpd = 'C' or scPayUpd = 'O' or scPayUpd = 'M' or
             //***scPayUpd = 'R';
             When scPayUpd = 'C' or scPayUpd = 'M' or scPayUpd = 'R';
             //*** End Chg ***** 10/12/23 *** 114292 ***************************

                // Execute External Claim Payment Procedure
                //*** Beg Chg ***** 10/12/23 *** 114292 ************************
                //***If scPayUpd = 'R';
                //***   outMode = 'R'; // Recoveries
                //***Else;
                //***   outMode = 'A'; // Adjustments
                //***Endif;
                If scPayUpd = 'M';
                   outMode = 'A'; // Multiple Adjustments
                Else;
                   outMode = scPayUpd;
                Endif;
                //*** End Chg ***** 10/12/23 *** 114292 *******************
                outClmRrn = DBRRNC;
                outPayRrn = inRRN;
                outTaxId  = PAfein;
                outTransCode = scPayTyp;

                ClmPayPrc(outMode:outClmRrn:outPayRrn:outTaxId:
                 outMod:outFromDt:outToDt:outEOB#:
                 outRecon#:outSysType:outBillType:
                 outPayment:outTransCode:outPayStatCod:
                 outPayValue1:outPayValue2:outRecvDt:inReturnCode);
                Leave;

             //-------------------------------------------
             // Void, Stop or Reversal Selections
             //-------------------------------------------
             When scPayUpd = 'V' or  scPayUpd = 'S' or scPayUpd = 'Z';
                // Validate Reason & Comments and then Process VSZ
                ValVSZRsnPrc(scPayUpd:scPayTyp:inRRN);

                // Cancel selected go back to do group
                If btnCancel = *on;
                   btnCancel = *off;
                   Clear scPayUpd; // Clear radio button group
                   Iter;
                Else;
                   Leave; // Processing has completed, exit
                Endif;

             Endsl;

          Endif; // btnSubmit = *on
       Enddo;

       Close wpiPmtPop;

       Return;
       End-proc PayDetailPrc;

       //*************************************************************
       // Procedure:
       // Validate User Selections in Payment Update Window
       //*************************************************************
       Dcl-proc ValPayUpPrc Export;
       Dcl-Pi *N;
        inPayUpd Char(1) const;
        inRRN Zoned(9:0) const;
        outERR Char(1);
       End-Pi;

       Dcl-S xxRem Zoned(9:2) inz;
       Clear outERR;

       // Retrieve Claims Checks Payment Record
       Chain(n) inRRN ppa05;

       // Retrieve Claim File Record
       Chain(n) (PAco#:PAfnd:PAfyr:PAcase) pcm00;

       // Payment Type Code not selected from dropdown
       If scPayTyp = ' '; // Payment Type Code
          outERR = 'R';
          PopOneBtn(errMsg(5):'Error');
          Return;
       Endif;

       //***If inPayUpd = 'V' or inPayUpd = 'S' or inPayUpd = 'Z';
       //*** beg chg ***** 08/23/23 ****************************************
       If inPayUpd = 'V' or inPayUpd = 'S' or inPayUpd = 'Z' or
          inPayUpd = 'C' or inPayUpd = 'R' or inPayUpd = 'O' or
          inPayUpd = 'M';
       //*** end chg ***** 08/23/23 ****************************************
          // Check user authority
          CheckUserAut (PAco#:PAfnd);
             If Not @usrAut; // Not authorized
                outERR = 'X';
                PopOneBtn(errMsg(4):'Error');
                Return;
             Endif;

             // If Reconciliation Date is exists, send error.
             If PArcdt <> 0;
                outERR = 'X';
                PopOneBtn(errMsg(3):'Error');
                Return;
             Endif;

             x = %lookup(PArecd:RT);
             xxREM = rr(x);

             // If Claim is open
             If CMstat = 'O';

                // If selection is a Reversal
                If inPayUpd = 'Z';

                   // Retrieve Check View File Record
                   Setll (PAco#:PAfnd:PAfyr:PAcase) prp00;
                   Dou %Eof(warpl);
                      Reade (PAco#:PAfnd:PAfyr:PAcase) prp00;
                      If %Eof(warpl);
                         Leave;
                      Endif;
                      // If Reserve Codes are the same and Status
                      // is blank, reduce the remain reserve amount.
                      If PArecd = RPrecd and RPstat = ' ';
                         xxRem -= RPamt;
                      Endif;
                   Enddo; // Dou EOF

                   If PAamt > xxREM;
                      outERR = 'X';
                      PopOneBtn(errMsg(1):'Error');
                      Return;
                   Endif;

                Endif; // inPayUpd = 'Z'
             Endif; // CMstat = 'O'

             // If void or stop type and total payments against reserve
             // code are less than selected transaction, load message.
             If inPayUpd = 'V' or inPayUpd = 'S';
                If PAamt > AR(X);
                   outERR = 'X';
                   PopOneBtn(errMsg(2):'Error');
                   Return;
                Endif;
             Endif;
       Endif;
       Return;

       End-proc ValPayUpPrc;
       //*************************************************************
       // Procedure:
       // Voids, Stops and Reversals Reason Window
       //*************************************************************
       Dcl-proc ValVSZRsnPrc;
       Dcl-Pi *N;
        inPayUpd Char(1) const;
        inPayTyp Char(2) const;
        inRRN Zoned(9:0) const;
       End-Pi;

       If Not %Open(wpiPmtPop);
          Open wpiPmtPop;
       Endif;

       // Determine Reason Heading
       Select;
       When inPayUpd = 'V';
          scRsnDscHd = 'Reason for Void';
       When inPayUpd = 'S';
          scRsnDscHd = 'Reason for Stop';
       When inPayUpd = 'Z';
          scRsnDscHd = 'Reason for Reversal';
       Endsl;

       Clear scRMDS;
       Clear scRMK1;
       Clear scRMK2;
       Clear scRMK3;

       // Display window for user to enter reason for V/S/Z.
       Dou btnCancel = *on;
          Exfmt rcdRsnCde;
          Clear erCmnt2;
          Clear erCmnt3;
          Clear mgCmnt2;
          Clear mgCmnt3;

          // Cancel selected
          If btnCancel = *on;
             Return;
          Endif;

          // Submit selected, check for duplicate comments entries
          If btnSubmit = *on;

             // Check for duplicate 'Comment' selections
             If scRmk1 <> ' ' and scRmk1 = scRmk2;
               erCmnt2 = *on;
               mgCmnt2 = 'A duplicate comment was entered.';
             Endif;
             If scRmk1 <> ' ' and scRmk1 = scRmk3;
               erCmnt3 = *on;
               mgCmnt3 = 'A duplicate comment was entered.';
             Endif;

             If scRmk2 <> ' ' and scRmk2 = scRmk3;
               erCmnt3 = *on;
               mgCmnt3 = 'Duplicate comment was entered.';
             Endif;

             // If no errors then exit
             If erCmnt2 = *on or erCmnt3 = *on;
                Iter;
             Else;
                Leave;
             Endif;
          Endif;
       Enddo;

       // Perform Void, Stop or Reversal
       ProcessVSZPrc(inPayUpd:inPayTyp:inRRN);
       Return;

       End-proc ValVSZRsnPrc;
       //*************************************************************
       // Process the Void or Stop or Reversal
       //*************************************************************
       Dcl-proc ProcessVSZPrc;
       Dcl-Pi *N;
        inPayUpd Char(1) const;
        inPayTyp Char(2) const;
        inRRN Zoned(9:0) const;
       End-Pi;

       Dcl-S @payhdg Char(10) inz;
       Dcl-S svSeq# Like(PAseq#) inz;
       Dcl-S xxSeq Like(PAseq#) inz;
       Dcl-S xxSeq# Like(PAseq#) inz;
       Dcl-S xxFein Zoned(9:0) inz;
       Dcl-S xxMod Zoned(3:0) inz;
       Dcl-S xxTrdt Zoned(7:0) inz;
       Dcl-S xxSS# Char(9) inz;
       Dcl-S xxPayUpd Char(1) inz;
       Dcl-S svACCD Like(PAcktp) inz;
       Dcl-S svACCT Like(PAacct) inz;
       Dcl-S svLTCD Like(PAltcd) inz;
       Dcl-S svBASQ Like(PAbasq) inz;
       Dcl-S svDSQ# Like(PAdsq#) inz;
       Dcl-S svFNAL Like(PAfnal) inz;
       Dcl-S svTRTP Like(PAtype) inz;
       Dcl-S svOUTS Like(ELouts) inz;
       Dcl-S ##CO# Char(3) inz;
       Dcl-S ##FND Char(3) inz;
       Dcl-S ##ACCT Char(20) inz;
       Dcl-S ##SRDT Char(8) inz;
       Dcl-S ##CALL Char(1) inz;
       Dcl-S ##EFDT Char(7) inz;
       Dcl-S ##APPL Char(2) inz;
       Dcl-S ##EFTC Char(1) inz;
       Dcl-S ##HDCY Char(1) inz;
       Dcl-S ##CHKQ Char(10) inz;
       Dcl-S ##FTYP Char(10) inz;
       Dcl-S ##CPYQ Char(10) inz;
       Dcl-S ##CKTP Char(10) inz;
       Dcl-S ##COID Char(20) inz;
       Dcl-S ##RTCD Char(20) inz;
       Dcl-S intRRN Zoned(9:0) inz;
       Dcl-S inCO#  Like(PAco#)  inz;
       Dcl-S inFND  Like(PAfnd)  inz;
       Dcl-S inFYR  Like(PAfyr)  inz;
       Dcl-S inCASE Like(PAcase) inz;
       Dcl-S inSEQ# Like(PAseq#) inz;
       Dcl-S @msg1 Char(50) inz;

       // Retrieve Claims Checks Payment Record
       Chain(n) inRRN ppa05;
       If %Found(wdpap);
          inCO#  = PAco#;
          inFND  = PAfnd;
          inFYR  = PAfyr;
          inCASE = PAcase;
          inSEQ# = PAseq#;
       Endif;

       // Retrieve Claim File Record
       Chain(n) (inCO#:inFND:inFYR:inCASE) pcm00;

       // Get The Next Available Sequence Number
       xxSEQ = *hival;
       Setgt (inCO#:inFND:inFYR:inCASE:xxSEQ) ppa02;
       Readpe(N) (inCO#:inFND:inFYR:inCASE) ppa02;
       If %Eof(wdpal);
          clear PAseq#;
       Endif;
       svSEQ# = PAseq# + 1;

       // Get the selected transcaction
       xxSeq# = inSEQ#;
       Chain inRRN ppa05;
       svDSQ# = PAdsq#;

       // Setup the base seq#
       If PAbasq = 0;
          svBasq = PAseq#;
       Else;
          svBasq = PAbasq;
       Endif;
       svACCD = PAcktp;
       svACCT = PAacct;
       svLTCD = PAltcd;
       svFNAL = PAfnal;

       // Update the selected record
       PAuser = Q1user;
       PApgm  = Q1pgm;
       PAdt   = Dateto7(SystemDate());
       PAtime = Timeto6(SystemTime());
       PAadst = inPayUpd;
       Update ppa05;

       // Retrieve Payment Transaction
       Chain (PAco#:inPayTyp) ppc00;

       PAseq# = svSEQ#;
       PAbasq = svBASQ;
       PAtype = PCtrtp;
       PAdesc = PCdesc;
       clear PAstat;
       PAuser = Q1user;
       PApgm  = Q1pgm;
       PAdt   = Dateto7(SystemDate());
       PAtrdt = Dateto7(SystemDate());
       PAtime = Timeto6(SystemTime());
       PAtrcd = PCkey;

       // If Reversal Type, populate Adjustment Status
       If inPayUpd = 'Z';
          PAadst = inPayUpd;
       Else;
          Clear PAadst;
       Endif;

       Clear PArcvd;
       Clear PAdue;
       Clear PApgtp;
       PAoff# = CMoff#;
       PAcktp = svACCD;
       PAacct = svACCT;
       PArqst = Q1user;
       PAltcd = svLTCD;
       Clear PAdpcd;
       Clear PAdpcu;
       PAfnal = svFNAL;
       Clear PArcdt;
       Clear PAradj;

       // If bank account# is blank
       If PAacct = ' ';

          // Retrieve Fund Master record
          Chain (inCO#:inFND) pfn00;
          ##CO# = %editc(FNco# : 'X');
          ##FND = %editc(FNfnd : 'X');

          // Retrieve Group Aux Rec
          Chain (FNco#:FNfnd) pf200;
          If %Found(wdf2l);
             ##CALL = F2ctyp; // Group Aux Payment type
          Endif;

          ##SRDT = %editc(Dateto8(SystemDate()) : 'X'); // Today's Date
          ##APPL = 'WC';
          ##EFTC = '  ';
          ##COID = '  ';

          // Retrieve Bank Account.
          ##acct = GetBankAcc(##CO#:##FND:##APPL:##SRDT:##CALL:
          ##HDCY: ##CHKQ: ##FTYP: ##EFDT: ##CPYQ:
          ##CKTP: ##EFTC: ##COID: ##RTCD);

          If ##RTCD = ' ';
             PAacct = ##ACCT;
          Else;
             Clear PAacct;
          Endif;
       Endif; // PAACCT=BLANK

       PAdsq# = svDSQ#;
       PAiusr = Q1user;
       PAipgm = Q1pgm;
       PAidt  = Dateto7(SystemDate());
       PAitim = Timeto6(SystemTime());
       Write PPA02;
       // IDC processing for voids/stops/reversals
       If isModEnabled('IDC') and PAretp = 'C';
          intRRN = DBrrn2;
          WriteTxLog(intRRN);
       Endif;

       // Search for the correct View Check record.
       xxSEQ# = PAbasq;
       Chain(n) (PAco#:PAfnd:PAfyr:PAcase:xxSEQ#) prp03;
       If Not %Found(wdrpl3);
          xxTRDT = PApodt;
          Chain (PAco#:PAfnd:PAfyr:PAcase:PAchk#:xxTRDT) prp02;
       Endif;
       If Not %Found(wdrpl);
          RPco#  = PAco#;
          RPfnd  = PAfnd;
          RPfyr  = PAfyr;
          RPcase = PAcase;
          RPchk# = PAchk#;
          Clear RPrmds;
          Clear RPrmk1;
          Clear RPrmk2;
          Clear RPrmk3;
          Clear RPckcn;

          If PAfein <> 0;

             // Access Payee Demographic record
             xxFEIN = PAfein;
             xxMOD  = PAmod;
             Chain (xxFEIN:xxMOD) ppd00;
             If %Found(wmpdl);
                If PDfsn <> ' ';
                   RPpnam = %Trim(PDname) + ',' + %Trim(PDfsn);
                Endif;

                RPadr1 = PDadr1;
                RPadr2 = PDadr2;
                RPcty = PDcty;
                RPst  = PDst;
                RPzip = PDzip;

                If PDdba1 = 'Y';
                   RPpnam = PDadr1;
                   RPadr1 = PDadr2;
                   Clear RPadr2;
                Endif;

                // Check for Foreign Country
                If PDctry <> ' ' and PDctry <> 'USA';
                   RPctry = PDctry;
                   RPadr3 = PDadr3;
                Endif;
             Endif; // %Found(wmpdl)
          Else;
             // Access Claimant Demographic record
             xxSS# = CMss#;
             Chain (xxSS#) pcd00;
             If %Found(wmcdl);
                If CDmid = ' ';
                   RPpnam = %trim(CDfsn) + ' ' + %trim(CDlsn);
                Else;
                   RPpnam = %trim(CDfsn) + ' ' + %trim(CDmid) +
                   ' ' + %trim(CDlsn);
                Endif;
                RPadr1 = CDadr1;
                RPadr2 = CDadr2;
                RPcty  = CDcty;
                RPst   = CDst;
                RPzip  = CDzip;
                If CDctry <> ' ' and CDctry <> 'USA';
                   RPctry = CDctry;
                   RPadr3 = CDadr3;
                Endif;

             Endif; // %Found(wmcdl)

          Endif; // PAfein <> 0
       Endif; // Not %Found(wdrpl)

       RPtrdt = PAtrdt;
       RPrmds = scRMDS;
       RPrmk1 = scRMK1;
       RPrmk2 = scRMK2;
       RPrmk3 = scRMK3;
       RPstat = 'A';
       RPuser = Q1user;
       RPpgm = Q1pgm;
       RPdt = PAdt;
       RPtime = PAtime;
       RPseq# = PAseq#;

       // Write the View Check record.
       Write PRP02;

       // If the claim is closed, call SRPRCC to allocate the
       // reserve record for this payment.
       If CMstat = 'C';
          xxPayUpd = inPayUpd;
          Callp PayRelCC(inCO#:inFND:inFYR:inCASE:PArecd:
           xxPayUpd:PAamt);
       Endif;

       // If the request is to reverse a transaction - Get the
       // original base record.
       If inPayUpd = 'Z';
          xxSEQ = svBASQ;
          Chain (inCO#:inFND:inFYR:inCASE:xxSeq) ppa02;
          If %Found(wdpal);
             PAuser = Q1user;
             PApgm  = Q1pgm;
             PAdt   = Dateto7(SystemDate());
             PAtime = Timeto6(SystemTime());
             Clear PAadst;
             Update ppa02;
          Endif;
       Endif;

       Chain (PAco#:PAfnd:PAfyr:PAcase) pcm00;
       If %Found(wmcml1);
          x = %lookup(PArecd:RT);
          If x = 0;
             x = 10;
          Endif;

          // Update claim reserve buckets based on the Reserval or
          // Void/Stop
          If inPayUpd = 'Z';
             RR(X) -= PAamt;
             AR(X) += PAamt;
             CMpctr += 1; // Payment Counter
          Else;
             RR(X) += PAamt;
             AR(X) -= PAamt;
             CMpctr -= 1; // Payment Counter
          Endif;

          CMuser = Q1user;
          CMpgm  = Q1pgm;
          CMdt   = Dateto7(SystemDate());
          CMtime = Timeto6(SystemTime());
          Update pcm00;
       Endif;

       // Update the employer detail file with the outstanding
       // reserve and total claims paid.
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
       If %Found(wdell);
          svOUTS = ELouts;
          If inPayUpd = 'V' or inPayUpd = 'S';
            ELtclp -= PAamt;
            ELouts += PAamt;
          Else;
            ELtclp += PAamt;
            ELouts -= PAamt;
          Endif;

          ELuser = Q1user;
          Elpgm = Q1pgm;
          ELdt = Dateto7(SystemDate());
          ELtime = Timeto6(SystemTime());
          Update Pel00;
       Endif;

       Select;
       When inPayUpd = 'V';
          @payhdg = 'Voided';
       When inPayUpd = 'S';
          @payhdg = 'Stopped';
       When inPayUpd = 'Z';
          @payhdg = 'Reversed';
       Endsl;

       // Load successful message
       Clear @msg1;
       @msg1 = 'Check# ' + %editc(PAchk# : 'X') +
        ' has been successfully ' + %trim(@payhdg);
       PopOneBtn(@msg1:'Notification');
       Return;
       End-proc ProcessVSZPrc;
       //*************************************************************
       // Procedure:
       // User Security
       // Update the Payment Information grid to allow void, stop and
       // reversal processing
       //*************************************************************
       Dcl-proc CheckUserAut Export;
       Dcl-Pi *N;
        inCO# Packed(3:0) const;
        inFND Packed(3:0) const;
       End-Pi;
       Dcl-S outResult Char(1) inz;
       //*** Beg Add *** 02/17/25 ***********************************
       If scPayUpd = 'C' or scPayUpd ='M';
          If CheckSecLevel('Adjustment':UPtype:UPlvl);
             @usrAut = *on;
          Else;
             @usrAut = *off;
          Endif;
          Return;
       Endif;
       If scPayUpd = 'R';
          If CheckSecLevel('RecovTrack':UPtype:UPlvl);
             @usrAut = *on;
          Else;
             @usrAut = *off;
          Endif;
          Return;
       Endif;
       //*** End Add *** 02/17/25 ***********************************
       Chain (inCO#:inFND) pcx00;
       If %Found(wmcxl);

          // Execute User Security Program 'SRCCSC'
          outResult = *on;
          Chain (Q1user) pup00;
          If %Found(wmupl);
             CheckAuth(UPtype:
                       %editc(UPlvl : 'X'):
                       CXvsZT:
                       %editc(CXvsZL : 'X'):
                       '1' :
                       outResult:
                       CXvsZ2:
                       CXvsZ3:
                       CXvsZ4:
                       CXvsZ5:
                       ' ');
             If outResult = *off;
                @usrAut = *on;
             Else;
                @usrAut = *off;
             Endif;
             Return;
          Endif;
       Endif;
      *
       End-proc CheckUserAut;
       //*************************************************************
       // Procedure:
       // Reason For Late Payment
       //*************************************************************
       Dcl-proc LatePaymentPrc Export;
       Dcl-Pi *N;
        clmRrn Zoned(9:0) const;
        inPdOnTm Char(2) const;
       End-Pi;

       Dcl-Ds qcKEY qualified;
          qcCO#  Zoned(3:0);
          qcFND  Zoned(3:0);
          qcFYR  Zoned(3:0);
          qcCASE Zoned(7:0);
       End-Ds qcKEY;

       Dcl-S q#Key Char(40) inz;
       Dcl-S q#Comm Char(256) inz;
       Dcl-S q#Com2 Char(19) inz;
       Dcl-S q#Cdat Char(8) inz;
       Dcl-S q#Dcid Char(12) inz;
       Dcl-S q#Pnam Char(10) inz;
       Dcl-S q#Secl Char(3) inz;
       Dcl-S q#User Char(10) inz;
       Dcl-S q#Pgm Char(10) inz;
       Dcl-S q#Fold Char(80) inz;
       Dcl-S q#Difl Char(1) inz;
       Dcl-S q#Ipgm Char(10) inz;
       Dcl-S q#Fl01 Char(1) inz;
       Dcl-S q#Fl02 Char(1) inz;
       Dcl-S q#Rmcd Char(5) inz;
       Dcl-S q#Seq# Char(5) inz;
       Dcl-S q#Sbyl Char(1) inz;
       Dcl-S q#Apid Char(2) inz;
       Dcl-S q#Idoc Char(12) inz;
       Dcl-S q#Ique Char(10) inz;
       Dcl-S q#Imgf Char(1) inz;
       Dcl-S q#Ipth Char(10) inz;
       Dcl-S q#Pidx Char(5) inz;
       Dcl-S q#Qpty Char(1) inz;
       Dcl-S q#Dctp Char(5) inz;
       Dcl-S q#Csid Char(2) inz;

       If Not %Open(wpiPmtPop);
          Open wpiPmtPop;
       Endif;

       // Retrieve Remark Date
       scRmkDate = Dateto8(SystemDate); // Today's Date

       // Retrieve requested claim rec
       Chain(n) clmRRN pcm02;

       Chain (CMco#:inPdOnTm) plr00;
       If %Found(wtabl) and LRdart <> ' ';
          scRmkCode = LRdart;
       Endif;


       Clear scLtRsnDsc;
       Dou btnCancel = *on;
          Exfmt rcdRsnLate;
          If btnCancel = *on;
             Return;
          Endif;
          If btnSubmit = *on;
             Clear qcKey;
             qcKEY.qcCO#  = CMco#;
             qcKEY.qcFND  = CMfnd;
             qcKEY.qcFYR  = CMfyr;
             qcKEY.qcCASE = CMcase;
             q#KEY = qcKEY;
             If scLtRsnDsc <> ' ';
                q#Comm = scLtRsnDsc;
             Else;
                // Assign DARTS Remark Code Description
                Chain (LRdart) pct00;
                If %Found(STCTL1);
                   q#Comm = CTdesc;
                Endif;
             Endif;
             Clear q#COM2;
             q#Cdat = %editc(Dateto8(SystemDate()) : 'X'); // Today mmddyyyy
             Clear q#Dcid;
             q#Pnam = 'W0070R';
             Clear q#Secl;
             q#User = Q1user;
             q#Pgm = Q1pgm;
             Clear q#Fold;
             Clear q#Difl;
             q#Ipgm = Q1pgm;
             Clear q#Fl01;
             Clear q#Fl02;
             q#Rmcd = LRdart;
             q#Seq# = '00000';
             q#Sbyl = 'N';
             Clear q#Apid;
             Clear q#Idoc;
             Clear q#Ique;
             Clear q#Imgf;
             Clear q#Ipth;
             Clear Q#Pidx;
             Clear Q#Qpty;
             Clear Q#Dctp;
             Clear Q#Csid;
             DartsRemark(q#Key:q#Comm:q#CDat:q#DCid:q#PNam:q#Secl:
              q#User:q#Pgm:q#Fold:q#Difl:q#IPgm:q#Fl01:q#Fl02:q#Rmcd:
              q#Seq#:q#Sbyl:q#Com2:q#ApId:q#IDoc:q#IQue:q#IMgf:q#IPth:
              q#PIdx:q#QPty:q#DCtp:q#CSid);
             Return;
          Endif;
       Enddo;
       End-proc LatePaymentPrc;
** ErrMsg
Reversal not successful. Reserves not adequate for claim.
Selected payment will result in negative reserves on the claim.
This check has already been cleared, unable to process this request.
Not authorized to perform payment updates.
Both Payment Button and Type Code in dropdown list must be selected.
