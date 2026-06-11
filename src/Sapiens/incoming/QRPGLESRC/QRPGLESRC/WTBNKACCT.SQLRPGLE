       CTL-OPT COPYRIGHT('(c) 2025 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no) ACTGRP(*caller) BNDDIR('WTBNDDIR');
       ctl-opt option(*nodebugio:*srcstmt);
       //******************************************************************
       //                                                                 *
       //  WTBNKACCT - Bank Account Master Maintenance.                   *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  This program is used to maintain the Bank Account Master.      *
       //                                                                 *
       //                                                                 *
       //  Created: 08/14/20  C:180  Project:  WC2731                     *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       //  07/29/25 C:062 Project: 153WBE                                 *
       //                 Make sure E-Pay screen is changed so IVC can be *
       //                 set.                                            *
       //                                                                 *
       //  03/17/25 C:291 Project: WC3002                                 *
       //                 Add Checkbox "Pad incomplete Blocks of 10 with  *
       //                 9s".                                            *
       //                                                                 *
       //  09/14/22 C:291 Project: 129WXH                                 *
       //                 Add Checkbox "Default recurring Payments to     *
       //                 claimant/dependents to EFT if EFT is set up".   *
       //                                                                 *
       //  06/15/22 C:33  Project: WC2663                                 *
       //                 Notify user of existing payment records with    *
       //                  Inv Type ID                                    *
       //                                                                 *
       //  05/19/22 C:300 Support: 102318                                 *
       //                 Bank Account Master - Main screen editing issues*
       //                 In DSPF: Correct Format of Effective Date and   *
       //                 Expiration Date                                 *
       //                                                                 *
       //  04/11/22 C:340 Project: WC2663                                 *
       //                 Invoice Cloud - Add Invoice Type ID             *
       //                                                                 *
       //  12/23/21 C:062 Ticket 100157(129)                              *
       //                 Make sure voucher flag is set to N instead of   *
       //                 blanks,                                         *
       //                                                                 *
       // 10/07/21  C:300 Support: 98537                                  *
       //                 In DSPF: increased the width of OutServiceClassCode
       //                                                                 *
       //  06/09/21 C:180 Project 133WSO                                  *
       //                 Allow change to MICR.                           *
       //                                                                 *
       //******************************************************************
       Dcl-Pi *N;
        @@KC char(1);
        @@KL char(1);
        @@MODE char(1);
        @@RRNA char(9) options(*nopass);
       End-Pi;

       Dcl-F wtbnkacct workstn handler('PROFOUNDUI(HANDLER)')
        sfile(sflBnkAcct:RrnBnkAcct);

       Dcl-F wtactpnl workstn handler('PROFOUNDUI(HANDLER)')
        include(rcdComPnl);

       Dcl-F wsckl disk usage(*INPUT) keyed infds(DBFDS1);
       Dcl-F wsckp disk usage(*update:*output) rename(PCK01:PCK02);
       Dcl-F wsrtl disk usage(*input:*update:*output) keyed ;
       Dcl-F wmbil9 disk usage(*input:*update) keyed ;
       Dcl-F wmevml1 disk usage(*input) keyed rename(pevm01:pevm01);
       //*** Begin Add *** 06/09/21 ******************************************
       Dcl-F wsckl1sec disk usage(*input:*update:*output) keyed;
       //*** End   Add *** 06/09/21 ******************************************

       //External Prototypes
       Dcl-Pr GetCompNam char(40);
        CompNum packed(3:0) const;
       End-Pr;

       Dcl-Pr UserCo# packed(3:0);
        User char(10) const;
        co_name char(40) options(*nopass);
       End-Pr;

       Dcl-Pr RTVCLTID  EXTPGM;
        strCltId char(3);
       End-Pr;

       Dcl-Pr SRCKOBJN  EXTPGM;
        strChar10 char(10);
        strChar01 char(1);
       End-Pr;


       //Copy Books
      /Free
      /Copy sPrModule
      /Copy sPrDate
      /Copy wPrBank
      /Copy wPrGenPop
      /End-Free


       //Program Status Data Structure
       Dcl-Ds SZQ1P ext psds;
       End-Ds;


       //File Information Data Structure
       Dcl-Ds DBFDS1 ;
         dbrrn1 int(10) pos(397);
       End-Ds;


       //Keys Data Structures
       Dcl-Ds KeyWSRTL likerec(PRT01:*KEY);


       //Other Data Structures
       Dcl-Ds DsFilter likerec(RcdFltBnkA);

       //*** Begin Add *** 06/09/21 ********************************************************
       Dcl-Ds KeyWSCKL1SEC likerec(PCKS01:*KEY);

       Dcl-S intIdx    packed(5:0);
       Dcl-S intCKSidx packed(5:0);

       //MICR security type - Code
       Dcl-Ds SecTypeDs ;
        scEpMiStp char(10);
         scEpMiStp1 overlay(scEpMiStp);
         scEpMiStp2 overlay(scEpMiStp:*next);
         scEpMiStp3 overlay(scEpMiStp:*next);
         scEpMiStp4 overlay(scEpMiStp:*next);
         scEpMiStp5 overlay(scEpMiStp:*next);
         scEpMiStp6 overlay(scEpMiStp:*next);
         scEpMiStp7 overlay(scEpMiStp:*next);
         scEpMiStp8 overlay(scEpMiStp:*next);
         scEpMiStp9 overlay(scEpMiStp:*next);
         scEpMiStpA overlay(scEpMiStp:*next);
         @SecType char(1) dim(10) Overlay(scEpMiStp) inz;
       End-Ds;

       //MICR Security Type - Message Id
       Dcl-Ds MsgIdTpDs;
        msEpMiStp char(70);
         msEpMiStp1 overlay(msEpMiStp);
         msEpMiStp2 overlay(msEpMiStp:*next);
         msEpMiStp3 overlay(msEpMiStp:*next);
         msEpMiStp4 overlay(msEpMiStp:*next);
         msEpMiStp5 overlay(msEpMiStp:*next);
         msEpMiStp6 overlay(msEpMiStp:*next);
         msEpMiStp7 overlay(msEpMiStp:*next);
         msEpMiStp8 overlay(msEpMiStp:*next);
         msEpMiStp9 overlay(msEpMiStp:*next);
         msEpMiStpA overlay(msEpMiStp:*next);
         @MsgIdTp char(7) dim(10) Overlay(MsEpMiStp) inz;
       End-Ds;

       //MICR Security Type - Error Indicator
       Dcl-Ds ErStpDs;
        erEpMiStp char(10);
         erEpMiStp1 overlay(erEpMiStp);
         erEpMiStp2 overlay(erEpMiStp:*next);
         erEpMiStp3 overlay(erEpMiStp:*next);
         erEpMiStp4 overlay(erEpMiStp:*next);
         erEpMiStp5 overlay(erEpMiStp:*next);
         erEpMiStp6 overlay(erEpMiStp:*next);
         erEpMiStp7 overlay(erEpMiStp:*next);
         erEpMiStp8 overlay(erEpMiStp:*next);
         erEpMiStp9 overlay(erEpMiStp:*next);
         erEpMiStpA overlay(erEpMiStp:*next);
         @ErStp char(1) dim(10) Overlay(erEpMiStp) inz(*off);
       End-Ds;

       //MICR security level - Code
       Dcl-Ds SecLevelDs ;
        scEpMiSlv char(30);
         scEpMiSlv1 overlay(scEpMiSlv);
         scEpMiSlv2 overlay(scEpMiSlv:*next);
         scEpMiSlv3 overlay(scEpMiSlv:*next);
         scEpMiSlv4 overlay(scEpMiSlv:*next);
         scEpMiSlv5 overlay(scEpMiSlv:*next);
         scEpMiSlv6 overlay(scEpMiSlv:*next);
         scEpMiSlv7 overlay(scEpMiSlv:*next);
         scEpMiSlv8 overlay(scEpMiSlv:*next);
         scEpMiSlv9 overlay(scEpMiSlv:*next);
         scEpMiSlvA overlay(scEpMiSlv:*next);
         @SecLevel zoned(3:0) dim(10) Overlay(scEpMiSlv) inz(*zeros);
       End-Ds;

       //MICR Security Level - Message Id
       Dcl-Ds MsgIdLvDs;
        msEpMiSlv char(70);
         msEpMiSlv1 overlay(msEpMiSlv);
         msEpMiSlv2 overlay(msEpMiSlv:*next);
         msEpMiSlv3 overlay(msEpMiSlv:*next);
         msEpMiSlv4 overlay(msEpMiSlv:*next);
         msEpMiSlv5 overlay(msEpMiSlv:*next);
         msEpMiSlv6 overlay(msEpMiSlv:*next);
         msEpMiSlv7 overlay(msEpMiSlv:*next);
         msEpMiSlv8 overlay(msEpMiSlv:*next);
         msEpMiSlv9 overlay(msEpMiSlv:*next);
         msEpMiSlvA overlay(msEpMiSlv:*next);
         @MsgIdLv char(7) dim(10) Overlay(MsEpMiSlv) inz;
       End-Ds;

       //MICR Security Level - Error Indicator
       Dcl-Ds ErSlvDs;
        erEpMiSlv char(10);
         erEpMiSlv1 overlay(erEpMiSlv);
         erEpMiSlv2 overlay(erEpMiSlv:*next);
         erEpMiSlv3 overlay(erEpMiSlv:*next);
         erEpMiSlv4 overlay(erEpMiSlv:*next);
         erEpMiSlv5 overlay(erEpMiSlv:*next);
         erEpMiSlv6 overlay(erEpMiSlv:*next);
         erEpMiSlv7 overlay(erEpMiSlv:*next);
         erEpMiSlv8 overlay(erEpMiSlv:*next);
         erEpMiSlv9 overlay(erEpMiSlv:*next);
         erEpMiSlvA overlay(erEpMiSlv:*next);
         @ErSlv char(1) dim(10) Overlay(erEpMiSlv) inz(*Off);
       End-Ds;

       //MICR security type (database) - Code
       Dcl-Ds CKSsecTypDs ;
        CKSepMiSt char(10);
         CKSepMiSt1 overlay(CKSepMiSt);
         CKSepMiSt2 overlay(CKSepMiSt:*next);
         CKSepMiSt3 overlay(CKSepMiSt:*next);
         CKSepMiSt4 overlay(CKSepMiSt:*next);
         CKSepMiSt5 overlay(CKSepMiSt:*next);
         CKSepMiSt6 overlay(CKSepMiSt:*next);
         CKSepMiSt7 overlay(CKSepMiSt:*next);
         CKSepMiSt8 overlay(CKSepMiSt:*next);
         CKSepMiSt9 overlay(CKSepMiSt:*next);
         CKSepMiStA overlay(CKSepMiSt:*next);
         @CKStype char(1) dim(10) Overlay(CKSepMiSt) inz;
       End-Ds;

       //MICR security level (database) - Code
       Dcl-Ds CKSsecLvlDs ;
        CKSepMiSl char(30);
         CKSepMiSl1 overlay(CKSepMiSl);
         CKSepMiSl2 overlay(CKSepMiSl:*next);
         CKSepMiSl3 overlay(CKSepMiSl:*next);
         CKSepMiSl4 overlay(CKSepMiSl:*next);
         CKSepMiSl5 overlay(CKSepMiSl:*next);
         CKSepMiSl6 overlay(CKSepMiSl:*next);
         CKSepMiSl7 overlay(CKSepMiSl:*next);
         CKSepMiSl8 overlay(CKSepMiSl:*next);
         CKSepMiSl9 overlay(CKSepMiSl:*next);
         CKSepMiSlA overlay(CKSepMiSl:*next);
         @CKSlevel zoned(3:0) dim(10) Overlay(CKSepMiSl) inz(*zeros);
       End-Ds;
       //*** End   Add *** 06/09/21 ********************************************************


       //Constants
       Dcl-C Warn220 Const('The use of this account will be restriced to -
                            credits only');
       Dcl-C Warn225 Const('The use of this account will be restriced to -
                            debits only');
       Dcl-C Upper Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
       Dcl-C Lower Const('abcdefghijklmnopqrstuvwxyz');


       //Stand Alone
       Dcl-S @@Flg   char(1);
       Dcl-S ##rrn   packed(9:0);
       //***  Begin Add  ***  06/15/22  *************
       Dcl-s decCount packed(15:0);
       //***  End   Add  ***  06/15/22  *************
       Dcl-S DtZeroUsa  date(*USA);
       Dcl-S DtZeroIso  date(*ISO);
       Dcl-S intCo#  packed(3:0);
       Dcl-S strCltId char(3);
       Dcl-S strGrpSel char(400) inz;
       Dcl-S strMode  char(1);
       //***  Begin Add  ***  06/15/22  *************
       Dcl-s svInvTypId like(CKinvTypId);
       //***  End   Add  ***  06/15/22  *************
       Dcl-S svSVCL packed(3:0);

       //Indicators
       Dcl-S isEFT ind;
       Dcl-S hasEFT ind;
       //*** Beg Add ***** 09/14/22 *******************
       Dcl-S hasEPaySv ind;
       //*** End Add ***** 09/14/22 *******************
       Dcl-S hasEPay ind;
       Dcl-S isEdit ind;
       Dcl-S isCancCont ind;
       Dcl-S isTab0Err ind;
       Dcl-S isTab1Err ind;
       Dcl-S isTab2Err ind;
        //*** Begin  Add  *** 04/11/22 **********************************
       Dcl-S isTab3Err ind;
        //*** End    Add  *** 04/11/22 **********************************


       //Initialization
       ProgramStart();

       //Main Loop
       Dou ExitNow= *On;
          //Clear the Subfile
          ClearSubfiles();

          //Load Bank Account Master subfile
          LoadBnkAcctSfl();

          //Display Screens
          FormatScreens();

          //Process Screens
          Select;
          When  BtnBaFltr = *On;
             Exsr FilterBa;
          When  BtnBaCfltr = *On;
             Exsr ClearFilterBA;
          When btnBaAdd = *On;
             isEdit = *Off;
             EditAddBnkAcct();
          Other;
             Exsr ProcSfl;
          Endsl;
       Enddo;

       *Inlr = *On;


       //*----------------------------------------------------------------
       //* Filter Bank Account Maintenance                               *
       //*----------------------------------------------------------------
       Begsr FilterBA;
        BtnBaFltr = *Off;
        visBaCfltr = *On;

        Dou BtnFltCan  = *on or BtnFltApp  = *on;
           ExFmt RcdFltBnkA;
        Enddo;

        //Filter Applied
        If BtnFltApp = *On;
           If scFltBaNam <> *blank;
              scFltBaNam = %xlate(Lower:Upper:scFltBaNam);
           Endif;

           //Save filter values
           If scFltBaNam <> *blank Or scFltBaNo <> *blank
            Or scFltAgrp <> *Zeros;
              dsfilter.scFltBaNam = scFltBaNam ;
              dsfilter.scFltBaNo  = scFltBaNo  ;
              dsfilter.scFltAgrp = scFltAgrp  ;
           Endif;
        Endif;

        // If cancel then restore previous filter selection.
        If BtnFltCan  = *on;
           scFltBaNam = dsfilter.scFltBaNam;
           scFltBaNo  = dsfilter.scFltBaNo;
           scFltAgrp  = dsfilter.scFltAgrp;
        Endif;
       Endsr;

       //*----------------------------------------------------------------
       //* Clear Bank Account Maintenance Filter                         *
       //*----------------------------------------------------------------
       Begsr ClearFilterBA;
        btnBaCfltr = *Off;
        visBaCfltr = *Off;
        Clear RcdFltBnkA;
       Endsr;

       //*----------------------------------------------------------------
       //* Process Subfile                                               *
       //*----------------------------------------------------------------
       Begsr ProcSfl;
        isEdit = *Off;
        Readc SflBnkAcct;

        Select;
        When BtnSelect = *On;
           If Not %eof and %parms() > 3;
              @@rrna = %editc(scBaRrn:'X');
           Endif;
           ExitNow = *On;
        Other;
           Dow Not %Eof;
              If LnkBaEdit = *on;
               // lnkBAedit = *off;
                 isEdit = *On;
                 ##rrn = scBaRrn;
                 EditAddBnkAcct(##rrn);
              Endif;
              Readc SflBnkAcct;
           Enddo;
        Endsl;
       Endsr;

       //*----------------------------------------------------------------
       //* Program Start                                                 *
       //*----------------------------------------------------------------
       Dcl-Proc ProgramStart;
        Dcl-Pi *N;
        End-Pi;

        //Load incoming parameters
        strMode = @@Mode;

        //Clear screens
        Clear RcdBnkAcct;
        Clear SflBnkAcct;
        //*** Beg Add ***** 09/14/22 *******************
        hasEpaySv = hasEpay;
        //*** End Add ***** 09/14/22 *******************
        Clear RcdAddEdit;
        //*** Beg Add ***** 09/14/22 *******************
        hasEpay = hasEpaySv;
        //*** End Add ***** 09/14/22 *******************

        //Application Modes
        Select;
        When strMode   = '1';
           scBAhdg1  = 'Bank Account Inquiry';
           scCol0 = '1';
           scSlctMode = *Off;
           dspBAadd  = *Off;
           dspBAedit = *Off;
           dspMode0   = *On;
        When strMode = '2';
           scBAhdg1 =  'Bank Account Select';
           scCol0 = '2';
           scSlctMode = *On;
           dspBAadd  = *Off;
           dspBAedit = *Off;
           dspMode0   = *On;
        Other;
           scBAhdg1  = 'Bank Account Maintenance';
           scCol0 = '0';
           scSlctMode = *Off;
           dspBAadd  = *On;
           dspBAedit = *On;
           dspMode0   = *Off;
        Endsl;

        //Company
        intCo# = userCo#(q1user);

        //Populate Action Panel with Company Name
        Clear scCompName;
        scCompName = GetCompNam(intCo#);

        //Retrieve Client-ID
        RtvCltID(strCltId);

        //Has EFT?
        hasEFT = isModEnabled('EFT');

        //Has E-Pay?
        Read pevm01;
        If Not(%Eof);
           hasEPay = isModEnabled('E-Pay');
        Endif;

        // Move EPay info to the second tab
        If hasEPay And Not(hasEFT);
           isMoveEPay = *On;
        EndIf;

        //Filter Settings
        visBaCfltr = *Off;
        Clear DsFilter;
        scGrpSel = LoadGrpSel();
       End-Proc;

       //*----------------------------------------------------------------
       //* Clear Subfiles                                                *
       //*----------------------------------------------------------------
       Dcl-Proc ClearSubfiles;
        Dcl-Pi *N;
        End-Pi;

        RrnBnkAcct = 0;
        clrBnkAcct  = *on;
        dspBnkAsfl = *off;
        dspBnkActl  = *off;
        Write RcdBnkAcct;
       End-Proc;

       //*----------------------------------------------------------------
       //* Load Bank Account Master Subfile                              *
       //*----------------------------------------------------------------
       Dcl-Proc LoadBnkAcctSfl;
        Dcl-Pi *N;
        End-Pi;

        Dcl-S isMet ind;

        clrBnkAcct  = *off;
        dspBnkActl = *on;
        rrnBnkAcct = *zeros;
        SetTabNames();

        Setll *loval pck01;
        Read wsckl;
        Dow not %eof(wsckl);
           isMet = MeetSel();
           If isMet = *On;
              scBAactno = CKacct;
              scBAname  = CKai01;
              If CKefdt > *Zero;
                 scBAefDt = %date(CKefDt:*cymd);
              Else;
                 scBAefDt = DtZeroIso;
              Endif;
              If CKexDt > *Zero;
                 scBAexDt = %date(CKexDt:*cymd);
              Else;
                 scBAexDt = DtZeroIso;
              Endif;
              scBAnxtChk = CKlchk;
              If rrnBnkAcct = 1;
                 scBArowSL = 'A';
              Else;
                 clear scBArowSL;
              Endif;
              scBAeftFnm = CKeftF;
              scBArrn = DBrrn1;
              rrnBnkAcct = rrnBnkAcct + 1;
              Write sflBnkAcct;
           Endif;
           Read wsckl;
        Enddo;

        If rrnBnkAcct > *Zero;
           dspBnkAsfl = *On;
        Else;
           dspBnkAsfl = *Off;
        Endif;

        rrnBnkAcct = 1;
       End-Proc;

       //*-----------------------------------------------------------------
       //* Edit/Add Bank Account Master                                   *
       //*-----------------------------------------------------------------
       Dcl-Proc EditAddBnkAcct;
        Dcl-Pi *N;
         @@rrn  packed(9:0)  options(*nopass);
        End-Pi;

        Dcl-S isReqForEFT Ind;
        Dcl-S isValid ind;
        Dcl-S intRRN packed(9:0);
        Dcl-S intExDt70 packed(7:0);
        Dcl-S #2co#  char(3);
        Dcl-S #2fnd  char(3);
        Dcl-S #2srdt char(8);
        Dcl-S #2call char(1);
        Dcl-S #2hdcy char(1);
        Dcl-S #2chkq char(10);
        Dcl-S #2ftyp char(10);
        Dcl-S #2efdt char(7);
        Dcl-S #2cpyq char(10);
        Dcl-S #2coid char(20);
        Dcl-S #2rtcd char(1);
        Dcl-S #2acct char(20);
        Dcl-S strWarning char(256);
        Dcl-S SVAI01 like(CKAI01);
        Dcl-S SVAI02 like(CKAI02);
        Dcl-S SVAI03 like(CKAI03);
        Dcl-S SVONUS like(CKONUS);
        Dcl-S SVCKDG like(CKCKDG);
        Dcl-S SVBNK# like(CKBNK#);
        Dcl-S SVTRNS like(CKTRNS);
        Dcl-S SVAXOU like(CKAXOU);
        Dcl-S SVRTUP like(CKRTUP);
        Dcl-S SVRTLW like(CKRTLW);
        Dcl-S strRTKY char(10);
        isValid = *Off;

        If isEdit= *on;
           intRRN = @@rrn;
           chain(N) intRRN PCK02;

           If %Found(WSCKP);
              //Bank Data
              scACCT     = CKacct;
              scAI01     = CKai01;
              scAI02     = CKai02;
              scAI03     = CKai03;

              //*** Begin Chg *** 05/19/22 ***************************************
              //***** If CKefdt > *Zeros;
              //*****    scEFDT = %date(CKefDt:*cymd);
              //***** Else;
              //*****    scEFDT = DtZeroIso;
              //***** Endif;
              //*****
              //***** If CKexdt > *Zeros;
              //*****    scEXDT = %date(CKexDt:*cymd);
              //***** Else;
              //*****    scEXDT = DtZeroIso;
              //***** Endif;
              If CKefdt > *Zeros;
                 scEFDT = dtMDYY(CKefdt);
              Else;
                 scEFDT = 0;
              Endif;

              If CKexdt > *Zeros;
                 scEXDT = dtMDYY(CKexdt);
              Else;
                 scEXDT = 0;
              Endif;
              //*** End   Chg *** 05/19/22 ***************************************

              scLCHK     = CKlchk;

              //Voucher
              scVCHR     = CKvchr;

              //Processing Type
              Select;
              When strCltId <> '114';
                 scLblVchr = 'Voucher';
              When CKvchr = 'Y';
                 scLblVchr = 'Processing Method';
                 scVCHR# = 'V';
              When CKvchr = 'N';
                 scLblVchr = 'Processing Method';
                 scVCHR# = 'P';
              When CKvchr = 'X';
                 scLblVchr = 'Processing Method';
                 scVCHR# = 'X';
              Other;
                 scLblVchr = 'Processing Method';
                 scVCHR# = CKvchr;
              Endsl;

              scCRUP     = CKcrup;
              scRPCD     = CKrpcd;
              scRPVS     = CKrpvs;
              scRDCD     = CKrdcd;
              scRDVS     = CKrdvs;
              scWPP      = CKwpp;       //Positive pay download id
              scWBK#     = CKwbk#;      //Positive pay Bank #

              //MICR Data
              scTRNS     = CKtrns;
              scBNK#     = CKbnk#;
              scCKDG     = CKckdg;
              scAXOU     = CKaxou;
              scONUS     = CKonus;

              //Fractional Routing
              scRTUP     = CKrtup;
              scRTLW     = CKrtlw;

              //Signatures
              scNAM1     = CKnam1;
              scNAM2     = CKnam2;

              //Voucher checkbox vs Processing method dropdown
              If strCltId = '114';
                 visVCHR# = *On;                  //Processing method
                 disVCHR# = *Off;
                 visVCHR = *Off;
               // disVCHR = *On;
                 lftProcDrp = 575;
              Else;
                 visVCHR = *On;                   //Voucher
               // disVCHR = *Off;
                 visVCHR# = *Off;
                 disVCHR# = *On;
              Endif;

              //EFT
              If CKidsa <> *blanks and CKidsa <> *zeros;
                 scIDST = CKidsa;
              Else;
                 scIDST = %editc(CKidst:'X');
              Endif;

              If CKiora <> *blanks and CKiora <> *Zeros;
                 scIORG = CKiora;
              Else;
                 scIORG = %editc(CKiorg:'X');
              Endif;

              scBNAM = CKbnam;
              scCNAM = CKconm;
              scCEDS = CKceds;
              scRFCD = CKrfcd;
              scORDI = CKordi;
              scSVCL = CKsvcl;
              scEFTF = CKeftf;
              scACID = CKacid;
              scBKLT = CKbklt;
              scEFSP = CKefsp;
              scBYPN = CKbypn;
              //*** Beg Add ***** 09/14/22 *******************
              scOvrEPay = CKOvrEPay;
              //*** End Add ***** 09/14/22 *******************
              //*** Beg Add ***** 03/17/25 *******************
              scPad9 = CKPad9;
              //*** End Add ***** 03/17/25 *******************

              If hasEPay;
                 scEPayVdr = CKePayVN;
                 scNxtEPay# = CKePay#;
                 If scNxtEPay# = 0;
                    scNxtEPay# = 1;
                 Endif;
              Endif;

C340    //*** Begin Add *** 04/11/22 ***************************************

              isInvType = IsModEnabled('S/R Online') or
                          IsModEnabled('Third Party Payment') ;
              scInvTypLb = 'Invoice Type ID (' +
               GetModuleProperty('Invoice Cloud':'Name') + ')';
              scInvTypId = CKinvTypId ;
            //***  Begin Add  ***  06/15/22  *************
              svInvTypId = CKinvTypId ;
            //***  End   Add  ***  06/15/22  *************

C340    //*** End   Add *** 04/11/22 ***************************************
        //*** Begin Add *** 06/09/21 ***************************************
              Clear scEpMiStp;
              scEpMiSlv = *Zeros;

              Clear CKSepMiSt;
              CKSepMiSl = *Zeros;

              KeyWSCKL1SEC.CKSePMiCo# = CKco#;
              KeyWSCKL1SEC.CKSePMiAcc = CKacct;
              KeyWSCKL1SEC.CKSePMiId  = 'CHGMICR';
              Chain(n) %kds(KeyWSCKL1SEC) PCKS01;

              If %Found();
                 scEpMiStp1 = CKSePMiSt1;
                 scEpMiStp2 = CKSePMiSt2;
                 scEpMiStp3 = CKSePMiSt3;
                 scEpMiStp4 = CKSePMiSt4;
                 scEpMiStp5 = CKSePMiSt5;
                 scEpMiStp6 = CKSePMiSt6;
                 scEpMiStp7 = CKSePMiSt7;
                 scEpMiStp8 = CKSePMiSt8;
                 scEpMiStp9 = CKSePMiSt9;
                 scEpMiStpA = CKSePMiStA;
                 scEpMiSlv1 = CKSePMiSl1;
                 scEpMiSlv2 = CKSePMiSl2;
                 scEpMiSlv3 = CKSePMiSl3;
                 scEpMiSlv4 = CKSePMiSl4;
                 scEpMiSlv5 = CKSePMiSl5;
                 scEpMiSlv6 = CKSePMiSl6;
                 scEpMiSlv7 = CKSePMiSl7;
                 scEpMiSlv8 = CKSePMiSl8;
                 scEpMiSlv9 = CKSePMiSl9;
                 scEpMiSlvA = CKSePMiSlA;
              Endif;
        //*** End   Add *** 06/09/21 ***************************************
              Clear RTspad;
              Clear RTcmwc;
              Clear RTcmwp;
              Clear RTcmag;

              If  CKidsa <> *blanks;
                 KeyWSRTL.RTkey = CKidsa ;
              Else;
                 KeyWSRTL.RTkey = %Editc(CKidst : 'X');
              Endif;

              If  KeyWSRTL.RTkey = *blanks;
                 KeyWSRTL.RTkey = CKTRNS + CKBNK# + %Editc(CKckdg : 'X');
              Endif;

              KeyWSRTL.RTco# = intCO#;
              Chain(n) %kds(KeyWSRTL) PRT01;

              If %Found();
                 scSPAD = RTspad;
                 scCMWC = RTcmwc;
                 scCMWP = RTcmwp;
                 scCMAG = RTcmag;
              Endif;

              scEFT# = CKeft#;

              //Save selected field values
              SVAI01 = CKAI01 ;
              SVAI02 = CKAI02 ;
              SVAI03 = CKAI03 ;
              SVONUS = CKONUS ;
              SVCKDG = CKCKDG ;
              SVBNK# = CKBNK# ;
              SVTRNS = CKTRNS ;
              SVAXOU = CKAXOU ;
              SVRTUP = CKRTUP ;
              SVRTLW = CKRTLW ;
           Endif;
        Else;
           //*** Beg Add ***** 09/14/22 *******************
           hasEpaySv = hasEpay;
           //*** End Add ***** 09/14/22 *******************
           Clear RcdAddEdit;
           //*** Beg Add ***** 09/14/22 *******************
           hasEpay = hasEpaySv;
           //*** End Add ***** 09/14/22 *******************
           SetTabNames();
        Endif;

        //Signature selections
        scSvClsel = 'GTco# = ' + %char(intCo#) + ' and GTTBID = ''SCLS''' +
         ' and GTstat <> ''I''';
        scNam1Sel = 'SGco# = ' + %char(intCo#) + ' and SGstat <> ''I''';
        scNam2Sel = 'SGco# = ' + %char(intCo#) + ' and SGstat <> ''I''';

        Dou BtnCanAE = *on;
           FormatScreens();
           Select;
           When BtnSavAE = *on;
              Exsr ValBnkAcct;
              BtnSavAE = *off;
              If isValid = *on;
                 Exsr SavBnkAcct;
                 Leave;
              Else;
                scTabClick = ChkTabError();
              Endif;
           When BtnCanAE = *on;
              BtnCanAE = *off;
              //*** Beg Add ***** 09/14/22 *******************
              hasEpaySv = hasEpay;
              //*** End Add ***** 09/14/22 *******************
              Clear rcdAddEdit;
              //*** Beg Add ***** 09/14/22 *******************
              hasEpay = hasEpaySv;
              //*** End Add ***** 09/14/22 *******************
              Leave;
           Other;
              Exsr ValBnkAcct;
           Endsl;
        Enddo;

        BtnBaAdd = *Off;
        lnkBAedit = *Off;
       //*-----------------------------------------------------------------
       //* Edit EFT tab
       //*-----------------------------------------------------------------
       Begsr EditEFT;
        //Edit required fields.
        If scIDst <> *Zeros And scIDst <> *Blanks
         Or scBNam <> *Blanks
         Or scIOrg <> *Zeros And scIOrg <> *Blanks
         Or scCNam <> *Blanks
         Or scSvCl <> *Zero
         Or scOrDI <> *Zero
         Or scCeDs <> *Blanks
         Or scEftF <> *Blanks;
           isReqForEFT = *On;
        Else;
           isReqForEFT = *Off;
        Endif;

        If isReqForEFT = *On;
           //Immediate Destination
           If scIDST = *Zeros Or scIDST = *Blanks;
              erIDST = *On;
              msIDST = 'WCR3402';
              isValid = *Off;
              isTab1Err = '1';
              isValid = *Off;
              isTab1Err = '1';
           Else;
              If %Check(' 0123456789':scIDST) = 0;
              Else;
                 erIDST = *On;
                 msIDST = 'WCR1087';
                 isValid = *Off;
                 isTab1Err = '1';
                 isValid = *Off;
                 isTab1Err = '1';
              Endif;
           Endif;
           //Immediate Origin
           If scIORG = *Zeros or scIORG = *blanks;
              erIORG = *On;
              msIORG = 'WCR3402';
              isValid = *Off;
              isTab1Err = '1';
              isValid = *Off;
              isTab1Err = '1';
           Else;
              If %Check(' 0123456789':scIORG) = 0;
              Else;
                 erIORG = *On;
                 msIORG = 'WCR1087';
                 isValid = *Off;
                 isTab1Err = '1';
                 isValid = *Off;
                 isTab1Err = '1';
              Endif;
           Endif;
           //Destination
           If scBNAM = *blanks;
              erBNAM = *On;
              isValid = *Off;
              isTab1Err = '1';
              isValid = *Off;
              isTab1Err = '1';
           Endif;
           //Company Name or Immediate Origin Name
           If scCNAM = *Blanks;
              erCNAM = *On;
              isValid = *Off;
              isTab1Err = '1';
              LeaveSr;
              isValid = *Off;
              isTab1Err = '1';
           Endif;
           //Company Entered Description
           If scCEDS = *Blanks;
              erCEDS = *On;
              isValid = *Off;
              isTab1Err = '1';
              isValid = *Off;
              isTab1Err = '1';
           Endif;
           //Originating DFI
           If scORDI = *Zeros;
              erORDI = *On;
              isValid = *Off ;
              isTab1Err = '1';
              isValid = *Off ;
              isTab1Err = '1';
           Endif;
           //Service Class Code
           If svSVCL = 200 and scSVCL <> 200;
              Select;
              When scSVCL = 220;
                 strWarning = Warn220;
              When scSVCL = 225;
                 strWarning = Warn225;
              Endsl;
              isCancCont = PopTwoBtn(strWarning:'WARNING':'Cancel':'Continue');
              If isCancCont = *On;
                 isValid = *Off;
                 isTab1Err = '1';
              Endif;
           Endif;
           //EFT file Name
           If scEFTF <> *Blanks;
              SRCKOBJN(scEFTF:@@Flg);
              If @@Flg = '1';
                 msEFTF = 'Invalid File Name Entered';
                 erEFTF = *On;
                 isValid = *Off;
                 isTab1Err = '1';
              Endif;
           Else;
              erEFTF = *On;
              msEFTF = 'This field cannot be blank.';
              isValid = *Off;
              isTab1Err = '1';
           Endif;
        Endif;

        If isValid = *On And scEFTF <> *Blanks;
           Setll intCo# pck01;
           Reade intCo# pck01;
           Dow Not(%Eof) and isValid = *On;
              If scEftf = CKeftf and scAcct <> CKacct;
                 If scIdst = CKidsa;
                    isCancCont = PopTwoBtn('The EFT file name matches -
                     that of an existing "immediate Destination #". EFT -
                     transactions will be grouped together for submission -
                     to the bank.':
                     'EFT File Name Message':'Cancel':'Submit');
                    Select;
                    When isCancCont = *Off;
                       Leave;
                    When isCancCont = *On;
                       isValid = *Off;
                       isTab1Err = '1';
                    Endsl;
                 Else;
                    msEFTF ='The EFT file name must be unique -
                     based on the "Immediate Destination #". Please enter -
                     either a different name or the name that exists for -
                     this "Immediate Destination #".';
                    erEFTF = *On;
                    isValid = *Off;
                    isTab1Err = '1';
                 Endif;
              Endif;
              If scEftf <> CKeftf and scIdst = CKidsa and scAcct <> CKacct;
                 isCancCont = PopTwoBtn('The EFT file name does not match -
                  the file name for an account already set for the same -
                  "Immediate Destination #". EFT transactions will -
                  not be combined for submission to the bank. Click Sumbit -
                  to continue with the file name entered.':
                  'EFT File Name Warning':'Cancel':'Submit');
                 Select;
                 When isCancCont = *Off;
                    Leave;
                 When isCancCont = *On;
                    isValid = *Off;
                    isTab1Err = '1';
                 Endsl;
              Endif;
              Reade intCo# pck01;
           Enddo;
        Endif;
        //Intermim Days
        If scACID < *Zero;
           erACID = *On;
           isTab1Err = '1';
           isValid = *Off;
           isTab1Err = '1';
        Endif;
        //Lead Time
        If scBKLT < *Zero;
           erBKLT = *On;
           isValid = *Off;
           isTab1Err = '1';
        Endif;
        //Next Available Eft Sequence Number
        If scEFT# < *Zero;
           erEFT# = *On;
           isValid = *Off;
           isTab1Err = '1';
        Endif;
       Endsr;
       //*-----------------------------------------------------------------
       //* Validate Bank Account Master                                   *
       //*-----------------------------------------------------------------
       Begsr ValBnkAcct;
        //Reset error indicators
        isValid = *on;
        erACCT = *Off;
        erEFDT = *Off;
        erEFDT = *Off;
        erAI01 = *Off;
        erLCHK = *Off;
        erTRNS = *Off;
        erBNK# = *Off;
        erONUS = *Off;
        erRTUP = *Off;
        erRTLW = *Off;
        erEFT# = *Off;
        erIDST = *Off;
        erIORG = *Off;
        erBNAM = *Off;
        erCNAM = *Off;
        erCEDS = *Off;
        erORDI = *Off;
        erEFTF = *Off;
        erACID = *Off;
        erBKLT = *Off;
        erNxtEPay# = *Off;
        isTab0Err = *Off;
        isTab1Err = *Off;
        isTab2Err = *Off;
        //*** Begin  Add  *** 04/11/22 **********************************
        isTab3Err = *Off;
        msInvTyp  = ' ';
        erInvTyp  = *Off;
        //*** End    Add  *** 04/11/22 **********************************
        //*** Begin Add *** 06/09/21 **************************************
        Reset @ErStp;
        Reset @ErSlv;
        //*** End   Add *** 06/09/21 **************************************

        //Validate Bank Information Tab

        //Add mode only - Bank Account must not exist
        If isEdit = *Off;
           Chain (intCo#:SCacct) PCK01;
           If %Found(WSCKL);
              msACCT = 'WC30065';
              isValid = *Off;
              erAcct = *On;
              isTab0Err = '1';
           Endif;
        Endif;

        //Required fields
        If scACCT = *blanks;
           msACCT = 'WCR0503';
           erAcct = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        //*** Begin Chg *** 05/19/22 ***************************************
        //***** If scEfDt =  DtZeroIso;
        If scEfDt = 0;
        //*** End   Chg *** 05/19/22 ***************************************
           msEFDT = 'WCR0556';
           erEfDt = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If scAI01 = *Blanks;
           msAI01 = 'WCR0503';
           erAI01 = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If SClchk <= *Zero;
           erLCHK = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If scTRNS = *Blanks;
           msTRNS = 'WCR0503';
           erTRNS = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If scBNK# = *Blanks;
           msBNK# = 'WCR0503';
           erBNK# = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If scONUS = *Blanks;
           msONUS = 'WCR0503';
           erONUS = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If scRTUP = *Blanks;
           msRTUP = 'WCR0503';
           erRTUP = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If scRTLW = *Blanks;
           msRTLW = 'WCR0503';
           erRTLW = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        //*** Begin Chg *** 05/19/22 ***************************************
        //***** If scEfDt > scExDt and scExDt > DtZeroIso;
        If scEfDt > scExDt and scExDt > 0;
        //*** End   Chg *** 05/19/22 ***************************************
           msEFDT =  'WCR0702';
           erEfDt = *On;
           isValid = *Off;
           isTab0Err = '1';
        Endif;

        If isValid = *on And isEdit = *On;
           Setll (intCo#:scACCT) pbi01;

           Dou %Eof;
           Reade(n) (intCo#:scACCT) pbi01;
              If %Eof(WMBIL9);
                 Leave;
              Endif;

              If BIstat <> 'A';
                 Iter;
              Endif;

              //*** Begin Chg *** 05/19/22 ***************************************
              //***** If scExDt > DtZeroIso;
              //*****    intExDt70=  %dec(%char(%date(%char(scExDt):*iso):*cymd0):7:0);
              If scExDt > 0;
                 intExDt70= dtCYMD(scExDt);
              //*** End   Chg *** 05/19/22 ***************************************

                 If BIstat = 'A' and BIefdt < intExDt70 and BIacct = CKacct;
                    #2co#  = %Editc(BIco#:'X');
                    #2fnd  = %EditC(BIfnd:'X');
                    //*** Begin Chg *** 05/19/22 ***************************************
                    //***** #2srdt = %char(scExDt:*iso);
                    #2srdt = %editc((scExDt):'X');
                    //*** End   Chg *** 05/19/22 ***************************************
                    #2acct = GetBankAcc(#2co#: #2fnd: BIappl:
                    #2srdt: #2call: #2hdcy: #2chkq: #2ftyp: #2efdt:
                    #2cpyq: BIcktp: BIeftc: #2coid: #2rtcd);

                    If #2acct = BIacct;
                       msEXDT = 'You cannot expire the bank account with +
                        this expiration date. Active bank information +
                        records exist as of this date without another bank +
                        account to transition to.';
                       erEXDT = *On;
                       isValid = *Off;
                       isTab0Err = '1';
                       Leave;
                    Endif;

                 Endif;
              Endif;
           Enddo;

        Endif;

        //Validate EFT Tab
        If hasEFT;
           Exsr EditEFT;
        EndIf;

        //Validate EPay Tab
        //*** Begin Change *** 06/09/21 **********************************
        //If hasEPay And scEPayVdr <> *Blanks And scNxtEPay# < 1;
        If hasEPay;
           If scEPayVdr <> *Blanks And scNxtEPay# < 1;
        //*** End   Change *** 06/09/21 **********************************
              If isMoveEPay = *On;
                 erNxtEPay# = *On;
                 isValid = *Off;
                 isTab1Err = *On;
              Else;
                 erNxtEPay# = *On;
                 isValid = *Off;
                 isTab2Err = *On;
              Endif;
        //*** Begin  Add  *** 06/09/21 **********************************
           Endif;

           For intIdx = 1 to 10;
            Select;
            When @SecType(intIdx) = *blank and @SecLevel(intIdx) > *Zero;
               @ErStp(intIdx) = *On;
               @MsgIdTp(intIdx) = 'WCR0503';
               isValid = *Off;
            When @SecLevel(intIdx) <= *Zero and @SecType(intIdx) > *blank;
               @ErSlv(intIdx) = *On;
               @MsgIdLv(intIdx) = 'WCR0502';
               isValid = *Off;
            Endsl;
           EndFor;
        //*** End    Add  *** 06/09/21 **********************************
        //*** Begin  Add  *** 04/11/22 **********************************

           If scInvTypId <> 0;
              Setll intCo# pck01;
              Reade intCo# pck01;
              Dow Not(%Eof) and isValid = *On;
                 If scAcct <> CKAcct and CKInvTypID = scInvTypId ;
                    If CKExdt = 0 Or
                     CKExdt > %dec(%char(%date(%char(scExDt):*iso):*cymd0):7:0);
                      msInvTyp  = 'Invoice Type ID exists on bank account +
                          with overlapping active range.';
                       erInvTyp  = *On;
                       isValid = *Off;
                       isTab3Err = *On;
                       Leave;
                    EndIf;
                 EndIf;
                 Reade intCo# pck01;
              EndDo;
            //***  Begin Add  ***  06/15/22  *************
              If isValid and svInvTypId <> scinvTypId and svInvTypId <> 0;
               // Non-billing records {not in ('A','B','D','F','H')}
                 Exec sql
                  select count(*) into :decCount
                   from WMEAP
                   where EAinvTypId = :svInvTypId
                    and EAarTy not in ('A','B','D','F','H');
                 If decCount = *zero;
                    Exec sql
                     select count(*) into :decCount
                      from WDEAP
                      join WTTTP on TTco#=EAco# and TTkey=EAARCD
                      where EAinvTypId = :svInvTypId
                       and TTtype not in ('A','B','D','F','H');
                 Endif;
                 If decCount = *zero;
                    Exec sql
                     select count(*) into :decCount
                      from WDEAP_HLD
                      join WTTTP on TTco#=EAco# and TTkey=EAARCD
                      where EAinvTypId = :svInvTypId
                       and TTtype not in ('A','B','D','F','H');
                 Endif;
                 If decCount <> *zero;
                    PopOneBtn('A/R Payments exist with the previous ' +
                     'Invoice Type ID (' + %trim(%char(svInvTypId)) + '). ' +
                     'Manual reconcilliation may be required.'
                     :'A/R Payments Exist');
                 Endif;
              Endif;
            //***  End   Add  ***  06/15/22  *************
           EndIf;

        //*** End    Add  *** 04/11/22 **********************************
        Endif;

       Endsr;

       //*-----------------------------------------------------------------
       //* Save Bank Account Master Records                               *
       //*-----------------------------------------------------------------
       Begsr SavBnkAcct;
        If isEdit = *On;
           chain intRRN PCK02;
        Else;
           Clear PCK02;
        Endif;

        //Move fields here
        CKacct = scACCT;
        //*** Begin Chg *** 05/19/22 ***************************************
        //***** CKefdt = %dec(%char(%date(%char(scEfDt):*iso):*cymd0):7:0);
        //*****
        //***** If scExDt > DtZeroIso;
        //*****    CKexdt = %dec(%char(%date(%char(scExDt):*iso):*cymd0):7:0);
        CKefdt = dtCYMD(scEfDt);

        If scExDt > 0;
           CKexdt = dtCYMD(scExDt);
        //*** End   Chg *** 05/19/22 ***************************************
        Else;
           CKexdt = *Zeros;
        Endif;

C340    //*** Begin Add *** 04/11/22 ***************************************

        CKinvTypId = scInvTypId ;

C340    //*** Begin Add *** 04/11/22 ***************************************

        CKai01 = scAI01;
        CKlchk = scLCHK;
        CKai02 = scAI02;
        CKvchr = scVCHR;
        //*** Begin Add *** 12/23/21 *********************************
        If CKvchr = ' ';
           CKvchr = 'N';
        EndIf;
        //*** End   Add *** 12/23/21 *********************************

        If strCltId = '114';
           Select;
           When scVCHR# = 'V';
              CKvchr = 'Y';
           When scVCHR# = 'P';
              CKvchr = 'N';
           When scVCHR# = 'X';
              CKvchr = 'X';
           Other;
              CKvchr = scVCHR#;
           Endsl;
        Endif;

        CKai03 = scAI03;
        CKcrup = scCRUP;
        CKrpcd = scRPCD;
        CKrpvs = scRPVS;
        CKwpp  = scWPP;

        If CKwpp = *blank;
           CKwpp = 'N';
        Endif;

        CKrdcd = scRDCD;
        CKrdvs = scRDVS;
        CKwbk# = scWBK#;
        CKtrns = scTRNS;
        CKbnk# = scBNK#;
        CKckdg = scCKDG;
        CKaxou = scAXOU;
        CKonus = scONUS;
        CKrtup = scRTUP;
        CKrtlw = scRTLW;
        CKnam1 = scNAM1;
        CKnam2 = scNAM2;
        If scIDST =  *blanks;
           CKidst = *Zeros;
        Else;
           CKidst = %int(scIDST);
        Endif;
        CKidsa = scIDST;
        CKrfcd = scRFCD;
        CKbypn = scBYPN;
        If scIORG = *blanks;
           CKiorg = *Zeros;
        Else;
           CKiorg = %int(scIORG);
        Endif;
        CKiora = scIORG;
        CKordi = scORDI;
        CKbnam = scBNAM;
        CKsvcl = scSVCL;
        CKconm = scCNAM;
        CKeftf = scEFTF;
        CKceds = scCEDS;
        CKacid = scACID;
        CKeft# = scEFT#;
        CKbklt = scBKLT;
        CKefsp = scEFSP;
        CKePayVN = scEPayVdr;
        CKePay# = scNxtEPay#;
        //*** Beg Add ***** 09/14/22 *******************
        CKOvrEPay = scOvrEPay;
        //*** End Add ***** 09/14/22 *******************
        //*** Beg Add ***** 03/17/25 *******************
        CKPad9 = scPad9;
        //*** End Add ***** 03/17/25 *******************


        //Change Info
        CKuser = Q1User;
        CKpgm  = Q1Pgm;
        CKtime = %Dec(%Time);
        CKDt   = DateTo7(%Date);

        If isEdit = *On;
           //Update Bank Account Mater
           Update PCK02;

           //Update MICR information
           If scAI01 <> svai01 or scAI02 <> svAI02 or scAI03 <> svAI03 or
            scONUS <> svONUS or scCKDG <> svCKDG or scBNK# <> svBNK# or
            scTRNS <> svTRNS or scAXOU <> svAXOU or scRTUP <> svRTUP or
            scRTLW <> svRTLW;
              Setll (CKco#:CKacct) PBI01;
              Reade (CKco#:CKacct) PBI01;

              Dow not %eof(WMBIL9);
                 BIAI01 = CKAI01 ;
                 BIAI02 = CKAI02 ;
                 BIAI03 = CKAI03 ;
                 BIONUS = CKONUS ;
                 BICKDG = %editc(CKCKDG:'X');
                 BIBNK# = CKBNK# ;
                 BITRNS = CKTRNS ;
                 BIAXOU = CKAXOU ;
                 BIRTUP = CKRTUP ;
                 BIRTLW = CKRTLW ;
                 Update PBI01;
                 Reade (CKco#:CKacct) PBI01;
              Enddo;

           Endif;
        Else;
           CKco# = intCo#;
           //Write Bank Account Master
           Write PCK02;
           Feod WSCKP;
        Endif;

        //Process Bank Routing
        Clear PRT01;
        strRTKY = CKidsa;

        If  strRTKY = *blanks;
           strRTKY = CKTRNS + CKBNK# + %Editc(CKckdg : 'X');
        Endif;

        Chain (CKco#:strRTKY) PRT01;
        RTco#  = intCo#;
        RTspad = SCspad;
        RTcmwc = SCcmwc;
        RTcmwp = SCcmwp;
        RTcmag = SCcmag;
        RTuser = CKuser;
        RTpgm  = CKpgm;
        RTdt   = CKdt;
        RTtime = CKtime;

        If %found(WSRTL);
           Update PRT01;
        Else;
           RTco#  = intCo#;
           RTkey  = strRTKY;
           RTiusr = RTuser;
           RTipgm = RTpgm;
           RTidt  = RTdt;
           RTitm  = RTtime;
           Write  PRT01;
           FEOD   WSRTL;
        Endif;

        //*** Begin Add *** 06/09/21 ***************************************
        Clear PCKS01;
        intCKSidx = *Zeros;

        KeyWSCKL1SEC.CKSePMiCo# = CKco#;
        KeyWSCKL1SEC.CKSePMiAcc = CKacct;
        KeyWSCKL1SEC.CKSePMiId  = 'CHGMICR';
        Chain %kds(KeyWSCKL1SEC) PCKS01;

        For intIdx = 1 to 10;
           If @SecType(intIdx) <> *blank;
              intCKSidx = intCKSidx + 1;
              @CKStype(intCKSidx)  = @SecType(intIdx);
              @CKSlevel(intCKSidx) = @SecLevel(intIdx);
           Endif;
        EndFor;

        If %found(WSCKL1SEC);
           Update PCKS01;
        Else;
           CKSePMiCo# = CKco#;
           CKSePMiAcc = CKacct;
           CKSePMiId  = 'CHGMICR';
           Write  PCKS01;
           FEOD   WSCKL1SEC;
        Endif;
        //*** End   Add *** 06/09/21 ***************************************
       Endsr;
       End-Proc;

       //*----------------------------------------------------------------
       //* Format Screens                                                *
       //*----------------------------------------------------------------
       Dcl-Proc FormatScreens;
        Dcl-Pi *N;
        End-Pi;

        If strCltId = '114';          //Pending adding VISWPP to BOTH fields on screen.
           visWPP = *On;
        Else;
           visWPP = *Off;
        Endif;

        TabHeight = 960;
        ApnlHeight = TabHeight+120;
        Write RcdComPnl;
        Write RcdTabBnkA ;

        Select;
        When BtnBaAdd = *On;
           ExFmt rcdAddEdit;
           Read RcdComPnl;
           Read RcdTabBnkA;
        When LnkBaEdit = *On;
           ExFmt rcdAddEdit;
           Read RcdComPnl;
           Read RcdTabBnkA;
        Other;
           Exfmt RcdBnkAcct;
           Read RcdComPnl;
           Read RcdTabBnkA;
        Endsl;

        scTabClick = *blank;
       End-Proc;

       //*----------------------------------------------------------------
       //* Meet Selection Criteria                                       *
       //*----------------------------------------------------------------
       Dcl-Proc MeetSel;
        Dcl-Pi *N ind;
        End-Pi;

        Dcl-S isMeet ind;
        Dcl-S isMeet1 ind ;
        Dcl-S isMeet2 ind;
        Dcl-S isMeet3 ind;
        Dcl-S intPos packed(5:0);

        isMeet = *On;
        isMeet1 = *On;
        isMeet2 = *On;
        isMeet3 = *On;

        If scFltBaNam <> *blank;
            ckai01 = %xlate(Lower:Upper:ckai01);
            intPos = %scan(%trim(scFltBaNam):CKai01);
            If intPos = 0;
               isMeet1 = *Off;
            Endif;
        Endif;

        If scFltBaNo <> *blank;
           If ckAcct <> scFltBaNo;
              isMeet2 = *Off;
           Endif;
        Endif;

        If scFltAgrp <> *blank;
           Setll (CKco#:CKacct) PBI01;
           If %Equal(wmbil9);
               Reade(n) (CKco#:CKacct) PBI01;
               Dow not %eof(wmbil9);
                  If %scan(%editc(BIfnd:'X'):scFltAgrp) = 0;
                     isMeet3 = *Off;
                  Else;
                     isMeet3 = *On;
                     Leave;
                  Endif;
                  Reade(n) (CKco#:CKacct) PBI01;
               Enddo;
           Else;
              isMeet3 = *Off;
           Endif;
        Endif;

        If isMeet1 = *Off or isMeet2 = *Off or isMeet3 = *Off;
           isMeet = *Off;
        Endif;

        Return isMeet;
       End-Proc;

       //*----------------------------------------------------------------
       //* Set Tab Names                                                 *
       //*----------------------------------------------------------------
       Dcl-Proc SetTabNames;
        Dcl-Pi *N;
        End-Pi;

        Dcl-S strHeader Char(80);

        strHeader = 'Bank Account Information';
C340    //*** Begin Add *** 07/29/25 ***************************************
        isInvType = IsModEnabled('S/R Online') or
                    IsModEnabled('Third Party Payment') ;
C340    //*** End   Add *** 07/29/25 ***************************************

        If hasEFT;
           strHeader = %Trim(strHeader) + ', EFT Information';
           isEFT = *On;
        EndIf;

C340    //*** Begin Chg *** 07/29/25 ***************************************
        //*** If hasEPay;
        If hasEPay Or isInvType = *On;
C340    //*** Begin chg *** 07/29/25 ***************************************
           strHeader = %Trim(strHeader) + ', E-Pay Information';
           isEPay = *On;
C340    //*** Begin Add *** 04/11/22 ***************************************
C340    //*** Begin Del *** 07/29/25 ***************************************
        //**********   isInvType = IsModEnabled('S/R Online') or
        //**********   IsModEnabled('Third Party Payment') ;
C340    //*** End   Del *** 07/29/25 ***************************************
              scInvTypLb = 'Invoice Type ID (' +
               GetModuleProperty('Invoice Cloud':'Name') + ')';
C340    //*** End   Add *** 04/11/22 ***************************************
        EndIf;

        tabHeader1 = strHeader;
        tabHeader2 = strHeader;
       End-Proc;

       //*-----------------------------------------------------------------
       //* Load Group Selections                                          *
       //*-----------------------------------------------------------------
       Dcl-Proc LoadGrpSel;
        Dcl-Pi *N char(500);
        End-Pi;

        Dcl-S strGrpSel  char(400);
        Dcl-S intLastComma packed(3:0);

        strGrpSel = *blanks;
        Setll *Loval wsckl;
        Read wsckl;

        Dow not %eof(wsckl);
           Setll (CKco#:CKacct) PBI01;
           If %Equal(wmbil9);
               Reade(n) (CKco#:CKacct) PBI01;
               Dow not %eof(wmbil9);
                  If %scan(%char(BIfnd):strGrpSel) = 0;
                     strGrpSel = %trim(strGrpSel) + %char(BIfnd) + ',';
                  Endif;
                  Reade(n) (CKco#:CKacct) PBI01;
               Enddo;
           Endif;
           Read wsckl;
        Enddo;

        //Remove last comma
        intLastComma = %checkr(' ':strGrpSel);
        If intLastComma > *Zero;
           %Subst(strGrpSel:IntLastComma) = ' ';
        Endif;

        strGrpSel = 'FNstat <> ''I'' and ' +
         'FNfnd IN(' + %trim(strGrpSel) + ')';
        Return strGrpSel;
       End-Proc;
       //*----------------------------------------------------------------
       //* Check for Tabs in Error                                       *
       //*----------------------------------------------------------------
       Dcl-Proc ChkTabError;
        Dcl-Pi *N char(1);
        End-Pi;

        Dcl-S strTabClick char(1);

        strTabClick = *blank;
        Select;
        When isTab0Err = *On;
           tabIndex = 0;
        When isTab1Err = *On;
           tabIndex = 1;
        When isTab2Err = *On;
           tabIndex = 2;
        //*** Begin  Add  *** 04/11/22 **********************************
        When isTab3Err = *On;
           tabIndex = 3;
        //*** End    Add  *** 04/11/22 **********************************
        Endsl;

        Return strTabClick;
        End-Proc;
