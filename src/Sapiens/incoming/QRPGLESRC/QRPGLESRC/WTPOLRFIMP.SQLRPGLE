     hCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H*Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title WTPOLRFIMP - Upload Rating Factors
     H Bnddir('WTBNDDIR')
     H*===============================================================
     H* Created:
     H*
     H* 07/07/20 C: 079 Project: 148WBU - Upload Rating Factors
     H*===============================================================
     H* Revisions
     H*
     H* 07/31/24  C:291 Support: PHW117126
     H*                 Only Auto attach Waiver form if Waiver Information
     H*                 checkbox is checked.
     H*
     H* 05/08/24  C:344 Support: PHW115560
     H*                 Default all 'Upload Factor' checkboxes so that
     H*                 each checkbox is cleared/unchecked regardless of
     H*                 of Client#.
     H*
     H* 05/24/23  C:333 Project: 148WEC
     H*                 Auto Attach Waiver Form
     H*
     H* 03/28/23  C:291 Project: 150WFV
     H*                 Add Upload of Large Deductible Fields:
     H*                 Percentage, Aggregate Amount and Plan Type.
     H*                 Fixed Experience Mod edits.
     H*
     H* 06/27/22  C:291 Project: 148WDA
     H*                 Add Upload of Waiver Information: Waiver Type,
     H*                 Percentage, Flat Charge, Min Charge, Max Charge,
     H*                 Apply to Prm level, Blanket waiver Description.
     H*
     H* 04/26/22  C:33  Support: 102070 {no comments}
     H*                 Remove comparisons of numeric fields to blank
     H*
     H* 07/19/21 C: 180 Project: 150WFJ - Add optional upload of seven
     H*          C:33            rating factors: Disc %, Incr Limits,
     H*                          Subj Prm Cr 1A and 1B, Small and Large
     H*                          Deductibles, and % of Manual.
     H*
     H* 02/25/21 C: 180 Project: 148WCG - Add optional upload of
     H*                                   rating factors.
     H*
     H*===============================================================
     fwtpolrfimpcf   e             workstn extfile('WTPOLRFIMP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflRfErr:rrnRatFac)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwdell2    if   e           k disk
     fwdrrl     if   e           k disk
       Dcl-F Wmrsl  disk usage(*input) keyed;
       Dcl-F Wmdsl3 disk usage(*input) keyed;
       Dcl-F Wtlml1 disk usage(*input) keyed;
       Dcl-F Wtabl  disk usage(*input) keyed include(Pbs01);
       Dcl-F Wtddl2 disk usage(*input) keyed rename(Pdd01:Pdd02);
       Dcl-F Wmldl1 disk usage(*input) keyed;
       //*** Beg Add ***** 03/28/23 ***********************************
       Dcl-F wtgtl  disk usage(*input) keyed rename(pgt01:pgt00);
       //*** End Add ***** 03/28/23 ***********************************
       //*** Beg Add ***** 05/24/23 ***********************************
       Dcl-F wtenl2 disk usage(*input) keyed rename(pen01:pen02);
       Dcl-F wtesl  disk usage(*input) keyed ;
       Dcl-F wmwvl4 disk usage(*input) keyed rename(pwv01:pwv04);
       Dcl-F ststl1 disk usage(*input) keyed ;
       Dcl-F wmmul2 disk usage(*input:*output) keyed ;
       Dcl-F WMSHL1 disk usage(*input) keyed ;
       Dcl-F wmdxl disk usage(*input) keyed rename(pdx01:pdx00);
     fwmenl6    uf a e           k disk    rename(pen01:pen06)
     f                                     PREFIX(a_)
       Dcl-F wdf2l disk usage(*input) keyed rename(pf201:pf200);
       Dcl-F wmeml disk usage(*input) keyed;
      *
       //*** End Add ***** 05/24/23 ***********************************
      //===================================================================
      // Prototypes
      //===================================================================
     *Entry
     d RFIMP           pr                  Extpgm('WTPOLRFIMP')
     d RFIMP           pi

     d ChkDlo          pr                  extPgm('CHKIFSOBJ')
     d  docName                     256    const
     d  error                          n

     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d   root                        10

     d RtvSuffix       pr                  ExtPgm('SA030R')
     d   suffix                       3

     d RtvCltId        pr                  ExtPgm('RTVCLTID')
     d  strCltId                      3

     d ClearError      pr
     d ExfmtScreens    pr
     d Initialize      pr
     d MainProcess     pr
     d ProgramStart    pr
     d SetCriteria     pr
     d Validate        pr              n
     d
      //*** Beg Add ***** 05/24/23 ***********************************
      *
     d GetCarrier#     pr             5s 0
      *
     d GetMiscDartCd   pr             5a
      *
     d WriteWMENL6     pr
      *
     d ProcessMidterm  pr
      *
     d SbmtMidTrmJob   pr                  Extpgm('W7050C1')
     d  outCo#                        3    const
     d  outFnd                        3    const
     d  outEmp#                       9    const
     d  outDiv                        5    const
     d  outFyr                        3    const
     d  outJob#                       6    const
     d  outEnd#                       5    const
     d  outQueue                     10    const
      *
      //*** End Add ***** 05/24/23 ***********************************
     d/copy sCopyPr01d
     d/copy sPrCompany
     d/copy wPrUsrprms
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrLetter
     d/copy sPrXML
     d/copy sPrMessage
     d/copy sPrRunCmd
     d/copy sPrSQL
      //*** Beg Add ***** 06/27/22 ******************************
     d/copy wPrEndorse
     d/copy wPrAgent
     d/copy wPrGroup
      //*** End Add ***** 06/27/22 ******************************
      //===================================================================
      // Fields
      //===================================================================
     d fullPathName    s            256    Varying
     d length          s              3s 0
     d isUseScript     s              1n   inz
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d strReturn       s                   Like(Upoutq)
     d strConfirmMsg   s             80
     d strRunCmd       s            500
     d suffix          s              3
     d numErrors       s              9s 0
     d msgText         s           1024a
     d PopMsg          s            256
     d PopTitle        s             50
     d xxDtCyMd        s              7s 0
     d xxDtCyMd2       s              7s 0
     d SysDt7          s              7s 0
     d xxCo#           s              3s 0
     d isErrorFactors  s              1n
     d ArcLibrary      s             10a
     d TempLibrary     s             10a
     d sys_date        s               D   datfmt(*USA)
     d isValidFormat   s              1n
     d msg2ndLevel     s           4096a
     d pattern         s             25a
     d charNum         s              1a
     d result          s              3s 0
     d charRecNotCopy  s              7a
      *
     d xxDtCyMd3       s              7s 0
      *** Beg Chg ***** 06/27/22 ***************************
     d*** strSql          s           2700a   inz
     d strSql          s           4000a   inz
      *** Beg Chg ***** 06/27/22 ***************************
     d xstrSqlSet      s            500a   inz
      *** Beg Chg ***** 06/27/22 ***************************
     d** ystrSqlSel      s           2000a   inz
     d ystrSqlSel      s           4000a   inz
      *** Beg Chg ***** 06/27/22 ***************************
     d dtZero          s               d
     disCheckboxDft    s              1n   inz('1')
     d strCltId        s              3a   inz
      *** Beg Add ***** 03/28/23 ***********************************
     d xxTBID          s              4a   inz
     d xxKey           s             10a   inz
      *** End Add ***** 03/28/23 ***********************************
      *** Beg Chg ***** 05/24/23 ***********************************
     d*** processType     c                   const('PEO Payroll Import')
     d processType     c                   const('Mid-Term Endorsement')
     d xxCarrier#      s              5s 0
     d xxBfLn          s             50    Dim(5)                               Before line
     d xxAfLn          s             50    Dim(5)                               After line
     d I               s              3  0
      *** End Chg ***** 05/24/23 ***********************************
      *
      //===================================================================
      // Data Structures
      //===================================================================
     d pathNameDta     ds                  dtaara('WTPOLRFIMP')
     d  lastPathName           1    256
     d dsMsgInfo       ds                  Likeds(dsRtnMsgInfo)
     d dsInMsgInfo     ds                  Likeds(dsRtvMsgInfo)
     d dsRateFactor    ds                  Inz  Qualified
     d  Fnd                           3s 0
     d  Emp#                          9s 0
     d  Div                           5s 0
     d  PfDt                           d
     d  State                         2a
     d  ExM1                          5s 4
     d  EmD1                           d
     d  ExM2                          5s 4
     d  EmD2                           d
     d  ExM3                          5s 4                                      //Begin Add 02/25/21
     d  EmD3                           d                                        //End Add 02/25/21
     d  DbCr                          5s 4
     d  SdEf                           d
     d  AsP2                          5s 2
     d  DvEf                           d
     d  Dev2                          5s 2
     d  D2Ef                           d
     d  AsP3                          5s 2
     d  Disc                          1a
     d  AsPm                          5s 2
     d  McEf                           d
     d  AsPs                          5s 2
     d  ScEf                           d
     d  IlCd                          2a
     d  IlEf                           d
     d  DcDe                          3a
     d  DcEf                           d
     d  AmP1                          9s 0
      //*** Beg Add ***** 06/27/22 ************************
     d  WvTyp                         1a
     d  WvPct                         5s 2
     d  WvFC                         11s 2
     d  WvMnC                         9s 2
     d  WvMxC                         9s 2
     d  WvPLv                         1a
     d  WvBDs                      1000a
      //*** End Add ***** 06/27/22 ************************
      //*** Beg Add ***** 03/28/23 ************************
     d  LgDePct                      12s 9
     d  LgDeAggAmt                    9s 0
     d  LgDePlnTyp                    2a
      //*** End Add ***** 03/28/23 ************************
      //*** Beg Add ***** 05/24/23 ************************
     d  WvEnEfDt                       d
     d  WvWav#                        5s 0
     d  WvSpName                     80a
     d  WvSpAdr1                     40a
     d  WvSpAdr2                     40a
     d  WvSpCty                      25a
     d  WvSpSt                        2a
     d  WvSpZip                       9s 0
     d  WvSpEstPrm                   11s 2
     d  WvSpAudPrm                   11s 2
      //*** End Add ***** 05/24/23 ************************
     d  ManP                          5s 2

      //*** Beg Add ***** 05/24/23 ************************
     d xxMiscDartsCd   s              5a
     d strEnForm       s                   Like(ENform)
     d strEnSt         s                   Like(ENst)
     d xxSt            s                   Like(ENst)
     d xxupd           s              1n
     d dt              s             10a
     d xxdt            s              8s 0
     d xxtemefdt       s              7s 0
     d  xxenform       s             15a
     d  xxenst         s              2a
     d  xxfyr          s              3s 0
     d  xxmax_seq      s              5s 0
     d  xxmax_end#     s              5s 0
     d  xxmax_wav#     s              5s 0
     d xxUserOutQ      s             10
      //*** End Add ***** 05/24/23 ************************

      //*** Beg Chg ***** 06/27/22 ************************
     d**NullInds       s              5i 0 dim(29)
      //*** Beg Chg ***** 03/28/23 ************************
     d***  NullInds       s              5i 0 dim(36)
      //*** Beg Chg ***** 05/24/23 ************************
     d***  NullInds       s              5i 0 dim(39)
     d  NullInds       s              5i 0 dim(51)
      //*** End Chg ***** 05/24/23 ************************
      //*** End Chg ***** 03/28/23 ************************
      //*** Beg Chg ***** 06/27/22 ************************
     d  strDs          ds                  Inz
     d    str1                     1021a
     d    str2                     1021a
     d    str3                     1021a
     d    str4                     1021a
       Dcl-Ds str2Ds likeDs(strDs) ;
       Dcl-S  strDiscType char(1) inz;
       Dcl-Ds KeyWmrsl  LikeRec(Prs01:*Key);
       Dcl-Ds KeyWdrrl  LikeRec(Prr01:*Key);
       Dcl-Ds KeyWmdsl3 LikeRec(Pds01:*Key);
       Dcl-Ds KeyWtlml1 LikeRec(Plm01:*Key);
       Dcl-Ds KeyWtabl  LikeRec(Pbs01:*Key);
       Dcl-Ds KeyWtddl2 LikeRec(Pdd02:*Key);
      //*** Beg Add ***** 05/24/23 ************************
     d SVLDA           DS          1024    dtaara(*lda)
     d  LDEND#                 1      5  0
     d  LDENDA                 1      5
     d  LDMYRR                 6     14  0
     d  LDEEDT                15     22  0
     d  LDFYR                 23     25  0
     d  LDPYRL                26     26  0
     d  ldStSpcfFlg           40     40
     d  ldWaiverFrm           41     55
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d*xxPrint                        1    overlay(svlda:258)
     d*xxPrinter                     10    overlay(svlda:259)
     d*xxTray                        50    overlay(svlda:269)
     d*xx#CopiesA                     3    overlay(svlda:319)
     d*xxEmail                        1    overlay(svlda:322)
     d*xxToEmail                    200    overlay(svlda:323)
     d*xxSubject                    150    overlay(svlda:523)
     d*xxBody                       350    overlay(svlda:673)
     d xxBFseq#                       5  0 overlay(svlda:258)
     d xxAFseq#                       5  0 overlay(svlda:263)
     d xxBFDesc                     150    overlay(svlda:268)
     d xxAFDesc                     150    overlay(svlda:418)
      //*** End Add ***** 05/24/23 ************************
      //===================================================================
      // Main
      //===================================================================

      /Free
       ProgramStart();
       MainProcess();
       *InLr = *on;
      /End-Free
      //===================================================================
      // ClearError - Clear Error/Message fields
      //===================================================================
     p ClearError      b
     d ClearError      pi

      /Free
        isImpErr = *off;
        errPathNam = *off;
      /End-Free

     p ClearError      e
      //===================================================================
      //  clrRfError - Clear the Rating Factor Error Subfile
      //===================================================================
     p clrRfError      b
     d clrRfError      pi

      /Free
        // Clear the subfile
        rrnRatFac  = 0;
        dspRfErr   = *off;
        clrRfErr   = *on;
        dspRfErCtl = *off;
        Write rcdRFacImp;
      /End-Free

     p clrRfError      e
      //===================================================================
      // CrtTblFactors - Create the FACTORS Table using SQL
      //===================================================================
     p CrtTblFactors   b
     d CrtTblFactors   pi

      /Free
         Exec SQL create table QTEMP/FACTORS
         (FND  DEC ( 3, 0),
          EMP# DEC ( 9, 0),
          DIV  DEC ( 5, 0),
          PFDT DATE ,
          STATE CHAR ( 2),
          EXM1 DEC ( 5, 4),
          EMD1 DATE ,
          EXM2 DEC ( 5, 4),
          EMD2 DATE ,
          EXM3 DEC ( 5, 4),
          EMD3 DATE ,
          DBCR DEC ( 5, 4),
          SDEF DATE ,
          ASP2 DEC ( 5, 2),
          DVEF DATE ,
          DEV2 DEC ( 5, 2),
          D2EF DATE,
          ASP3 DEC ( 5, 2),
          DISC CHAR ( 1),
          ASPM DEC ( 5, 2),
          MCEF DATE,
          ASPS DEC ( 5, 2),
          SCEF DATE,
          ILCD CHAR ( 2),
          ILEF DATE,
          DCDE CHAR ( 3),
          DCEF DATE,
          AMP1 DEC ( 9, 0),
          WVTYP CHAR (1),                        //*** Beg Add ***** 06/27/22 ****************
          WVPCT DEC (5, 2),
          WVFC  DEC (11, 2),
          WVMNC DEC (9, 2),
          WVMXC DEC (9, 2),
          WVPLV CHAR (1),
          WVBDS CHAR (1000),                     //*** End Add ***** 06/27/22 ****************
          LGDEPCT DEC (12, 9),                   //*** Beg Add ***** 03/28/23 ****************
          LGDEAGGAMT DEC (9, 0),
          LGDEPLNTYP CHAR (2),                   //*** End Add ***** 03/28/23 ****************
          WVENEFDT DATE,                         //*** Beg Add ***** 05/24/23 ****************
          WVWAV# DEC (5, 0),
          WVSPNAME CHAR (80),
          WVSPADR1 CHAR (40),
          WVSPADR2 CHAR (40),
          WVSPCTY  CHAR (25),
          WVSPST   CHAR (2),
          WVSPZIP DEC (9, 0),
          WVSPESTPRM DEC (9, 2),
          WVSPAUDPRM DEC (9, 2),                 //*** End Add ***** 05/24/23 ****************
          MANP DEC ( 5, 2));
      /End-Free

     p CrtTblFactors   e
       //==================================================================
       // EdtTblFactors - Edit the table Factors
       //==================================================================
     p EdtTblFactors   b
     d EdtTblFactors   pi

     d intDtYrAgo      s              7P 0
       //*** Beg Add ***** 06/27/22 *************************
     d chrWvrForm      s             15
     d chrPrtPgm       s             10
     d chrAddPgm       s             10
     d chrWvBlkPrjDs   s            500
     d chrWvrType      s              1
     d intRtnDiv       s              5  0
     d chrWvrTypeVbg   s             20
     d strJrst         s              2
     d intDiv          s              5  0
     d divCount        s              5  0
     d divCountF       s              5  0
     d divCountC       s              5  0
       //*** End Add ***** 06/27/22 *************************

       // Removed Select statement - not needed. Just return if
       //  we don't have a valid WDELP or WDRRP.
       // Added read to WDRRP and WMRSP here so that we have just one
       //  read/chain per transaction

       // Validate the Employer
       Monitor;
          xxDtCyMd = Dateto7(dsRateFactor.PfDt);
       On-error;
          xxDtCyMd = 0;
       Endmon;
      // Return if policy not valid
       Chain (xxCo#:dsRateFactor.Fnd:dsRateFactor.Emp#:
        dsRateFactor.Div:xxDtCyMd) pel01;
       If Not %Found(WDELL2);
          isErrorFactors = *on;
          scErr = 'Employer Detail not found';
          LoadImpErrGrd();
          Return;
       Endif;

       // Get Rating and Statistics
       Setgt (xxCO#:dsRateFactor.Fnd:dsRateFactor.State:ELpFDt) prr01;
       Readpe (xxCO#:dsRateFactor.Fnd:dsRateFactor.State) prr01;
       If %Eof;
          isErrorFactors = *on;
          scErr = 'Rating and Statistics not set up for state.';
          LoadImpErrGrd();
          Return;
       Endif;
       // Get rating state
       keyWmrsl.RSco# = ELco#;
       keyWmrsl.RSrtcd = ELrtcd;
       Chain %Kds(KeyWmrsl) Prs01;

       // Experience Mod 1 - If entered then date must be present
       Monitor;
          xxDtCyMd = Dateto7(dsRateFactor.EmD1);
       On-error;
          xxDtCyMd = 0;
       Endmon;

       If dsRateFactor.ExM1 > 0 and xxDtCyMd = 0 and isExM1 = *On;
          isErrorFactors = *on;
          scErr = 'A valid effective date is required ' +
           'for Experience Mod 1.';
          LoadImpErrGrd();
       Endif;

       // Experience Mod 2 - If entered then date must be present
       Monitor;
          xxDtCyMd2 = Dateto7(dsRateFactor.EmD2);
       On-error;
          xxDtCyMd2 = 0;
       Endmon;

       If dsRateFactor.ExM2 > 0 and xxDtCyMd2 = 0 and isExM2 = *On;
          isErrorFactors = *on;
          scErr = 'A valid effective date is required ' +
           'for Experience Mod 2.';
          LoadImpErrGrd();
       Endif;

       // Experience Mod 3 - If entered then date must be present
       Monitor;
          xxDtCyMd3 = Dateto7(dsRateFactor.EmD3);
       On-error;
          xxDtCyMd3 = 0;
       Endmon;

       If dsRateFactor.ExM3 > 0 and xxDtCyMd3 = 0 and isExM3 = *On;
          isErrorFactors = *on;
          scErr = 'A valid effective date is required ' +
           'for Experience Mod 3.';
          LoadImpErrGrd();
       Endif;

       // Experience Mod 1 Effective Date must be within one year prior
       // or equal to Policy From Date;
       Monitor;
          xxDtCyMd = Dateto7(dsRateFactor.EmD1);
          //Subtract one year
          intDtYrAgo =
          %Dec(%Char(%Date(xxDtCyMd:*cymd) - %years(1):*cymd0):7:0);
       On-error;
          xxDtCyMd = 0;
       Endmon;

       If (xxDtCyMd > 0 and isExM1 = *On);
          If xxDtCymd > ELpfdt or xxDtCymd < intDtYrAgo;
             isErrorFactors = *on;
             scErr = 'Experience Mod 1 Eff. Date must be within one year ' +
              'prior or equal to Policy From Date';
             LoadImpErrGrd();
          Endif;
       Endif;

       // Experience Mod 2 Effective Date between policy from/to dates
       Monitor;
          xxDtCyMd2 = Dateto7(dsRateFactor.EmD2);
       On-error;
          xxDtCyMd2 = 0;
       Endmon;

       If (xxDtCyMd2 < ELpFDt or xxDtCyMd2 > ELpTDt) and
          (xxDtCyMd2 > 0 and isExM2 = *On);
          isErrorFactors = *on;
          scErr = 'Experience Mod 2 Eff. Date must be must be in ' +
           'policy date range.';
          LoadImpErrGrd();
       Endif;

       // Experience Mod 3 Effective Date between policy from/to dates
       Monitor;
          xxDtCyMd3 = Dateto7(dsRateFactor.EmD3);
       On-error;
          xxDtCyMd3 = 0;
       Endmon;

       If (xxDtCyMd3 < ELpFDt or xxDtCyMd3 > ELpTDt) and
          (xxDtCyMd3 > 0 and isExM3 = *On );
          isErrorFactors = *on;
          scErr = 'Experience Mod 3 Eff. Date must be must be in ' +
           'policy date range.';
          LoadImpErrGrd();
       Endif;

       // Prior Experience Mod(s) must be present
       //*** Beg Chg ***** 03/28/23 ************************************
       //***If dsRateFactor.ExM2 > 0 ;
       If dsRateFactor.ExM2 > 0 and isExM2 = '1';
       //*** End Chg ***** 03/28/23 ************************************
          If (dsRateFactor.ExM1 = 0 and isExM1 = '1') or
             (ELExM1 = 0 and isExM1 = '0');
             isErrorFactors = *on;
             scErr = 'Experience Mod 1 must be present when ' +
              'Experience Mod 2 is present';
             LoadImpErrGrd();
          Endif;
       Endif;

       //*** Beg Chg ***** 03/28/23 ************************************
       //***If dsRateFactor.ExM3 > 0;
       If dsRateFactor.ExM3 > 0 and isExM3 = '1';
       //*** End Chg ***** 03/28/23 ************************************
          If (dsRateFactor.ExM2 = 0 and isExM2 = '1') or
             (ELExM2 = 0 and isExM2 = '0');
             isErrorFactors = *on;
             scErr = 'Experience Mod 2 must be present when ' +
              'Experience Mod 3 is present';
             LoadImpErrGrd();
          Endif;
          If (dsRateFactor.ExM1 = 0 and isExM1 = '1') or
             (ELExM1 = 0 and isExM1 = '0');
             isErrorFactors = *on;
             scErr = 'Experience Mod 1 must be present when ' +
              'Experience Mod 3 is present';
             LoadImpErrGrd();
          Endif;
       Endif;

       // Experience Mod Eff Dates must be sequential
       // Experience Mod 2 Eff. Date must be after Experience Mod 1
       // Effective Date
       If isExM1 = *On;
          Monitor;
             xxDtCyMd = Dateto7(dsRateFactor.EmD1);
          On-error;
             xxDtCyMd = 0;
          Endmon;
       Else;
             xxDtCyMd = ELemd1;
       Endif;

       Monitor;
          xxDtCyMd2 = Dateto7(dsRateFactor.EmD2);
       On-error;
          xxDtCyMd2 = 0;
       Endmon;

       If (xxDtCyMd2 <= xxDtCyMd) and xxDtCyMd2 > 0 and isExM2 = *On;
          isErrorFactors = *on;
            scErr = 'Experience Mod 2 Eff. Date must be after ' +
             'Experience Mod 1 Eff. Date';
            LoadImpErrGrd();
       Endif;

       // Experience Mod 3 Eff. Date must be after Experience Mod 2
       // Effective Date
       If isExM2 = *On;
          Monitor;
             xxDtCyMd2 = Dateto7(dsRateFactor.EmD2);
          On-error;
             xxDtCyMd2 = 0;
          Endmon;
       Else;
          xxDtCyMd2 = ELemd2;
       Endif;

       Monitor;
          xxDtCyMd3 = Dateto7(dsRateFactor.EmD3);
       On-error;
          xxDtCyMd3 = 0;
       Endmon;

       If (xxDtCyMd3 <= xxDtCyMd2) and xxDtCyMd3 > 0 and isExM3 = *On;
          isErrorFactors = *on;
            scErr = 'Experience Mod 3 Eff. Date must be after ' +
             'Experience Mod 2 Eff. Date';
            LoadImpErrGrd();
       Endif;

       // Experience Mod 3 Eff. Date must be after Experience Mod 1
       // Effective Date
       If isExM1 = *On;
          Monitor;
             xxDtCyMd = Dateto7(dsRateFactor.EmD1);
          On-error;
             xxDtCyMd = 0;
          Endmon;
       Else;
             xxDtCyMd = ELemd1;
       Endif;

       Monitor;
          xxDtCyMd3 = Dateto7(dsRateFactor.EmD3);
       On-error;
          xxDtCyMd3 = 0;
       Endmon;

       If (xxDtCyMd3 <= xxDtCyMd) and xxDtCyMd3 > 0 and isExM3 = *On;
          isErrorFactors = *on;
            scErr = 'Experience Mod 3 Eff. Date must be after ' +
             'Experience Mod 1 Eff. Date';
            LoadImpErrGrd();
       Endif;


       // Standard Dev 1 - If entered then Deviation 1 heading must be present
       If dsRateFactor.AsP2 > 0 and isDev1 = *On;
        // Moved read to WDRRP to top MLD
          If (RRdv1H = *blanks or RRdv1c = *Zeros);
             If RRst <> 'NM';                     //Allow entry for NM
                isErrorFactors = *on;
                scErr = %Trim(RtvMsgD('WMESGG':'WC31339'));
                LoadImpErrGrd();
             Endif;
          Endif;
       Endif;

       // Standard Dev 1 Percent - If entered must be <= 100
       If dsRateFactor.AsP2 > 100 and isDev1 = *On;
          isErrorFactors = *on;
          scErr = 'Standard Dev 1 ' +
           %Trim(RtvMsgD('WMESGG':'WC30076'));
          LoadImpErrGrd();
       Endif;

       // Standard Dev 1 Percent - If entered then date must be present
       Monitor;
          xxDtCyMd = Dateto7(dsRateFactor.DvEf);
       On-error;
          xxDtCyMd = 0;
       Endmon;

       If dsRateFactor.AsP2 > 0 and xxDtCyMd = 0 and isDev1 = *On;
          isErrorFactors = *on;
            scErr = 'Standard Dev % 1 Eff. Date must be present ' +
             'when Standard Dev % 1 is present';
            LoadImpErrGrd();
       Endif;

       // Standard Dev 1 Effective Date between policy from/to dates
       Monitor;
          xxDtCyMd = Dateto7(dsRateFactor.DvEf);
       On-error;
          xxDtCyMd = 0;
       Endmon;

       If (xxDtCyMd < ELpFDt or xxDtCyMd > ELpTDt) and
          (xxDtCyMd > 0 and isDev1 = *On);
          isErrorFactors = *on;
          scErr = 'Standard Deviation 1 Eff. Date must be in ' +
           'policy date range.';
          LoadImpErrGrd();
       Endif;


       // Standard Dev 2 - If entered then Deviation 2 heading must be present
       If dsRateFactor.Dev2 > 0 and isDev2 = *On;
        // Moved read to WDRRP to top MLD
          If (RRdv2H = *blanks or RRdv2c = *Zeros);
             If strCltId = '025' or strCltId = '136';
              //Allow it for these clients
             Else;
                isErrorFactors = *on;
                scErr = %Trim(RtvMsgD('WMESGG':'WC31340'));
                LoadImpErrGrd();
             Endif;
          Endif;
       Endif;

       // Standard Dev 2 Percent - If entered must be <= 100
       If dsRateFactor.Dev2 > 100 and isDev2 = *On;
          isErrorFactors = *on;
          scErr = RtvMsgD('WMESGG':'WC30076');
          scErr = 'Standard Dev 2 ' +
           %Trim(RtvMsgD('WMESGG':'WC30076'));
          LoadImpErrGrd();
       Endif;

       // Standard Dev 2 Percent - If entered then date must be present
       Monitor;
          xxDtCyMd2 = Dateto7(dsRateFactor.D2Ef);
       On-error;
          xxDtCyMd2 = 0;
       Endmon;

       If dsRateFactor.Dev2 > 0 and xxDtCyMd2 = 0 and isDev2 = *On;
          isErrorFactors = *on;
            scErr = 'Standard Dev % 2 Eff. Date must be present ' +
             'when Standard Dev % 2 is present';
            LoadImpErrGrd();
       Endif;


       // Standard Dev 2 Effective Date between policy from/to dates
       Monitor;
          xxDtCyMd2 = Dateto7(dsRateFactor.D2Ef);
       On-error;
          xxDtCyMd2 = 0;
       Endmon;

       If (xxDtCyMd2 < ELpFDt or xxDtCyMd2 > ELpTDt) and
          (xxDtCyMd2 > 0 and isDev2 = *On);
          isErrorFactors = *on;
          scErr = 'Standard Deviation 2 Eff. Date must be in ' +
           'policy date range.';
          LoadImpErrGrd();
       Endif;


       // Schedule Debit/Credit must be > 0
       If dsRateFactor.DbCr < 0 and isDbCr = *On;
          isErrorFactors = *on;
            scErr = 'Schedule Debit/Credit can not be less than zero.';
            LoadImpErrGrd();
       Endif;

       // Schedule Debit/Credit edit against WDRRP
       If dsRateFactor.DbCr > 0 and isDbCr = *On;
          If dsRateFactor.DbCr < RRcMax and RRcMax <> 0 or
             dsRateFactor.DbCr > RRdMax and RRdMax <> 0;
             isErrorFactors = *on;
             scErr = %Trim(RtvMsgD('WMESGG':'WC40050'));
             LoadImpErrGrd();
          Endif;
          If RRdbcd = 0 ;
             isErrorFactors = *on;
             scErr = %Trim(RtvMsgD('WMESGG':'WC31344'));
             LoadImpErrGrd();
          Endif;
          If RRcrcd= 0 ;
             isErrorFactors = *on;
             scErr = %Trim(RtvMsgD('WMESGG':'WC31343'));
             LoadImpErrGrd();
          Endif;
       Endif;

       // If Schedule Debit/Credit entered then Schedule Debit
       // Effective Date must be entered
       Monitor;
          xxDtCyMd = Dateto7(dsRateFactor.SdEf);
       On-error;
          xxDtCyMd = 0;
       Endmon;

       If dsRateFactor.DbCr > 0 and xxDtCyMd = 0 and isDbCr = *On;
          isErrorFactors = *on;
            scErr = 'If Schedule Debit/Credit entered then ' +
             'Schedule Debit/Credit Eff Date must be present';
            LoadImpErrGrd();
       Endif;

       // Schedule Debit/Credit Effective Date between policy from/to dates
       Monitor;
          xxDtCyMd = Dateto7(dsRateFactor.SdEf);
       On-error;
          xxDtCyMd = 0;
       Endmon;

       If (xxDtCyMd < ELpFDt or xxDtCyMd > ELpTDt) and
          (xxDtCyMd > 0 and isDbCr = *On);

          isErrorFactors = *on;
          scErr = 'Schedule Credit/Debit Eff. Date must be ' +
           'policy date range.';
          LoadImpErrGrd();
       Endif;

       //Stock Discount
       If isDisc = *On;
          If dsRateFactor.AsP3 > *Zero and dsRateFactor.Disc > *blank;
             scErr = 'Discount % and Discount code are mutually exclusive.';
             LoadImpErrGrd();
          Endif;
          If dsRateFactor.AsP3 > 100;
             isErrorFactors = *On;
             scErr = 'Discount percentage cannot be greater than 100';
             LoadImpErrGrd();
          Endif;
          If dsRateFactor.Disc > *Blank;
             KeyWmdsl3.DSco#   = ELco#;
             KeyWmdsl3.DSfnd   = ELfnd;
             KeyWmdsl3.DSst    = RSjrst;
             KeyWmdsl3.DScode  = dsRateFactor.Disc;
             KeyWmdsl3.DSefdt  = ELpfdt;
             setgt %kds(KeyWmdsl3) Pds01;
             readpe %kds(KeyWmdsl3:4) Pds01;
             If %eof(wmdsl3);
                isErrorFactors = *On;
                scErr = 'Discount code is invalid.';
                LoadImpErrGrd();
             Endif;
          Endif;
       Endif;

       //Subject Premium 1A
       If isAsPm = *On;
        // Check if set up
          If RR1acd = *Zero and dsRateFactor.AsPm <> 0;
             isErrorFactors = *On;
             scErr = 'Rating and Statistics not set up for ' +
              'Subject Premium 1A.';
             LoadImpErrGrd();
          Endif;

          If dsRateFactor.AsPm < 0 Or dsRateFactor.AsPm > 100;
             isErrorFactors = *On;
             scErr = 'Subject Premium 1A percentage must be ' +
              'between 0 and 100.';
             LoadImpErrGrd();
          Endif;

          //Subject premium 1A Effective Date
          Select;
          When dsRateFactor.McEf = dtZero and dsRateFactor.AsPm <> 0;
             isErrorFactors = *On;
             scErr = 'Subject Premium 1A effective date must be entered.';
             LoadImpErrGrd();
          When dsRateFactor.McEf <> dtZero and dsRateFactor.AsPm = 0;
             isErrorFactors = *On;
             scErr = 'Subject Premium 1A effective date is invalid.';
             LoadImpErrGrd();
          When dsRateFactor.McEf <> dtZero;
             Monitor;
                xxDtCyMd = Dateto7(dsRateFactor.McEf);
             On-error;
                xxDtCyMd = 0;
                isErrorFactors = *On;
                scErr = 'Subject Premium 1A effective date is invalid.';
                LoadImpErrGrd();
             Endmon;
             //Must not be expired
             If xxDtCyMd > ELmcex and ELmcex > *Zero;
                isErrorFactors = *on;
                scErr = 'Subject Premium 1A is expired in ' +
                 'Policy Year Detail.';
             LoadImpErrGrd();
             Endif;
             //Subject Premium 1A Effective Date between policy from/to dates
             If (xxDtCyMd < ELpFDt or xxDtCyMd > ELpTDt) and xxDtCyMd > 0;
                isErrorFactors = *on;
                scErr = 'Subject Premium 1A Effective Date must be in ' +
                 'policy date range.';
                LoadImpErrGrd();
             Endif;
          Endsl;
       Endif;

       //Subject Premium 1B
       If isAsPs = *On;
         // Check if set up
          If RR1bcd = *Zero and dsRateFactor.AsPs <> 0;
             isErrorFactors = *On;
             scErr = 'Rating and Statistics not set up for ' +
              'Subject Premium 1B.';
             LoadImpErrGrd();
          Endif;

           If dsRateFactor.AsPs < 0 Or dsRateFactor.AsPs > 100;
             isErrorFactors = *On;
             scErr = 'Subject Premium 1B percentage must be ' +
              'between 0 and 100.';
             LoadImpErrGrd();
          Endif;

          //Subject premium 1B Effective Date
          Select;
          When dsRateFactor.ScEf = dtZero and dsRateFactor.AsPs <> 0;
             isErrorFactors = *On;
             scErr = 'Subject Premium 1B effective date must be entered.';
             LoadImpErrGrd();
          When dsRateFactor.ScEf <> dtZero and dsRateFactor.AsPs = 0;
             isErrorFactors = *On;
             scErr = 'Subject Premium 1B effective date is invalid.';
             LoadImpErrGrd();
          Other;
             Monitor;
                xxDtCyMd = Dateto7(dsRateFactor.ScEf);
             On-error;
                xxDtCyMd = 0;
                isErrorFactors = *On;
                scErr = 'Subject Premium 1B effective date is invalid.';
                LoadImpErrGrd();
             Endmon;
             //Must not be expired
             If xxDtCyMd > ELscex and ELscex > *Zero;
                isErrorFactors = *on;
                scErr = 'Subject Premium 1B is expired in ' +
                 'Policy Year Detail.';
                LoadImpErrGrd();
             Endif;
             //Subject Premium 1B Effective Date between policy from/to dates
             If (xxDtCyMd < ELpFDt or xxDtCyMd > ELpTDt) and xxDtCyMd > 0;
                isErrorFactors = *on;
                scErr = 'Subject Premium 1B effective date must be in ' +
                 'policy date range.';
                LoadImpErrGrd();
             Endif;
          Endsl;
       Endif;

       //Increased Limits
       If IsIlCd = *On;
          KeyWtlml1.LMco# = ELco#;
          KeyWtlml1.LMfnd = ELfnd;
          KeyWtlml1.LMst  = RSjrst;
          KeyWtlml1.LMkey = dsRateFactor.IlCd;
          KeyWtlml1.LMefdt = ELpfdt;
          Setgt %kds(KeyWtlml1) Plm01;
          Readpe %kds(KeyWtlml1:4) Plm01;
          Dow not %eof(Wtlml1) and LMstat = 'I';
             Readpe %kds(KeyWtlml1:4) Plm01;
          Enddo;
          If (%Eof(Wtlml1) and dsRateFactor.IlCd <> *blanks) or
           (dsRateFactor.IlEf <> dtZero and dsRateFactor.IlCd = *blanks);
             isErrorFactors = *On;
             scErr = 'Invalid Increased Limits code.';
             LoadImpErrGrd();
          Endif;

          //Increase limits Effective Date
          Select;
          When dsRateFactor.IlEf = dtZero and dsRateFactor.IlCd <> *blanks;
             isErrorFactors = *On;
             scErr = 'Increased Limits effective date is required.';
             LoadImpErrGrd();
          Other;
             Monitor;
                xxDtCyMd = Dateto7(dsRateFactor.IlEf);
             On-error;
                xxDtCyMd = 0;
                isErrorFactors = *On;
                scErr = 'Increased Limits effective date is invalid.';
                LoadImpErrGrd();
             Endmon;

             //Must not be expired
             If xxDtCyMd > ELilex and ELilex > *Zero;
                isErrorFactors = *On;
                scErr = 'Increased Limits is expired in policy year detail';
                LoadImpErrGrd();
             Endif;

             //Increased limits Effective Date between policy from/to dates
             If (xxDtCyMd < ELpFDt or xxDtCyMd > ELpTDt) and xxDtCyMd > 0;
                isErrorFactors = *on;
                scErr = 'Increased Limits effective date must be in ' +
                 'policy date range.';
                LoadImpErrGrd();
             Endif;
          Endsl;
       Endif;

       //Small Deductible
       If isDcde = *On;
          //Billing schedule code cannot be Large Deductible
          KeyWtabl.BSco# = ELco#;
          KeyWtabl.BSkey = %Editc(ELbscd:'X');
          chain %kds(KeyWtabl) Pbs01;
          Select;
          When BSlpgm = 'L'
           and (dsRateFactor.DcDe <> *blank or dsRateFactor.DcEf <> dtZero);
             isErrorFactors = *on;
             scErr = 'Billing plan cannot be flagged as Large Deductible' +
              ' to include Deductible';
             LoadImpErrGrd();
          Other;
             //Small deductible code must be active
             KeyWtddl2.DDco#  = ELco#;
             KeyWtddl2.DDfnd  = ELfnd;
             KeyWtddl2.DDst   = RSjrst;
             KeyWtddl2.DDcode = dsRateFactor.DcDe;
             KeyWtddl2.DDefdt = ELpfdt;
             Setgt %Kds(KeyWtddl2) Pdd02;
             Readpe %Kds(KeyWtddl2:4) Pdd02;
             Dow Not %Eof(Wtddl2) and DDstat = 'I';
                Readpe %Kds(KeyWtddl2:4) Pdd02;
             Enddo;
             If (%Eof(Wtddl2) and dsRateFactor.DcDe <> *blanks) or
              (dsRateFactor.DcEf <> dtZero and dsRateFactor.DcDe = *blanks);
                scErr = 'Invalid deductible code.';
                LoadImpErrGrd();
             Endif;

             //Small deductible Effective Date
             Select;
             When dsRateFactor.DcEf = dtZero and dsRateFactor.DcDe <> *blanks;
                isErrorFactors = *On;
                scErr = 'Deductible effective date is required.';
                LoadImpErrGrd();
             Other;
                Monitor;
                   xxDtCyMd = Dateto7(dsRateFactor.DcEf);
                On-error;
                   xxDtCyMd = 0;
                   isErrorFactors = *On;
                   scErr = 'Deductible effective date is invalid.';
                   LoadImpErrGrd();
                Endmon;
                //Must not be expired
                If xxDtCyMd > ELdcex and ELdcex > *Zero;
                   isErrorFactors = *On;
                   scErr = 'Deductible is expired in policy year detail.';
                   LoadImpErrGrd();
                Endif;
                //Small deductible Effective Date between policy from/to dates
                If (xxDtCyMd < ELpFDt or xxDtCyMd > ELpTDt) and xxDtCyMd > 0;
                   isErrorFactors = *on;
                   scErr = 'Small deductible effective date must be in ' +
                    'policy year date range.';
                   LoadImpErrGrd();
                Endif;
             Endsl;
          Endsl;
       Endif;

       //Large Deductible
       If isAmP1 = *On;
          Clear LDefD1;
          Chain (ELco#:Elfnd:Elemp#:Eldiv:Elfyr) Pld01;
          Select;
          When LDefD1 = *Zero and dsRateFactor.AmP1 <> 0;
             isErrorFactors = *On;
             scErr = 'Large deductible not set up on policy.';
             LoadImpErrGrd();
          When LDefD1 <> *zero and dsRateFactor.AmP1 = 0;
             isErrorFactors = *On;
             //*** Beg Chg ***** 03/28/23 ******************************
             //***scErr = 'Large deductible must be set up on policy.';
             scErr = 'Large deductible Amount must be between 0 and 999999999.';
             //*** End Chg ***** 03/28/23 ******************************
             LoadImpErrGrd();
          //*** Beg Add ***** 03/28/23 ******************************
          When LDefD1 <> *zero and
           (dsRateFactor.AmP1 < 0 or dsRateFactor.AmP1 > 999999999);
             isErrorFactors = *On;
             scErr = 'Large deductible Amount must be between 0 and 999999999.';
             LoadImpErrGrd();
          //*** End Add ***** 03/28/23 ******************************
          Endsl;
          //*** Beg Add ***** 03/28/23 ******************************
          //Billing Schedule code must be Large deductible
          KeyWtabl.BSco# = ELco#;
          KeyWtabl.BSkey = %Editc(ELbscd:'X');
          chain %kds(KeyWtabl) Pbs01;
          If BSlpgm <> 'L' and dsRateFactor.Amp1 > 0;
             isErrorFactors = *on;
             scErr = 'Billing plan must be Large Deductible' +
              ' to include Large Deductible Amount';
             LoadImpErrGrd();
          Endif;
          //*** End Add ***** 03/28/23 ******************************
       Endif;

       //*** Beg Add ***** 03/28/23 ******************************
       //Large Deductible Percentage
       If isPct1 = *On;
          Clear LDefD1;
          Clear LDplv1;
          Clear LDcls1;
          Chain (ELco#:Elfnd:Elemp#:Eldiv:Elfyr) Pld01;
          //Large Deductible Percentage must be between 0 and 100
          If dsRateFactor.LgDePct < 0 or dsRateFactor.LgDePct > 100;
             isErrorFactors = *On;
             scErr = 'Large Deductible Percentage must be between 0 and 100.';
             LoadImpErrGrd();
          Endif;
          //Billing Schedule code must be Large deductible
          KeyWtabl.BSco# = ELco#;
          KeyWtabl.BSkey = %Editc(ELbscd:'X');
          chain %kds(KeyWtabl) Pbs01;
          If BSlpgm <> 'L' and dsRateFactor.LgDePct > 0;
             isErrorFactors = *on;
             scErr = 'Billing plan must be Large Deductible' +
              ' to include Large Deductible Percentage';
             LoadImpErrGrd();
          Endif;
          //Large Deductible plan Effective date must be setup
          If LDefD1 = *Zero and dsRateFactor.LgDePct <> 0;
             isErrorFactors = *On;
             scErr = 'Large deductible plan must be setup on ' +
              'Policy to include Large Deductible Percentage';
             LoadImpErrGrd();
          Endif;
          //Large Deductible Billing Calculation level must be setup
          If dsRateFactor.LgDePct > 0 and LDplv1 = *Blanks;
             isErrorFactors = *On;
             scErr = 'Billing Calculation Level must be entered ' +
              'to include Large Deductible Percentage';
             LoadImpErrGrd();
          Endif;
          //Large Deductible Statistical code must be setup
          If dsRateFactor.LgDePct > 0 and LDcls1 = 0;
             isErrorFactors = *On;
             scErr = 'Statistical code must be entered ' +
              'to include Large Deductible Percentage';
             LoadImpErrGrd();
          Endif;
       Endif;

       //Aggregate Amount
       If isamG1 = *On;
          Clear LDefD1;
          Chain (ELco#:Elfnd:Elemp#:Eldiv:Elfyr) Pld01;
          //Large Deductible Aggregate Amount must be between 0 and 999,999,999
          If dsRateFactor.LgDeAggAmt < 0 or dsRateFactor.LgDeAggAmt > 999999999;
             isErrorFactors = *On;
             scErr = 'Large Deductible Aggregate Amount must be ' +
              'between 0 and 999999999.';
             LoadImpErrGrd();
          Endif;
          //Large Deductible plan Effective date must be setup
          If LDefD1 = *Zero and dsRateFactor.LgDeAggAmt <> 0;
             isErrorFactors = *On;
             scErr = 'Large deductible plan must be setup on ' +
              'Policy to include Aggregate Amount';
             LoadImpErrGrd();
          Endif;
          //Billing Schedule code must be Large deductible
          KeyWtabl.BSco# = ELco#;
          KeyWtabl.BSkey = %Editc(ELbscd:'X');
          chain %kds(KeyWtabl) Pbs01;
          If BSlpgm <> 'L' and dsRateFactor.LgDeAggAmt > 0;
             isErrorFactors = *on;
             scErr = 'Billing plan must be Large Deductible' +
              ' to include Large Deductible Aggregate Amount';
             LoadImpErrGrd();
          Endif;
       Endif;

       //Large Deductible Plan Type
       If isPln1 = *On;
          Clear LDefD1;
          Chain (ELco#:Elfnd:Elemp#:Eldiv:Elfyr) Pld01;
          xxTbid = '  DT';
          xxkey = dsRateFactor.LgDePlnTyp;
          Chain (ELco#:xxTbid:xxkey) pgt00;
          If Not %Found(WTGTL);
             isErrorFactors = *on;
             scErr = 'Large Deductible Plan Type is Not Valid';
             LoadImpErrGrd();
          Endif;
          //Large Deductible plan Effective date must be setup
          If LDefD1 = *Zero and dsRateFactor.LgDePlnTyp <> *Blanks;
             isErrorFactors = *On;
             scErr = 'Large deductible plan must be setup on ' +
              'Policy to include Large Deductible Plan Type';
             LoadImpErrGrd();
          Endif;
          //Billing Schedule code must be Large deductible
          KeyWtabl.BSco# = ELco#;
          KeyWtabl.BSkey = %Editc(ELbscd:'X');
          chain %kds(KeyWtabl) Pbs01;
          If BSlpgm <> 'L' and dsRateFactor.LgDePlnTyp <> *Blanks;
             isErrorFactors = *on;
             scErr = 'Billing plan must be Large Deductible' +
              ' to include Large Deductible Plan Type';
             LoadImpErrGrd();
          Endif;
       Endif;
       //*** End Add ***** 03/28/23 ******************************

       //% of Manual
       If isManP = *On;
          If dsRateFactor.ManP < 0 Or dsRateFactor.ManP > 100;
             isErrorFactors = *on;
             scErr = 'Percentage of Manual must be between 0 and 100.';
             LoadImpErrGrd();
          Endif;
          If dsRateFactor.ManP <> *Zero;
             Clear LDefD1;
             Chain (ELco#:Elfnd:Elemp#:Eldiv:Elfyr) Pld01;
             If LDefd1 = *Zero;
                isErrorFactors = *On;
                scErr = 'Large deductible must be set up for Percentage ' +
                 'of Manual.';
                LoadImpErrGrd();
             Endif;
          Endif;
       Endif;

       //*** Beg Add ***** 06/27/22 ******************************
       //Waiver Information
       If isWvInfo = *On;
          //Waiver Information update not allowed for States
          If RRWVAC = 0;
             isErrorFactors = *on;
             scErr = 'Waiver Informaton update is not allowed ' +
             'for ' + RSjrst + '.';
             LoadImpErrGrd();
          Else;
             //Waiver Type
             If dsRateFactor.WvTyp <> *Blank and dsRateFactor.WvTyp <> 'B' and
              dsRateFactor.WvTyp <> 'S';
                isErrorFactors = *on;
                scErr = 'Valid Values for Waiver Type are ''S'', ' +
                '''B'' or Blank.';
                LoadImpErrGrd();
             Else;
                If dsRateFactor.WvTyp <> *Blank;
                //Division must be eligible for Waiver
                   strJrst = RSjrst;
                   If GetWaiverInfo(ElCo#:ElFnd:strJrst:ELpFDt:chrWvrForm:
                    chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:ElFyr:ElEmp#:ElDiv)=*Off;
                      If %Subst(chrWvBlkPrjDs:495:6) <> 'IGNORE';
                         isErrorFactors = *on;
                         scErr = chrWvBlkPrjDs;
                         LoadImpErrGrd();
                      Endif;
                   Endif;
                   //All the Divisions must have same waiver type
                   Select;
                   When dsRateFactor.WvTyp = 'B';
                      chrWvrType = 'S';
                      chrWvrTypeVbg = 'specific waivers';
                   When dsRateFactor.WvTyp = 'S';
                      chrWvrType = 'B';
                      chrWvrTypeVbg = 'blanket waivers';
                   EndSl;
                   //All divisions in Spreadsheet must have same waiver type
                   Reset divCount;
                   Exec Sql Select count(*) Into :divCount
                            From QTEMP/FACTORS
                            Where Fnd =:dsRateFactor.Fnd and
                                  Emp#=:dsRateFactor.Emp# and
                                  PFdt=:dsRateFactor.PFdt and
                                  WVtyp=:chrWvrType;
                   If divCount > 0;
                      isErrorFactors = *on;
                      scErr = 'All Div must have same Waiver type. Other ' +
                      'Div in Spreadsheet are flagged for ' +
                      %Trim(chrWvrTypeVbg) + '.';
                      LoadImpErrGrd();
                   Else;
                      //Check if all the Divisions in the spreadsheet are changing Waiver type
                      //Count divisions with same waiver type in FACTORS
                      Exec Sql Select count(*) Into :divCountF
                             From QTEMP/FACTORS
                             Where Fnd =:dsRateFactor.Fnd and
                                   Emp#=:dsRateFactor.Emp# and
                                   PFdt=:dsRateFactor.PFdt and
                                   WVtyp=:dsRateFactor.WVtyp;
                      //Count total divisions on Policy year
                      Exec Sql Select count(*) Into :divCountC From WDELP
                               Where ELCo#=:ELco# and
                                     ELfnd =:dsRateFactor.Fnd and
                                     ELemp#=:dsRateFactor.Emp# and
                                     ELFyr=:ELfyr;
                      //If all the divisions in FACTORS are NOT changing to same Waiver Type
                      If divCountF <> divCountC and divCountF > 0;
                         strJrst = RSjrst;
                         Exec Sql Select Min(ELdiv) Into :intDiv
                                  From WDELP
                                  Join WMRSP On RSco# = ELco# And RSrtcd=ELrtcd
                                  Where ELco# = :xxCo# and ELfnd = :ELFnd and
                                       ELfyr = :ELFyr and ELemp# = :ELemp# and
                                       ELpfdt<> :ELptdt and ELwvtp=:chrWvrType
                                       and RSjrst = :strJrst and
                                       ELdiv <> :dsRateFactor.Div;
                         If IsValidFetch(sqlStt);
                            isErrorFactors = *on;
                            scErr = 'Other Divisions on this policy are' +
                            ' already flagged for ' + %Trim(chrWvrTypeVbg) +
                            ' in ' + RSjrst + '.';
                            LoadImpErrGrd();
                         Endif;
                      Endif;
                   Endif;
                   //If Waiver Type is 'B', Percentage or Flat Charge must be entered
                   If dsRateFactor.WvTyp = 'B' and dsRateFactor.WvPct = 0 and
                    dsRateFactor.WvFC = 0;
                      isErrorFactors = *on;
                      scErr = 'Percentage or Flat Charge must be entered if ' +
                              'Waiver Type is ''B''.';
                      LoadImpErrGrd();
                   Endif;
                Endif;
             Endif;
             //Waiver Percentage
             // Waiver percentage cannot be less than zero.
             If dsRateFactor.WvPct < 0;
                isErrorFactors = *on;
                scErr = 'Waiver percentage cannot be less than zero.';
                LoadImpErrGrd();
             Else;
                //Waiver Percentage is mutually exclusive with Flat Charge
                If dsRateFactor.WvPct > 0 and dsRateFactor.WvFc > 0;
                   isErrorFactors = *on;
                   scErr = 'Waiver Percentage is mutually exclusive with ' +
                    'Flat Charge.';
                   LoadImpErrGrd();
                Endif;
             Endif;
             // Waiver Flat Charge cannot be less than zero.
             If dsRateFactor.WvFc < 0;
                isErrorFactors = *on;
                scErr = 'Waiver Flat Charge cannot be less than zero.';
                LoadImpErrGrd();
             Endif;
             //Waiver Minimum Charge
             If dsRateFactor.WvMnC < 0;
                isErrorFactors = *on;
                scErr = 'Waiver Minimum Charge cannot be less than zero.';
                LoadImpErrGrd();
             Endif;
             //Waiver Maximum Charge
             If dsRateFactor.WvMxC < 0;
                isErrorFactors = *on;
                scErr = 'Waiver Maximum Charge cannot be less than zero.';
                LoadImpErrGrd();
             Endif;
             //Waiver Premium Level
             If dsRateFactor.WvTyp <> *Blank;
                If dsRateFactor.WvPlv <> 'J' and dsRateFactor.WvPlv <> 'E' and
                 dsRateFactor.WvPlv <> 'S' and dsRateFactor.WvPlv <> 'D' and
                 dsRateFactor.WvPlv <> 'M';
                   isErrorFactors = *on;
                   scErr = 'Premium level Flag must be set to either ''M'', ' +
                    '''J'', ''S'', ''E'' or ''D''.';
                   LoadImpErrGrd();
                Endif;
             Else;
                If dsRateFactor.WvPlv <> *Blank;
                   isErrorFactors = *on;
                   scErr = 'Premium Level must be blank if Waiver ' +
                    'Type is Blank.';
                   LoadImpErrGrd();
                Endif;
             Endif;
             //*** Beg Chg ***** 05/24/23 ***************************
             //Blanket Waiver Description must be Blank for Waiver type other than 'B'
             //***If dsRateFactor.WvTyp <> 'B' and dsRateFactor.WvBDs <> *Blank;
             //***   isErrorFactors = *on;
             //***   scErr = 'Waiver Description must be blank.';
             //***   LoadImpErrGrd();
             //***Endif;
             //Project/Job Description is required For Specific Waiver
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvBDs = *Blank;
                isErrorFactors = *on;
                scErr = 'Waiver Project/Job Description is required For '+
                        'Specific Waiver Type';
                LoadImpErrGrd();
             Endif;
             //*** End Chg ***** 05/24/23 ***************************
             //*** Beg Add ***** 05/24/23 ***************************

             //Waiver Endorsement Effective Date must be present
             If dsRateFactor.WvTyp <> ' ' and dsRateFactor.WvEnEfDt = dtZero;
                isErrorFactors = *on;
                scErr = 'Waiver Endorsement effective date cannot be zero when'+
                        ' Waiver Type is not blank';
                LoadImpErrGrd();
             Endif;

             //Validate Waiver Endorsement Effective data if entered
             If dsRateFactor.WvEnEfDt <> dtZero;
                Monitor;
                   xxDtCyMd = Dateto7(dsRateFactor.WvEnEfDt);
                On-error;
                   xxDtCyMd = 0;
                   isErrorFactors = *on;
                   scErr = 'Waiver Endorsement effective date is invalid.';
                   LoadImpErrGrd();
                Endmon;
             Endif;

             //Validate, waiver endorsement eff date is applicable for policy effective date.
             xxDtCyMd = Dateto7(dsRateFactor.WvEnEfDt);
             If (xxDtCyMd < ELpfDt or xxDtCyMd > ELptDt);
                isErrorFactors = *on;
                scErr = 'Waiver Endorsement Effective date must be between '+
                         'policy from Date and policy to date.';
                LoadImpErrGrd();
             Endif;

             //Edit for Specific Waiver Policy Rating State <> Waiver Address State
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.State
                                  <> dsRateFactor.WvSpSt;
                isErrorFactors = *On;
                scErr = 'Specific waiver state is invalid.';
                LoadImpErrGrd();
             Endif;

             //Edit for no. applicable endorsement form based on state/effective date.
             If xxDtCyMd > 0;
                // State forms
                xxSt = dsRateFactor.State;
                strEnForm = *blanks;
                strEnSt = *blanks;
                Setll (xxCo#:dsRateFactor.Fnd:xxSt) pen02;
                Reade (xxCo#:dsRateFactor.Fnd:xxSt) pen02;
                Dow not %eof(wtenl2);
                   If ENedDt <= ELpfDt and ENtype = 'Waiver';
                      strEnForm = EnForm;
                      strEnSt = EnSt;
                      Leave;
                   Endif;
                   Reade (xxCo#:dsRateFactor.Fnd:xxSt) pen02;
                Enddo;

                //National forms: Read through WTENP to see if a national record exists and check
                // WTESP for included states or excluded states.
                If strEnForm = *blanks;
                   isErrorFactors = *off;
                   Setll (xxco#:dsRateFactor.Fnd:*blanks) pen02;
                   Reade (xxco#:dsRateFactor.Fnd:*blanks) pen02;
                   Dow not %eof(wtenl2);
                      If enType = 'Waiver';
                         Chain (xxCo#:ENform:xxSt) pes01;
                         If %Found(wtesl);
                            If  not((ESapex ='A' And ESst <> xxSt) or
                             (ESapex ='E' and ESst = xxSt));
                               If ENedDt <= ELpfDt and ENtype = 'Waiver';
                                  strEnForm = Enform;
                                  strEnSt = EnSt;
                                  Leave;
                               Endif;
                            Endif;
                         Else;
                            strEnForm = Enform;
                            strEnSt = EnSt;
                            Leave;
                         Endif;
                      Endif;
                      Reade (xxco#:dsRateFactor.Fnd:*blanks) pen02;
                   Enddo;
                Endif;
             Endif;

             // If no form was found then apply the error
             If strEnForm = *blanks;
                isErrorFactors = *on;
                scErr = 'There is not a waiver endorsement form setup for '+
                         'this state/effective date';
                LoadImpErrGrd();
             Endif;

             //If Specific Waiver then sequence number must exist in the file
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvWav# > 0;
                Chain (xxCo#:dsRateFactor.Fnd:dsRateFactor.Emp#:
                       dsRateFactor.Div:dsRateFactor.WvWav#) pwv04;
                If not %found(Wmwvl4);
                   isErrorFactors = *on;
                   scErr = 'The specific waiver sequence number does not '+
                            'exist and cannot be updated.';
                   LoadImpErrGrd();
                Endif;
             Endif;

             //For Blanket Waivers if there is no default Project/Job Description setup for the
             //group they give an error.
             If dsRateFactor.WvTyp = 'B' and dsRateFactor.WvBDs = *Blank;
                Chain (xxCo#:dsRateFactor.Fnd:xxSt:strEnForm) pen02;
                If ENblkWvDs = *blank;
                   isErrorFactors = *on;
                   scErr = 'The blanket waiver description is blank and the'+
                            ' default description has not been setup for this '+
                            'waiver endorsement form.';
                   LoadImpErrGrd();
                Endif;
             Endif;
             //Specific Waiver Name
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpName = *blank;
                isErrorFactors = *on;
                scErr = 'The waiver name is required for specific waivers.';
                LoadImpErrGrd();
             Endif;

             // Specific Waiver Address
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpAdr1 = *blank;
                isErrorFactors = *on;
                scErr = 'The waiver address is required for specific waivers.';
                LoadImpErrGrd();
             Endif;
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpCty = *blank;
                isErrorFactors = *on;
                scErr = 'The waiver city is required for specific waivers.';
                LoadImpErrGrd();
             Endif;

             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpSt = *blank;
                isErrorFactors = *on;
                scErr = 'The waiver state is required for specific waivers.';
                LoadImpErrGrd();
             Endif;

             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpSt <> *blank;
                Chain (dsRateFactor.WvSpSt) pst01;
                If not %found(ststl1);
                   isErrorFactors = *on;
                   scErr = 'The waiver state is not a valid state.';
                   LoadImpErrGrd();
                Endif;
             Endif;

             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpZip = *zeros;
                isErrorFactors = *on;
                scErr = 'The waiver zip code is required for specific waivers.';
                LoadImpErrGrd();
             Endif;

             //Specific Waiver Estimated Premium - required if waiver based on percentage of
             // premium on the policy
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpEstPrm = *zeros
              and dsRateFactor.WvPct > 0;
                isErrorFactors = *on;
                scErr = 'Estimated waiver premium is required for specific '+
                        'waivers when the waiveris based on a percentage of '+
                        'premium on the policy.';
                LoadImpErrGrd();
             Endif;

             //Specific Waiver Audited Premium - required if waiver based on percentage of premium
             // on the policy
             If dsRateFactor.WvTyp = 'S' and dsRateFactor.WvSpAudPrm = *zeros
              and dsRateFactor.WvPct > 0;
                isErrorFactors = *on;
                scErr = 'Audited waiver premium is required for specific '+
                        'waivers when the waiver is based on a percentage of '+
                        'premium on the policy.';
                LoadImpErrGrd();
             Endif;

             //*** End Add ***** 05/24/23 ***************************
          Endif;
       Endif;
       //*** End Add ***** 06/27/22 ******************************

     p EdtTblFactors   e
      //===================================================================
      // ExfmtScreens- display screen
      //===================================================================
     p ExfmtScreens    b
     d ExfmtScreens    pi

      /Free
       Select;
       When TabIndex = 0;
          //*** Beg Chg ***** 06/27/22 ******************************
          //TabHeight = 620;
          //*** Beg Chg ***** 03/28/22 ******************************
          //***TabHeight = 670;
          //*** Beg Chg ***** 05/24/23 ******************************
          //***TabHeight = 730;
          TabHeight = 850;
          //*** End Chg ***** 05/24/23 ******************************
          //*** End Chg ***** 03/28/22 ******************************
          //*** End Chg ***** 06/27/22 ******************************
          APnlHeight = TabHeight + 40;
          Write rcdComPnl;
          Write RcdTabRfac;
          If scPathName = *blanks;
             In pathNameDta;
             scPathName = lastPathName;
          Endif;
          Exfmt RcdRfacImp;
          Read rcdComPnl;
          Read rcdTabRfac;
       EndSl;
      /End-Free

     p ExfmtScreens    e
      //===================================================================
     *// ImpFiletoTable - Import the Rating Factors File to table FACTORS
      //===================================================================
     p ImpFileToTable  b
     d ImpFileToTable  pi

      /Free
        isValidFormat = *on;
        strRunCmd = 'CPYFRMIMPF FROMSTMF(';
        strRunCmd = %Trim(strRunCmd) + '''' + %Trim(scPathName) + '''' + ')';
        strRunCmd = %Trim(strRunCmd) + ' TOFILE(QTEMP/FACTORS)';
        strRunCmd = %Trim(strRunCmd) + ' MBROPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RCDDLM(*CRLF)';
        strRunCmd = %Trim(strRunCmd) + ' FLDDLM(*TAB)';
        strRunCmd = %Trim(strRunCmd) + ' ERRRCDOPT(*REPLACE)';
        strRunCmd = %Trim(strRunCmd) + ' RPLNULLVAL(*FLDDFT)';
        Monitor;
           RunCmd(strRunCmd:500);
        On-Error;
           // Get the message indicating how many records were not uploaded
           // If more than 2 inform the user the data was not formatted properly
           dsInMsgInfo = dsRtvMsgInfo;
           dsInMsgInfo.type = '*LAST';
           dsMsgInfo = RtvMsg('*':dsInMsgInfo);
           dsInMsgInfo.type = '*PRV';
           Dow dsMsgInfo.MsgID <> *blanks;
              If dsMsgInfo.MsgID = 'CPC2959';
                 Clear msg2ndLevel;
                 Exec Sql
                 select MESSAGE_SECOND_LEVEL_TEXT into :msg2ndLevel
                     from table(qsys2.joblog_info('*')) A
                     where ORDINAL_POSITION = :dsmsginfo.pos;
                 pattern = 'records were not copied';
                 result = %scan(%trim(pattern):%trim(msg2ndLevel));
                 If result > 0;
                    result = result - 2;
                    Clear CharRecNotCopy;
                    charNum = %Subst(msg2ndLevel:result:1);
                    Dow charNum >= '0' and charNum <= '9';
                       CharRecNotCopy = charNum + %Trim(CharRecNotcopy);
                       result = result - 1;
                       charNum = %Subst(msg2ndLevel:result:1);
                    Enddo;
                    If %Dec(%Trim(CharRecNotCopy):7:0) > 1;
                       PopTitle = 'Improper Format Message';
                       PopMsg = 'The data was not formatted properly for the +
                                 Upload';
                       PopOneBtn(PopMsg:PopTitle);
                       isValidformat = *off;
                       Leave;
                    Endif;
                 Endif;
                 Leave;
              Endif;
              If dsMsgInfo.MsgID = 'CPFA0D4';
                 Clear msg2ndLevel;
                 Exec Sql
                 select MESSAGE_SECOND_LEVEL_TEXT into :msg2ndLevel
                     from table(qsys2.joblog_info('*')) A
                     where ORDINAL_POSITION = :dsmsginfo.pos;
                 pattern = 'Error number 3029';
                 result = %scan(%trim(pattern):%trim(dsMsgInfo.MsgText));
                 If result > 0;
                    PopTitle = 'File Lock';
                    PopMsg = 'Please close the Spreadsheet and text file and' +
                     ' Submit the job again.';
                    PopOneBtn(PopMsg:PopTitle);
                    isValidformat = *off;
                    Leave;
                 Endif;
              Endif;
              dsInMsgInfo.pos  = dsMsgInfo.pos;
              dsMsgInfo = RtvMsg('*':dsInMsgInfo);
           Enddo;
        Endmon;
      /End-Free

     p ImpFileToTable  e
      //===================================================================
      // Initialize - Initialization procedure
      //===================================================================
     p Initialize      b
     d Initialize      pi

      /Free
      /End-Free

     p Initialize      e
      //===================================================================
      // LoadImpErrGrd - Load the Rating Factor Import Error Grid
      //===================================================================
     p LoadImpErrGrd   b
     d LoadImpErrGrd   pi

      /Free
        scFnd    = dsRateFactor.Fnd;
        scEmp#   = dsRateFactor.Emp#;
        scDiv    = dsRateFactor.Div;
        Monitor;
           scPfDt   = dsRateFactor.PfDt;
        On-Error;
        Endmon;
        scRtSt   = dsRateFactor.State;
        scExM1   = dsRateFactor.ExM1;
        Monitor;
           scEmD1   = dsRateFactor.EmD1;
        On-Error;
        Endmon;
        scExM2   = dsRateFactor.ExM2;
        Monitor;
           scEmD2   = dsRateFactor.EmD2;
        On-Error;
        Endmon;
        scExM3   = dsRateFactor.ExM3;
        Monitor;
           scEmD3   = dsRateFactor.EmD3;
        On-Error;
        Endmon;
        scDbCr   = dsRateFactor.DbCr;
        Monitor;
           scSdEf   = dsRateFactor.SdEf;
        On-Error;
        Endmon;
        scAsP2   = dsRateFactor.AsP2;
        Monitor;
           scDvEf   = dsRateFactor.DvEf;
        On-Error;
        Endmon;
        scDev2   = dsRateFactor.Dev2;
        Monitor;
           scD2Ef   = dsRateFactor.D2Ef;
        On-Error;
        Endmon;
        scAsP3 = dsRateFactor.AsP3;
        scDisc = dsRateFactor.Disc;
        scAsPm = dsRateFactor.AsPm;
        Monitor;
           scMcEf = dsRateFactor.McEf;
        On-Error;
        Endmon;
        scAsPs = dsRateFactor.AsPs;
        Monitor;
           scScEf = dsRateFactor.ScEf;
        On-Error;
        Endmon;
        scIlCd = dsRateFactor.IlCd;
        Monitor;
           scIlEf = dsRateFactor.IlEf;
        On-Error;
        Endmon;
        scDCde = dsRateFactor.DCde;
        Monitor;
           scDcEf = dsRateFactor.DcEf;
        On-Error;
        Endmon;
        scAmP1 = dsRateFactor.AmP1;
        scManP = dsRateFactor.ManP;
        //*** Beg Add ***** 06/27/22 ************************
        scWvType = dsRateFactor.WvTyp;
        Monitor;
           scWvPerc = dsRateFactor.WvPct;
        On-Error;
        Endmon;
        Monitor;
           scWvFc = dsRateFactor.WvFc;
        On-Error;
        Endmon;
        Monitor;
           scWvMnc = dsRateFactor.WvMnc;
        On-Error;
        Endmon;
        Monitor;
           scWvMxc = dsRateFactor.WvMxc;
        On-Error;
        Endmon;
        scWvPrLvl = dsRateFactor.WvPlv;
        scWvDesc = dsRateFactor.WvBds;
        //*** End Add ***** 06/27/22 ************************
        //*** Beg Add ***** 03/28/23 ************************
        Monitor;
           scPct1 = dsRateFactor.LgDePct;
        On-Error;
        Endmon;
        Monitor;
           scAmg1 = dsRateFactor.LgDeAggAmt;
        On-Error;
        Endmon;
        scPln1 = dsRateFactor.LgDePlnTyp;
        //*** End Add ***** 03/28/23 ************************

        //*** Beg Add ***** 05/24/23 ************************
        Monitor;
           scWvEnEfDt = dsRateFactor.WvEnEfDt;
        On-Error;
        Endmon;
        scWvWav#   = dsRateFactor.WvWav#;
        scWvSpName = dsRateFactor.WvSpName;
        scWvSpAdr1 = dsRateFactor.WvSpAdr1;
        scWvSpAdr2 = dsRateFactor.WvSpAdr2;
        scWvSpCty  = dsRateFactor.WvSpCty;
        scWvSpSt   = dsRateFactor.WvSpSt;
        scWvSpZip  = dsRateFactor.WvSpZip;
        scWvSpEstP = dsRateFactor.WvSpEstPrm;
        scWvSpAudP = dsrateFactor.WvSpAudPrm;
        //*** End Add ***** 05/24/23 ************************
        rrnRatFac = rrnRatFac + 1;
        OutRfERrn = rrnRatFac;
        If rrnRatFac = 1;
       // scRfGrdSl = *on;
        Else;
       // scRfGrdSl = *off;
        Endif;
        Write sflRfErr;

      /End-Free

     p LoadImpErrGrd   e
      //===================================================================
      // ProcTblFactors - Read the table FACTORS and call edit routine
      //===================================================================
     p ProcTblFactors  b
     d ProcTblFactors  pi
      *** Beg Add ***** 06/27/22 ***************************
     D sqlSttSv        s              5a
      *** End Add ***** 06/27/22 ***************************
      /Free
        Exec Sql Declare C1 Cursor for
          Select * from QTEMP/Factors;

        Exec Sql Open C1;
        // Set Subfile indicators
        dspRfErr   = *on ;
        clrRfErr   = *off;
        dspRfErCtl = *on;
        isErrorFactors = *off;
        // Fetch records from FACTORS file and check for error conditions
        Reset dsRateFactor;
        Dou Not IsValidFetch(SqlStt);
           Exec SQL
           Fetch Next From C1 into :dsRateFactor :NullInds;
           If isValidFetch(SqlStt);
              //*** Beg Add ***** 06/27/22 ***************************
              sqlSttSv = SqlStt;
              //*** End Add ***** 06/27/22 ***************************
         // Perform edits
              EdtTblFactors();
              //*** Beg Add ***** 06/27/22 ***************************
              sqlStt = SqlSttSv;
              //*** End Add ***** 06/27/22 ***************************
              //*** Beg Add ***** 05/24/23 ***************************
              Exec Sql Update QTEMP/FACTORS
                 Set WvEnName = :strEnForm, WvEnSt = :strEnSt
                 Where Current of C1;
              //*** End Add ***** 05/24/23 ***************************
         // Invalid Fetch.  Leave the loop.
           Else;
              Leave;
           Endif;
           Reset dsRateFactor;
        Enddo;

        Exec Sql Close C1;

      /End-Free

     p ProcTblFactors  e
      //===================================================================
      // MainProcess - Main process procedure
      //===================================================================
     p MainProcess     b
     d MainProcess     pi

      /Free
       //isExM1 = *on;
       //isExM2 = *on;
       //isExM3 = *on;
       //isDev1 = *on;
       //isDev2 = *on;
       //isDbCr = *on;
       //*** Beg Chg *** 05/08/24 *** PHW115560 *********************
       //***isCheckboxDft = *on;
       isCheckboxDft = *off;
       //*** End Chg *** 05/08/24 *** PHW115560 *********************
       //*** Beg Del *** 05/08/24 *** PHW115560 *********************
       //***Select;
       //***When strCltId = '150';
          //***isCheckboxDft = *off;
       //***Endsl;
       //*** End Del *** 05/08/24 *** PHW115560 *********************
       isExM1 = isCheckboxDft;
       isExM2 = isCheckboxDft;
       isExM3 = isCheckboxDft;
       isDev1 = isCheckboxDft;
       isDev2 = isCheckboxDft;
       isDbCr = isCheckboxDft;
       isDisc = isCheckboxDft;
       isAmP1 = isCheckboxDft;
       isDcDe = isCheckboxDft;
       isILCd = isCheckboxDft;
       isAsPm = isCheckboxDft;
       isAsPs = isCheckboxDft;
       //*** Beg Add ***** 06/27/22 ******************************
       isPct1 = isCheckboxDft;
       isAmG1 = isCheckboxDft;
       isPln1 = isCheckboxDft;
       //*** End Add ***** 06/27/22 ******************************
       //*** Beg Add ***** 03/28/23 ******************************
       isWvInfo = isCheckboxDft;
       //*** End Add ***** 03/28/23 ******************************
       isManP = *off;

      // Process until exit button is pressed.
       Dou ExitNow = *On;
          clrRfError();
          clear sflRfErr;
          clrRfErr = *off;
          dspRfErr = *On;
          dspRfErCtl = *on;
          // Initially the error grid is hidden
          If BtnSubmit <> *on;
             ExfmtScreens();
          Endif;
          Select;
          When BtnSubmit = *On ;
             BtnSubmit = *Off;
             ClearError();
            // Validate the screen (folder path and document name exists)
             If Validate();
                // Update the data area with the folder path selected
                UpdDataArea();
                // Create temporary SQL table called Factors in QTEMP library
                CrtTblFactors();
                //
                // Import the IFS file to the newly created table
                ImpFiletoTable();
                If Not isValidFormat;
                   Leave;
                Endif;
                //*** Beg Add ***** 05/24/23 **********************************
                Exec SQL Alter table QTEMP/FACTORS
                  add WvEnName char(15)
                  add WvEnSt char(2);
                //*** End Add ***** 05/24/23 **********************************
                //
                // If the file imported succsessfully then read through the table and
                // perform edits on the field values and display the grid if there are errors
                ProcTblFactors();
                If isErrorFactors;
                   isImpErr = *on;
                   rrnRatfac = 1;
                   ExfmtScreens();
                   Dou ExitNow = *On;
                      Select;
                      When BtnSubmit = *On;
                         ClearError();
                         Leave;
                      When BtnCancel = *On;
                         Leave;
                      Endsl;
                   Enddo;
                   If BtnSubmit = *On;
                      Iter;
                   Else;
                     Leave;
                   Endif;
                Else;
                   // All edits passed so archive and update WDELP
                   strRunCmd = 'CPYF FROMFILE(WDELP)';
                   strRunCmd = %Trim(strRunCmd) + ' TOFILE(';
                   strRunCmd = %Trim(strRunCmd) + %Trim(ArcLibrary) + '/';
                   strRunCmd = %Trim(strRunCmd) + 'WDELPSAVE)';
                   strRunCmd = %Trim(strRunCmd) + ' MBROPT(*REPLACE)';
                   strRunCmd = %Trim(strRunCmd) + ' CRTFILE(*YES)';
                   //*** Beg Add ***** 05/24/23 **********************************
                   strRunCmd = %Trim(strRunCmd) + ' FMTOPT(*MAP *DROP)';
                   //*** End Add ***** 05/24/23 **********************************
                   Monitor;
                      RunCmd(strRunCmd:98);
                   On-Error;
                   Endmon;
                   // Update WDELP with Policy Rating Factors from the import file
                   UpdWdelp();
                   UpdWMldp();

       //*** Beg Add ***** 05/24/23 **********************************
      /Free
        //*** Beg Add ***** 07/31/24 ************************************
        If isWvInfo = *On;
        //*** End Add ***** 07/31/24 ************************************
           Exec Sql Declare C2 Cursor for
            Select * from QTEMP/Factors;

           Exec Sql Open C2;
           // Fetch records from FACTORS file and check for error conditions
           Reset dsRateFactor;
           Dou Not IsValidFetch(SqlStt);
              Exec SQL
              Fetch Next From C2 into :dsRateFactor:NullInds,:xxenform,:xxenst;
              If isValidFetch(SqlStt);
                 UpdWMwvp();
                 If SqlStt = '00000';
                    AttachWaiver();
                    SbmWvrEnd();
                 Endif;
              Else;
                 Leave;
              Endif;
              Reset dsRateFactor;
           Enddo;

           Exec Sql Close C2;
        //*** Beg Add ***** 07/31/24 ************************************
        Endif;
        //*** End Add ***** 07/31/24 ************************************

      /End-Free

       //*** End Add ***** 05/24/23 **********************************
                   Leave;
                Endif;
             EndIf;
          When BtnCancel = *On;
             BtnCancel = *Off;
             Leave;
          When BtnRefresh= *On;
             BtnRefresh= *Off;
             Initialize();
             ClearError();
          EndSl;

       EndDo;
      /End-Free

     p MainProcess     e
      //===================================================================
      // ProgramEnd - End Program
      //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi
      /Free
        *inlr = *on;
        Return;
      /End-free
     p ProgramEnd      e
      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
        // Exec Sql Set Option commit = *None;
        Initialize();
        //*** Beg Add ***** 05/24/23 ***************************************
        xxUserOutQ = GetUsrOutQ(Q1user);
        //*** End Add ***** 05/24/23 ***************************************
        scCompName = GetCompNam(userCo#(q1user));
        Chain Q1user pup00;
        xxCo# = UserCo#(Q1User);
        strReturn = 'ONDEMAND  ';
        // Retrieve library suffix and build archive library name
        RtvSuffix(suffix);
        If suffix = *blanks;
           TempLibrary = 'PDSTEMP';
           ArcLibrary  = 'PDSFILARC';
        Else;
           ArcLibrary  = 'PDSFIL' + %Trim(suffix) + 'A';
           TempLibrary = 'PDSTEMP' + %Trim(suffix);
        Endif;
        // Create Data Area to store folder path
        strRunCmd = 'CRTDTAARA DTAARA(PDSOBJ';
        strRunCmd = %Trim(strRunCmd) + %Trim(Suffix) + '/';
        strRunCmd = %Trim(strRunCmd) + 'WTPOLRFIMP)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TYPE(*CHAR) LEN(256)';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'TEXT(''WTPOLRFIMP Data Area'')';
        Monitor;
           RunCmd(strRunCmd:98);
        On-Error;
        Endmon;

        //Retrieve Client-ID
        RtvCltID(strCltId);

        If strCltId = '150';
           isNot150 = *Off;
           ttManp = *blanks;
        Else;
           isNot150 = *On;
           ttManp = 'Not available';
        Endif;
      /End-Free

     p ProgramStart    e

      //===================================================================
      //  UpdDataArea - Update Data Area with Path Name selected
      //===================================================================
     p UpdDataArea     b
     d UpdDataArea     pi

      /Free
        strRunCmd = 'CHGDTAARA DTAARA(PDSOBJ';
        strRunCmd = %Trim(strRunCmd) + %Trim(Suffix) + '/';
        strRunCmd = %Trim(strRunCmd) + 'WTPOLRFIMP';
        strRunCmd = %Trim(strRunCmd) + ' ' + '(1 256))';
        strRunCmd = %Trim(strRunCmd) + ' ' + 'VALUE(''';
        strRunCmd = %Trim(strRunCmd) + %Trim(scPathName) + '''' + ')';
        Monitor;
           RunCmd(strRunCmd:500);
        On-Error;
        Endmon;
       //*** Beg Add ***** 06/27/22 *************************
       In *Lock pathNameDta;
       lastPathName = scPathName;
       Out pathNameDta;
       //*** End Add ***** 06/27/22 *************************
      /End-Free

     p UpdDataArea     e
      //===================================================================
      //  UpdWdelp - Update WDELP with Rating Factors from table FACTORS
      //===================================================================
     p UpdWdelp        b
     d UpdWdelp        pi

      /Free
        sys_date = %date();
        sysDt7 = DateTo7(Sys_Date);

                                                                     //For older code see prior rele
       //Exec Sql Set Option Commit = *None;

        If isExM1 = *On;
           xstrSqlSet = 'ELexm1, ELemd1, ';
           ystrSqlSel = 'case when EXM1 is null ' +
                        'then 0 else cast(EXM1 as decimal(5,4)) end, ' +
                        'case when EMD1 is null then 0 else ' +
                        'cast(EMD1 as decimal(8,0))-19000000 end, ' ;
        Endif;
        If isExM2 = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELexm2, ELemd2, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when EXM2 is null ' +
                        'then 0 else cast(EXM2 as decimal(5,4)) end, ' +
                        'case when EMD2 is null then 0 else ' +
                        'cast(EMD2 as decimal(8,0))-19000000 end, ' ;
        Endif;
        If isExm3 = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELexm3, ELemd3, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when EXM3 is null ' +
                        'then 0 else cast(EXM3 as decimal(5,4)) end, ' +
                        'case when EMD3 is null then 0 else ' +
                        'cast(EMD3 as decimal(8,0))-19000000 end, ' ;
        Endif;
        If isDbCr = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELdbcr, ELsdef, ELsdex, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when DBCR is null ' +
                        'then 0 else cast(DBCR as decimal(5,4)) end, ' +
                        'case when SDEF is null then 0 else ' +
                        'cast(SDEF as decimal(8,0))-19000000 end,0, ' ;
        Endif;
        If isDev1 = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELasp2, ELdvef, ELdvex, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when ASP2 is null ' +
                        'then 0 else cast(ASP2 as decimal(5,2)) end, ' +   //7/19/21 5,2 not 5,4
                        'case when DVEF is null then 0 else ' +
                        'cast(DVEF as decimal(8,0))-19000000 end,0, ' ;
        Endif;
        if isDev2 = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELdev2, ELd2ef, ELd2ex, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when DEV2 is null ' +
                        'then 0 else cast(DEV2 as decimal(5,2)) end, ' +     //07/19/21 5,2 not 5,4
                        'case when D2EF is null then 0 else ' +
                        'cast(D2EF as decimal(8,0))-19000000 end,0, ' ;
        Endif;

        If isDisc = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELasp3, ELdisc, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when ASP3 is null ' +
                        'then 0 else cast(ASP3 as decimal(5,2)) end, ' +
                        'case when Disc is null ' +
                        'then '' '' else Disc end, ' ;

        Endif;

        If isAsPm = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELaspm, ELmcef, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when AsPm is null ' +
                        'then 0 else cast(AsPm as decimal(5,2)) end, ' +
                        'case when McEf is null then 0 else ' +
                        'cast(McEf as decimal(8,0))-19000000 end, ' ;
        Endif;

        If isAsPs = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELasps, ELscef, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when AsPs is null ' +
                        'then 0 else cast(AsPs as decimal(5,2)) end, ' +
                        'case when ScEf is null then 0 else ' +
                        'cast(ScEf as decimal(8,0))-19000000 end, ' ;
        Endif;

        If isIlCd = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELilcd, ELilef, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when IlCd is null ' +
                        'then '' '' else IlCd end, ' +
                        'case when IlEf is null then 0 else ' +
                        'cast(IlEf as decimal(8,0))-19000000 end, ' ;
        Endif;

        If isDcDe = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELdcde, ELdcef, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when DcDe is null ' +
                        'then '' '' else DcDe end, ' +
                        'case when DcEf is null then 0 else ' +
                        'cast(DcEf as decimal(8,0))-19000000 end, ' ;
        Endif;

        If isManP = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELmanp, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when Manp is null ' +
                        'then 0 else cast(ManP as decimal(5,2)) end, ';
        Endif;

        //*** Beg Add ***** 06/27/22 ******************************
        If isWvInfo = *On;
           xstrSqlSet  = %trim(xstrSqlSet) + ' ELwvtp, ELwvpc, ELfchg, ' +
                         'ELwmin, ELwmax, ELwvfl, ELblnkwd, Elovrwd, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when WvTyp is null ' +
                        'then '' '' else WvTyp end, ' +
                        'case when WvPct is null then 0 else ' +
                        'cast(WvPct as decimal(5,2)) end, ' +
                        'case when WvFc is null then 0 else ' +
                        'cast(WvFc as decimal(11,2)) end, ' +
                        'case when WvMnC is null then 0 else ' +
                        'cast(WvMnC as decimal(9,2)) end, ' +
                        'case when WvMxC is null then 0 else ' +
                        'cast(WvMxC as decimal(9,2)) end, ' +
                        'case when WvPLv is null ' +
                        'then '' '' else WvPLv end, ' +
                        'case when WvBDs is null ' +
                        'then '' '' else WvBDs end, ' +
                      //*** Beg Chg ***** 05/24/23 ******************
                       //***'case when WvBDs is null ' +
                        'case when WvBDs is null or WvTyp = ''S'' '+
                      //*** End Chg ***** 05/24/23 ******************
                        'then '' '' else ''Y'' end, ' ;

        Endif;
        //*** End Add ***** 06/27/22 ******************************

        xstrSqlSet  = %trim(xstrSqlSet) +  ' ELuser, ELdt, ELpgm, ELtime';

        ystrSqlSel =  %trim(ystrSqlSel) + ' ' + '''' +
                      %trim(q1user) + '''' + ', ' +
                      %editc(sysDt7:'X') + ', ' +  '''' +
                      %trim(q1Pgm) + '''' +  ', ' +
                     'decimal(current time)' +
                     ' from QTEMP/FACTORS where ELco# = ' + %editc(UPco#:'X') +
                     ' and ELfnd = FND and ELemp# = EMP# and ELdiv = DIV' +
                     ' and ELpfdt = cast(PFDT as decimal(8,0))-19000000' +
                     ' and ELrtcd in(Select RSrtcd from WMRSP' +
                     ' where ELco# = RSco# and RSjrst = STATE))';

        strSql='update WDELP set (' + %trim(xstrSqlset) + ') = (select ' +
                %trim(ystrSqlSel) +
                ' where exists(select 1 from QTEMP/FACTORS where ELco# = ' +
                %Editc(UPco#:'X') + ' and ELfnd = FND and ELemp# = EMP#  and ' +
                'ELdiv = DIV and ELpfdt = cast(PFDT as decimal(8,0))-19000000' +
                ' and ELrtcd in(Select RSrtcd from WMRSP ' +
                'where ELco# = RSco# and RSjrst = STATE))';

        strDs = strSql;

        str2Ds = ystrSqlSel;
        exec sql execute immediate:strSql;

        xxfyr = elfyr;

        // Check for Errors
        If isValidFetch(SqlStt);
           PopTitle = 'Successful Update';
           PopMsg = 'The data has been updated successfully';
           PopOneBtn(PopMsg:PopTitle);
        Else;
           // Copy FACTORS to PDSTEMP/Job Number
           strRunCmd = 'CPYF FROMFILE(FACTORS)';
           strRunCmd = %Trim(strRunCmd) + ' TOFILE(';
           strRunCmd = %Trim(strRunCmd) + %Trim(TempLibrary);
           strRunCmd = %Trim(strRunCmd) + '/' + 'J' + %Trim(%Char(Q1jobn));
           strRunCmd = %Trim(strRunCmd) + ') MBROPT(*REPLACE)';
           strRunCmd = %Trim(strRunCmd) + ' CRTFILE(*YES)';
           Monitor;
              RunCmd(strRunCmd:98);
           On-Error;
           Endmon;

           PopTitle = 'UnSuccessful Update';
           PopMsg = 'The data in WDELP has NOT been updated successfully.';
           PopMsg = %Trim(PopMsg) + '  Please contact Support.';
           PopMsg = %Trim(PopMsg) + '  Job Info: Job= ' + %Trim(Q1job);
           PopMsg = %Trim(PopMsg) + '  , User= ' + %Trim(Q1cusr);
           PopMsg = %Trim(PopMsg) + ', Number= ' + %Char(Q1jobn);
           PopOneBtn(PopMsg:PopTitle);
        Endif;
      /End-Free

     p UpdWdelp        e
      //===================================================================
      // Validate - Validate screen fields
      //===================================================================
     p Validate        b
     d Validate        pi              n
     d docExists       s               n
     d Lo              c                   const('abcdefghijklmnopqrstuvwxyz')
     d root            s             10
     d rootPlus        s             20
     d sFldPos         s              2  0
     d suffix          s              3
     d Up              c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     d x               s              3  0 inz(1)
     d y               s              3  0
     d intCharPos      s              3  0

      /Free
       // Must start with a '/'
       If %subst(scPathName:1:1) <> '/';
          errPathNam = *On;
          msgPathNam = 'The import file path name must start with a ''/''.';
          Return *Off;
       Endif;

       fullPathName = %Xlate(lo:up:scPathName);
       // Must be a .TXT file
       length = %Len(%Trim(fullPathName));
       If %Subst(fullPathName:length-3:4) <> '.TXT';
          errPathNam = *On;
          msgPathNam = 'The file extension must be .TXT.';
          Return *Off;
       Endif;

       // Make sure doc exists (also checks authority);
       ChkDlo(fullPathName:docExists);
       If Not(docExists);
          errPathNam = *On;
          msgPathNam = 'Document does not exist or authority is restricted.';
          Return *Off;
       Endif;

       Return *On;
      /End-Free

     P Validate        e
       //*----------------------------------------------------------------
       //* UpdWmldp - Update Large Deductible File                        *
       //*----------------------------------------------------------------
       Dcl-Proc UpdWmldp;
        Dcl-Pi *N;
        End-Pi;
        //*** Beg Add ***** 03/28/23 **************************************
        Clear xstrSqlSet;
        Clear ystrSqlSel;
        //*** End Add ***** 03/28/23 **************************************
        If isAmP1 = *On;
           xstrSqlSet = 'LDamP1, ';
           ystrSqlSel = 'case when AmP1 is null ' +
                        'then 0 else cast(AmP1 as decimal( 9,0)) end, ';
        //*** Beg Add ***** 03/28/23 **************************************
        Endif;
        If isPct1 = *On;
           xstrSqlSet = %trim(xstrSqlSet) + ' LDpct1, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when LgDePct is null ' +
                        'then 0 else cast(LgDePct as decimal(12,9)) end, ';
        Endif;
        If isamG1 = *On;
           xstrSqlSet = %trim(xstrSqlSet) + ' LDamG1, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when LgDeAggAmt is null ' +
                        'then 0 else cast(LgDeAggAmt as decimal(9,0)) end, ';
        Endif;
        If isPln1 = *On;
           xstrSqlSet = %trim(xstrSqlSet) + ' LDPln1, ';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                        'case when LgDePlnTyp is null ' +
                        'then '' '' else LgDePlnTyp end, ';
        Endif;
        //*** End Add ***** 03/28/23 **************************************

        xstrSqlSet  = %trim(xstrSqlSet) +  ' LDuser, LDdt, LDpgm, LDtime';

        ystrSqlSel =  %trim(ystrSqlSel) + ' ' + '''' +
                      %trim(q1user) + '''' + ', ' +
                      %editc(sysDt7:'X') + ', ' +  '''' +
                      %trim(q1Pgm) + '''' +  ', ' +
                     'decimal(current time)' +
                     ' from QTEMP/FACTORS, WDELP where' +
                     ' LDco# = ' + %editc(UPco#:'X') +
                     ' and LDco# = ELco#' +
                     ' and LDfnd = FND and FND = ELfnd' +
                     ' and LDemp# = EMP# and EMP# = ELemp#'+
                     ' and LDdiv = DIV and DIV = ELdiv' +
                     ' and cast(pfdt as decimal(8,0))-19000000 = ELpfdt' +
                     ' and LDfyr = elfyr';

        strSql='update WMLDP set (' + %trim(xstrSqlset) + ') = (select ' +
                %trim(ystrSqlSel) + ')' +
                ' where exists(select 1 from QTEMP/FACTORS, WDELP where' +
                ' LDco# = ' + %Editc(UPco#:'X') +
                ' and LDco# = ELco#' +
                ' and LDfnd = FND and LDfnd = ELfnd' +
                ' and LDemp# = EMP# and LDemp# = ELemp#' +
                ' and LDdiv = DIV and LDdiv = ELdiv' +
                ' and CAST(PFDT as decimal(8,0))-19000000 = ELpfdt' +
                ' and LDfyr = ELfyr) ';

        strDs = strSql;

        exec sql execute immediate:strSql;

        // Check for Errors
        If isValidFetch(SqlStt);
        Else;
           //*** Beg Add ***** 06/27/22 *************************
           If SqlStt <> '02000';          //SqlStt='02000'= No row found in Fetch, update or Delete
           //*** End Add ***** 06/27/22 *************************
              // Copy FACTORS to PDSTEMP/Job Number
              strRunCmd = 'CPYF FROMFILE(FACTORS)';
              strRunCmd = %Trim(strRunCmd) + ' TOFILE(';
              strRunCmd = %Trim(strRunCmd) + %Trim(TempLibrary);
              strRunCmd = %Trim(strRunCmd) + '/' + 'J' + %Trim(%Char(Q1jobn));
              strRunCmd = %Trim(strRunCmd) + ') MBROPT(*REPLACE)';
              strRunCmd = %Trim(strRunCmd) + ' CRTFILE(*YES)';
              Monitor;
                 RunCmd(strRunCmd:98);
              On-Error;
              Endmon;

              PopTitle = 'UnSuccessful Large Deductible Update';
              PopMsg = 'The data in WMLDP has NOT been updated successfully.';
              PopMsg = %Trim(PopMsg) + '  Please contact Support.';
              PopMsg = %Trim(PopMsg) + '  Job Info: Job= ' + %Trim(Q1job);
              PopMsg = %Trim(PopMsg) + '  , User= ' + %Trim(Q1cusr);
              PopMsg = %Trim(PopMsg) + ', Number= ' + %Char(Q1jobn);
              PopOneBtn(PopMsg:PopTitle);
           //*** Beg Add ***** 06/27/22 *************************
           Endif;
           //*** End Add ***** 06/27/22 *************************
        //*** Beg Del ***** 03/28/23 **************************************
        //***Endif;
        //*** End Del ***** 03/28/23 **************************************
        Endif;
       End-Proc;
        //*** Beg Add ***** 05/24/23 **************************************
       //*----------------------------------------------------------------
       //* UpdWmwvp - Write/Update Waiver Information File                 *
       //*----------------------------------------------------------------
       Dcl-Proc UpdWmwvp;
        Dcl-Pi *N;
        End-Pi;

        sys_date = %date();
        sysDt7 = DateTo7(Sys_Date);
        dt  = %char(dsratefactor.wvenefdt);
        xxdt = %dec(%subst(dt:1:4) + %subst(dt:6:2) + %subst(dt:9:2):8:0);
        xxtemefdt = xxdt - 19000000;

        Exec Sql
         Declare C5 cursor for
         select max(MUend#) from wmmup
         where muco# = :xxco# and
          MUFnd = :dsratefactor.fnd and
          MUEmp# = :dsratefactor.emp# and
          MUDiv = :dsratefactor.div and
          MUFyr = :xxfyr;

        Exec Sql
         Open C5;

        Exec Sql
         Fetch C5 into :xxmax_end#;

        Exec Sql
         CLose C5;

        xxmax_end# +=1;

        Clear xstrSqlSet;
        Clear ystrSqlSel;
        xstrSqlSet = 'WVco#, WVfnd, WVfyr, WVemp#, WVdiv, WVname,'+
              'WVnam2, WVadr1, WVadr2, WVcty, WVst, WVzip, WVepay, WVapay,'+
              'WVswpc, WVfchg, WVwmin, WVapre, WVefdt, WVemst,'+
              'WVenm, WVblkPrjDs, WVproj, WVprj2, WVperOrg,';

        ystrSqlSel =  %editc(UPco#:'X') + ',' + %editc(ELfnd:'X') + ',' +
                      %editc(ELfyr:'X') + ',' + %editc(ELemp#:'X') + ',' +
                %editc(ELdiv:'X') + ',' + '''' + %trim(dsRateFactor.WvSpName) +
                   '''' + ',' + '''' + %trim(dsRateFactor.WvSpName) + '''' +
               ',' + '''' + %trim(dsRateFactor.WvSpAdr1) + '''' + ',' + '''' +
                   %trim(dsRateFactor.WvSpAdr2) + '''' + ',' + '''' +
                      %trim(dsRateFactor.WvSpCty) + '''' + ',' + '''' +
                      %trim(dsRateFactor.WvSpSt) + '''' + ',' +
                      %editc(dsRateFactor.WvSpZip:'X') +
                      ',' + %editc(dsRateFactor.WvSpEstPrm:'1') + ',' +
                      %editc(dsRateFactor.WvSpAudPrm:'1')+ ','+%editc(00000:'1')
                      +',' + %editc(dsRateFactor.WvFc:'1') + ',' +
                %editc(dsRateFactor.WvMnC:'1') + ','+ %editc(000000000.00:'1')+
            ',' + %editc(xxtemefdt:'X') +
            ',' + '''' + %trim(strEnSt) + '''' + ',' +
         ''''+ %trim(strEnForm)+ '''' + ',' + '''' + %trim(dsRateFactor.WvBds) +
           '''' + ',' +''''+'''' + ',' + ''''+'''' + ',' + '''' + '''' +',';

        // Update existing waiver
        Chain (xxCo#:dsRateFactor.Fnd:dsRateFactor.Emp#:
               dsRateFactor.Div:dsRateFactor.WvWav#) pwv04;
        If %found(Wmwvl4);
           // File maintenance fields
           xxupd = *on;
           In *Lock *Dtaara;
            xxBFseq# = dsRateFactor.WvWav#;
            xxAFseq# = dsRateFactor.WvWav#;
            xxBFDesc = WVblkprjds;
            xxAFDesc = dsRateFactor.WvBds;
           Out *dtaara;
           xstrSqlSet  = %trim(xstrSqlSet) +  ' WVuser, WVdt, WVpgm, WVtime'
                          + ', WVend# , WVprt#';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' + '''' +
                         %trim(q1user) + '''' + ', ' +
                         %editc(sysDt7:'X') + ', ' +  '''' +
                         %trim(q1Pgm) + '''' +  ', ' +
                         'decimal(current time)' +', ' +
                         %editc(xxmax_end#:'X') + ',' +
                         %editc(xxmax_end#:'X');

           strSql='update WMWVP set (' + %trim(xstrSqlset) + ') = (' +
                   %trim(ystrSqlSel) + ')' +
                   ' where WVco# = ' + %editc(UPco#:'X') +
                         ' and WVfnd =' + %editc(dsRateFactor.fnd:'X')  +
                         ' and WVemp# = ' + %editc(dsRateFactor.emp#:'X') +
                         ' and WVdiv = ' + %editc(dsRateFactor.div:'X') +
                         ' and WVwav# = ' + %editc(dsRateFactor.wvwav#:'X') ;

        Else;
           // Write new waiver to WMWVP

           If dsRateFactor.WVwav# = *Zeros;

              Exec Sql
               Declare C4 cursor for
               select max(WVwav#) from wmwvp
               where wvco# = :xxco# and
                WVFnd = :dsratefactor.fnd and
                WVEmp# = :dsratefactor.emp# and
                WVDiv = :dsratefactor.div and
                WVFyr = :xxfyr ;

              Exec Sql
               Open C4;

              Exec Sql
               Fetch C4 into :xxmax_Wav#;

              Exec Sql
               Close C4;
              xxmax_Wav# += 1;
           Else;
              xxmax_Wav# = dsRateFactor.WVwav#;
           Endif;

           xstrSqlSet  = %trim(xstrSqlSet) + 'WVwav#, WViusr, WVidt, WVipgm,'+
                         'WVitm, WVuser, WVdt, WVpgm, WVtime, WVstat, WVend#,'+
                         'WVprt#, WVeprt, WVcpct, WVcfcg';
           ystrSqlSel =  %trim(ystrSqlSel) + ' ' +
                   //*** %editc(dsRateFactor.WVwav#:'X') + ', ' + '''' +
                         %editc(xxmax_Wav#:'X') + ', ' + '''' +
                         %trim(q1user) + '''' + ',' + %editc(sysDt7:'X')+
                         ', ' + '''' + %trim(q1pgm) + '''' + ', ' +
                         'decimal(current time)' + ', ' + '''' +
                         %trim(q1user) + '''' + ', ' +
                         %editc(sysDt7:'X') + ', ' +  '''' +
                         %trim(q1Pgm) + '''' +  ', ' +
              'decimal(current time)' +',' + '''' + %trim(WVstat) +'''' + ',' +
                %editc(xxmax_end#:'X') + ',' + %editc(xxmax_end#:'X') + ',' +
                 '''' + %trim(WVeprt) + '''' + ', '+ %editc(WVcpct:'1') + ',' +
                         %editc(WVcfcg:'1');

           strSql='insert into WMWVP (' + %trim(xstrSqlset) + ') values (' +
                   %trim(ystrSqlSel) + ')';

        EndIf;

        strDs = strSql;

        exec sql execute immediate:strSql;

        // Check for Errors
        If isValidFetch(SqlStt);
           If xxupd ;
              PopTitle = 'Successful Update';
              PopMsg = 'The data has been updated successfully';
           Else;
              PopTitle = 'Successful Insert';
              PopMsg = 'The data has been inserted successfully';
           Endif;
           PopOneBtn(PopMsg:PopTitle);
        Else;
           If SqlStt <> '02000';          //SqlStt='02000'= No row found in Fetch, update or Delete
              // Copy FACTORS to PDSTEMP/Job Number
              strRunCmd = 'CPYF FROMFILE(FACTORS)';
              strRunCmd = %Trim(strRunCmd) + ' TOFILE(';
              strRunCmd = %Trim(strRunCmd) + %Trim(TempLibrary);
              strRunCmd = %Trim(strRunCmd) + '/' + 'J' + %Trim(%Char(Q1jobn));
              strRunCmd = %Trim(strRunCmd) + ') MBROPT(*REPLACE)';
              strRunCmd = %Trim(strRunCmd) + ' CRTFILE(*YES)';
              Monitor;
                 RunCmd(strRunCmd:98);
              On-Error;
              Endmon;

              PopTitle = 'UnSuccessful Wavier Update';
              PopMsg = 'The data in WMWVP has NOT been updated successfully.';
              PopMsg = %Trim(PopMsg) + '  Please contact Support.';
              PopMsg = %Trim(PopMsg) + '  Job Info: Job= ' + %Trim(Q1job);
              PopMsg = %Trim(PopMsg) + '  , User= ' + %Trim(Q1cusr);
              PopMsg = %Trim(PopMsg) + ', Number= ' + %Char(Q1jobn);
              PopOneBtn(PopMsg:PopTitle);
           Endif;
        Endif;
        In *Lock *Dtaara;
         xxAFseq# = xxmax_Wav#;
         xxAFDesc = dsRateFactor.WvBds;
        Out *dtaara;

       End-Proc;
       //*----------------------------------------------------------------
       //* AttachWaiver - Attach the Waiver to the Policy in WMMUP        *
       //*----------------------------------------------------------------
       Dcl-Proc AttachWaiver;
        Dcl-Pi *N;
        End-Pi;
         Monitor;
            xxDtCyMd = Dateto7(dsRateFactor.PfDt);
         On-error;
            xxDtCyMd = 0;
         Endmon;
        Chain (xxCo#:dsRateFactor.Fnd:dsRateFactor.Emp#:
         dsRateFactor.Div:xxDtCyMd) pel01;
        If %Found(WDELL2);
           Chain (xxco#:dsratefactor.fnd:dsratefactor.emp#:dsratefactor.div:
            xxfyr:xxenst:xxenform:*zeros) pmu01;
           If Not %Found(WMMUL2);
              MUCo# = xxco#;
              MUFnd = dsratefactor.fnd;
              MUEmp# = dsratefactor.emp#;
              MUDiv = dsratefactor.div;
              MUFyr = xxfyr;
sau           //MUEnd# = 0;
sau           MUEnd# = xxmax_end#;
              MUSeq# = 0;
              MUForm = xxenform;
              MUEnst = xxenst;
              Chain (xxco#:dsratefactor.fnd:xxenst:xxenform) pen02;
              If %Found(WTENL2);
                 MUDesc = ENDesc;
                 MUDsc2 = ENDsc2;
              Else;
                 MUDesc = '';
                 MUDsc2 = '';
              Endif;
              MUEedt = elpfdt;
              MUExfl = 'I';
              MUMnau = ENMnau;
              MUPrnt = ENPrnt;
              MUStat = 'A';
              MUUser = q1user;
              MUPgm  = q1pgm;
              MUTime = %Dec(%Time():*hms);
              MUDt = %Dec(%Date():*Cymd);
              MUIusr = q1user;
              MUIpgm = q1pgm;
              MUIdt = mudt;
              //Write the record
              Write pmu01;

           Else;
           // Write 2nd record
           Exec Sql
            Declare C3 cursor for
            select max(museq#) from wmmup
            where muco# = :xxco# and
             MUFnd = :dsratefactor.fnd and
             MUEmp# = :dsratefactor.emp# and
             MUDiv = :dsratefactor.div and
             MUFyr = :xxfyr and
             MUEnd# = 0;

           Exec Sql
            Open C3;

           Exec Sql
            Fetch C3 into :xxmax_seq;

           Exec Sql
            CLose C3;

              MUCo# = xxco#;
              MUFnd = dsratefactor.fnd;
              MUEmp# = dsratefactor.emp#;
              MUDiv = dsratefactor.div;
              MUFyr = xxfyr;
              MUEnd# = xxmax_end#;
              MUSeq# = 0;
              MUForm = xxenform;
              MUEnst = xxenst;
              Chain (xxco#:dsratefactor.fnd:xxenst:xxenform) pen02;
              If %Found(WTENL2);
                 MUDesc = ENDesc;
                 MUDsc2 = ENDsc2;
              Else;
                 MUDesc = '';
                 MUDsc2 = '';
              Endif;
              MUEedt = ELPfdt;
              MUExfl = 'I';
              MUMnau = ENMnau;
              MUPrnt = ENPrnt;
              MUStat = 'A';
              MUUser = q1user;
              MUPgm  = q1pgm;
              MUTime = %Dec(%Time():*hms);
              MUDt = %Dec(%Date():*Cymd);
              MUIusr = q1user;
              MUIpgm = q1pgm;
              MUIdt = mudt;
              //Write the record
              Write pmu01;

           Endif;
        Endif;
       End-Proc;
       //*----------------------------------------------------------------
       //* SbmWvrEnd -                                                 *
       //*----------------------------------------------------------------
       Dcl-Proc SbmWvrEnd;
        Dcl-Pi *N;
        End-Pi;

        Dcl-S xxPrtPgm Char(10) Inz;
        Dcl-S xxAddPgm Char(10) Inz;
        Dcl-S xxWvrForm Char(15) Inz;
        Dcl-S xxWvrType Char(1) Inz;

        If dsRateFactor.WvTyp <> *Blanks;
           Chain (xxco#:dsrateFactor.fnd:dsRateFactor.emp#:
            dsRateFactor.div:xxDtCyMd) pel01;
           If %Found(WDELL2);
              If Dateto7(dsRateFactor.WvEnEfDt) >= ELpfdt and
               Dateto7(dsRateFactor.WvEnEfDt) < ELptdt;
                 Chain (ELco#:ELfnd:ELemp#:ELfyr:'P') psh01;
                 If %Found(WMSHL1) and SHstat > '2';
                    If GetWaiverInfo(ELco#:ELfnd:dsRateFactor.state:ELpfdt:
                     xxWvrForm:xxPrtPgm:xxAddPgm) = *On;
                       In *Lock *Dtaara;
                       If dsRateFactor.state <> *Blank;
                          ldStSpcfFlg = 'Y';
                       Else;
                          ldStSpcfFlg = ' ';
                       Endif;
                       ldWaiverFrm = xxWvrForm;
                       Out *Dtaara;

                       xxMiscDartsCd = GetMiscDartCd();
                       ProcessMidterm();

                    Endif;
                 Endif;
              Endif;
           Endif;
        Endif;

       End-Proc;
      //----------------------------------------------------------------
      // Process Midterm Endorsement for policy
      //----------------------------------------------------------------
     p ProcessMidterm  b
     d ProcessMidterm  pi

      /Free
       isUseScript = UseScriptura(processType);
       If isUseScript;
          xxProcType = processType;
          In *Lock *Dtaara;
          If xxMiscDartsCd <> *Blanks;
             xxDart = 'Y';
             xxRmCd = xxMiscDartsCd;
          Else;
             xxDart = 'N';
             xxRmCd = *blanks;
          Endif;
          ldEnd# = MUend#;
          ldEeDt = dtMDYY(MUeedt);
          Out *dtaara;

          Chain (Elco#:ELfnd:Elemp#:ELdiv) pem01;
          If %Found(WMEML);
             xxCarrier# = GetCarrier#();
             WriteWMENL6();
          Endif;

          xxSubmit = OutputWindow(xxProcType:xxMass:'Y':'M');
       Endif;
       SbmtMidTrmJob(%Editc(MUco#:'X'):
                     %Editc(MUfnd:'X'):
                     %Editc(MUemp#:'X'):
                     %Editc(MUdiv:'X'):
                     %Editc(MUfyr:'X'):
                     %Editc(Q1jobn:'X'):
                     %Editc(MUend#:'X'):
                     xxUserOutQ);
      /End-Free

     p ProcessMidterm  e
      //------------------------------------------------------------------------*
      // GetMiscDartCd - returns misc Darts Code
      //------------------------------------------------------------------------*
     p GetMiscDartCd   b
     d GetMiscDartCd   pi             5a

      /Free
       Chain (ELCo#:ELfnd) pdx00;
       Return DXmisc;
      /End-Free

     p GetMiscDartCd   e
      //----------------------------------------------------------------
      // WriteWMENL6 - writes to Midterm Workfile
      //----------------------------------------------------------------
     p WriteWMENL6     b
     d WriteWMENL6     pi

     d strPayrollDesc  s                   like(a_ENalin)

      /Free
       Clear pen06;
       a_ENco#  = ELco#;
       a_ENfnd  = ELfnd;
       a_ENemp# = ELemp#;
       a_ENdiv  = ELdiv;
       a_ENfyr  = ELfyr;
       a_ENreq# = 1;
       a_ENend# = LDend#;
       a_ENeedt = LDeedt;
       a_ENjob  = Q1jobn;
       a_ENnam1 = EMname;
       a_ENrmcd = xxMiscDartsCd;
       a_ENmsfl = 'S';
       a_ENfdn1 = EMfdnm;
       a_ENenm1 = EMname + %subst(EMnam2:1:10);
       a_ENenm2 = %subst(EMnam2:11:50);
       a_ENefn1 = EMfdnm + %subst(EMfdn2:1:10);
       a_ENefn2 = %subst(EMfdn2:11:50);
       a_ENcon1 = EMcont;
       a_ENatt1 = EMattn;
       a_ENcn31 = EMcnt3;
       a_ENfen1 = EMfein;
       a_ENtob1 = EMtpob;
       a_ENma11 = EMmad1;
       a_ENma21 = EMmad2;
       a_ENmct1 = EMmcty;
       a_ENmst1 = EMmst;
       a_ENmzp1 = EMmzip;
       a_ENpa11 = EMpad1;
       a_ENpa21 = EMpad2;
       a_ENpct1 = EMpcty;
       a_ENpst1 = EMpst;
       a_ENpzp1 = EMpzip;
       a_ENnam2 = EMname;
       a_ENanm1 = EMname + %subst(EMnam2:1:10);
       a_ENanm2 = %subst(EMnam2:11:50);
       a_ENfdn2 = EMfdnm;
       a_ENafn1 = EMfdnm + %subst(EMfdn2:1:10);
       a_ENafn2 = %subst(EMfdn2:11:50);
       a_ENfen2 = EMfein;
       a_ENtob2 = EMtpob;
       a_ENfnnm = GetGrpName(EMfnd);
       a_ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:%Dec(%date():*cymd):
                  a_ENagnm);
       a_ENcar1 = xxCarrier#;
       a_ENpfd1 = ELpfdt;
       a_ENptd1 = ELptdt;
       a_ENdsc1 = 'Waiver of Subrogation Add/Update';
       a_ENdt   = %Dec(%date():*cymd);
       a_ENtime = %Dec(%time():*hms);

       // Before Line
       I = 1;
       If xxupd;
          xxBfLn(I) = %editc(xxBFseq#:'X');
          I += 1;
          xxBfLn(I) = xxBFDesc;
          xxupd = *off;
       Endif;

       // After Line
       I = 1;
       If xxAFseq# <> *Zeros;
          xxAfLn(I) = %editc(xxAFseq#:'X');
       Endif;
       I += 1;
       xxAfLn(I) = xxAFDesc;

       For I = 1 to 5;
          If xxBfLn(I) <> *Blanks or xxAfLn(I) <> *Blanks;
             a_ENBlin = xxBfLn(I);
             a_ENAlin = xxAfLn(I);
             Write pen06;
             a_ENtime = a_ENtime + 1;
             a_ENSeq# = a_ENSeq# + 1;
          Endif;
       Endfor;
      /End-Free

     p WriteWMENL6     e
      //------------------------------------------
      // Get Carrier #
      //------------------------------------------
     p GetCarrier#     b
     d GetCarrier#     pi             5s 0

      /Free
       Chain (ELco#:ELfnd) pf200;
       Return F2car#;
      /End-Free

     p GetCarrier#     e
        //*** End Add ***** 05/24/23 **************************************
