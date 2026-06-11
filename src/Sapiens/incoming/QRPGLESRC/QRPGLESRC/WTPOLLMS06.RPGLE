     h**********************************************************************
     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h*
     h*  Description: Response program to update Third Party Payment records
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WPILOCMAIN, WPILOCMST
     h*  Calls to:
     h*
     h*  Created on 06/26/2019 by C:65  for SY407
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h* 10/22/25 C:349 Support: 119523                                     *
     h*                Edit for payment plan when S/R Online enabled.      *
     h*                Set soft error message when discrepancy is found.   *
     h*                Close and delete file overrides before exiting to   *
     h*                prevent abend during next call.                     *
     h*                                                                    *
     h* 04/24/25 C:079 Support: 118909 (Vital)                             *
     h*                Override to WMP3P member in case override was       *
     h*                set to the ARCHIVE member                           *
     h*                                                                    *
     h* 05/14/21 C:180 Project: WC1986                                     *
     h*                Add Default SRD info Note.                          *
     h*                Add payment plan validation when enabling 3rd party.*
     h* 09/21/21 C:136 Don't edit for S/R Online policy here.              *
     h*                Add env as a key field to WMP3L so that Quote can   *
     h*                write a record.                                     *
     h*                                                                    *
     h* 05/03/21 C:180 Support xxxx -                                      *
     h*                Remove improper chain                               *
     h*                Do not add record unless record is enabled          *
     h*                (Enable checkbox checked) DSPF changes as well as   *
     h*                pgm changes.                                        *
     h*                                                                    *
     h*                Add Defaul Info check box to 3rd Party grid.        *
     h*                Add new parm to detect quote environment.           *
     h*                Add default starting policy year for quote env.     *
     h*                Archive Third Party Record.                         *
     h*                                                                    *
     h**********************************************************************
     F*** Beg Chg *** 04/24/25 ****************************************
     F***wmp3p     uf a e             disk    rename(pp301:pp30p)
     Fwmp3p     uf a e             disk    rename(pp301:pp30p) UsrOpn
     F*** End Chg *** 04/24/25 ****************************************
     /*** Begin Add    *** 05/03/21 ****************************************
     f                                     infds(p3FDS)
     /*** End   Add    *** 05/03/21 ****************************************
     F*** Beg Chg *** 04/24/25 ****************************************
     F***wmp3l     if   e           k disk    rename(pp301:pp300)
     Fwmp3l     if   e           k disk    rename(pp301:pp300) UsrOpn
     F*** End Chg *** 04/24/25 ****************************************
     /*** Begin Delete *** 05/03/21 ****************************************
     f**                                   infds(p3FDS)
     /*** End   Delete *** 05/03/21 ****************************************
     Fwmemp     if   e             disk    rename(pem01:pem0p)
      //*** Begin Add *** 05/14/21 *****************************************
       Dcl-F wtabl usage(*input) keyed include(PBS01);
      //*** End   Add *** 05/14/21 *****************************************
      //*** Begin Add *** 10/22/25 *****************************************
       Dcl-F wdell1 usage(*input) keyed;
      //*** End   Add *** 10/22/25 *****************************************
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      *** Begin Add *** 05/14/21 ************************************
      /copy wPrBank
      *** End   Add *** 05/14/21 ************************************
      *** Begin Add *** 05/14/21 ************************************
      /copy wPrEmplr
      *** End   Add *** 05/14/21 ************************************
      *** Beg Add *** 04/24/25 **************************************
      /copy sprGenPop
      /copy sprFiles
      *** End Add *** 04/24/25 **************************************

      *entry
     DThirdPayUpdate   pr                  ExtPgm('WTPOLLMS06')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DThirdPayUpdate   pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     DstrType          s                   like(P3PrType)
     DstrEnbld         s                   like(P3Enbld)
      *** Begin Add *** 05/14/21 ************************************
       Dcl-Pr SRONLI  Extpgm;
         Q3co#    char(3);
         Q3fnd    char(3);
         Q3emp#   char(9);
         Q3div    char(5);
         Q3fyr    char(3);
         Q3efdt   char(8);
         Q3mode   char(1);
         Q3err    char(1);
         Q3data   char(100);
         Q3paType char(1);
         Q3Env    char(1);
       End-Pr;
       Dcl-S strQ3co#    char(3) inz;
       Dcl-S strQ3fnd    char(3) inz;
       Dcl-S strQ3emp#   char(9) inz;
       Dcl-S strQ3div    char(5) inz;
       Dcl-S strQ3fyr    char(3) inz;
       Dcl-S strQ3efdt   char(8) inz;
       Dcl-S strQ3mode   char(1) inz;
       Dcl-S strQ3err    char(1) inz;
       Dcl-S strQ3data   char(100) inz;
       Dcl-S strQ3paType char(1) inz;
       Dcl-S strQ3env    char(1) inz;
       Dcl-S strBsCd     char(3) inz;
       Dcl-C WC30391 Const('Valid/Active ACH setup required for S/R online -
        payment plan.');
       Dcl-C WC40173 Const('This is not a self reporting policy.');
       Dcl-C WC40174 Const('Policy is not set for online self reporting.');
       Dcl-C WC40175 Const('No third party setup.');
       Dcl-C WCR0513 Const('Invalid table value found in field.');
       Dcl-DS KeyWTBSP likerec(PBS01:*KEY);
     DintSfyr          s                   like(P3Sfyr)
     DstrEnv           s                   like(P3EnvFlg)
     DstrInfD          s                   like(P3DftSrD)
     DstrEnv2          s              1
     DintELfyr3p       s                   like(P3Sfyr)
     DisDftSrD3p       s               N   inz(*Off)
     DstrDftSrD3p      s              1
     DstateRepDiv      s              5  0 inz
      *** End   Add *** 05/14/21 ************************************
     DxxEfdt           s                   like(P3Efdt)
     DxxExdt           s                   like(P3Efdt)
     DsysDt            s                   like(P3Efdt)
     DintEfdt          s              8  0 inz
     DintExdt          s              8  0 inz
     Dp3RRN            s              9  0 inz
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s            100    inz
     D errMsg          s            500    inz
     D msg             s            100    inz
       //*** Beg Add *** 04/24/25 ***************************************
       Dcl-S intCmdLen packed(15:5) inz(36);
       Dcl-S CmdOvrDbf char(36);
       Dcl-S CmdDltOvr char(36);
       //*** End Add *** 04/24/25 ***************************************

     d p3FDS           ds
     d  p3RRNVal             397    400b 0
     d*** Beg Add *** 04/24/25 *********************************************
       Dcl-Pr RunCommand ExtPgm('QCMDEXC');
        inCmd    char(2750) const;
        inCmdLen packed(15:5) const;
       End-Pr;
     d*** End Add *** 04/24/25 *********************************************

     d psds          esds                  extname(szq1p)

      /Free
       //
       // Incoming RRN is for employer master record.
       //
       // input data is in format: |emrrn|type|enabled|Sfyr|Env|efdt|exdt|InfD|Q
        // rrn     - wmp3p rrn
        // type    - type of process - 100A
        // enabled - Y or blank flag for process being enabled.
        // Starting Policy Year
        // Environment
        // efdt    - MDYY of record eff date.
        // exdt    - MDYY of record exp date.
        // Infd    - Default State Reporting Division Information Flag
        // Qte     - Qte environment.
        // SFyr    - Starting policy year
        // Fyr     - Fund year
        //*** Beg Add *** 04/24/25 *********************************
       If not %Open(WMP3L);
          CmdOvrDbf = %Trim('OVRDBF FILE(WMP3L) MBR(WMP3L)');
          RunCommand(CmdOvrDbf:intCmdLen);
          Open WMP3L;
       Endif;
       If not %Open(WMP3P);
          CmdOvrDbf = %Trim('OVRDBF FILE(WMP3P) MBR(WMP3P)');
          RunCommand(CmdOvrDbf:intCmdLen);
          Open WMP3P;
       Endif;
        //*** End Add *** 04/24/25 *********************************
       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'rtnCode' :'GenParm scan error');
         //*** Begin Add *** 10/22/25 *****************************************
         Exsr DltOverride;
         //*** End   Add *** 10/22/25 *****************************************
         Return;
       Endif;

       Chain inRRN pem0p;
       If not %found;
          BuildJSON(outJSON :'rtnCode' :'No record found for Emp Master RRN ' +
           'passed: ' + %char(inRRN));
          //*** Begin Add *** 10/22/25 *****************************************
          Exsr DltOverride;
          //*** End   Add *** 10/22/25 *****************************************
          Return;
       Endif;

       //*** Begin Change *** 05/14/21 ****************************************
       //If p3RRN <> 0;
          //Chain p3RRN pp30p;
          //If not %found;
          //   BuildJSON(outJSON :'rtnCode' :'No record found for RRN ' +
          //    'passed: ' + %char(p3RRN));
          //   Return;
          //Endif;

       If p3RRN <> 0;
          Chain p3RRN pp30p WMP3Pds;
          If not %found;
             BuildJSON(outJSON :'rtnCode' :'No record found for RRN ' +
              'passed: ' + %char(p3RRN));
             //*** Begin Add *** 10/22/25 *****************************************
             Exsr DltOverride;
             //*** End   Add *** 10/22/25 *****************************************
             Return;
          Endif;
         //*** Begin Add *** 10/22/25 *****************************************
          //Retrieve billing schedule code from Employer Detail File
          Chain (EMCo#:EMFnd:p3sfyr:EMEmp#:EMDiv) pel01;
          If %found;
            strBsCd = %EditC(ELBSCD:'X');
          Endif;
         //*** End   Add *** 10/22/25 *****************************************
       Else;
         Clear pp30p;
       Endif;
       //*** End  Change *** 05/14/21 ****************************************

       //*** Begin Delete *** 05/03/21 ****************************************
       //Else;
          //Chain(n) (EMCo#:EMFnd:EMEmp#:EMDiv:strType) pp300;
          //If %found;
          //   p3RRN = p3RRNVal;
          //Endif;
       //Endif;
       //*** End   Delete *** 05/03/21 ****************************************


       sysDt = DateTo7(SystemDate);

       Exsr ErrorCheck;

       //*** Begin Add  *** 05/14/21 ****************************************
       If p3RRN <> 0;
          ArchiveWMP3P(WMP3Pds:'UPDATE');
       Endif;
       //*** End   Add  *** 05/14/21 ****************************************

       P3Enbld  = strEnbld;
       P3Efdt   = xxEfdt;
       P3Exdt   = xxExdt;
       //*** Begin Add *** 05/14/21 ********************************************
       P3Sfyr   = intSfyr;
       P3EnvFlg = strEnv;
       P3DftSrd  = strInfD;
       //*** End  Add *** 05/14/21 *********************************************
       P3User   = q1User;
       P3Pgm    = q1Pgm;
       P3Dt     = sysDt;
       P3Time   = TimeTo6(SystemTime);

       If p3RRN <> 0;
          Update pp30p;
       //*** Begin Add *** 05/14/21 ********************************************
          P3rrnVal = P3rrn;
       //*** End  Add *** 05/14/21 *********************************************
       Else;
          //*** Begin Add *** 05/03/21 *****************************************
          //*** Beg Chg ***** 09/21/21 *****************************************
          //   Chain (EMCo#:EMFnd:EMEmp#:EMDiv:strType) pp300;
          //   If not %found and P3enbld = 'Y';
          If strEnbld = 'Y';
          //*** End Chg ***** 09/21/21 *****************************************
          //*** End   Add *** 05/03/21 *****************************************
             P3Co#    = EMCo#;
             P3Fnd    = EMFnd;
             P3Emp#   = EMEmp#;
             P3Div    = EMDiv;
             P3PrType = strType;
             //*** Begin Add *** 05/14/21 **************************************
             //*** Beg Chg *** 09/21/21 ****************************************
             //  P3EnvFlg = strEnv2;
             P3EnvFlg = strEnv;
             //*** End Chg *** 09/21/21 ****************************************
             P3Iusr   = q1User;
             P3Ipgm   = q1Pgm;
             P3Idt    = sysDt;
             P3Itm    = TimeTo6(SystemTime);
             //*** End   Add *** 05/14/21 **************************************
             P3User   = q1User;
             P3Pgm    = q1Pgm;
             P3Dt     = sysDt;
             P3Time   = TimeTo6(SystemTime);
             Write pp30p;
          //*** Begin Add *** 05/03/21 *****************************************
          Endif;
          //*** End   Add *** 05/03/21 *****************************************
       Endif;

       //*** Begin Change *** 05/14/21 *****************************************
       //Chain(n) (EMCo#:EMFnd:EMEmp#:EMDiv:strType) pp300;
       //If %found;
       //   BuildJSON(outJSON :'rtnRRN' : %editc(P3RRNVal:'X'));
       //Endif;

       BuildJSON(outJSON :'rtnRRN' : %editc(P3RRNVal:'X'));
       BuildJSON(outJSON :'rtnSfyr' : %Editc(P3Sfyr:'X'));
       BuildJSON(outJSON :'rtnEnv' : P3EnvFlg);
       //*** End  Change  *** 05/14/21 ***************************************

       //* Output dates for display on grid.
       //*** Begin Chg *** 10/22/25 *****************************************
       //BuildJSON(outJSON :'rtnEfdt' : %editw(DtMDYY(P3Efdt):'  /  /    '));
       BuildJSON(outJSON :'rtnEfdt':%xlate('\':'/':%EditC(DtMDYY(P3Efdt):'X')));
       //*** End   Chg *** 10/22/25 *****************************************
       If P3Exdt <> 0;
       //*** Begin Chg *** 10/22/25 *****************************************
          //BuildJSON(outJSON :'rtnExdt' : %editw(DtMDYY(P3Exdt):'  /  /    '));
       BuildJSON(outJSON :'rtnExdt':%xlate('\':'/':%EditC(DtMDYY(P3Exdt):'X')));
       //*** End   Chg *** 10/22/25 *****************************************
       Endif;

       //*** Begin Add *** 05/14/21 ***************************************
       BuildJSON(outJSON :'rtnInfD' :P3DftSrd);
       strDftSrD3p = *blanks;
       stateRepDiv = GetStRptDv(P3co#:P3Fnd:P3emp#);
       If P3div <> stateRepDiv;
          isDftSrD3p = Has3rdPartyAcct(P3co#:P3fnd:P3emp#:P3div:P3prtype:
           sysDt:intELfyr3p:strEnv2:strDftSrD3p);
       Endif;
       BuildJSON(outJSON :'rtnDftSrD':%trim(strDftSrD3p));
       //** End   Add *** 05/14/21 ****************************************
       BuildJSON(outJSON :'rtnCode' : 'Data updated.');
       //*** Beg Add *** 04/24/25 *********************************         ***
       //*** Beg Del *** 10/22/25 *********************************
       //If %Open(WMP3L);
       //   Close WMP3L;
       //   CmdDltOvr = %Trim('DLTOVR FILE(WMP3L)');
       //   RunCommand(CmdDltOvr:intCmdLen);
       //Endif;
       //If %Open(WMP3P);
       //   Close WMP3P;
       //   CmdDltOvr = %Trim('DLTOVR FILE(WMP3P)');
       //   RunCommand(CmdDltOvr:intCmdLen);
       //Endif;
       //*** End Del *** 10/22/25 *********************************
                //*** End Add *** 04/24/25 ************************************
       //*** Begin Add *** 10/22/25 *****************************************
       Exsr DltOverride;
       //*** End   Add *** 10/22/25 *****************************************

       RtnCode = ' ';
       Return;

       //=================================================================
       // Check for errors
       //=================================================================
       BegSr ErrorCheck;

         //*** Beg Add ***** 10/22/25 *****************
         Clear msg;
         //*** End Add ***** 10/22/25 *****************
         errMsg = ' ';
         xxEfdt = 0;
         xxExdt = 0;

         //* Default effective date to later of current date or employer eff
         //* date if one is not present.
         If intEfdt = 0;
            If sysDt < EMEFD1 and EMEFD1 <> 0;
               xxEfdt = EMEFD1;
            Else;
               xxEfdt = sysDt;
            Endif;
         Else;
         //* Validate the effective date when entered.
            xxEfdt = DtCymd(intEfdt);
            If xxEfdt = 0;
               msg = 'Effective date must be a valid date.';
               Exsr AddError;
            Endif;
         Endif;

         //* Validate the expiration date when entered.
         If intExdt <> 0;
            xxExdt = DtCymd(intExdt);
            If xxExdt = 0;
               msg = 'Expiration date must be a valid date.';
               Exsr AddError;
            Endif;
         Endif;

         //* Ensure Expiration date is not prior to Effective date.
         If xxExdt <> 0 and xxEfdt <> 0 and xxExdt < xxEfdt;
            msg = 'Expiration date cannot be prior to effective date.';
            Exsr AddError;
         Endif;

       //*** Begin Add *** 05/14/21 ********************************************
       //*** beg del ***** 09/21/21 *****************
       //*** Beg Add ***** 10/22/25 *****************
       //10/22/25 * Reactivate the code below to validate the payment plan but
       //10/22/25 * make the error a soft error.
       //* Check payment plan if attempting to enable 3rd Party.
            If strEnbld = 'Y'
            and errMsg = *Blanks;
              KeyWTBSP.bsco# = P3co#;
              KeyWTBSP.bskey = strBsCd;
              Chain %kds(KeyWTBSP) PBS01;
              If BSonli = 'Y';
                 strQ3co#  = %editc(P3co#:'X');
                 strQ3fnd  = %editc(P3fnd:'X');
                 strQ3emp# = %editc(P3emp#:'X');
                 strQ3div  = %editc(P3div:'X');
                 strQ3fyr  = %editc(P3Sfyr:'X');
                 strQ3efdt = %editc(%dec(%char(%date(sysDt:*cymd):*usa0):8:0):
                  'X');
                 strQ3mode = '1';
                 strQ3err  = *blanks;
                 strQ3data = strBsCd;
                 strQ3paType = *blank;
                 strQ3env  = strEnv2;
                 SRONLI(strQ3co#:strQ3fnd:strQ3emp#:strQ3div:strQ3fyr:strQ3efdt:
                  strQ3mode:strQ3err:strQ3data:strQ3paType:strQ3env);

                 Select;
                 When strQ3err = '0';  // Edits Passed
                 When strQ3err = '1';  // No EFT
                    msg = WC30391;
                    Exsr AddError;
                 When strQ3err = '2';  // Not Online Self Reporting
                    msg = WC40174;
                    Exsr AddError;
                 When strQ3err = '3';  // Not Self Reporting
                    msg = WC40173;
                    Exsr AddError;
                 When strQ3err = '4';  // No Third Party
                    msg = WC40175;
                    Exsr AddError;
                 When strQ3err = 'E';  // Miscellaneous Error
                    msg = WCR0513;
                    Exsr AddError;
                 EndSl;
                 If errMsg <> ' ';
                    BuildJSON(outJSON :'rtnSoftError' :%trim(errMsg));
                    Clear errMsg;
                 Endif;
              Else;
                 BuildJSON(outJSON :'rtnSoftError' :'X');
              Endif;
            Endif;
           //*** End Add ***** 10/22/25 *****************
           //*** end chg ***** 09/21/21 *****************
       //************** End   Add *** 05/14/21 *********************************
         If errMsg <> ' ';
            BuildJSON(outJSON :'rtnError' :%trim(errMsg));
            Return;
         Endif;

       EndSr;
       //=================================================================
       // add message to error message string
       //=================================================================
       BegSr AddError;

         If errMsg = ' ';
            errMsg = msg;
         Else;
            errMsg = %trim(errMsg) + '  ' + %trim(msg);
         Endif;

       EndSr;
       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;

         value = *blanks;

         If startPos > 0;
            endPos = %Scan('|':inGenParm:startPos);
            If endPos > 0;
               len = endPos-startPos;
               If len > 0;
                  value = %Subst(inGenParm:startPos:len);
               Endif;
            Endif;
         Endif;

       EndSr;
       //=================================================================
       // Process genparm
       //=================================================================
       BegSr ProcessGenParm;

         startPos = 0;
         endPos = 0;

         Monitor;
         // extract wmp3p rrn
           startPos = %Scan('|':inGenParm)+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           If value = ' ' or value = 'No RRN';
              value = '0';
           Endif;
           p3RRN = %int(%trim(value));

         // extract Type
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strType = %trim(value);

         // extract Enabled flag
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strEnbld = %trim(value);

         //*** Begin Add *** 05/14/21 ****************************************
         // Starting Policy Year
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           intSfyr = %int(%trim(value));

         // Environment flag
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strEnv = %trim(value);
         //*** End   Add *** 05/14/21 ****************************************

         // extract record Eff Date
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           intEfdt = %int(%xlate('/':' ':%trim(value)));

         // extract record Exp Date
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           intExdt = %int(%xlate('/':' ':%trim(value)));

        //*** Begin Add *** 05/14/21 ************************************
        // extract Default State Reporting Div Info
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strInfD = %trim(value);

        // extract Environment
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strEnv2 = %trim(value);

        // extract Policy Year
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           intELfyr3p = %int(%trim(value));

        // extract Billing Schedule code
           startPos = endPos+1;
           If startPos <= 1;
             LeaveSr;
           Endif;
           Exsr ExtractValue;
           strBsCd = %subst(value:1:3);
        //*** End   Add *** 05/14/21 ************************************

           Rtncode = ' ';
         On-Error;
           Rtncode = 'X';
         EndMon;

       EndSr;
      //*** Begin Add *** 10/22/25 *****************************************
       //=================================================================
       // Delete file overrides
       //=================================================================
       BegSr DltOverride;

         If %Open(WMP3L);
            Close WMP3L;
            CmdDltOvr = %Trim('DLTOVR FILE(WMP3L)');
            Monitor;
              RunCommand(CmdDltOvr:intCmdLen);
            On-Error;
            EndMon;
         Endif;
         If %Open(WMP3P);
            Close WMP3P;
            CmdDltOvr = %Trim('DLTOVR FILE(WMP3P)');
            Monitor;
              RunCommand(CmdDltOvr:intCmdLen);
            On-Error;
            EndMon;
         Endif;

       EndSr;
      //*** Begin End *** 10/22/25 *****************************************
      /End-Free
