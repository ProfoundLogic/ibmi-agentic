     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to Return Bank Account Info
     h*               And also return Visibility indicators for E-Pay info
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTGRPPRI
     h*  Calls to:
     h*
     h*  Created on 03/28/2020 by C:300 for WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwsckl     if   e           k disk    rename(pck01:pck0p)
      * E-Pay Vendor Bank Account File
     fwdb2l1    if   e           k disk    rename(pb201:pb200)
      * E-Pay Vendor Maintenance File
     fwmevml1   if   e           k disk    rename(pevm01:pevm00)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrModule

      *entry
     DOutGrpCfg        pr                  ExtPgm('WTGRPPRI05')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DOutGrpCfg        pi
     D inRRN                          9  0
     D inGenParm                    512
     D outJSON                    32767
     D RtnCode                        1

     D comp            s                   like(CKco#)
     D acct            s                   like(CKacct)
     D startPos        s              5  0 inz
     D endPos          s              5  0 inz
     D len             s              5  0 inz
     D value           s             20    inz

     d isEpay          s               n   inz
     d visClmInd       s              1    inz
     d visClmMed       s              1    inz
     d visClmExp       s              1    inz
     d visClmLeg       s              1    inz

     d psds          esds                  extname(szq1p)

      /Free
       //                          1   5  8  9
       // input data is in format: ,ccc,acct
        // ccc = Co#, acct = Bank Account

       Exsr ProcessGenParm;
       If RtnCode = 'X';
         RtnCode = ' ';
         BuildJSON(outJSON :'RtnCode' :'GenParm scan error');
         Return;
       Endif;
       Chain(n) (comp:acct) pck0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

      // Check If E-Pay is installed and setup at bank account master WSCKL
      // Then Check if the vendor is setup for the Application code for this account
       isEpay = *off;
       isEpay = isModEnabled('E-Pay');
       //Chain (comp:acct) pck0p;
       If %found(wsckl);
          If CKePayVn = *blanks;
             isEpay = *off;
          Endif;
       Endif;
       If isEpay = *on;
          isEpay = *off;
          Setll *loval pevm00;
          Read pevm00;
          Dow Not %Eof(WMEVML1);
           //If EVMappl = BIappl and EVMVend = CKePayVn;
             If EVMVend = CKePayVn;         // Checking only 1 for ADD mode
                isEPay = *on;
                Leave;
             Endif;
             Read pevm00;
          Enddo;
       Endif;

      // Check visibility of Available E-Pay Payment (Reserve) Types
       If isEpay = *on;
          Setll *loval pevm00;
          Reade CKePayVn pevm00;
          Dow Not %Eof(wmevml1);
             If EVMpTyp = 'C';
                visClmInd = 'Y';
             Endif;
             If EVMpTyp = 'M';
                visClmMed = 'Y';
             Endif;
             If EVMpTyp = 'E';
                visClmExp = 'Y';
             Endif;
             If EVMpTyp = 'L';
                visClmLeg = 'Y';
             Endif;
             Reade CKePayVn pevm00;
          Enddo;
       Endif;


       BuildJSON(outJSON :'Points' :CKai01);
       BuildJSON(outJSON :'Points1' :CKai02);
       BuildJSON(outJSON :'Points2' :CKai03);
       BuildJSON(outJSON :'Points3' :CKrtup);
       BuildJSON(outJSON :'Points4' :CKrtlw);
       BuildJSON(outJSON :'Points5' :CKtrns);
       BuildJSON(outJSON :'Points6' :CKbnk#);
       BuildJSON(outJSON :'Points7' :%char(CKckdg));
       BuildJSON(outJSON :'Points8' :CKaxou);
       BuildJSON(outJSON :'Points9' :CKonus);
       BuildJSON(outJSON :'Points10' :CKvchr);
       BuildJSON(outJSON :'Points11' :CKnam1);
       BuildJSON(outJSON :'Points12' :CKnam2);

       If isEpay = *On;
          BuildJSON(outJSON :'Points13' :'Y');
       Else;
          BuildJSON(outJSON :'Points13' :'N');
       Endif;
       BuildJSON(outJSON :'Points14' :visClmInd);
       BuildJSON(outJSON :'Points15' :visClmMed);
       BuildJSON(outJSON :'Points16' :visClmExp);
       BuildJSON(outJSON :'Points17' :visClmLeg);

       RtnCode = ' ';
       Return;

       //=================================================================
       // extract values from genparm
       //=================================================================
       BegSr ExtractValue;
       value = *blanks;
       If startPos > 0;
         endPos = %Scan(',':inGenParm:startPos);
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
       // extract Company No.
         startPos = %Scan(',':inGenParm)+1;
         If startPos <= 1;
           LeaveSr;
         Endif;
         Exsr ExtractValue;
         comp = %int(%trim(value));
       // extract Bank Account
         startPos = %Scan(',':inGenParm:startPos)+1;
         Exsr ExtractValue;
         acct = %trim(value);
         Rtncode = ' ';
       On-Error;
         Rtncode = 'X';
       EndMon;
       EndSr;
      /End-Free
