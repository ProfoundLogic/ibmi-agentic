     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h*
     h*  Description: Response program to return CheckBox Values for Bank Info Detail
     h*               And also return Visibility indicators for E-Pay info
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: WTGRPPRI
     h*  Calls to:
     h*
     h*  Created on 04/25/2020 by C:300 for WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fwmbip     if   e             disk    rename(pbi01:pbi0p)
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
     DOutGrpCfg        pr                  ExtPgm('WTGRPPRI06')
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

     d isEpay          s               n   inz
     d visClmInd       s              1    inz
     d visClmMed       s              1    inz
     d visClmExp       s              1    inz
     d visClmLeg       s              1    inz

     d psds          esds                  extname(szq1p)

      /Free
       Chain inRRN pbi0p;
       If not %found;
          BuildJSON(outJSON :'RtnCode' :'No record found for RRN passed: ' +
           %char(inRRN));
          Return;
       Endif;

      // Check If E-Pay is installed and setup at bank account master WSCKL
      // Then Check if the vendor is setup for the Application code for this account
       isEpay = *off;
       isEpay = isModEnabled('E-Pay');
       Chain (BIco#:BIacct) pck0p;
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
             If EVMappl = BIappl and EVMVend = CKePayVn;
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

       outJSON = ' ';
       BuildJSON(outJSON :'Points' :BIlogo);
       BuildJSON(outJSON :'Points1' :BIhdcy);
       If isEpay = *On;
          BuildJSON(outJSON :'Points2' :'Y');
       Else;
          BuildJSON(outJSON :'Points2' :'N');
       Endif;
       BuildJSON(outJSON :'Points3' :visClmInd);
       BuildJSON(outJSON :'Points4' :visClmMed);
       BuildJSON(outJSON :'Points5' :visClmExp);
       BuildJSON(outJSON :'Points6' :visClmLeg);

       RtnCode = ' ';
       Return;

      /End-Free
