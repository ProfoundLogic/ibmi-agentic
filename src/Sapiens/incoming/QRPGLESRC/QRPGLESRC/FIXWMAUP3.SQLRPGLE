       CTL-OPT COPYRIGHT('(c) 2024 Sapiens. All Rights Reserved.');
       CTL-OPT dftactgrp(*no) ACTGRP(*caller) BNDDIR('WTBNDDIR');
       //******************************************************************
       //                                                                 *
       //  FIXWMAUP3 - Update Audit Received Date and Final Audit Run Dt  *
       //                                                                 *
       //  Description:                                                   *
       //                                                                 *
       //  This program will set Audit Reveived Date and Final Audit run  *
       //  Date with the Transaction date of the latest Audit Bill run.   *
       //                                                                 *
       //  Created: 10/14/24 C:291  Support:  PHW115558                   *
       //                                                                 *
       //******************************************************************
       //  Revisions:                                                     *
       //                                                                 *
       //                                                                 *
       //******************************************************************
       Dcl-Pi *N;
        inRRN packed (9:0);
       End-Pi;

       Dcl-F wmaup  Disk Usage(*Input:*Update) Rename(pau01:pau0p) ;
       Dcl-F wdehl6 Disk Usage(*Input) Keyed Rename(peh01:peh06);
       Dcl-F wmupl  Disk Usage(*Input) Keyed;

       //Copy Books
      /Free
      /Copy sPrDate
      /Copy sPrSql
      /End-Free

       //Program Status Data Structure
       Dcl-Ds SZQ1P ext psds;
       End-Ds;

       //Stand Alone
       Dcl-S currDate Zoned(7:0);
       Dcl-S currTime Zoned(6:0);
       Dcl-S flgAudBillRan char(1);
       Dcl-S tranDate Zoned(7:0);

       Dcl-ds dsWMAUP ExtName('WMAUP') Prefix(q);
        rrnWMAUP Zoned(9:0);
       End-Ds;
       Dcl-s NullInds Int(5) dim(35);

       // Main Procesing
       Exec Sql Set Option Commit = *None;
       currTime = %Dec(%Char(%Time():*iso0):6:0);
       currDate = %Dec(%Char(%Date():*cymd0):7:0);

       Chain Q1user PUP01;
       Exec Sql
         Declare wmaup_c1 Cursor For select wmaup.*, rrn(wmaup) From wmaup
         Where AURCVD = 0
         Order By AUCO#, AUFND, AUEMP#, AUDIV, AUFYR;
       Exec Sql
       Open wmaup_c1;
       Dou Not IsValidFetch(sqlStt);
          Exec Sql Fetch Next From wmaup_c1 Into :dsWMAUP:NullInds;
          Select;
          When sqlStt = '02000';
             Leave;
          When Not IsValidFetch(sqlStt);
             Leave;
          Endsl;
          //Retrieve Transaction date of latest Audit run
          flgAudBillRan = 'N';
          Clear tranDate;
          Setgt (qAUco#:qAUfnd:qAUemp#:qAUdiv:qAUfyr) peh06;
          Dou %Eof(wdehl6);
             Readpe (qAUco#:qAUfnd:qAUemp#:qAUdiv:qAUfyr) peh06;
             If %Eof;
                Leave;
             EndIf;
             If EHFAF = 'A';
                flgAudBillRan = 'Y';
                tranDate = EHtrdt;
                Leave;
             Endif;
          Enddo;
          If tranDate <> 0 and flgAudBillRan = 'Y';
             Chain rrnWMAUP pau0p;
             If %Found(WMAUP);
                AUrcvd = tranDate;
                If AUprdt = 0;
                   AUprdt = tranDate;
                Endif;
                AUuser = Q1user;
                AUpgm  = Q1pgm;
                AUtime = currTime;
                AUdt   = currDate;
                Update pau0p;
             Endif;
          Endif;
       Enddo;

       Exec Sql
       Close wmaup_c1;

       *InLr = *on;

