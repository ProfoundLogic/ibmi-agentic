     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title W1099RP - 1099 Processing Prompt
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  1099 Processing Prompt
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created on 9/11/2015 by C:295 for Project: WC2362
     h**********************************************************************
     h*  Revisions:
     h*  01/18/17 C:180 Support 45100 - Fix SQL year selection.
     h*
     h*  01/03/17 c:62 Support 44921(114)
     h*                Expand SQL statement work field.
     h*
     h*  01/25/16 c:62 Support 39511(63)
     h*                Don't use %char that drops the leading zero.
     h*
     h*    01/07/16 C:65  Project: WC2401
     h*                   Recompile Only
     h*
     h*
     h**********************************************************************
     fw1099fpneccf   e             workstn extfile('W1099FPNEC')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fwxt5p     o    e             disk    rename(pt501:pt50p) usropn
     fwxtnp     o    e             disk    rename(ptn01:ptn0p) usropn
     fwxgsp     if   e           k disk    rename(pgs01:pgs0p)
     fwxgsl     if   e           k disk    rename(pgs01:pgs00)
     fwdf2l     if   e           k Disk    rename(pf201:pf200)
     fwmacp     if   e             Disk    rename(pac01:pac0p)

     d psds          esds                  extname(szq1p)
      * constant
     d intFlgDim       c                   10


     d outKC           s              1
     d outKL           s              1
     d inCall          s              1
     d inRRn           s              9  0

     d decEarlyYear    s                   Like(scYr)
     d decPrevYr       s                   Like(scYr)
     d isPrevYrAvail   s              1n

     d*curYear         s                   Like(scYr)
     d inWait          s               n   Inz(*Off)
     d NoYear          s               n   Inz(*Off)
     d inCancel        s               n   Inz(*Off)
     d conInfMsgTitle  c                   '******* Information Message *******'
     d onError         s               n   Inz(*Off)
     d Error           s               n   Inz(*Off)
     d strCo#          s              3
     d IntCO#          s              3  0
     d strFnd          s              3
     d strYr           s              4
     d strComm         s              1
     d strOutQ         s             10
     d strPofl         s              1
     d strIncl         s              1
     d strInfoMsgF     s            256    inz
     d*** Begin Chg *** 01/03/17 *********************************************
     d*strFund         s            256    inz
     d strFund         s            800    inz
     d*** End   Chg *** 01/03/17 *********************************************
     d strCommand      s            255
     d*** Begin Chg *** 01/03/17 *********************************************
     d*sqlStmt         s            250    inz
     d sqlStmt         s           2250    inz
     d*** End   Chg *** 01/03/17 *********************************************
     d strInfoMsg      s            256    inz
     d outMode         s              1
     d outTapd         s              1
     d outCcnt         s              8
     d outSeqn         s              8
     d strdev          s             10
     d strccnt         s              8
     d strseqn         s              8
     d strSavFnd       s              3

     d ArrYear         s                   Like(scYr)  Dim( 999 ) descend
     d StartPosition   s             10i 0
     d FoundPosition   s             10i 0
     d Index           s             10i 0
     d IndexYr         s             10i 0
     d Count           s             10i 0
     d Counti          s             10i 0
     d strYear         s              4a
     d strfinyear      s              4a   Inz(' ')
     d intarr          s               n   Inz(*Off)

     d inFND           s              3  0
     d outConm         s             40
     d outFnam         s             40
     d outLvl          s              3  0
     d outType         s              1
     d outQ            s             10

     dqcmdExc          pr                  ExtPgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     d ComAPI          pr                  extpgm('WA001R')
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d outConm                       40
     d outFnam                       40
     d outLvl                         3  0
     d outType                        1
     d outQ                          10

     d* Call - Create 1099 Workfile & Report - Submitter
     d w1099c1         pr                  extpgm('W1099C1')
     d strCo#                         3
     d strFnd                         3    const
     d strYr                          4
     d strComm                        1
     d strOutQ                       10
     d strMode                        1    Const
     d strPofl                        1
     d strIncl                        1

     d* Call - Submit Tape creation w/Initialize Tape
     d w1099c3         pr                  extpgm('W1099C3NEC')
     d strdev                        10
     d strccnt                        8
     d strseqn                        8

     d* Call - W1099RT
     d w1099rt         pr                  extpgm('W1099RTNEC')
     d outKC                          1
     d outKL                          1
     d outMode                        1
     d outTapd                        1
     d outCcnt                        8
     d outSeqn                        8

     d* Program Start
     d ProgramStart    pr

     d* Main Processing
     d Main            pr

     d/copy sPrDate
     d/copy sPrString
     d/copy SPrCompany

     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrUsrPrms
     d/copy wTclmTblPr
     d/copy sPrGenPop
    
     c* Entry of Program.
     c     *entry        plist
     c                   parm                    outKC
     c                   parm                    outKL
     c                   parm                    inCall
     c                   parm                    inRRn

      /free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       If scCancel = *on;
          outKL = *on;
       Endif;

       *inlr = *on;

       Return;
      /end-free
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /free

       // Load Reporting Year and Default Values.
          scPrint = 'L';
          scIncMan ='N';
          strInfoMsgF = 'The 1099 work file is currently being used.-
           Cannot submit request.';

             Exsr LoadYear1;

          // Load Action panel with the Company Name.
          // Get company name
          clear scCompName;
          intCo# = userCo#(q1user);
          // Get company name
          scCompName = GetCompNam(intCo#);
          aPnlHeight = 610;
       //===================================================================
       // Load Year1
       //===================================================================
       BegSr LoadYear1;

       Clear arrYear;

       Setll (*Loval) pgs00;
       Read pgs00;
       Dow not %eof(wxgsl);
          If GSIefl = 'I';
             If strFund = *Blanks;
                strFund = (%trim(%Char(GSfnd)));
             Else;
                strFund = %trim(strFund) + ',' + (%trim(%Char(GSfnd)));
              EndIf;
          EndIf;
       Read pgs00;
       Enddo;

       Exsr WmacpYear;
       Exsr WdpapYear;
       Exsr WdabpYear;

       // Load the year to screen variable
       Exsr ReportingYear;
       isNoEnter = '0';
       scYrChoice = scYrValue;
       scYrWidth = 65;
       scYrTip = *blanks;
       scEnterTip = *blanks;
       If scYrChoice = *blanks;
          isNoEnter = '1';
          scYrChoice = 'No years available for selection.';
          scYrWidth = 210;
          scYrTip = 'This process will not be availble to continue.';
          scEnterTip = 'No years available for selection.';
       Endif;

       Endsr;

       //===================================================================
       // Load Year from WMACP
       //===================================================================
       BegSr WmacpYear;
       sqlstmt = *blanks;

       //*** Begin change *** 01/18/17 *************************************
       //sqlstmt = 'Select distinct ''20''||substr(char(ACTRDT),2,2) '+
       //'from wmacp where ACtrDT > (select min(ACTRDT) from wmacp '+
       //'where ACFND in(' +  %trim(strFund) + '))';

       sqlstmt = 'Select distinct ''20''||substr(char(ACTRDT),2,2) '+
       'from wmacp where ACtrDT > (select min(ACTRDT) from wmacp '+
       'where ACtrDT > 1000100 and ACFND in(' +  %trim(strFund) + '))';
       //*** End change *** 01/18/17 *************************************

       Exec SQL Prepare sql1 From :sqlstmt;
       Exec SQL Declare Year1 Cursor For sql1;

       Exec SQL Open Year1;
       Exec SQL Fetch From Year1 into :decEarlyYear;
       Dow SqlCod <> 0100;
          If decEarlyYear <> *Zeros;
              count = count + 1;
              ArrYear(count) = decEarlyYear;
          EndIf;
          Exec SQL Fetch next From Year1 into :decEarlyYear;
       Enddo;
       Exec SQL close Year1;

       Endsr;
       //===================================================================
       // Load Year from Wdpap
       //===================================================================
       BegSr WdpapYear;
       sqlstmt = *blanks;

       //*** Begin Change *** 01/18/17 *************************************
       //sqlstmt = 'Select distinct ''20''||substr(char(PATRDT),2,2) '+
       //'from wdpap where PATRDT > (select min(PATRDT) from wdpap '+
       //'where PAFND in(' +  %trim(strFund) + '))';

       sqlstmt = 'Select distinct ''20''||substr(char(PATRDT),2,2) '+
       'from wdpap where PATRDT > (select min(PATRDT) from wdpap '+
       'where PATRDT > 1000100 and PAFND in(' +  %trim(strFund) + '))';
       //*** End   Change *** 01/18/17 *************************************

       Exec SQL Prepare sql2 From :sqlstmt;
       Exec SQL Declare Year2 Cursor For sql2;

       Exec SQL Open Year2;
       Exec SQL Fetch From Year2 into :decEarlyYear;
       Dow SqlCod <> 0100;
          If decEarlyYear <> *Zeros;
                count = count + 1;
                ArrYear(count) = decEarlyYear;
          EndIf;
          Exec SQL Fetch next From Year2 into :decEarlyYear;
       Enddo;
       Exec SQL close Year2;

       Endsr;
       //===================================================================
       // Load Year from Wdabp
       //===================================================================
       BegSr WdabpYear;
       sqlstmt = *blanks;

       //*** Begin Change *** 01/18/17 *************************************
       //sqlstmt = 'Select distinct ''20''||substr(char(ABDUE),2,2) '+
       //'from wdabp where ABDUE > (select min(ABDUE) from wdabp '+
       //'where ABFND in(' +  %trim(strFund) + '))';

       sqlstmt = 'Select distinct ''20''||substr(char(ABDUE),2,2) '+
       'from wdabp where ABDUE > (select min(ABDUE) from wdabp '+
       'where ABDUE > 1000100 and ABFND in(' +  %trim(strFund) + '))';
       //*** End   Change *** 01/18/17 *************************************

       Exec SQL Prepare sql3 From :sqlstmt;
       Exec SQL Declare Year3 Cursor For sql3;

       Exec SQL Open Year3;
       Exec SQL Fetch From Year3 into :decEarlyYear;
       Dow SqlCod <> 0100;
          If decEarlyYear <> *Zeros;
                count = count + 1;
                ArrYear(count) = decEarlyYear ;
          EndIf;
          Exec SQL Fetch next From Year3 into :decEarlyYear;
       Enddo;
       Exec SQL close Year3;

       Endsr;
       //===================================================================
       // Load ReportingYear
       //===================================================================
       BegSr ReportingYear;

          Clear scYrValue;

       // Load The Reporting Year with existing previous year.
          sorta ArrYear;
          indexYr = 1;
          Dou (indexYr = count);
             strYear  = %trim(%char(ArrYear(indexYr)));
             If strYear <> *blanks;
                If %int(strYear) = (%subdt(%date():*Years));
                   IndexYr = IndexYr + 1;
                Else;
                scyr = %int(strYear);
                Leave;
             Endif;
          Endif;
          Enddo;


       // Load The Reporting Year sequentially.
          counti= 1;
          for counti to count;
          strfinyear  = %trim(%char(ArrYear(counti)));

            If scYrValue = *blanks;
               scYrValue = strfinyear;

            ElseIf %scan(strfinyear:scYrValue) = 0;
               scYrValue = %trim(scYrValue) + ',' + strfinyear;
            EndIf;
          Endfor;

       Endsr;

      /end-free
     p ProgramStart    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       Select;
       // Process Workfile
          When inCall = 'F';

          // Process until exit button is pressed.
          DoU scCancel = *On Or (scEnter = *On And inwait = *Off);
              Exsr ExfmtScreens;

              If scEnter =*on;
                 Exsr Validate;
                 If NoYear = *on;
                    Leave;
                 Endif;
                 If Error  = *on;
                    scEnter =*off;
                 Endif;
              Endif;

          // Process Workfile
             If inCall = 'F' and scEnter =*on ;
                Exsr ProcessWorkfil;
             Endif;
          Enddo;

          // Process Trasmit
             When inCall = 'T';
             Exsr ProcessTransmit;
          Endsl;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 550;
          Write rcdComPnl;
          Write rcdTabGrp;
          Exfmt rcdWorkFil;
          Read rcdComPnl;
          Read rcdTabGrp;
       EndSl;

       Endsr;
       //===================================================================
       // Validate
       //===================================================================
       BegSr Validate;

       decPrevYr = (%subdt(%date():*Years)-1);
       isPrevYrAvail = %scan(%trim(%char(decPrevYr)):scYrValue) <> 0;


       If scYr <> decPrevYr;
          strInfoMsg = 'The year selected is not the prior calendar year.';
          If not isPrevYrAvail;
             strInfoMsg = %trim(strInfoMsg) +
              ' The prior calendar year is not available for the -
               selected groups.';
          Endif;
          strInfoMsg = %trim(strInfoMsg) +
           ' Please verify that the year selected is correct.';
          scEnter = PopTwoBtn(strInfoMsg:conInfMsgTitle:'Continue':'Cancel');
       Endif;

       Endsr;
       //===================================================================
       // Process Workfile
       //===================================================================
       Begsr ProcessWorkfil;

       // Clear WXT5P
       onError = *on;
       inWait = *off;
       incancel = *Off;

       Monitor;
          strCommand = 'CLRPFM WXT5P';
          Callp qcmdexc(strCommand:%len(strCommand));
          Exsr SubmitWorkfile;
          On-Error;
          If PopTwoBtn(strInfoMsgF:conInfMsgTitle:'Wait':'Exit') = '1';
             inwait = *on;
             LeaveSr;
          Endif;
       EndMon;

       EndSr;
       //===================================================================
       // SubmitWorkfile
       //===================================================================
       Begsr SubmitWorkfile ;
          strSavFnd = *blanks;
          // Copy active groups from WXGSP to WXTNP
          Read pgs0p;
          Dow not %eof(wxgsp);
             If GSIefl = 'I';
                //  Save the Group number for submit job parm, if only one group
                Select;
                When strSavFnd = *blanks;
                   //*** Begin Chg *** 01/25/16 **********************************
                   //**********strSavFnd = %char(GSFnd);
                   strSavFnd = %EditC(GSFnd:'X');
                   //*** End   Chg *** 01/25/16 **********************************
             // When strSavFnd <> *zeros;
             //    strSavFnd = *zeros;
                Endsl;
                If not %open(wxT5p);
                   Open wxT5p;
                Endif;
                //*** Begin Chg *** 01/25/16 **********************************
                //**********T5Co# =  %char(GSCo#);
                //**********T5Fnd =  %char(GSfnd);
                T5Co# =  %EditC(GSCo#:'X');
                T5Fnd =  %EditC(GSfnd:'X');
                //*** End   Chg *** 01/25/16 **********************************
                T5Tcc =  GSTcc;
                Chain (GSCo#:GSfnd) pf200 ;
                If %Found(WDF2L);
                   T5Fein=  F2Fein;
                Endif;
                write pt50p;
                If %Open(wxT5p);
                   Close wxT5p;
                Endif;
             Endif;
             Read pgs0p;
          Enddo;

          ComAPI(GSCo#:*zeros:outConm:outFnam:outLvl:outType:outQ);

          //*** Begin Chg *** 01/25/16 **********************************
          //********** strCo#  =  %char(GSCo#) ;
          strCo#  =  %EditC(GSCo#:'X') ;
          //*** End   Chg *** 01/25/16 **********************************
          strFnd  =  strSavFnd;
       // If strFnd = *zeros;
       //    Clear strFnd;
       // Endif;
          //*** Begin Chg *** 01/25/16 **********************************
          //**********strYr   =  %char(scyr)  ;
          strYr   =  %EditC(scyr:'X')  ;
          //*** End   Chg *** 01/25/16 **********************************
          strComm =  sctype;
          strOutQ =  outQ;
          strPofl =  scPrint;
          strIncl =  scIncMan;

          // Call CLP to submit the list
          w1099c1(strCo#:strFnd:strYr:strComm:strOutQ:'D':strPofl:strIncl);
       Endsr;
       //===================================================================
       // Process Transmit
       //===================================================================
       Begsr ProcessTransmit;

       // Clear WXTNP
       onError = *on;
       inWait = *off;
       incancel = *Off;

       Monitor;
          strCommand = 'CLRPFM WXTNP';
          Callp qcmdexc(strCommand:%len(strCommand));
          Exsr SubmitTransmit;
          On-Error;
          If PopTwoBtn(strInfoMsgF:conInfMsgTitle:'Wait':'Exit');
             outKl = *on;
          Endif;
       EndMon;

       EndSr;
       //===================================================================
       // Submit Transmit
       //===================================================================
       Begsr SubmitTransmit ;
          strSavFnd = *blanks;
          // Copy active groups from WXGSP to WXTNP
          Read pgs0p;
          Dow not %eof(wxgsp);
             If GSIefl = 'I';
                //  Save the Group number for submit job parm, if only one group
                Select;
                When strSavFnd = *blanks;
                   //*** Begin Chg *** 01/25/16 **********************************
                   //**********strSavFnd = %char(GSFnd);
                   strSavFnd = %EditC(GSFnd:'X');
                   //*** End   Chg *** 01/25/16 **********************************
             // When strSavFnd <> *zeros;
             //    strSavFnd = *zeros;
                Endsl;
                If not %open(wxtnp);
                   Open wxtnp;
                Endif;
                //*** Begin Chg *** 01/25/16 **********************************
                //**********TNCo# =  %char(GSCo#);
                //**********TNGrp =  %char(GSfnd);
                TNCo# =  %EditC(GSCo#:'X');
                TNGrp =  %EditC(GSfnd:'X');
                //*** End   Chg *** 01/25/16 **********************************
              //TNTcc =  GSTcc;
              //Chain (GSCo#:GSfnd) pf200 ;
              //If %Found(WDF2L);
              //   TNFein=  F2Fein;
              //Endif;
                write ptn0p;
                If %Open(wxtnp);
                   Close wxtnp;
                Endif;
             Endif;
             Read pgs0p;
          Enddo;

       // Call W1099RT
          outMode = 'A';
          outCcnt =*blanks;
          outSeqn =*blanks;
          outTapd =*blanks;
          w1099rt(outkc:outkl:outMode:outTapd:outCcnt:outSeqn);
          If outkc = *on or outkl=*on;
             Leavesr;
          else;
           strseqn = outSeqn;
           strccnt = outCcnt;
           strdev  = outTapd;

             // Call W1099C3 - Submit Tape creation w/Initialize Tape
             w1099c3 (strdev:strccnt:strseqn);
          Endif;
       Endsr;
      /end-free
     p Main            e
