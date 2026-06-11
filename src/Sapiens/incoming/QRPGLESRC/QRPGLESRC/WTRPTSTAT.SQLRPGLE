     h Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     h*****************************************************************
     h/title WTRPTSTAT - Statistical Submissions
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h********************************************************************
     h*
     h* Description: Statistical Submissions
     h*
     h* Called from: W7350R
     h* Calls to:    None
     h*
     h* Created   : 04/20/21
     h* Programmer: C:291
     h* Project   : 148WCJ
     h*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/01/23 C:291 Ticket: 104371                                *
     H*                 When filter is applied and selected all the   *
     H*                 records by "Include All" option to delete,    *
     H*                 it also deleted records that were not selected*
     H*                                                               *
     H*  11/29/22 C:342 Ticket: 104212(OIC)                           *
     H*                 Screen blow up when trying to load more than  *
     H*                 9999 records into subfile                     *
     H*                                                               *
     H*  06/10/22 c:62 Ticket: 102512(148)                            *
     H*                Complete the deletion proess                   *
     H*                                                               *
     H*****************************************************************
     fWtRptStat cf   e             workstn extfile('WTRPTSTAT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflStatSub:rrnStat)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)

     fWmssl1    uf   e           k disk    Infds(dbfds1)
     fWmssp     uf a e             disk    rename(pss01:pss0p)
     fWdf2l     if   e           k disk    rename(pf201:pf200)
     fWmsrl     if   e           k disk    rename(psr01:psr00)
     fWxwsl     uf a e           k disk    rename(pws01:pws00) usropn
     fWxwsl1    uf a e           k disk
     fwmupl     if   e           k disk    rename(pup01:pup00)

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

      //Standalone fields
     d isExit          s              1
     d isCancel        s              1
     d isSubmit        s               n
     d isMode          s              1
     d outRRN          s              9  0
     d intTime         s              9  0
     d isDeleted       s               n   inz(*off)
     d isDelresp       s               n   inz(*off)
     d flgDltMsg       s              1    inz('N')
     d flgFirstRcd     s              1    inz('Y')
     d flgConMsg       s              1    inz('Y')
     d svCarrier       s                   like(F2car#)
     d intCo#          s              3  0
     d intUnits        s              5  0
     d intRrn          s              9  0
     d xxRrn           s              9  0
     d ##Rrn#          s              9  0
     d saveRrn         s                   like(rrnStatRow)
     d doLoad          s               n   inz(*on)
      //*** Begin Add *** 11/29/22 **********************************
     d RecCount        s              9  0 inz
     d WrtCount        s              9  0 inz
     d meetWCSR        s              1
     d ValFrmMt        s                   like(SSrpmy)
     d ValToMt         s                   like(SSrpmy)
     d SubFrmDt        s                   like(SSsbdt)
     d SubToDt         s                   like(SSsbdt)
     d isValid         s               n
     d isFrstIter      s               n   inz(*on)
     d isFrstDsp       s               n   inz(*on)
     d charMsg         s            100
      //*** End   Add *** 11/29/22 **********************************

      *Entry parameters
     d StatSubmission  pr                  Extpgm('WTRPTSTAT')
     d  inMode                        1
     d StatSubmission  pi
     d  inMode                        1

      * Extract Data
     d ExtractData     pr                  extpgm('W7350R02')
     d inExit                         1
     d inCancel                       1
     d outMode                        1
     d outRrn                         9  0

      * Submit Data
     d SubmitData      pr                  extpgm('W7352R')
     d inExit                         1
     d inCancel                       1
     d outMode                        1
     d outTime                        9  0
     d outUsrOutq                    10

      * Units
     d Units           pr                  extpgm('W7354R')
     d inExit                         1
     d inCancel                       1
     d outMode                        1
     d outRrn                         9  0

      * SubCorrection
     d SubCorrections  pr                  extpgm('W7352R2')
     d inExit                         1
     d inCancel                       1

      * Resubmissions
     d Resubmissions   pr                  extpgm('W7352R3')
     d inExit                         1
     d inCancel                       1

     d W7417C4         pr                  extpgm('W7417C4')
     d  outFlag                       1    const

      * Clear Subfiles
     d ClrSubFiles     pr

      * Load Endorsement Form grid
     d LoadStatGrid    pr

      * Main Processing
     d Main            pr

      //*** Begin Add *** 11/29/22 **********************************
      * Filter for WC statistical reporting screen.
     d MeetFlt         pr

      * Show filter if record count of WMSSP > 9999
     d ProcFltWCSR     pr
      //*** End   Add *** 11/29/22 **********************************

      * Process other options
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      //*** Begin Add *** 11/29/22 **********************************
      * Validate value entered on filter screen.
     d ValidateWCSR    pr              n
      //*** End   Add *** 11/29/22 **********************************

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrCompany
     d/copy sPrRunCmd
     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy wprGenPop
       //*** Begin Add *** 11/29/22 **********************************
     d/copy wPrPolErr
       //*** End   Add *** 11/29/22 **********************************
    
      /Free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       Close wxwsl;
       RunCmd('DLTF FILE(QTEMP/WXWSL)':22);
       RunCmd('DLTF FILE(QTEMP/WXWSP)':22);
       *InLr = *on;

      /End-Free
      //=================================================================
       // ClrQtempFile  -  Clear files in Qtemp
       //===================================================================
     p ClrQtempFile    b
     d ClrQtempFile    pi
      /Free
        If %Open(wxwsl);
           Close wxwsl;
        Endif;
        RunCmd('CLRPFM FILE(QTEMP/WXWSP)':24);
      /End-Free
     p ClrQtempFile    e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free
       //Endorsement form subfile
       rrnStat = 0;
       clrStat = *On;
       dspStatSub  = *Off;
       dspStatCtl  = *Off;

       Write rcdWCStat;

       clrStat = *Off;
       dspStatSub = *On;
       dspStatCtl = *On;
      /End-Free
     p ClrSubfiles     e

      //=================================================================
       // CrtQtempFile  -  Clear files in Qtemp
       //===================================================================
     p CrtQtempFile    b
     d CrtQtempFile    pi
      /Free
       RunCmd('SYDUPFILE FROM(*LIBL/WXWSP) TO(QTEMP/*SAME)':43);
       RunCmd('SYDUPFILE FROM(*LIBL/WXWSL) TO(QTEMP/*SAME)':43);
       ClrQtempFile();
       If not %Open(wxwsl);
          Open wxwsl;
       Endif;
      /End-Free
     p CrtQtempFile    e

       //===================================================================
       // Load Stattistical Submissions Grid
       //===================================================================
     p LoadStatGrid    b
     d LoadStatGrid    pi
      /Free
       If inMode = '1';
          dspVisb = *Off;
       Else;
          dspVisb = *On;
       Endif;

       scInMode = inMode;

       dspStatSub = *On;
       clear dbrrn1;

       If not %Open(wxwsl);
          Open wxwsl;
       Endif;

       //*** Begin Add *** 11/29/22 **********************************
       WrtCount = *zeros;
       //*** End   Add *** 11/29/22 **********************************
       Setll (intCo#) pss01;
       Reade(n) (intCo#) pss01;
       Dow Not %Eof(wmssl1);

          //*** Begin Add *** 11/29/22 **********************************
          meetWCSR = *on;
          // Validate for filter
          If isValid = *on;
             MeetFlt();
          Endif;
          If meetWCSR = *on and WrtCount < 9999;
          //*** End   Add *** 11/29/22 **********************************
          //Group
          ScGroup = SSfnd;

          //Valuation Month
          scValMonth = dt6MDYY(SSrpmy);
          scRpmy = SSrpmy;

          //Submission Date
          scSubmDate = CvtToDate(SSsbDt);

          //Carrier
          chain (intCo#:SSfnd) pf200;
          If %found(wdf2l);
             scCarrier = F2car#;
          Else;
             scCarrier = 0;
          Endif;

          //Reporting Levels
          scRptLvl = SSrptl;

          //Select checkbox
          scSelect = ' ';
          nxchgStat = *off;
          Chain (intCo#:SSfnd:SSrpmy) pws00;
          If %Found(wxwsl);
             scSelect = 'Y';
             nxchgStat = *on;
          Endif;

          //No of Units
          intUnits = 0;
          Setll (intCo#:SSfnd:SSrpmy) psr00;
          Reade (intCo#:SSfnd:SSrpmy) psr00;
          Dow not %Eof(wmsrl);
             intUnits = intUnits + 1;
             Reade (intCo#:SSfnd:SSrpmy) psr00;
          Enddo;
          scNoOfUnit = intUnits;

          Select;
          When inmode = '1';
            isLnkVisb = *Off;
          Other;
            isLnkVisb = *On;
          Endsl;

          scStatRrn# = dbrrn1;
          rrnStat = rrnStat + 1;
          //*** Begin Add *** 11/29/22 **********************************
          WrtCount = WrtCount + 1;
          //*** End   Add *** 11/29/22 **********************************

          clear scStaRowSl;
          If (rrnStat = rrnStatRow) or (rrnStat = 1 and rrnStatRow = 0);
             scStaRowSl = 'A';
          Endif;

          Write sflStatSub;
          //*** Begin Add *** 11/29/22 **********************************
          Endif;
          //*** End   Add *** 11/29/22 **********************************
          Reade(n) (intCo#) pss01;
       Enddo;

       rrnStat = 1;
      /End-Free
     p LoadStatGrid    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /Free
       // Process until exit button is pressed.
       Dou ExitNow = *On;
          //*** Begin Add *** 11/29/22 **********************************
          If isFrstIter = *on or WrtCount >= 9999;
             RecCount = *zeros;
             Exec SQL
             select Count(*) into :RecCount From wmssl1;
             If RecCount > 9999;
                ProcFltWCSR();
             Endif;

             // Screen Exit
             If Btnfilcan = *on;
             Btnfilcan = *off;
                Leave;
             Endif;
          Endif;
          isFrstIter = *off;
          //*** End   Add *** 11/29/22 **********************************
          If doLoad = *on;
             ClrSubfiles();             //Clear Sub-files
             LoadStatGrid();            //Load Statistical submission Grid
          Endif;
          If rrnPosStat <> 0 and rrnPosStat <= rrnStatRow;
             rrnStat = rrnPosStat;
          Else;
             rrnStat = 1;
          Endif;
          saveRrn = rrnStatRow;

          //*** Begin Add *** 11/29/22 **********************************
          If WrtCount < 9999;
             isFrstDsp = *off;
          //*** End   Add *** 11/29/22 **********************************
          // Process Endorse Screen
          Exsr ExfmtScreens;
          //*** Begin Add *** 11/29/22 **********************************
          Endif;
          //*** End   Add *** 11/29/22 **********************************
          If rrnStatRow = 0 and saveRrn <> 0;
             rrnStatRow = saveRrn;
          Endif;

          doLoad = *On;
          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When btnPerAct = *on;
             ProcPerAct();
          When btnSubmtRe = *on;
             ProcResubmit();
          When btnSubmCor = *on;
             ProcCorrection();
          When btnExtrDta = *on;
             ProcExtract();
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             ClrQtempFile();
          //*** Begin Add *** 11/29/22 **********************************
          // Filter Processing.
          When BtnFilter = *on;
             BtnFilter = *off;
             Dou BtnFilCan = *on or BtnFilApp = *on;
                ProcFltWCSR();
             Enddo;
             If BtnFilCan = *on;
                BtnFilCan = *off;
                isValid = *on;
                doLoad = *off;
             Endif;

          // Filter clear processing
          When BtnClrFltr = *on;
             BtnClrFltr = *off;
             clear RcdWCSRFlt;
             ValFrmMt = *zeros;
             ValToMt = *zeros;
             SubFrmDt = *zeros;
             SubToDt = *zeros;
             If RecCount > 9999;
                ProcFltWCSR();
                If BtnFilCan = *on;
                   ExitNow = *on ;
                Endif;
             Endif;
          //*** End   Add *** 11/29/22 **********************************
          Other;
             ProcOther();
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;
       // Initialize Screen fields
       scSelAct = '--- Select List Action ---';
       Select;
       When TabIndex = 0;
          TabHeight = 850;
          APnlHeight = TabHeight + 50;
          Write rcdComPnl;
          Write rcdTabStat;
          Exfmt rcdWCStat;
          Read rcdComPnl;
          Read rcdTabStat;
       EndSl;
       Endsr;
      /End-Free
     p Main            e

       //*** Begin Add *** 11/29/22 **********************************
       //==================================================================
       // MeetFlt - Determine if the records meet the Filter criteria
       //==================================================================
     p MeetFlt         b
     d MeetFlt         pi
      /free

       If (scFrmGrp <= SSfnd and scToGrp >= SSfnd) or
          (scFrmGrp = 0 and scToGrp = 0);
          meetWCSR = *on;
       Else;
          meetWCSR = *off;
       Endif;


       If meetWCSR = *on;
          // Check for valuation month
          If (ValFrmMt <= SSrpmy and ValToMt >= SSrpmy) or
             (ValFrmMt = 0 and ValToMt = 0);
             meetWCSR = *on;
          Else;
             meetWCSR = *off;
          Endif;
       Endif;

       If meetWCSR = *on;
          // Check for submission date
          If (SubFrmDt <= SSsbdt and SubToDt >= SSsbdt) or
             (SubFrmDt = 0 and SubToDt = 0);
             meetWCSR = *on;
          Else;
             meetWCSR = *off;
          Endif;
       Endif;

      /end-free
     p MeetFlt         e

       //*** End   Add *** 11/29/22 **********************************
       //===================================================================
       // ProcCorrection  -  Process Corrections
       //===================================================================
     p ProcCorrection  b
     d ProcCorrection  pi
      /Free
        isExit = '0';
        isCancel = '1';
        SubCorrections(isExit:isCancel);
      /End-Free
     p ProcCorrection  e

       //===================================================================
       // ProcessDelete - Delete Stat record
       //===================================================================
     p ProcessDelete   b
     d ProcessDelete   pi
      /Free
       isDeleted = *off;
       flgDltMsg = 'N';
       ReadC sflStatSub;
       Dow not %Eof;
          scStatDel  = *Off;
          If flgDltMsg = 'N';
             isDelresp = PopTwoBtn('Do you want to delete selected WC -
              Statistical records?': 'Confirm Delete':'Yes':'No':0:140);
             flgDltMsg = 'Y';
             If isDelResp = *Off;
                Leave;
             Endif;
          Endif;
          Select;
          //*** Beg Chg ***** 08/01/23 **************************
          //***When isDelResp = *On;
          When isDelResp = *On and scSelect = 'Y';
          //*** End Chg ***** 08/01/23 **************************
             Chain scStatRrn# pss0p;
             //*** Begin Add *** 06/10/22 **********************************
             ExSr DeleteTransmission;
             //*** End   Add *** 06/10/22 **********************************
             Delete pss0p;
             isDeleted = *On;
          Endsl;
          ReadC sflStatSub;
       Enddo;
       If isDeleted = *On;
          PopOneBtn('Statistical Submission deleted!':'Delete':
           'Ok':0:140);
       Endif;
       //*** Begin Add *** 06/10/22 **************************************
       //*==================================================================
       //* DeleteTransmission - Deletes the WCSTAT Transmission Work Files
       //*==================================================================
       BegSr DeleteTransmission;

         // Delete Link,header,name,address record
         Exec Sql Delete from WMSRP
            Where SRco# = :SSco# And SRfnd = :SSfnd And SRrpmy = :SSrpmy;

         // Delete Exposure records
         Exec Sql Delete from WDSEP
            Where SEco# = :SSco# And SEfnd = :SSfnd And SErpmy = :SSrpmy;

         // Delete Loss records
         Exec Sql Delete from WDSLP
            Where SLco# = :SSco# And SLfnd = :SSfnd And SLrpmy = :SSrpmy;

         // Delete Unit total record
         Exec Sql Delete from WMSQP
            Where SQco# = :SSco# And SQfnd = :SSfnd And SQrpmy = :SSrpmy;

         // Delete ICR records
         Exec Sql Delete from WDSIP
            Where SIco# = :SSco# And SIfnd = :SSfnd And SIrpmy = :SSrpmy;

       EndSr;
       //*** End   Add *** 06/10/22 **************************************
      /End-free
     p ProcessDelete   e

       //===================================================================
       // ProcExtract  -  Process Extract
       //===================================================================
     p ProcExtract     b
     d ProcExtract     pi
      /Free
        isExit = '0';
        isCancel = '1';
        isMode = 'A';
        outRRN = 0;
        ExtractData(isExit:isCancel:isMode:outRRN);
      /End-Free
     p ProcExtract     e

       //*** Begin Add *** 11/29/22 **********************************
       //==================================================================
       // ProcFltWCSR process filter if record count > 9999
       //==================================================================
     p ProcFltWCSR     b
     d ProcFltWCSR     pi
     d intIncr         s              3  0
      /free

       isValid = *off;

       Dow isValid = *off;
          If isFrstDsp = *on;
             isFrstDsp = *off;
             TabHeight = 850;
             APnlHeight = TabHeight + 50;
             Write rcdComPnl;
             Write RcdTabStat;
             ClrSubfiles();
             rrnStat = 1;
             Write sflStatSub;
             Write RcdWCStat;
             rrnStat = 0;
          Endif;
          If RecCount > 9999;
             visWCSRFlt = *on;
          Endif;
          Exfmt RcdWCSRFlt;
          If Btnfilapp = *on;
             If (scFrmGrp = 0 and scToGrp = 0 and
              scValFrmMt = 0 and scValToMt = 0 and
              scSubFrmDt = 0 and scSubToDt = 0);
                isValid = *on;
             elseif ValidateWCSR();
                isValid = *on;
                endif;
             endif;
          If BtnfilCan = *on;
             leave;
          endif;
       Enddo;

      /end-free
     p ProcFltWCSR     e
       //*** End   Add *** 11/29/22 **********************************

       //===================================================================
       // ProcOther - Process Other Options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
      /Free
       ReadC sflStatSub;
       Dow not %Eof;
          //process Units
          Select;
          When scStatunit = *On;
             scStatUnit = *Off;
             isExit = '0';
             isCancel = '1';
             isMode = ' ';
             outRRN = scStatRRN#;
             Units(isExit:isCancel:isMode:outRRN);
          //process Delete
          When scStatDel = *On;
             scStatDel  = *Off;
             isDelresp = PopTwoBtn('Do you want to delete WC Statistical -
              records?': 'Confirm Delete':'Yes':'No':0:140);
             Select;
             When isDelResp = *On;
                Chain scStatRrn# pss0p;
                //*** Begin Add *** 06/10/22 *******************************
                ExSr DeleteTransmission;
                //*** End   Add *** 06/10/22 *******************************
                Delete pss0p;
                PopOneBtn('Statistical Submission deleted!':'Delete':
                 'Ok':0:140);
             Endsl;
          Endsl;
          ReadC sflStatSub;
       Enddo;
       //*** Begin Add *** 06/10/22 **************************************
       //*==================================================================
       //* DeleteTransmission - Deletes the WCSTAT Transmission Work Files
       //*==================================================================
       BegSr DeleteTransmission;

         // Delete Link,header,name,address record
         Exec Sql Delete from WMSRP
            Where SRco# = :SSco# And SRfnd = :SSfnd And SRrpmy = :SSrpmy;

         // Delete Exposure records
         Exec Sql Delete from WDSEP
            Where SEco# = :SSco# And SEfnd = :SSfnd And SErpmy = :SSrpmy;

         // Delete Loss records
         Exec Sql Delete from WDSLP
            Where SLco# = :SSco# And SLfnd = :SSfnd And SLrpmy = :SSrpmy;

         // Delete Unit total record
         Exec Sql Delete from WMSQP
            Where SQco# = :SSco# And SQfnd = :SSfnd And SQrpmy = :SSrpmy;

         // Delete ICR records
         Exec Sql Delete from WDSIP
            Where SIco# = :SSco# And SIfnd = :SSfnd And SIrpmy = :SSrpmy;

       EndSr;
       //*** End   Add *** 06/10/22 **************************************
      /End-Free
     p ProcOther       e

       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi
      /Free
         Select;
         When scSelAct = 'Submit Data File';
            ProcSubmtData();
         When scSelAct = 'Delete';
            ProcessDelete();
         Endsl;
      /End-Free
     p ProcPerAct      e

       //===================================================================
       // ProcResubmit - Process Resubmissions
       //===================================================================
     p ProcResubmit    b
     d ProcResubmit    pi
      /Free
        isExit = '0';
        isCancel = '1';
        Resubmissions(isExit:isCancel);
      /End-Free
     p ProcResubmit    e

       //===================================================================
       // ProcSubmtData - Submit Data file
       //===================================================================
     p ProcSubmtData   b
     d ProcSubmtData   pi
      /Free
       ReadC sflStatSub;
       flgFirstRcd = 'Y';
       flgConMsg = 'N';
       Dow not %Eof;
          If scSelect = 'Y';
             If flgFirstRcd = 'Y';
                flgFirstRcd = 'N';
                svCarrier = scCarrier;
                ClrQtempFile();
                isSubmit = *On;
                If not %Open(wxwsl);
                   Open wxwsl;
                Endif;
             Endif;
             WSco# = intCo#;
             WSfnd = scGroup;
             WSrpmy= scRpmy;
             WSpgm = Q1pgm;
             WSuser= Q1user;
             WStime= Timeto6(SystemTime());
             WSDT  = Dateto7(SystemDate());
             Write pws00;
             If scCarrier <> svCarrier and flgConMsg = 'N';
                flgConMsg = 'Y';
                isDelresp = PopTwoBtn('The NCCI Carrier # does not match ' +
                 'across all the groups that have been selected to be ' +
                 'included. This will cause a data rejection error unless ' +
                 'specific approval from the receiving Bureau has been granted.'
                 :'Information Message':'Continue':'Cancel':0:140);
                If isDelResp = *Off;
                   isSubmit = *Off;
                   Leave;
                Endif;
             Endif;
          Endif;
          ReadC sflStatSub;
       Enddo;
       if isSubmit = *On;
          intTime= Timeto6(SystemTime());
          Setll (intCo#) pws00;
          Reade (intCo#) pws00;
          Dow not %Eof(wxwsl);
             WSuser = Q1user;
             WStime = intTime;
             Write pws01;
             Reade (intCo#) pws00;
          Enddo;
          isExit = '0';
          isCancel = '1';
          isMode = 'D';
          outRRN = 0;
          SubmitData(isExit:isCancel:isMode:intTime:UPoutq);
          Select;
          When isCancel = '1';
          Other;
             ClrQtempFile();
          Endsl;
       Endif;

      /End-Free
     p ProcSubmtData   e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free

       Select;
       When inMode = '1';
          dspVisb = *Off;
       Other;
          dspVisb = *On;
       Endsl;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

       If inMode = '1';
         ChoSelAct = ' ';
       Else;
         ChoSelAct = '--- Select List Action ---,Submit Data File,Delete';
       Endif;
       scSelAct = '--- Select List Action ---';

       Chain (q1user) pup00;

       tabClick = *Off;
       //Create files in QTEMP
       CrtQtempFile();

      /End-Free
     p ProgramStart    e

       //*** Begin Add *** 11/29/22 **********************************
       //===================================================================
      //===================================================================
      // Validate Filter criteria
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateWCSR    b
     d ValidateWCSR    pi              n
     d isError         s               n   inz
      /Free

       isError = *off;

       msFrmGrp   = *blanks;
       erFrmGrp   = *off;
       msValFrmMt = *blanks;
       erValFrmMt = *off;
       msValToMt  = *blanks;
       erValToMt  = *off;
       msSubFrmDt = *blanks;
       erSubFrmDt = *off;
       msSubToDt  = *blanks;
       erSubToDt  = *off;

       ValFrmMt  = dtCYMD(scValFrmMt)/100;
       ValToMt   = dtCYMD(scValToMt)/100;

       SubFrmDt  = dtCYMD(scSubFrmDt);
       SubToDt   = dtCYMD(scSubToDt);

       If scFrmGrp <> 0;
          if (scToGrp = 0) or (scToGrp < scFrmGrp and scToGrp <> 0);
             erFrmGrp = *on;
             msFrmGrp = 'WCR0750';
             isError  = *on;
          endif;
       Endif;

       If scValFrmMt <> 0;
          If DtCheck(scValFrmMt) = 0;
             erValFrmMt = *on;
             msValFrmMt = 'WCR0500';
             isError  = *On;
          Endif;
       Endif;

       If scValToMt  <> 0;
          If DtCheck(scValToMt) = 0;
             erValToMt = *on;
             msValToMt = 'WCR0500';
             isError  = *On;
          Endif;
       Endif;

       If ( ValFrmMt <> 0 and erValFrmMt = *off and erValToMt = *off);
          if ValToMt = 0 or (ValToMt < ValFrmMt and ValToMt <> 0);
             erValFrmMt = *on;
             msValFrmMt = 'WCR0529';
             erValToMt  = *on;
             msValToMt  = 'WCR0529';
             isError    = *on;
          endif;
       Endif;

       If scSubFrmDt <> 0;
          If DtCheck(scSubFrmDt) = 0;
             erSubFrmDt = *on;
             msSubFrmDt = 'WCR0500';
             isError  = *On;
          Endif;
       Endif;

       If scSubToDt  <> 0;
          If DtCheck(scSubToDt) = 0;
             erSubToDt = *on;
             msSubToDt = 'WCR0500';
             isError  = *On;
          Endif;
       Endif;

       If erSubFrmDt = *off and erSubToDt = *off;
          erSubToDt  = AscendDates(scSubFrmDt:scSubToDt:charMsg);
          If erSubToDt  = *on;
             erSubFrmDt = *on;
             msSubToDt  = charMsg;
             msSubFrmDt = charMsg;
             isError  = *On;
          Endif;
       Endif;
      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateWCSR    e

      //*** End   Add *** 11/29/22 **********************************
