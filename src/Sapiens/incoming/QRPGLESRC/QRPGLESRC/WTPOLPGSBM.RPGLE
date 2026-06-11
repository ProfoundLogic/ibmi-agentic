     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H bnddir('WTBNDDIR')
     H*****************************************************************
    H/TITLE WTPOLPGSBM - NCCI POLICY INFORMATION PAGE - PROMPT
     H*****************************************************************
     H**REWRITE:               C:307     PROJECT: WC2400          **
     H*****************************************************************
    H* REVISIONS:
     H*
     H* 08/14/24 C:65  Project: Phw#117111 (Amwins)
     H*                Pull the policy effective date for the policy
     H*                from the earliest found on policy year instead
     H*                of from the state reporting division.  Remove un-
     H*                needed updating of submission history record fields.
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
     H*
     H* 06/23/21 C:318 Project WC2302 - Auto tab closure for
     H*                deletion.
     h*
     H* 06/08/20  C:65  Project: Phw#89454
     H*                 Remove lock on chain to submission history prior
     H*                 to display of screen, as file is no longer updated.
     H*
     H* 05/16/19  C:295 Support: 77567
     H*                 Cancellation on sciptura pop-up not working
     H*                 properly.
     H*
     H* 03/12/18  C:65  Project: 150WAD
     H*                 Chain to emplr detail prior to checking pfdt.
     H*
     H* 11/28/17  C:65  Project: Rob EBF fix
     H*                 Chain to state reporting division to avoid
     H*                 incorrectly flagging a resubmission as a
     H*                 re-write.
     H*
     H* 05/08/17  C:306 Support: 47119
     H*              1.)Added a functionality to select a group on
     H*                 Generate Policy Page Submission Requests that
     H*                 is accessed from the menu.
     H*              2.)Fixed the issue where screen was abending we
     H*                 user was not entering any date range value.
     H*
     H* 03/27/17  C:65  Project: WC2400
     H*                 Correct the change which moved the update of
     H*                 submission history to include key change
     H*                 check again.
     H*
     H*****************************************************************
      * Files Declaration
     fwtpolpgsbmcf   e             workstn extfile('WTPOLPGSBM')
     f                                     SFILE(sflpolpag:sflrrn)
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
    f** Employer Master File
     fwmeml     if   e           k Disk    Rename(pem01:pem00)
    f** Employer Detail Files
     fwdelp     if   e             Disk    Rename(pel01:pel0p)
     fwdell1    if   e           k Disk    Rename(pel01:pel01)
     f*** begin add ***  08/13/21  ***********************************
     F                                     infds(dbfds)
     f*** end add *****  08/13/21  ***********************************
     fwdell3    if   e           k Disk    Rename(pel01:pel03)
     F*** begin add ***  08/14/24  ***********************************
     Fwdellb    if   e           k disk    prefix(b)
     F                                     rename(pel01:pel0b)
     F*** end add *****  08/14/24  ***********************************
    f** NCCI Reporting - Submission History
     fwmshl1    uf   e           k Disk    Rename(psh01:psh01)
    f** Employer Endorsement Forms File
     fwmmul     if   e           k Disk    Rename(pmu01:pmu00)
    f** Admin/DARTS Control Definition File
     fwmexl     if   e           k disk    Rename(pex01:pex00)
    f** System & Application Control File Logical
     fssylf     if   e           k disk    Rename(ssy01:ssy00)
     f                                     IGNORE(SWC01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SFB01)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SDR01)
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)
     f                                     IGNORE(SGL01)
     f                                     IGNORE(SP201)
     f
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** begin add ***  08/13/21  ***********************************
     D dbfds           ds
     D  dbrrn                397    400b 0
     d polRRN          s              9  0 inz
     D*** end add *****  08/13/21  ***********************************


     d endcon          c                   const('WC000311A')

     d isPopup         s               n   Inz(*off)
     d isValid         s               n   Inz(*off)
     d isFlag31        s               n   Inz(*off)
     d strcltid        s              3    inz
     d intUsrCo#       s              3  0 inz
     d intUsrFnd       s              3  0 inz
     d intCo#          s              3  0 inz
     d outCo#          s              3    inz
      //***  Begin Add  ***  11/28/17  *****************************
     d intDiv          s                   inz like(ELdiv)
      //***  End   Add  ***  11/28/17  *****************************
     d intFnd          s              3  0 inz
     d outFnd          s              3    inz
     d intEmp#         s              9  0 inz
     d outEmp#         s              9    inz
     d intFyr          s              3  0 inz
     d outFyr          s              3    inz
     d outDiv          s              5    inz
     d strType         s              1    inz
     d isVolLbrFlg     s              1    inz
     d btnRefresh      s              1    inz(*off)
     d inFromDate      s             10    inz
     d outFromDate     s             10    inz
     d inFromDate7     s              7  0 inz
     d inToDate        s             10    inz
     d outToDate       s             10    inz
     d inToDate7       s              7  0 inz
     d inLastDate      s             10    inz
     d inLastDateI     s               D   DATFMT(*ISO) inz
     d inSHSBDT8       s              8  0 inz

     d strReissue      s              1    inz
     d strSyk          s              2    inz
     d strMsg          s            100    inz

     d isUseScript     S              1n   inz
     d processType     c                   const('Policy Page')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d inEmp#          s              9
     d inDiv           s              5
     d inOutq          s             10
     d outOutq         s             10
     d indart          s              1
     d outdart         s              1
     d inHard          s              1
     d outHard         s              1
     d inAgto          s              1
     d outAgto         s              1
     d intrrn          s              9  0 inz
     d inForm          s             15

     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)

      *****************************************************************
     * Prototype Declaration
      *****************************************************************

     * Clear Error/Message fields
     d ClrErr          pr

     * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d strCltId                       3

     * Intialize the Screen fields
     d LoadInVal       pr

     * Main Processing
     d Main            pr

     * Policy Page Report Proc.
     d PolPagProc      pr

     * Policy Page Report Submit
     d PolPagSbm       pr                  ExtPgm('W7412C')
     d outCo#                         3
     d outFnd                         3
     d outEmp#                        9
     d outDiv                         5
     d outFyr                         3
     d strReissue                     1
     d outFrDt                        8
     d outToDt                        8
     d outOutQ                       10
     d outdart                        1
     d outHard                        1
     d outAgto                        1

     * Program Start
     d ProgramStart    pr

     * Validate Panel Sreen Values
     d ValidPnl        pr

     *** *Entry Procedure.
     d WtPolPgSbmR     pr                  Extpgm('WTPOLPGSBM')
     d inPrds                        40A   const
     d inCall                         1A
     d inRRN                          9
     d inRtc                          1A
     d inCo#                          3
     d inFnd                          3
     d inFyr                          3
      *
     d WtPolPgSbmR     pi
     d inPrds                        40A   const
     d inCall                         1A
     d inRRN                          9
     d inRtc                          1A
     d inCo#                          3
     d inFnd                          3
     d inFyr                          3

     ** Copy Book Declarations.
     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy sCopyPolPr
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy sPrLetter
     d/copy sPrXML

     ***----------------------------***
     *** Main Logic starts here.
     ***----------------------------***
      /Free
      // Start Of Program
       ProgramStart();

      // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

      //===================================================================
      // ClrErr - Clear Error/Message fields
      //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       msFromDate = *Blanks;
       msToDate   = *Blanks;
       msDarts    = *Blanks;
       erFromDate = *off;
       erToDate   = *off;
       erDarts    = *off;
       isValid    = *off;

      /End-Free
     p ClrErr          e

      //===================================================================
      // LoadInVal - Intialize the Screen fields
      //===================================================================
     p LoadInVal       b
     d LoadInVal       pi
      /Free

       Clear RcdPolpage;
       isdisSbmt = *off;
       vsInfoMsg = *off;
       vsCnfRSbm = *off;
       scInfoMsg = *blanks;

       //**** Beg Add *** 05/08/17 *******************************//
       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
       //**** End Add *** 05/08/17 *******************************//

      /End-Free
     p LoadInVal       e

      //===================================================================
      // Main - Main processing
      //===================================================================
     p Main            b
     d Main            pi
      /Free

      // Process until exit button is pressed.
       Dou ExitNow = *On ;

          isUseScript = UseScriptura(processType);
          visScript = *off;
          If isUseScript or isFlag31 = *off;
             visScript = *on;
          Endif;

         // Check resubmission history
          If isPopUp = '1';   // If only PopUp
             strtype = 'P';
             strReissue = 'N';
             //***  Begin Add  ***  03/12/18  *****************************
             Chain (ELco#:ELfnd:ELfyr:ELemp#:intDiv) pel01;
             //***  End   Add  ***  03/12/18  *****************************
          //*** begin chg ***  06/08/20  *****************************************
          //*****   Chain (intCo#:intFnd:intEmp#:intfyr:strType) psh01;
             Chain(n) (intCo#:intFnd:intEmp#:intfyr:strType) psh01;
          //*** end chg *****  06/08/20  *****************************************
             If %Found(wmshl1) ;
                If SHstat = '4' or SHcode = 'I' or SHcode = 'T' or SHcode = 'N';
                   strReissue = 'Y';
                EndIf;

                If Btnrefresh = *off and SHstat > '2';
                   isdisSbmt = *on;
                   vsInfoMsg = *on;
                   vsCnfRSbm = *on;
                   If SHsbDt <> *zeros;
                      inShsbDt8 = DtMdYy(SHsbDt);
                      inLastDateI = Cvttodate(inShsbDt8);
                      scInfoMsg = 'Policy page has already been submitted on ' +
                       %char(inLastDateI);
                   Else;
                      scInfoMsg = 'Policy page has already been submitted.';
                   EndIf;
                EndIf;
          //*** begin del ***  08/14/24  ****************************************
          //****If ELpfDt <> SHpfDt and SHpfDt <> 0 and SHstat = '4';
          //****   SHsbDt = SHpfDt;
          //****   SHpfDt = ELpfDt;
          //****   SHcode = 'K';
          //****Else;
          //****   SHcode = *Blanks;
          //****   SHsbDt = 0 ;
          //****EndIf;
          //*** end del *****  08/14/24  ****************************************

           //   SHstat = '2';
           //   Update psh01;
             Endif;
          Endif;   //Endif only PopUp

         // Load screen values
          scYear = intfyr;

         //Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
            // Clear Errors
             ClrErr();
            // Validate Edits
             ValidPnl();

            //Give Info Msg that policies with Voluntary labor charge have a
            // 'WC000311A' Endorsement attached.

             If isVolLbrFlg = ' ';
             //*** Beg Add *** 05/08/17****************************/
                If isValid = *Off;
             //*** Beg Add *** 05/08/17****************************/
                   Exsr LodVolLbrChrg;
             //*** Beg Add *** 05/08/17****************************/
                EndIf;
             //*** Beg Add *** 05/08/17****************************/

                If isVolLbrFlg = *on;
                   Write rcdComPnl;
                   Write RcdTabPolP;
                   Exfmt RcdPlPgsfl;
                EndIf;

                If ExitNow = *on;
                   *InLr = *on;
                EndIf;

             EndIf;

             xxSubmit = *on;
         //***Beg Chg **** 05/08/17 ******************************/
          //** If isUseScript;
             If isUseScript and isValid = *Off and isPopUp = *on;
         //***End Chg **** 05/08/17 ******************************/
                xxProcType = processType;
                xxMass = ' ';
                In *Lock *dtaara;

                Clear svLDA;

                Chain (intCo#:intFnd) pex00;
                If %found and EXpPcd <> ' ';
                   xxDart = 'Y';
                   xxRMCD = EXpPcd;
                Else;
                   xxDart = 'N';
                   xxRMCD = ' ';
                Endif;

                Out *dtaara;
          //*** begin chg ***  08/13/21  ***********************************
          //***** xxSubmit = OutputWindow(xxProcType:xxMass);
                If polRRN <> 0;
                   xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':polRRN);
                Else;
                   xxSubmit = OutputWindow(xxProcType:xxMass);
                Endif;
          //*** end chg *****  08/13/21  ***********************************
             Endif;

             If xxSubmit = *off;
      //***  Begin Chg  ***  05/16/19  *****************************
      //***     Exsr  ExfmtScreens;
                Leave;
      //***  End   Chg  ***  05/16/19  *****************************
             Endif;

             If isValid = *Off;

                If isPopUp = '1';   // If only PopUp
                //***  Begin Add  ***  03/12/18  *****************************
                   Chain (ELco#:ELfnd:ELfyr:ELemp#:intDiv) pel01;
                //***  End   Add  ***  03/12/18  *****************************
             //*** begin add ***  08/14/24  ****************************************
                   Setll (intCo#:intFnd:intFyr:intEmp#) pel0b;
                   Reade(n) (intCo#:intFnd:intFyr:intEmp#) pel0b;
             //*** end add *****  08/14/24  ****************************************
                   chain (intCo#:intFnd:intEmp#:intfyr:strType) psh01;
                   If %found(wmshl1) ;
                //*** begin chg ***  03/27/27  ****************************************
                //*****      SHstat = '2';
                //*****      Update psh01 %fields(SHstat);
             //*** begin chg ***  08/14/24  ****************************************
             //****   If ELpfDt <> SHpfDt and SHpfDt <> 0 and SHstat = '4';
             //****      SHsbDt = SHpfDt;
             //****      SHpfDt = ELpfDt;
                      If BELpfDt <> SHpfDt and SHpfDt <> 0 and SHstat = '4';
                         SHsbDt = SHpfDt;
                         SHpfDt = BELpfDt;
             //*** end chg *****  08/14/24  ****************************************
                         SHcode = 'K';
                      Else;
                         SHcode = *Blanks;
                         SHsbDt = 0 ;
                      EndIf;

                      SHstat = '2';
                      Update psh01;
                //*** end chg *****  03/27/17  ****************************************
                   EndIf;
                EndIf;

                PolPagproc();
                Leave;
             EndIf;

          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Intialize the Screen fields
             LoadInVal();
             ClrErr();

          EndSl;

       Enddo;

       BegSr LodVolLbrChrg;
        isClear = *On;
        Write RcdPlPgsfl;
        isClear    = *Off;
        dspPlPgSub = *On;
        dspPlPgCtl = *On;
        sflRrn     =  1;
        inForm     = EndCon;

        If intEmp# = *Zero;

           inFromDate7 = DateTo7(%date(scFromDate:*usa));
           inToDate7 = DateTo7(%date(scToDate:*usa));
           Setll (intCo#:intFnd:inFromdate7:*loval:*loval) pel03;
           Reade (intco#:intFnd) pel03;
           Dow Not %Eof(Wdell3);
              If ELpfDt < scToDate  and sflRrn < 9999;
                 If ELvlCa <> *Zero;
                    Chain(n) (ELco#:ELfnd:ELemp#:ElFyr:strType) psh01;
                    If SHstat = '2' and %found(wmshl1);
                       Chain (SHco#:SHfnd:SHemp#:SHdiv:SHfyr:inForm) pmu00;
                       If not%found(wmmul);
                          isVolLbrFlg = '1';
                          scEmp# = Elemp#;
                          scDiv =  ELdiv;
                          scFyr =  Elfyr;
                          Chain (SHco#:SHfnd:SHemp#:SHdiv) pem00;
                          scName = EMname;
                          Write sflPolPag;
                          sflRrn = sflRRn + 1;
                       EndIf;
                    EndIf;
                 EndIf;
              EndIf;
              Reade (intco#:intFnd) pel03;
           Enddo;
        Else;
           Setll (intCo#:intFnd:intFyr:intEmp#:*loval) pel01;
           Reade (intCo#:intFnd:intFyr:intEmp#) pel01;
           Dow not %eof(Wdell1) and sflRrn < 9999;
              Chain(n) (ELco#:ELfnd:ELemp#:ElFyr:strType) psh01;
              If %found(wmshl1) and SHstat ='2' and ELvlCa <> 0;
                 Chain (SHco#:SHfnd:SHemp#:SHdiv:Shfyr:inForm) pmu00;
                 If not %found(wmmul);
                    isVolLbrFlg = '1';
                    scEmp# = ELemp#;
                    scDiv  = ELdiv;
                    scFyr  = ELfyr;
                    Chain (SHco#:SHfnd:SHemp#:SHdiv) pem00;
                    scName = EMname;
                    Write SflPolPag;
                    sflrrn = sflrrn + 1;
                 EndIf;
              EndIf;
              Reade (intCo#:intFnd:intFyr:intEmp#) pel01;
           Enddo;
        EndIf;
       EndSr;

      //===================================================================
      // Write Screen Formats;
      //===================================================================
       BegSr ExfmtScreens;
        Select;
        When TabIndex = 0;
           TabHeight =  710;
           APnlHeight = 770;

           If isPopUp = '1';      // popup
              Exfmt RcdPolPagT;
           Else;                 // window
              Write rcdComPnl;
              Write RcdTabPolP;
              Exfmt RcdPolPage;
              Read rcdComPnl;
              Read RcdTabPolP;
           EndIf;
        EndSl;
       Endsr;

      /End-Free
     p Main            e

      //===================================================================
      // Policy Page processing procedure.
      //===================================================================
     p PolPagProc      b
     d PolPagProc      pi
      /Free
       outCo#     = %EditC(intCo#:'X');
      //*** Beg Chg *** 05/08/17 *******************************/
       //**outFnd     = %EditC(intFnd:'X');
      //*** End Chg *** 05/08/17 *******************************/
       If isPopUp = '1';
          outEmp#  = %EditC(intEmp#:'X');
      //*** Beg Add *** 05/08/17 *******************************/
           outFnd  = %EditC(intFnd:'X');
      //*** End Add *** 05/08/17 *******************************/
       Else;
          outEmp#  = '000000000';
      //*** Beg Add *** 05/08/17 *******************************/
           outFnd  = %editc(scFnd:'X');
      //*** End Add *** 05/08/17 *******************************/
       EndIf;

       outDiv       = *Zeros;
       outFyr       = %EditC(intFyr:'X');
       outFromdate  = %EditC(scFromDate:'Z');
       outToDate    = %EditC(scToDate:'Z');

       outOutQ      = inOutQ;
       outDart      = scDarts;
       outHard      = 'Y';
       outAgto      = 'Y';

       PolPagSbm(outCo#:outFnd:outEmp#:outDiv:outFyr:strReissue:
        outFromDate:outToDate:outOutq:outDart:outHard:outAgto);
      /End-Free
     p PolPagProc      e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free

      // Intialize the Screen fields
       LoadInVal();

      // Load Action panel with the Company Name.
       intUsrCo#  = userCo#(q1user);
       scCompName = GetCompNam(intUsrCo#);
       intUsrFnd  = UserGrp(q1user);
      //*** Beg Add *** 05/08/17 *************************//
       scFnd  = UserGrp(q1user);
      //*** End Add *** 05/08/17 *************************//
       inOutQ     = GetUsrOutq(q1user);

       intCo# = intUsrCo#;
       intFnd = intUsrFnd;

      // When screen is called as pop up
       If q1prms >= 7;               // popup
          isPopUp = '1';
          intrrn = %Dec(inRrn:9:0);
      //**** Begin Add  ***  06/23/21  ****************************************
          OutElRrn = intrrn;
      //**** End Add  ***  06/23/21  ****************************************
       Else;                         // menu
          isPopUp = '0';
       EndIf;

       intEmp# = *zeros;
      //*** begin add ***  08/13/21  ***********************************
       polRRN = 0;
      //*** end add *****  08/13/21  ***********************************
       If intrrn <> 0;
          Chain intrrn pel0p;
          If %Found(Wdelp);
             //***  Begin Add  ***  11/28/17  *****************************
             intDiv = GetStRptDv(ELco#:ELfnd:ELemp#);
             Chain (ELco#:ELfnd:ELfyr:ELemp#:intDiv) pel01;
             //***  End   Add  ***  11/28/17  *****************************
          //*** begin add ***  08/13/21  ***********************************
              polRRN = dbrrn;
          //*** end add *****  08/13/21  ***********************************
             intEmp# = ELemp#;
             intCo# = ELco#;
             intFnd = ELfnd;
             intFyr = ELfyr;
          Endif;
       Endif;

      // Get the client id
       GetClientID(strCltId);
       If strCltId = '093';
          isKRMS = *on;
       Endif;

      //Check if office is installed.
       strSYK = 'SY' ;
       Chain strSyk ssy00;
       If not %found(ssylf) or SYofIf <> 'Y';
          isFlag31 = *off;
          scDarts = 'N';
       Else;
          scDarts = 'Y';
          isFlag31= *on;
       EndIf;

       Return;
      /End-Free
     p ProgramStart    e

      //===================================================================
      // ValidPnl - Validate Panel Screen Fields
      //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free
        Select;
           When isPopUp;
              Exsr CheckRcdPolPagT;
           Other;
              Exsr CheckRcdPolPage;
        EndSl;

        Begsr CheckRcdPolPage;
       //*** Beg Add **** 05/08/17 ************************//
          // Validation Check for Group
          If scFnd = *Zeros;
             erFnd   = *On;
             msFnd   = 'WCR0503';
             isValid = *On;
          EndIf;
       //*** End Add **** 05/08/17 ************************//

          //Date Validation
          If scFromDate = *Zeros;
             erFromDate = *On;
             msFromDate = 'WCR1391';
             isValid    = *on;
          Endif;

          If scToDate = *Zeros;
             erToDate = *On;
             msToDate = 'WCR1391';
             isValid  = *on;
          Endif;

          If scFromDate <> *Zeros;
             erFromDate = ValidDate(scFromDate:strMsg);
             If erFromDate = *On;
                msFromDate = StrMsg;
                isValid    = *On;
             Endif;
          Endif;

          If scToDate <> *Zeros;
             erToDate = ValidDate(scToDate:strMsg);
             If erToDate = *On;
                msToDate = StrMsg;
                isValid  = *On;
             Endif;
          Endif;

         //"From Date" cannot be greater than "To date"
          If erToDate = *off and erFromDate = *off;
             erToDate = AscendDates(scFromDate:scToDate:strMsg);
             If erToDate = *On;
                msToDate = strMsg;
                erFromDate = *On;
                msFromDate = strMsg;
                isValid  = *On;
             Endif;
          Endif;


      // If CLT093 And "Resubmit policy for interstate change only" = Y
        If strcltid = '093';
           If scStat <> 'Y' and scStat <> ' ';
              erstat = *on;
              msStat = 'WCR1241';
              isValid  = *On;
           EndIf;

           If scStat = 'Y';
              Chain (intCo#:intFnd:intEmp#:intFyr:strType) psh01;
              If %found(wmshl1);
                 SHSTAT = 'Y';
                 Update psh01;
              EndIf;
           EndIf;
        EndIf;
        Endsr;

        Begsr CheckRcdPolPagT;
      //Validate the Resolve to Darts Flag.
       If isPopUp = '1';          //pop up
          If scDarts <> 'Y' and scDarts <> 'N';
             erDarts = *On;
             msDarts = 'WCR0501';
             isValid  = *On;
          EndIf;
       EndIf;

       // If CLT093 And "Resubmit policy for interstate change only" = Y
        If strcltid = '093';
           If scStat <> 'Y' and scStat <> ' ';
              erstat = *on;
              msStat = 'WCR1241';
              isValid  = *On;
           EndIf;

           If scStat = 'Y';
              Chain (intCo#:intFnd:intEmp#:intFyr:strType) psh01;
              If %found(wmshl1);
                 SHstat = 'Y';
                 Update psh01;
              EndIf;
           EndIf;
        EndIf;
        Endsr;
      /end-free
     p ValidPnl        e
