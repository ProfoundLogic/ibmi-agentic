     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTRPTAGACT - Agent Activity Report program
     h/define CONVERT
     hdftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTAGACT - Agent Activity Report
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h* 03/01/18 c:62 Ticket: 64490                                   *
     h*               Don't disable the agent select                  *
     h*                                                               *
     h*****************************************************************
     f
     fWtrptagactcf   e             workstn extfile('WTRPTAGACT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
    f*** User Parameters File
     fwmupl     if   e           k disk    rename(pup01:pup00)
     f*
    f*** Agent Assignment File
     fwmaap     if   e             disk    rename(paa01:paa0p)
      *
    f*** Agent Master File
     Fwmagp     if   e             disk    rename(pag01:pag0p)
    *** Group select Intermediate file.
     fwxgsl     if   e           k Disk    reName(pgs01:pgs00)

     *****************************************************************
     *** Stand Alone variables declartions.
     *****************************************************************
     d charMsg         s            100
    d
     d intCo#          s              3  0
     d intMod          s              3  0
     d isExit          s               n   inz(*off)
     d
     d inco#           s              3
     d inFrFnd         s              3
     d inToFnd         s              3
     d inFrDt          s              8
     d inToDt          s              8
     d inDateType      s                   like(scDtRanTyp)
     d inPQB           s              1
     d inAgtSort       s                   like(scASrt)
     d inFein          s              9
     d inMod           s              3
     d inPFB           s                   like(scTypOut)
     d inDSB           s                   like(scTypRpt)
     d inOutQ          s             10
     d inMbr           s             10
     d inPSRT          s                   like(scPriSort)
     d inFQFL          s                   like(scEmpQTyp)
     d inGrpSlct       s              1a
     d intGrpCnt       s              5  0 inz(0)
      *
     d insuffix        s              3    inz

     d/copy sCopyPR01d
     *****************************************************************
     * Prototype Declaration
     *****************************************************************

     d/copy sPrCompany
     d/copy sPrString
     d/copy sPrDate
     d/copy wPrUsrPrms
     d/copy wPrGroup
     d/copy wprpolerr
     d/copy wPrAgent

     *** Entry Prototype.
     d wtrptagactp     pr                  extpgm('wtrptagact')
     d  inprds1                      40
     d  incall1                       1

     d wtrptagactp     pi
     d  inprds1                      40
     d  incall1                       1

     *** Intialize the Screen fields
     d LoadInVal       pr

     *** Main Processing
     d Main            pr

     *** Program Start
     d ProgramStart    pr

     *** Validate Panel Sreen Values
     d ValidPnl        pr              n

     *** Search Fein
     d SearchFein      pr                  like(scAgtFein)
     d  infeinSr                           like(scAgtFein)
     d  inmodSr                            like(AAMOD)
     d
     d CreateMbrs      pr
     d GetQteTyps      pr              n
     d OverrideMbrs    pr
     d RemoveMbrs      pr

     d UpdateMbr       pr                  extpgm('UPDMBR')
     D  inMode                        1    const
     D  inFile                       10    const
     D  inMbr                        10

     *** Agent activity Report submitter.
     d SubmitPgm       pr                  extpgm('W7870C')
     d  inco#                         3    const
     d  inFrFnd                       3    const
     d  inToFnd                       3    const
     d  inFrDt                        8    const
     d  inToDt                        8    const
     d  inDateType                         const like(scDtRanTyp)
     d  inPQB                         1    const
     d  inAgtSort                          const like(scASrt)
     d  inFein                        9    const
     d  inMod                         3    const
     d  inPFB                              const like(scTypOut)
     d  inDSB                              const like(scTypRpt)
     d  inOutQ                       10    const
     d  inMbr                        10    const
     d  inPSRT                             const like(scPriSort)
     d  inFQFL                             const like(scEmpQTyp)
     d  inGrpSlct                     1    const

     *** Get suffix name.
     d GetSuffix       pr                  extpgm('SA030R')
     d insuffix                       3

      //===================================================================
      // Main logic starts here
      //===================================================================
      /Free
      // Start Of Program
       ProgramStart();

      // Main Procesing
       Main();

       *InLr = *on;

      /End-Free


      //===================================================================
      // LoadInVal - Intialize the Screen fields
      //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       scASrt = 'N';
       isDisbAgt = *off;

      /End-Free
     p LoadInVal       e

      //===================================================================
      // Main - Main processing
      //===================================================================
     p Main            b
     d Main            pi

      /Free

      // Process until exit button is pressed.
       Dow BtnCancel = *off and isExit = *off;


         // Display Screen
          Exsr ExfmtScreens;

          Select;
         // Submit button click event
          When BtnSubmit = *on;
            // Validate screen edits
             If ValidPnl();
                If not GetQteTyps();
                   ExSr Submitter;
                   isExit = *on;
                Else;
                    If inmbr <> *Blanks;
                       RemoveMbrs();
                       isExit = *on;
                    EndIf;
                EndIf;
             EndIf;

         // Cancel button click event
          When BtnCancel = *on;
             isExit = *On;

         // Agent Search button click event
          When BtnAgtSrch = *on;
             BtnAgtSrch = *off;
             Monitor;
                intmod = %int(scAgtMod);
             On-error *all;
               intmod = 0;
             EndMon;
             scAgtFein = SearchFein(scAgtFein:intmod);

         // Refresh button click event
          When BtnRefresh = *On;
             BtnRefresh =*off;
            // Clearing the record format.
             clear RcdAgActRp;
            // Intialize the Screen fields
             LoadInVal();

          EndSl;
       Enddo;

      //===================================================================
      // Write Screen Formats;
      //===================================================================
       BegSr ExfmtScreens;
          TabHeight = 770;   // Adjusting the Tab Height
          APnlHeight = TabHeight + 60; // Adjusting the Action Panel Height

          Write rcdComPnl;
          Write RcdTabAAR;
          Exfmt RcdAgActRp;
          Read rcdComPnl;
          Read RcdTabAAR;

       EndSr;

      //===================================================================
      // SubmitPgm - Agent Activity Report Submitter
      //===================================================================
       Begsr Submitter;
        inco#      = %editc(intCo#:'Z');
        inFrFnd    = *blanks;
        inToFnd    = *blanks;

        inFrDt     = %editc(scFromDt:'Z');
        inToDt     = %editc(scToDt:'Z');
        inDateType = scDtRanTyp;
        inAgtSort  = scASrt;
        inFein     = %editc(scAgtFein:'Z');
        inMod      = scAgtMod;
        inPFB      = scTypOut;
        inDSB      = scTypRpt;
 
        inPSRT     = scPriSort;
        inFQFL     = scEmpQTyp;

        inGrpSlct  = 'Y';

        SubmitPgm(inco#:inFrFnd:inToFnd:inFrDt:inToDt:inDateType:inPQB
         :inAgtSort:inFein:inMod:inPFB:inDSB:inOutQ:inMbr:inPSRT:inFQFL
         :inGrpSlct);
       Endsr;

      /End-Free

     p Main            e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

      // Intialize the Screen indicators and fields.
       BtnCancel = *off;

      // Intialize the Screen fields
       LoadInVal();

      // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

      // Retrieve the suffix
       GetSuffix(insuffix);
       scRptLine = 'This report produces output file WFQ2P in library '+
                  'PDSRPT' + %trim(insuffix) + ', if selected.';


      // Retrieve Output Queue of the User.
       Chain(n) q1user pup00;
       If %Found(Wmupl);
          inOutQ = Upoutq;
       EndIf;

      // Checking multiple group selection.
           scAgtToolT = *blanks;

           Setll (intCo#:*loval) pgs00;
           reade (intCo#) pgs00;
           Dow not %eof(wxgsl);
           If GSIeFl = 'I';
              intGrpCnt = intGrpCnt + 1;
           Endif;

           //*** Begin Del *** 03/01/18 ****************************
           //***  If intGrpCnt > 1;
           //***     isDisbAgt = *on;
           //***     scAgtToolT = 'Agent Entry is Valid Only' +
           //***       ' When Running For a Single Group';
           //***     Leave;
           //***  EndIf;
           //*** End   Del *** 03/01/18 ****************************
           reade (intCo#) pgs00;
           EndDo;

      /End-Free
     p ProgramStart    e

      //===================================================================
      // ValidPnl - Validate Panel Screen Fields
      //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n
      /Free

       msFromDt   = *blanks;
       erFromDt   = *off;
       msToDt     = *blanks;
       erToDt     = *off;
       msDtRanTyp = *blanks;
       erDtRanTyp = *off;
       msPriSort  = *blanks;
       erPriSort  = *off;
       msAgtMod   = *blanks;
       erAgtMod   = *off;
       msAgtFein  = *blanks;
       erAgtFein  = *off;
       msEmpQTyp  = *blanks;
       erEmpQTyp  = *off;
       msTypOut   = *blanks;
       erTypOut   = *off;
       msTypRpt   = *blanks;
       erTypRpt   = *off;

      // Validation Check for Effective From Date.
       erFromDt = ValidDate(scFromDt:charMsg);
       If erFromDt = *on;
          msFromDt = 'WCR0500';
          Return *off;
       Endif;

      // Validation Check for Effective To Date.
       erToDt = ValidDate(scToDt:charMsg);
       If erToDt = *on;
          msToDt = 'WCR0500';
          Return *off;
       Endif;

       If erToDt = *off and erFromDt = *off;
          erToDt = AscendDates(scFromDt:scToDt:charMsg);
          If erToDt = *on;
             erFromDt = *on;
             msToDt   = 'WCR0529';
             msFromDt = 'WCR0529';
             Return *off;
          Endif;
       Endif;

      // Type of Date Range
       If scDtRanTyp <> 'T' and scDtRanTyp <> 'E'
        and scDtRanTyp <> 'O' and scDtRanTyp <> 'N';
          msDtRanTyp = 'WCR0503';
          erDtRanTyp = *on;
          Return *off;
       Endif;


      // Primary Sort.
       If scPriSort <> 'AN' and scPriSort <> 'AA';
          If scAsrt ='N' and scPriSort = *blanks;
             scPriSort = 'AN';
          Else;
             msPriSort = 'WCR0503';
             erPriSort = *on;
             Return *off;
          EndIf;
       EndIf;

      // Agent FEIN and MOD.
       Monitor;
         intMod = %int(scAgtMod);
       On-Error *all;
         intMod = 0;
       EndMon;

       If scAgtMod <> *Blanks and not ValidFein(scAgtFein:intMod);
          msAgtMod = 'WCR0505';
          erAgtMod = *on;
          Return *off;
       EndIf;

       If scAgtFein <> *zeros and not ValidFein(scAgtFein);
          msAgtFein = 'WCR0505';
          erAgtFein = *on;
          Return *off;
       EndIf;

      // Employer Quote Type
       If scEmpQTyp <>'F' and scEmpQTyp <>'W' and scEmpQTyp <>'B';
          msEmpQTyp = 'WCR0503';
          erEmpQTyp = *on;
          Return *off;
       EndIf;

      // Type of Output
       If scTypOut <> 'P' and scTypOut <> 'F' and scTypOut <> 'B';
          msTypOut = 'WCR0503';
          erTypOut = *on;
          Return *off;
       EndIf;

      // Type of Report
       If scTypRpt <> 'D' and scTypRpt <> 'S' and scTypRpt <> 'B';
          msTypRpt = 'WCR0503';
          erTypRpt = *on;
          Return *off;
       EndIf;

       Return *on; //Return *on indicator all validation are passed.

      /End-Free
     p ValidPnl        e

    C*====================================================================
    C* SearchFein - Process the agent FEIN search
    C*====================================================================
     PSearchFein       b

     d SearchFein      pi                  like(scAgtFein)
     d  infeinSr                           like(scAgtFein)
     d  inmodSr                            like(AAmod)
     d
     d intAArrn        s              9  0
     d intAGrrn        s              9  0
     d error           s              1    inz(' ')
     d inKc            s               n   inz(*off)
     d inKl            s               n   inz(*off)

      /Free
       select;

       Other;
          intAGrrn = AgtSrchAG(inkc:inkl:error);
          If intAGrrn <> *zero;
             Chain intAGrrn pag0p;
             scAgtMod = %editc(AGmod:'1');
             scAgentNam = AGname;
             return AGFein;
          EndIf;
       Endsl;

       return infeinSr;
      /End-Free
     PSearchFein       e

    C*====================================================================
    C* CreateMbrs - Create the members for WXQ2*
    C*====================================================================
     PCreateMbrs       b
     d CreateMbrs      pi
     d
     d inkc            s               n
     d inkl            s               n
      /free
       inMbr = %trim(q1user);
       UpdateMbr('C':'WXQ2P':inMbr);
      /End-Free
     PCreateMbrs       e

    C*====================================================================
    C* GetQteTyps - Get the quote types from W/W
    C*====================================================================
     PGetQteTyps       b
     D GetQteTyps      pi              n

     D QteTypePgm      pr                  extpgm('W7870C1')
     D  inMode                        1    const
     D  inPQB                         1
     D  inMbr                        10
     D  inkc                           n
     D  inkl                           n

     d  inkl           s               n
     d  inkc           s               n
      /Free
       If inMbr = *blanks;
          CreateMbrs();
       EndIf;

       QteTypePgm(' ':inPQB:inMbr:inkc:inkl);

       return inkc or inkl;
      /End-Free
     PGetQteTyps       e


    C*====================================================================
    C* OverrideMbrs - Override the new members for WXQ2*
    C*====================================================================
     POverrideMbrs     b
     d OverrideMbrs    pi
      /Free
       UpdateMbr('O':'WXQ2L':inMbr);
       UpdateMbr('O':'WXQ2L1':inMbr);
       UpdateMbr('O':'WXQ2L2':inMbr);
       UpdateMbr('O':'WXQ2P':inMbr);
       return;
      /End-Free
     POverrideMbrs     e

    C*====================================================================
    C* RemoveMbrs - Remove the new members for WXQ2*
    C*====================================================================
     PRemoveMbrs       b
     d RemoveMbrs      pi
      /Free
       UpdateMbr('R':'WXQ2P':inMbr);
       return;
      /End-Free
     PRemoveMbrs       e
