     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2463 - Prompt for Provider Payment Report
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTPRPMT - Prompt for Provider Payment Report
     h*****************************************************************
     h* Revisions                                                     *
     h*                                                               *
     h* 08/29/17 C:267 Project: PHW47591                              *
     h*               -Changed 'Installment Date Range' to 'Date Range'
     h*               -Changed error msg for Group to use 'WC30685'   *
     h*                'Group is invalid'.                            *
     h*               -Changed error msg for To/From Date to use      *
     h*                'WCR1394 - To date must be greater than        *
     h*                from date'.                                    *
     h*                                                               *
     h*****************************************************************

     fWTRPTPRPMTcf   e             workstn extfile('WTRPTPRPMT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
    f* User Parameter Master File
     f*
     fWtabl     if   e           k disk    Rename(pat01:pat00)
     fWmfnl     IF   E           K DISK    Rename(pfn01:pfn00)
     fsmcol     IF   E           k DISK    Rename(pco01:pco00)
     fWmupl     IF   E           K DISK    Rename(pup01:pup00)
     fStctl1    IF   E           K DISK
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     fWmeml     if   e           k disk    Rename(pem01:pem00)
     FWmexl     IF   E           K DISK    Rename(pex01:pex00)
     FWmahl     IF   E           K DISK    Rename(pah01:pah00)
     FWmarl     IF   E           K DISK    Rename(par01:par00)
     FWdf1l     IF   E           K DISK    Rename(pf101:pf100)
     FWmpdl     IF   E           K DISK    Rename(ppd01:ppd00)

     dpsds           esds                  extname(szq1p)


      *
     d WtProvPmt       pr                  Extpgm('WTRPTPRPMT')
     d
     d WtProvPmt       pi

     d charMsg         s            100
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELFyr)
     d strOutq         s                   Like(Upoutq)
     d inSelect        s           1024
     d strCmntDsc      s             30    inz(*Blanks)
      *
     d outCltId        s              3
     d outCo#          s              3
     d outFnd          s              3
     d outFyr          s              3
     d outFein         s              9
     d inFein          s              9
     d outName         s             40
     d inName          s             40
     d outMod          s              3
     d inMod           s              3
     d outMode         s              1
     d outFrmDt        s              8
     d outToDt         s              8
     d outOutq         s             10
     d inCall1         s              1
     d inRTcd          s              1
     *****************************************************************
     * Prototype Declaration
     *****************************************************************

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

     * Clear Error/Message fields
     d ClrErr          pr

     * FEIN/Mod Search Proc.
     d FEINmodSrch     pr

     * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

     * Get FEIN/Mod
     d GetFEINmod      pr                  ExtPgm('W0030R')
     d inCall1                        1
     d inFEIN                         9
     d inMod                          3
     d inRtcd                         1
     d inName                        40

     * Intialize the Screen fields
     d LoadInVal       pr

     * Main Processing
     d Main            pr

     * Program Start
     d ProgramStart    pr

     * Provider Payment Proc.
     d ProvPmtProc     pr

     * New/Renewal Business Report Submit
     d ProvPmtSbm      pr                  ExtPgm('W7544C')
     d outCo#                         3
     d outFnd                         3
     d outFyr                         3
     d outFein                        9
     d outMod                         3
     d outFrmDt                       8
     d outToDt                        8
     d outOutq                       10
     d outMode                        1

     * Set Criteria
     d SetCriteria     pr

     * Validate Panel Sreen Values
     d ValidPnl        pr

      /Free
      // Start Of Program
       ProgramStart();

      // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

      //===================================================================
      // ClrErr - Clear Error/Message fields
      //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       isValid    = *off;
       msFnd      = *blanks;
       erFnd      = *off;
       msFrmDt    = *blanks;
       erFrmDt    = *off;
       msToDt     = *blanks;
       erToDt     = *off;
       msPolYr    = *blanks;
       erPolyr    = *off;
       msFEIN     = *blanks;
       erFEIN     = *off;
       msMode     = *blanks;
       erMode     = *off;

      /End-Free

     p ClrErr          e

      //===================================================================
      // FEINmodSrch - FEIN/Mode Search
      //===================================================================
     p FEINmodSrch     b
     d FEINmodSrch     pi

      /Free
       inFEIN  = %editc(scFEIN:'X');
       inMod   = %editc(scMod:'X');
       inCall1 = '2';
       inName = *Blanks;

       GetFEINmod(inCall1:inFEIN:inMod:inRtcd:inName);

       If inRTcd  = 'A';
          Return;
       Endif;

       If inRTcd  = '*';
          scFEIN = %dec(inFEIN:9:0);
          scMod  = %dec(inMod:3:0);
          scName = inName;
       Endif;

      /End-Free

     p FEINmodSrch     e

      //===================================================================
      // LoadInVal - Intialize the Screen fields
      //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdProvPmt;

      //Get Default Year
       Chain q1user Wmupl;
       If %found(wmupl);
          scFyr = UPfyr;
       Endif;

      /End-Free

     p LoadInVal       e

      //===================================================================
      // Main - Main processing
      //===================================================================
     p Main            b
     d Main            pi

      /Free

      // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          setCriteria();

         // Display Screen
          Exsr ExfmtScreens;

          Select;
             When selFEINMod = *On;
                FEINmodSrch();

             When BtnSubmit = *on;
               // Clear Errors
                ClrErr();
               // Validate Edits
                ValidPnl();
                If isValid = *Off;
                   ProvPmtproc();
                   Leave;
                EndIf;

             When BtnCancel = *on;
                BtnCancel = *off;
                Leave;

            // do nothing - catch tab click event
             When tabClick = *on;
                tabClick = *off;

             When BtnRefresh = *On;
                BtnRefresh =*Off;
                SetCriteria();
                LoadInVal();
          EndSl;

       Enddo;

      //===================================================================
      // Write Screen Formats;
      //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
           TabHeight = 770;
           APnlHeight = TabHeight + 50;

           Write rcdComPnl;
           Write RcdTabPrPy;

           Exfmt RcdProvPmt;

           Read rcdComPnl;
           Read RcdTabPrPy;

       EndSl;

       Endsr;

      /End-Free

     p Main            e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

      // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       scFnd  = userGrp(q1user);
       intFnd = scFnd;

      // Get the client id
       GetClientID(outCltId);

      // Intialize the Screen fields
       LoadInVal();

      // Retrieve Output Queue of the User.
       Chain q1user pup00;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // ProvPmtProc - Provider Payment Report Process Submit
      //===================================================================
     p ProvPmtProc     b
     d ProvPmtProc     pi

      /Free

       outCo# = %EditC(intCo#:'X');

       If scFnd <> 0;
          outFnd = %EditC(scFnd:'X');
       EndIf;

       outFyr  = %editc(scFyr:'X');
       outFEIN = %editc(scFEIN:'X');
       outMod  = %editc(scMod:'X');
       outFrmDt= %editc(scFrDt:'X');
       outToDt = %editc(scTodt:'X');
       outOutq = strOutq;
       outMode = 'D';

       ProvPmtSbm(outCo#:outFnd:outFyr:outFEIN:outMod:outFrmDT:
                  outToDt:outOutq:outMode);

      /End-Free

     p ProvPmtProc     e

      //===================================================================
      // SetCriteria
      //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

      /End-Free

     p SetCriteria     e

      //===================================================================
      // ValidPnl - Validate Panel Screen Fields
      //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

      // Validation Check for Fund
       If scFnd   = *Zeros;
          erFnd   = *On;
          //*** Beg Chg *** 08/29/17 ******************************
          //msFnd   = 'WC30622';
          msFnd   = 'WC30685';
          //*** End Chg *** 08/29/17 ******************************
          isValid = *On;
       EndIf;

      // Validate the from Installment date is valid
       If DtCheck(scFrDt) = *Zeros;
          erfrmDt  = *on;
          msFrmDt  = 'WCR0500';
          isValid  = *on;
       Endif;

      // Validate the To Installment date is valid
       If DtCheck(scToDt) = *Zeros;
          erToDt  = *on;
          msToDt  = 'WCR0500';
          isValid = *on;
       Endif;

      //Installment "from date" cannot be greater than "to date"
       If erToDt  = *off and erfrmDt = *off;
         //*** Beg Chg *** 08/29/17 ******************************
         //erToDt  = AscendDates(scFrDt:scToDt:charMsg);
          //If erToDt   = *on;
          If scFrDt > scToDt;
             erToDt   = *on;
             erfrmDt  = *on;
             //msToDt   = charMsg;
             //msFrmDT  = charMsg;
             msToDt   = 'WCR1394';
             msFrmDT  = 'WCR1394';
         //*** End Chg *** 08/29/17 ******************************
             isValid  = *on;
          Endif;
       Endif;

      // Validation Check for Policy Year
       If scFyr <> *Zeros;
          Chain (intco#:scfnd:scFyr) pf100;
          If not %found(Wdf1l);
             erPolYr  = *On;
             msPolYr  = 'WCR0517';
             isValid  = *On;
          EndIf;
       EndIf;

      // Validation Check for FEIN number
       If scFEIN <> *Zeros;
          Chain (scFEIN:scMod) ppd00;
          If not %found(Wmpdl);
             erFEIN = *On;
             msFEIN = 'WCR0546';
             isValid  = *On;
          EndIf;
       Endif;

      /End-Free
     p ValidPnl        e
