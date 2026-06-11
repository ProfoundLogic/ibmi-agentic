     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2471 - Prompt for Employer List
     h/define CONVERT
     h dftactgrp(*no) actgrp('QILE')
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTEMPLT- Prompt for Employer List
     h*****************************************************************
     h* Revisions                                                     *
     h*                                                               *
     h*****************************************************************

     fWTRPTEMPLTcf   e             workstn extfile('WTRPTEMPLT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     f*
     F*                                    INFDS(FIDS01)
     FWmfnl     IF   E           K DISK    Rename(pfn01:pfn00)
     FSmcol     IF   E           K DISK    Rename(pco01:pco00)
     FWmupl     IF   E           K DISK    Rename(pup01:pup00)
     FWdf1l     IF   E           K DISK    Rename(pf101:pf100)
     fwdell1    IF   E           K disk    Rename(pel01:pel00)

     dpsds           esds                  extname(szq1p)

      *
     d WtEmplrLst      pr                  Extpgm('WTRPTEMPLT')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtEmplrLst      pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d charMsg         s            100
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
     d inCo#           s              3
     d inFnd           s              3
     d inFyr           s              3
     d inType          s              1
     d inOutq          s             10
     d inMode          s              1
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

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

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

      * Employer List Proc.
     d EmplrLstProc    pr

      * Employer List
     d EmplrLstSbm     pr                  ExtPgm('W7140C')
     d inCo#                          3
     d inFnd                          3
     d inFyr                          3
     d inType                         1
     d inOutq                        10
     d inMode                         1

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // EmplrLstProc - Employer List Process Submit
       //===================================================================
     p EmplrLstProc    b
     d EmplrLstProc    pi

      /Free

       inCo# = %EditC(intCo#:'X');

       If scFnd <> 0;
          inFnd = %EditC(scFnd:'X');
       EndIf;

       inFyr  = %editc(scPOLYEAR:'X');

       If scALPHANUM = 'A';
          intype = 'A';
       Endif;

       If scALPHANUM = 'N';
          intype = 'N';
       Endif;

       inOutq = strOutq;
       inMode = 'D';

       EmplrLstSbm(inCo#:inFnd:inFyr:inType:inOutq:inMode);

      /End-Free

     p EmplrLstProc    e

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       Clear RcdEmplLst;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       scFnd  = userGrp(q1user);
       intFnd = scFnd;

       //scPOLYEAR  = 0;
       scALPHANUM = 'A';

      /End-Free

     p LoadInVal       e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnCancel = *On ;

          setCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          Select;
          When BtnSubmit = *on;
             // Validate Edits
             If ValidPnl() = *On;
                EmplrLstProc();
                Leave;
             EndIf;

          When BtnCancel = *on;
             BtnCancel = *off;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh = *Off;
             LoadInVal();
          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

          When TabIndex = 0;
             TabHeight = 770;
             APnlHeight = TabHeight + 60;

             Write rcdComPnl;
             Write RcdTabEmpL;

             Exfmt RcdEmplLst;

             Read rcdComPnl;
             Read RcdTabEmpL;

       EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInVal();

       // Get the client id
       GetClientID(outCltId);

       // Retrieve Output Queue of the User.
       Chain q1user pup00;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e

       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

      /End-Free

     p SetCriteria     e

       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n

      /Free

       // Validation Check for Fund
       If scFnd   = *Zeros;
          erFnd   = *On;
          msFnd   = 'WC30622';
          Return *off;
       EndIf;

       // Validation Check for Policy Year
       If scPOLYEAR  = *Zeros;
          erPolYear  = *On;
          msPolyear  = 'WCR0503';
          Return *Off;
       EndIf;

       // Validation Check for valid Policy Year
       //If scPOLYEAR <> *Zeros;
       //   Chain (intco#:scfnd:scPOLYEAR) pf100;
       //   If not %found(Wdf1l);
       //      erPolYr  = *On;
       //      msPolYr  = 'WCR0517';
       //      isValid  = *On;
       //   EndIf;
       //EndIf;

       Return *On;

      /End-Free
     p ValidPnl        e
