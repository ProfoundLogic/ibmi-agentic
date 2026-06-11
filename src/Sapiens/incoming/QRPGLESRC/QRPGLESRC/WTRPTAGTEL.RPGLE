     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTRPTAGTEL - Prompt for Agent Employer Listing
     h/define CONVERT
     h dftactgrp(*no) actgrp(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTAGTEL - Agent Employer Listing
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h*                                                               *
     h*****************************************************************

     fWtrptagtelcf   e             workstn extfile('WTRPTAGTEL')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     fWmeml     if   e           k disk
     fWdelp     if   e             disk
     f                                     Rename(pel01:pel0p)
     fWmaap     if   e             disk

     dpsds           esds                  extname(szq1p)

      *
     d WtAgtEmpL       pr                  Extpgm('WTRPTAGTEL')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d WtAgtEmpL       pi
     d  inPrds                       40A   const
     d  inCall                        1A

     d strEmpRng       s              2
     d strAgtRng       s              2
     d outMode         s              1

     d charMsg         s            100
     d outRrn#         s              9
     d intCo#          s                   Like(ELco#)
     d strOutq         s             10
     d inSelect        s           1024
      *
     d outCo#          s              3
     d outFnd          s              3
     d outPedtF        s              8
     d outPedtT        s              8
     d outEmpStDt      s              8
     d outEmpSl        s              1
     d outEmpType      s              1
     d outAcSelect     s              1
     d outPriSort      s              2
     d outSecSort      s              2
     d outEmpFl        s              1
     d outAgtFl        s              1
     d outPrtype       s              2
     d outEmp#         s              9
     d outDiv          s              5
     d outFyr          s              3
     d outAgtRngTyp    s              2
     d outAgtRngF      s             15
     d outAgtRngT      s             15
     d outEmRngTyp     s              2
     d outEmpRngF      s             15
     d outEmpRngT      s             15
     d outEmpNmf       s              2
     d outEmpNmT       s              2
     d outEmpNbrF      s              9
     d outEmpNbrT      s              9
     d outAgtNmF       s              2
     d outAgtNmT       s              2
     d outAgtNbrF      s              5
     d outAgtNbrT      s              5
     d outOutType      s              1
     d outCanAgt       s              1
     d outOutq         s             10
     d outAgtStDt      s              8
     d OutCltId        s              3
     d strEmRngTyp     s              2
     d strEmpRngF      s             15
     d strEmpRngT      s             15
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy sprDate

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall                         1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Process Screen to submit Agent Employer list report
     d ProcessScreen   pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Validate Panel Sreen Values
     d ValidPnl        pr              n

      * Deposit Report Proc.
     d AgtEmpLProc     pr

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

      * Deposit Recap Report Submit
     d AgtEmpLSbm      pr                  ExtPgm('W7345C')
     d outCo#                         3
     d outFnd                         3
     d outPEDtF                       8
     d outPEDtT                       8
     d outEmpStDt                     8
     d outEmpSl                       1
     d outEmpType                     1
     d outAcSelect                    1
     d outPriSort                     2
     d outSecSort                     2
     d outEmpFl                       1
     d outAgtFl                       1
     d outPrtype                      2
     d outEmp#                        9
     d outDiv                         5
     d outFyr                         3
     d outAgtRngTyp                   2
     d outAgtRngF                    15
     d outAgtRngT                    15
     d outEmRngTyp                    2
     d outEmpRngF                    15
     d outEmpRngT                    15
     d outEmpNmF                      2
     d outEmpNmT                      2
     d outEmpNbrF                     9
     d outEmpNbrT                     9
     d outAgtNmF                      2
     d outAgtNmT                      2
     d outAgtNbrF                     5
     d outAgtNbrT                     5
     d outOutType                     1
     d outCanAgt                      1
     d outOutq                       10
     d outAgtStDt                     8

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // ProcessScreen - Process Screen to submit the report
       //===================================================================
     p ProcessScreen   b
     d ProcessScreen   pi

      /Free
       // Load company
       outCo# = %EditC(intCo#:'X');

       // Load Group
       If scFnd <> 0;
          outFnd   = %EditC(scFnd:'X');
       EndIf;

       // Load Policy Effective from date
       If scPEffFrDt <> 0;
          outPEDtF = %EditC(scPEffFrDt:'X');
       Else;
          outPEDtF = *Blanks;
       EndIf;

       // Load Policy Effective To date
       If scPEffToDt <> 0;
          outPEDtT = %EditC(scPEffToDt:'X');
       Else;
          outPEDtT = *Blanks;
       EndIf;

       // Load Run Date
       outEmpStDt = %editC(scEmpStDt:'X');

       Evalr outEmpNbrF =  %editC(scEmpNbrF:'X');
       Evalr outEmpNbrt =  %editC(scEmpNbrT:'X');
       Evalr outAgtNbrF =  %editC(scAgtNbrF:'X');
       Evalr outAgtNbrT =  %editC(scAgtNbrT:'X');

       // Load Emplyer selection/Employer Type/Account Select
       outEmpSl       = scEmpSel;
       outEmpType     = 'B';
       outAcSelect    = 'E';

       // Load Primary Sort/Secondary Sort
       outPriSort     = scPriSort;
       outSecSort     = scSecSort;

       // Load Employee/Agent & Print Type
       outEmpFl       = 'M';
       outAgtFl       = 'M';
       outPrtype      = 'EI';

       // Load Employee/Division/Year
       outEmp#       = *Blanks;
       outDiv        = *Blanks;
       outFyr        = *Blanks;

       // Load Agent range name or number
       outAgtNmF  = scEmpNmf;
       outAgtNmT  = scEmpNmT;

       Select;
          When scAgtNmT  <> *Blanks;
             outAgtRngTyp = 'AA';
             outAgtRngF = scAgtNmF;
             outAgtRngT = scAgtNmT;

          When scAgtNbrT <> *zeros;
             outAgtRngTyp = 'AN';
             Evalr outAgtRngF = %editC(scAgtNbrF:'X');
             Evalr outAgtRngT = %editC(scAgtNbrT:'X');

       EndSl;

       // Load Employee range name or number
             outEmpNmf  = scEmpNmf;
             outEmpNmT  = scEmpNmT;

       Select;
          When scEmpNmT  <> *Blanks;
             strEmRngTyp = 'EA';
             strEmpRngF = scEmpNmf;
             strEmpRngT = scEmpNmT;

          When scEmpNbrT <> *zeros;
             strEmRngTyp = 'EN';
             Evalr strEmpRngF = %editC(scEmpNbrF:'X') + '00000';
             Evalr strEmpRngT = %editC(scEmpNbrT:'X') + '99999';

       EndSl;

       Select;
          When strEmRngTyp <> *Blanks and outAgtRngTyp = *Blanks;
             outAgtRngTyp  = strEmRngTyp;
             outAgtRngF    = strEmpRngF;
             outAgtRngT    = strEmpRngT;

          When strEmRngTyp <> *Blanks and outAgtRngTyp <> *Blanks;
             outEmRngTyp = strEmRngTyp;
             outEmpRngF  = strEmpRngF;
             outEmpRngT  = strEmpRngT;
          EndSl;

       outOutType    = scOutType;
       outCanAgt     = scCanAgt;

       If scAgtStDt = *zeros;
          scAgtStDt = *Hival;
       EndIf;
       If outcltId = '133';
          outAgtStDt = %editC(scAgtStDt:'X');
       EndIf;
       outMode     = 'D';

       AgtEmpLSbm (outCo#:outFnd:outPEDtF:outPEDtT:outEmpStDt:outEmpSl:
        outEmpType:outAcSelect:outPriSort:outSecSort:outEmpFl:outAgtFl:
        outPrtype:outEmp#:outDiv:outFyr:outAgtRngTyp:outAgtRngF:outAgtRngT:
        outEmRngTyp:outEmpRngF:outEmpRngT:outEmpNmF:outEmpNmT:outEmpNbrf:
        outEmpNbrt:outAgtNmF:outAgtNmT:outAgtNbrF:outAgtNbrT:outOutType:
        outCanAgt:outOutq:outAgtStDt);

      /End-Free

     p ProcessScreen   e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       clear RcdAgtEmpL;
       scCanAgt = 'Y';

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       scFnd  = userGrp(q1user);
       scEmpStDt = DateTo8(%Date());

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

          SetCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          Select;

          When BtnSubmit = *on;
             // Clear Errors
             ClrErr();
             // Validate Edits

             If ValidPnl();
                ProcessScreen();
                Leave;
             EndIf;
          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             ProgramStart();
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = 830;

          Write rcdComPnl;
          Write RcdTabAEL;

          Exfmt RcdAgtEmpL;

          Read rcdComPnl;
          Read RcdTabAEL;

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

       // Retrieve Output Queue of the User.
       outOutQ = GetUsrOutq(q1user);

       // Get the client id
       GetClientID(outCltId);
       If outCltId = '113';
          agtSt = *on;
       Else;
          agtSt = *off;
       Endif;

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
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       msFnd      = *blanks;
       erFnd      = *off;
       msPEffToDt = *blanks;
       erPEffToDt = *off;
       msPEffFrDt = *blanks;
       erPEffFrDt = *off;
       msPriSort  = *blanks;
       erPriSort  = *off;
       msSecSort  = *blanks;
       erSecSort  = *off;

       msEmpNmF   = *blanks;
       erEmpNmF   = *off;
       msEmpNmT   = *blanks;
       erEmpNmT   = *off;
       msEmpNbrF  = *blanks;
       erEmpNbrF  = *off;
       msEmpNbrT  = *blanks;
       erEmpNbrT  = *off;

       msAgtNmF   = *blanks;
       erAgtNmF   = *off;
       msAgtNmT   = *blanks;
       erAgtNmT   = *off;
       msAgtNbrF  = *blanks;
       erAgtNbrF  = *off;
       msAgtNbrT  = *blanks;
       erAgtNbrT  = *off;

      /End-Free

     p ClrErr          e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi              n

      /Free

       // Validation Check for Group
       If scfnd = *Zeros;
          erFnd   = *On;
          msFnd   = 'WCR0503';
          return  *Off;
       EndIf;

       // Validation Check for Effective To Date.
       erPEffToDt = ValidDate(scPeffToDt:charMsg);
       If erPEffToDt = *on;
          msPEffToDt = charMsg;
          return  *Off;
       Endif;

       // Validation Check for Effective From Date.
       erPEffFrDt = ValidDate(scPeffFrDt:charMsg);
       If erPEffFrDt = *on;
          msPEffFrDt = charMsg;
          return  *Off;
       Endif;

       If erPEffToDt = *off and erPEffFrDt = *off;
          erPEffToDt = AscendDates(scPeffFrDt:scPeffToDt:charMsg);
          If erPEffToDt = *on;
             erPEffFrDt = *on;
             msPEffToDt = charMsg;
             msPEffFrDt = charMsg;
          return  *Off;
          Endif;
       Endif;

       // Validation Check for Primary Sort.
       If scPriSort = *Blanks;
          erPriSort  = *On;
          MSPriSort  = 'WCR0503';
          return  *Off;
       EndIf;

       // Validation Check Employer Name & Number Range
       // (1) Both Name Range and Number range are Mutually Exclusive
       If (scEmpNmF <> *Blanks or scEmpNmT <> *Blanks)
        and (scEmpNbrF <> *Zeros or scEmpNbrT <> *Zeros);
          erEmpNmF  = *On;
          msEmpNmF  = 'WCR0918';
          erEmpNmT  = *On;
          msEmpNmT  = 'WCR0918';
          erEmpNbrF  = *On;
          msEmpNbrF  = 'WCR0918';
          erEmpNbrT  = *On;
          msEmpNbrT  = 'WCR0918';
          return  *Off;
       EndIf;

       // (2) Both FROM Name and To Name not enterted
       If scEmpNmF = *Blanks and scEmpNmT <> *Blanks;
          erEmpNmF  = *On;
          msEmpNmF  = 'WCR0503';
          return  *Off;
       EndIf;
       If scEmpNmF <> *Blanks and scEmpNmT = *Blanks;
          erEmpNmT  = *On;
          msEmpNmT  = 'WCR0503';
          return  *Off;
       EndIf;

       // (3) FROM Name/Number is greater than TO Name/Number
       If scEmpNmF > scEmpNmT;
          erEmpNmF  = *On;
          msEmpNmF  = 'WCR0750';
          return  *Off;
       EndIf;
       If scEmpNbrF > scEmpNbrT;
          erEmpNbrF  = *On;
          msEmpNbrF  = 'WCR0750';
          return  *Off;
       EndIf;

       // Validation Check Agent Name & Number Range
       // (1) Both Name Range and Number range are Mutually Exclusive
       If (scAgtNmF <> *Blanks or scAgtNmT <> *Blanks)
        and (scAgtNbrF <> *Zeros or scAgtNbrT <> *Zeros);
          erAgtNmF  = *On;
          msAgtNmF  = 'WCR0918';
          erAgtNmT  = *On;
          msAgtNmT  = 'WCR0918';
          erAgtNbrF  = *On;
          msAgtNbrF  = 'WCR0918';
          erAgtNbrT  = *On;
          msAgtNbrT  = 'WCR0918';
          return  *Off;
       EndIf;

       // (2) Both FROM Name and To Name not enterted
       If scAgtNmF = *Blanks and scAgtNmT <> *Blanks;
          erAgtNmF  = *On;
          msAgtNmF  = 'WCR0503';
          return  *Off;
       EndIf;
       If scAgtNmF <> *Blanks and scAgtNmT = *Blanks;
          erAgtNmT  = *On;
          msAgtNmT  = 'WCR0503';
          return  *Off;
       EndIf;

       // (3) FROM Name/Number is greater than TO Name/Number
       If scAgtNmF > scAgtNmT;
          erAgtNmF  = *On;
          msAgtNmF  = 'WCR0750';
          return  *Off;
       EndIf;
       If scAgtNbrF > scAgtNbrT;
          erAgtNbrF  = *On;
          msAgtNbrF  = 'WCR0750';
          return  *Off;
       EndIf;

       Return  *On;

      /End-Free
     p ValidPnl        e
