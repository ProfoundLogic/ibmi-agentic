     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE WTEDIANRP - EDI Annual Report - Prompt
     H*********************************************************************
    H*  DESCRIPTION:  This program prompts for selection criteria to run *
    H*                the EDI Annual Report.                             *
     H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU        StoneRiver new menu items >> EDI Annual Report.    *
     H*                                                                   *
    H*  CALLS TO:                                                        *
     H*                                                                   *
     H*                                                                   *
    H*  REWRITE: 02/20/17      C:306     PROJECT: WC2454                 *
     H*********************************************************************
     H*********************************************************************
    H* REVISIONS:                                                        *
     H*                                                                   *
     H*********************************************************************
    H* INDICATORS:
     H*********************************************************************
     * Files Declaration                                                 *
     fwtedianrp cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     f
     *  System table file
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     f
     *  EDI Log File
     fwmlgl     if   e           k disk    rename(plg01:plg00)
     f
     * Prototypes

     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d intCo#          s              3  0 inz(0)
     d strMsg          s            100a
     d inco#           s              3a
     d injst           s              2a
     d inyear          s              4a
     d induedt         s              8a
     d inMntcde        s              2a
     d

     * Screen Error Check.
     d ErrorCheck      pr              n

     * Main Processing
     d Main            pr

     * Load the maintanence type code drop down.
     d LoadMainType    pr

     * Program Start
     d ProgramStart    pr

     * Process submitter
     d ProcessSbm      pr                  extpgm('W8052C')
     d inco#                          3a
     d injst                          2a
     d inyear                         4a
     d induedt                        8a
     d inMntcde                       2a

     d/copy scopypr01d

      /Free
      // Start Of Program
       ProgramStart();
      // Main Procesing
       Main();

       *Inlr = *on;
      /End-Free

      //===================================================================
      // Main - Main processing
      //===================================================================
     p Main            b
     d Main            pi
      /free
      //*  Process until cancel button is pressed.

       Dou BtnCancel = *On;

        scTabHght  = 710;
        aPnlHeight = 770;
        Write RcdComPnl;
        Write RcdTbEDIAn;   // Tab record format
        Exfmt RcdEDIAnRp;   // main Record format.
        Read RcdComPnl;
        Read RcdTbEDIAn;

       Select;

      // Submit button trapping.
       When BtnSubmit = *on;
          BtnSubmit = *off;
            If not ErrorCheck();
               inco#    = %editc(intCo#:'X');
               injst    = scEDIState;
               inyear   = %editc(scEDIYear:'X');
               induedt  = %editc(scDueDate:'X');
               inMntcde = scMntTypCd;

               ProcessSbm(inco#:injst:inyear:induedt:inMntcde);
               Leave;
            Endif;

      // Refresh button trapping.
       When BtnRefresh = *On;
          BtnRefresh =*Off;
         // Reload the program start values again.
          ProgramStart();

      // State change event trapping.
       When BtnSttChg = *on;
          BtnSttChg = *off;
          LoadMainType();
       Other;

       EndSl;
       EndDo;

      /End-Free

     p Main            e

      //===================================================================
      // LoadMainType - Load Maintenance code type Dropdown values.
      //===================================================================
     p LoadMainType    b
     d
     d strKey          s              4a   inz(' ')
     d isStateFnd      s               n   inz(*off)
      /Free
       Clear selMntTypC;  // Selection field
       Clear scMntTypCd;  // Maintenance type code screen field.
       Clear strKey;
       isStateFnd = *off;

       strKey = scEDIState + 'E2';
       setll (intCo#:strKey) pgt00;
       reade (intCo#:strKey) pgt00;
       Dow not %Eof(wtgtl);
          If GTstat = 'A';
             selMntTypC = 'GTSTAT = ''A'' and GTTBID =  + ''' + strKey + ''' +
              and GTCO# = ' + %char(intCo#);
             scMntTypCd = 'AN';

             isStateFnd = *On;
             Leave;
         EndIf;

          reade (intCo#:strKey) pgt00;
       EndDo;


      // If State specific table is not found then load with SMTC table.

       If isStateFnd = *off;
          strKey = 'SMTC'; // Read SMTC values.
          selMntTypC = 'GTSTAT = ''A'' and GTTBID =  + ''' + strKey + ''' +
           and GTCO# = ' + %char(intCo#);
          scMntTypCd = 'AN';
       EndIf;

      /End-Free
     p LoadMainType    e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free
        clear RcdEDIAnRp;

       // Load Action panel with the Company Name.
        Clear scCompName;
        intCo# = userCo#(q1user);
        scCompName = GetCompNam(intCo#);

       // Load SMTC values by default for the first time when screen is opened.
        LoadMainType();

      /End-Free
     p ProgramStart    e

      //===================================================================
      // Error Check Procedure.
      //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n
     d isError         s               n   inz(*off)

     d                 ds
     d lgduedate                      8  0
     D  lgdtmm                 1      2  0
     D  lgdtdd                 3      4  0
     D  lgdtyr                 5      8  0

     D                 ds
     D scDuedatDs                     8  0
     D  scrdtmm                1      2  0
     D  scrdtdd                3      4  0
     D  scrdtyr                5      8  0

      /Free
       isError    = *off;
       msEDIState = *blanks;
       erEDIState = *off;
       msEDIYear  = *blanks;
       erEDIYear  = *off;
       msDueDate  = *blanks;
       erDueDate  = *off;
       msMntTypCd = *blanks;
       erMntTypCd = *off;

      // State can not be blank.
       If scEDIState = *blanks;
          msEDIState = 'WCR0503';
          erEDIState = *on;
          isError    = *on;
       EndIf;

      // Year cannot be Zero.
       If scEDIYear = *Zeros;
          msEDIYear = 'WCR0502';
          erEDIYear = *on;
          isError   = *on;
       EndIf;

      // Due Date cannot be Zero.
       If scDueDate = *Zeros;
          msDueDate = 'WCR0500';
          erDueDate = *on;
          isError   = *on;
       EndIf;

      // Invalid Due Date.
       If scDueDate <> *Zeros;
          erDueDate = ValidDate(scDueDate:strMsg);
          If erDueDate = *On;
             msDueDate = StrMsg;
             isError   = *on;
          Endif;
       EndIf;

      // No Duplicates Allowed.
       If isError = *off;
          scDuedatDs = scDueDate;
          Setll *loval plg00;
          Read plg00;
          Dow not %Eof(wmlgl);
             lgduedate = dtMDYY(LGdue);

             If scrdtyr = LGdtyr and scMntTypCd = LGmtc and
              scEDIState = LGjst;
                msEDIState = 'WCR0959';
                erEDIState = *on;
                msEDIYear  = 'WCR0959';
                erEDIYear  = *on;
                msDueDate  = 'WCR0959';
                erDueDate  = *on;
                msMntTypCd = 'WCR0959';
                erMntTypCd = *on;
                isError    = *on;
                Leave;
             EndIf;
             Read plg00;
          EndDo;
       EndIf;

       If isError = *on;
          return *on;
       Else;
          return *off;
       EndIf;
      /End-Free
     p ErrorCheck      e
