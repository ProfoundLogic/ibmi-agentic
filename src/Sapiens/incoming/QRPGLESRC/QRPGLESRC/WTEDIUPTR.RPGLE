     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE WTEDIUPTR - EDI Update Transaction - Log Creation
     H*********************************************************************
    H*  DESCRIPTION:  This program prompts for selection criteria to run
    H*                the EDI Update Transaction Log Creation
     H*
    H*  CALLED FROM:
    H*    MENU        StoneRiver new menu items >> EDI Update Transactions
     H*
    H*  CALLS TO:
     H*
     H*
    H* Created By:  C:053  Proj:WC2515   Date:  10/23/18
     H*********************************************************************
     H*********************************************************************
    H* REVISIONS:
     H* 10/07/19    C:180      Support 82824
     H*                        Fix MTC dropdown values.
     H*                        DSPF file changes as well.
     H*
     H*********************************************************************
    H* INDICATORS:
     H*********************************************************************
     * Files Declaration
     fwtediuptr cf   e             workstn
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
     d inclmsts        s              1a
     d inclmtyp        s              1a
     d instrdt         s              8a
     d inenddt         s              8a
     d inbased         s              8a
     d inTransID       s              3a
     d inMntcde        s              2a
     d inPaymnts       s              1a
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
     d ProcessSbm      pr                  extpgm('W8055C')
     d inco#                          3a
     d injst                          2a
     d inclmsts                       1a
     d inclmtyp                       1a
     d instrdt                        8a
     d inenddt                        8a
     d inbased                        8a
     d inTransID                      3a
     d inMntcde                       2a
     d inPaymnts                      1a

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
        Write RcdTbEDI;   // Tab record format
        Exfmt RcdEDIRPT;   // main Record format.
        Read RcdComPnl;
        Read RcdTbEDI;

       Select;

      // Submit button trapping.
       When BtnSubmit = *on;
          BtnSubmit = *off;
            If not ErrorCheck();
               inco#    = %editc(intCo#:'X');
               injst    = scEDIState;
               inclmsts = scClmSts;
               inclmtyp = scClmTyp;
               instrdt  = %editc(scStrDate:'X');
               inenddt  = %editc(scEndDate:'X');
               inbased  = scBased;
               inTransID= scTransID;
               inMntcde = scMntTypCd;
               inPaymnts= scPaymnts;

               ProcessSbm(inco#:injst:inclmsts:inclmtyp:instrdt:
                          inenddt:inbased:intransid:inmntcde:inpaymnts);
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
       // BEGIN DELETE 10/07/19 *********************************
       //strKey = scEDIState + 'E2';
       //setll (intCo#:strKey) pgt00;
       //reade (intCo#:strKey) pgt00;
       //Dow not %Eof(wtgtl);
          //If GTstat = 'A';
           //selMntTypC = 'GTSTAT = ''A'' and GTTBID =  + ''' + strKey + ''' +
           // and GTCO# = ' + %char(intCo#);

           //isStateFnd = *On;
           //Leave;
       //EndIf;

       // reade (intCo#:strKey) pgt00;
       //EndDo;
       // END   DELETE 10/07/19 *********************************


      // If State specific table is not found then load with SMTC table.

       If isStateFnd = *off;
          strKey = 'SMTC'; // Read SMTC values.
          selMntTypC = 'GTSTAT = ''A'' and GTTBID =  + ''' + strKey + ''' +
           and GTCO# = ' + %char(intCo#);
       EndIf;

      /End-Free
     p LoadMainType    e

      //===================================================================
      // ProgramStart - Initial Program
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /Free
        clear RcdEDIRPT;

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
       msStrDate  = *blanks;
       erStrDate  = *off;
       msClmSts   = *blanks;
       erClmSts   = *off;
       msClmTyp   = *blanks;
       erClmTyp   = *off;
       msBased    = *blanks;
       erBased    = *off;
       msTransId  = *blanks;
       erTransId  = *off;
       msMntTypCd = *blanks;
       erMntTypCd = *off;

      // State can not be blank.
       If scEDIState = *blanks and isError = *Off;
          msEDIState = 'WCR0503';
          erEDIState = *on;
          isError    = *on;
       EndIf;

      // Claim status can not be blank.
       If scClmSts = *blanks and isError = *Off;
          msClmSts  = 'WCR0503';
          erClmSts  = *on;
          isError   = *on;
       EndIf;

      // Claim type can not be blank.
       If scClmTyp = *blanks and isError = *Off;
          msClmTyp  = 'WCR0503';
          erClmTyp  = *on;
          isError   = *on;
       EndIf;

      // Start Date cannot be Zero.
       If scStrDate = *Zeros and isError = *Off;
          msStrDate = 'WCR0500';
          erStrDate = *on;
          isError   = *on;
       EndIf;

      // Invalid Start Date.
       If scStrDate <> *Zeros and isError = *Off;
          erStrDate = ValidDate(scStrDate:strMsg);
          If erStrDate = *On;
             msStrDate = StrMsg;
             isError   = *on;
          Endif;
       EndIf;

      // Invalid End Date.
       If scEndDate <> *Zeros and isError = *Off;
          erEndDate = ValidDate(scEndDate:StrMsg);
          If erEndDate = *On;
             msEndDate = StrMsg;
             isError   = *on;
          Endif;
       EndIf;

      // Create Based on ID can not be blank.
       If scBased   = *blanks and isError = *Off;
          msBased   = 'WCR0503';
          erBased   = *on;
          isError   = *on;
       EndIf;

      // Transaction ID can not be blank.
       If scTransId = *blanks and isError = *Off;
          msTransId = 'WCR0503';
          erTransId = *on;
          isError   = *on;
       EndIf;

      // Maintenance type code can not be blank.
       If scMntTypCd = *blanks and isError = *Off;
          msMntTypCd = 'WCR0503';
          erMntTypCd = *on;
          isError   = *on;
       EndIf;

      // No Duplicates Allowed.
       //If isError = *off;
       // scDuedatDs = scDueDate;
       // Setll *loval plg00;
       // Read plg00;
       // Dow not %Eof(wmlgl);
       //    lgduedate = dtMDYY(LGdue);
       //
       //    If scrdtyr = LGdtyr and scMntTypCd = LGmtc and
       //     scEDIState = LGjst;
       //       msEDIState = 'WCR0959';
       //       erEDIState = *on;
       //       msEDIYear  = 'WCR0959';
       //       erEDIYear  = *on;
       //       msDueDate  = 'WCR0959';
       //       erDueDate  = *on;
       //       msMntTypCd = 'WCR0959';
       //       erMntTypCd = *on;
       //       isError    = *on;
       //       Leave;
       //    EndIf;
       //    Read plg00;
       // EndDo;
       //EndIf;

       If isError = *on;
          return *on;
       Else;
          return *off;
       EndIf;
      /End-Free
     p ErrorCheck      e
