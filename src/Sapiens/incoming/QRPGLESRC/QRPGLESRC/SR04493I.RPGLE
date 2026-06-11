     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title SR04493I- Minimum Payroll for Officers or Partners Endorsement
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Minimum Payroll for Officers or Partners Endorsement
     h*
     h*  Called from: SR04493
     h*  Calls to:    None
     h*
     h* Created   : 04/28/18
     h* Programmer: C:33
     h* Project   : WC2603
     h**********************************************************************
     H*                                                               *
     H*  THIS PROGRAM COLLECTS CUSTOM DATA FOR ENDORSEMENTS AND       *
     H*  STORES IT IN FILE WLEDP, USING GENERIC DATA FIELDS.          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     fSR04493F  cf   e             workstn extfile('SR04493F  ')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

     fWdell     if   e           k disk    Rename(pel01:pel00)
     fWmeml     if   e           k disk    Rename(pem01:pem00)
     fWmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fWxfmp1    uf a e           k disk    Rename(pfm01:pfm0p1)

     d psds          esds                  extname(szq1p)

     d* Entry Parameters
     d @@prds          s             40
     d @@Mode          s              1
     d @@Co#           s              3
     d @@Fnd           s              3
     d @@Emp#          s              9
     d @@Div           s              5
     d @@Fyr           s              3

     d company#        s                   like(F3co#)
     d group#          s                   like(F3fnd)
     d employer#       s                   like(F3emp#)
     d division        s                   like(F3div)
     d year            s                   like(F3fyr)
     d form            s                   like(F3form)

     d clientID        s              3    inz('000')
     d inmode          s              1
     d inTabIndex      s              5  0
     d intCo#          s              3  0
     d intRrn          s              9  0
     d xxRrn           s              9  0
     d ##Rrn#          s              9  0
     d ##CltId         s              3
     d isMenuAvbl      s               n   inz(*on)
     d isValid         s              1n   inz(*off)
     d isError         s              1n   inz(*off)
     d xxForm          c                   Const('WC040493  ')
      *****************************************************************
      * Prototype Declaration
      *****************************************************************
      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Write/Update Endorsement Record
     d WrtUpdWXFMP1    pr

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
    
      * Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@Prds
     c                   Parm                    @@Mode
     c                   Parm                    @@Co#
     c                   Parm                    @@Fnd
     c                   Parm                    @@Emp#
     c                   Parm                    @@Div
     c                   Parm                    @@Fyr

      /Free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free
       company#  =  %Int(@@Co#);
       group#    =  %Int(@@Fnd);
       employer# =  %Int(@@Emp#);
       division  =  %Int(@@Div);
       year      =  %Int(@@Fyr);
       form      =  xxForm;
       scFnd     =  %Int(@@Fnd);
       scEmp#    =  %Int(@@Emp#);
       scDiv     =  %Int(@@Div);
       scFyr     =  %Int(@@Fyr);

       // Read the Employer Detail File
       Chain (company#:group#:employer#:division:year) pel00;

       // Load Policy Action Bar
       LoadActionBar(isMenuAvbl);

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Process Endorse Screen
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *On;
             tabClick = *Off;
          // Write/Update WXFMP1
          When scSave = *On;
             WrtUpdWXFMP1();
             Leave;
          // Leave
          When scCancel = *On;
             Leave;
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       ExSr LoadScreen;

       Select;
       When TabIndex = 0;
          TabHeight = 1050;
          APnlHeight = TabHeight + 50;
          Write rcdPolPnl;
          Write rcdTabEndr;
          Exfmt rcdEndr;
          Read rcdPolPnl;
          Read rcdTabEndr;
       EndSl;

       Endsr;
       //===================================================================
       // Load Screen Fields
       //===================================================================
       BegSr LoadScreen;

          Chain(n) (company#:group#:year:employer#:division:form) pfm0p1;
          If %Found(wxfmp1);
             scMinPrem = F3am01;
             scMaxPrem = F3am02;
          Else;
             scMinPrem = 0;
             scMaxPrem = 0;
          EndIf;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Program Mode.
       If q1prms >= 2;
          inmode = 'A';
       Endif;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       ##Rrn# = 0;
       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // WrtUpdWXFMP1 - Write/Update WXFMP1
       //===================================================================
     p WrtUpdWXFMP1    b
     d WrtUpdWXFMP1    pi

      /Free
       Chain (company#:group#:year:employer#:division:form) pfm0p1;

       //Location/Operation name/Title
       F3am01 =  scMinPrem;
       F3am02 =  scMaxPrem;

       F3pgm  = q1pgm;
       F3user = q1user;
       F3dt   = DateTo7(SystemDate);
       F3time = TimeTo6(SystemTime);

        If Not %Found(wxfmp1);
           F3co#  =  company#;
           F3fnd  =  group#;
           F3fyr  =  year;
           F3emp# =  employer#;
           F3div  =  division;
           F3form =  form;

           F3ipgm = q1pgm;
           F3iusr = q1user;
           F3idt  = DateTo7(SystemDate);
           Write pfm0p1;
        Else;
           Update pfm0p1;
        Endif;

      /End-Free

     p WrtUpdWXFMP1    e
       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
