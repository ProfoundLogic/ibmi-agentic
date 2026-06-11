     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title SR29312I- Designated Governmental Entity Endorsement
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Designated Governmental Entity Endorsement
     h*
     h*  Called from: SR29312
     h*  Calls to:    None
     h*
     h* Created   : 08/07/18
     h* Programmer: C:291
     h* Project   : 150WDB
     h**********************************************************************
     H*                                                               *
     H*  THIS PROGRAM COLLECTS CUSTOM DATA FOR ENDORSEMENTS AND       *
     H*  STORES IT IN FILE WXFMP1 & WXFMP2, USING GENERIC DATA FIELDS.*
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     fSR29312F  cf   e             workstn extfile('SR29312F  ')
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
     fWxfmp2    uf a e           k disk    Rename(pfm02:pfm0p2)

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
     d xxForm          c                   Const('WC290312  ')
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

      * Validate Add/Edit screen
     d Validate        pr

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

       // Load Screen
       ExSr LoadScreen;

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Process Endorse Screen
          Exsr ExfmtScreens;
          isValid = *off;

          Select;
          // do nothing - catch tab click event
          When tabClick = *On;
             tabClick = *Off;
          // Write/Update WXFMP1
          When scSave = *On;
             Validate ();
             If isValid = *off;
                WrtUpdWXFMP1();
                Leave;
             Endif;
          When scCancel = *On;
             Leave;
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

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
             scDaysAfNt = F3a701;
          Else;
             scDaysAfNt = 0;
          EndIf;

          Chain(n) (company#:group#:year:employer#:division:form) pfm0p2;
          If %Found(wxfmp2);
             scDGvEntNm = F4C701;
             scDGvEntA1 = F4C702;
             scDGvEntA2 = F4C703;
             scDGvCity  = F4C704;
             scDGvState = F4ST01;
             scDGvZip   = F4FEIN;
          Else;
             scDGvEntNm = *Blank;
             scDGvEntA1 = *Blank;
             scDGvEntA2 = *Blank;
             scDGvCity  = *Blank;
             scDGvState = *Blank;
             scDGvZip   = *Zeros;
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

       //# of days after written notice
       F3a701 =  scDaysAfNt;

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

       Chain (company#:group#:year:employer#:division:form) pfm0p2;

       //Name/Address of Designated Governmental Entity
       F4C701 =  scDGvEntNm;
       F4C702 =  scDGvEntA1;
       F4C703 =  scDGvEntA2;
       F4C704 =  scDGvCity;
       F4ST01 =  scDGvState;
       F4FEIN =  scDGvZip;

       F4pgm  = q1pgm;
       F4user = q1user;
       F4dt   = DateTo7(SystemDate);
       F4time = TimeTo6(SystemTime);

        If Not %Found(wxfmp2);
           F4co#  =  company#;
           F4fnd  =  group#;
           F4fyr  =  year;
           F4emp# =  employer#;
           F4div  =  division;
           F4form =  form;

           F4ipgm = q1pgm;
           F4iusr = q1user;
           F4idt  = DateTo7(SystemDate);
           Write pfm0p2;
        Else;
           Update pfm0p2;
        Endif;
      /End-Free

     p WrtUpdWXFMP1    e

       //===================================================================
       // Validate - Edit/Add State Code
       //===================================================================
     p Validate        b
     d Validate        pi
      /Free
          isError = *Off;
          mgDaysAfNt = ' ';
          erDaysAfNt = *off;
          mgDGvEntNm = ' ';
          erDGvEntNm = *off;
          mgDGvEntA1 = ' ';
          erDGvEntA1 = *off;
          mgDGvEntA2 = ' ';
          erDGvEntA2 = *off;
          mgDGvCity = ' ';
          erDGvCity = *off;
          mgDGvState = ' ';
          erDGvState = *off;
          mgDGvZip = ' ';
          erDGvZip = *off;

          If scDaysAfNt = *Zero;
             mgDaysAfNt = 'Number of Days after Written Notice is required';
             erDaysAfNt = *on;
             isError = *On;
          Endif;

          If scDGvEntNm = *Blanks;
             mgDGvEntNm = 'Name of Designated Governmental Entity is required';
             erDGvEntNm = *on;
             isError = *On;
          Endif;

          If scDGvEntA1 = *Blanks;
             mgDGvEntA1 = 'Address of Designated Governmental Entity +
                            is required';
             erDGvEntA1 = *on;
             isError = *On;
          Endif;

          If scDGvCity = *Blanks;
             mgDGvCity = 'City is required';
             erDGvCity = *on;
             isError = *On;
          Endif;

          If scDGvState = *Blanks;
             mgDGvState = 'State is required';
             erDGvState = *on;
             isError = *On;
          Endif;

          If scDGvZip = *Zeros;
             mgDGvZip = 'Zip Code is required';
             erDGvZip = *on;
             isError = *On;
          Endif;

          If isError = *on;
             isValid = *on;
          Endif;

      /End-Free

     p Validate        e

       //===================================================================
      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
