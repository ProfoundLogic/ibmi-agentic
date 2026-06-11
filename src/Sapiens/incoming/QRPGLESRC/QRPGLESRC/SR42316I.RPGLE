     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE SR42316I - Additional Entry Program for WC420316
     h**********************************************************************
     h*                                                                    *
     h*  Description:  Collects additional information for form WC420316   *
     h*                                                                    *
     h*  Called from: SR42316                                              *
     h*  Calls to:    None                                                 *
     h*                                                                    *
     H* CREATED   : 09/21/17                                               *
     H* PROGRAMMER: C:267                                                  *
     H* PROJECT   : WC2527                                                 *
     H*             copied from SR42312I                                   *
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h* 10/17/17 c:62 Ticket 50083                                        *
     h*               Get PEO info from employer 1 div 0                   *
     h*                                                                    *
     h**********************************************************************
     FSR42316F  cf   e             workstn handler('PROFOUNDUI(HANDLER)')

     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)

      ** Additional Information File
     fwxfmp1    uf a e           k Disk
      ** Group Master File
     fwmfnl     if   e           k Disk
      ** Employer Master File
     fwmeml     if   e           k Disk
      ** Employer Detail File
     fWdell     if   e           k disk    infds(dbfds)

     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  dbrrn                397    400b 0

     d @@TabIndex      s              5
     d xxCo#           s                   like(EMco#)
     d xxFnd           s                   like(EMfnd)
     d xxEmp#          s                   like(EMemp#)
     d xxDiv           s                   like(EMdiv)
     d xxFyr           s              3  0
     d xxFrm1          s             15
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d clientID        s              3    inz('000')
     d inmode          s              1
     d intCo#          s              3  0
     d xxRrn           s              9  0
     d ##CltId         s              3
     d isMenuAvbl      s               n   inz(*on)
     d isValid         s              1n   inz(*off)
     d xxForm          c                   Const('WC420316  ')
     d*** Begin Add *** 10/17/17 **************************************
     d peoEmp#         s                   Like(ELemp#) Inz(1)
     d peoDiv          s                   Like(ELdiv) Inz(0)
     d showScreen      s               n   Inz(*Off)
     d*** End   Add *** 10/17/17 **************************************
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
    d* *entry
     d SR42316I        pr                  ExtPgm('SR42316I')
     d inprds                        40
     d incall                         1
     d inco#                          3
     d infnd                          3
     d inemp#                         9
     d indiv                          5
     d infyr                          3
     d*
     d SR42316I        pi
     d inprds                        40
     d incall                         1
     d inco#                          3
     d infnd                          3
     d inemp#                         9
     d indiv                          5
     d infyr                          3
     d
      *
      /Free

       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free
       xxCo#  =  %Int(inCo#);
       xxFnd  =  %Int(inFnd);
       xxEmp# =  %Int(inEmp#);
       xxDiv  =  %Int(inDiv);
       xxFyr  =  %Int(inFyr);
       xxFrm1= xxForm;

       //GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       // Read the Employer Detail File
       Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr)pel01;

       xxRRN = dbRRN;
       // Load Policy Action Bar
       LoadActionBar(isMenuAvbl);

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Load Endorse Screen
          Exsr predsp;

          // Process Endorse Screen
          //*** Begin Chg *** 10/17/17 ********************************
          //***Exsr ExfmtScreens;
          If showScreen = *On;
             Exsr ExfmtScreens;
          EndIf;
          //*** End   Chg *** 10/17/17 ********************************

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;

          // Add Detail Record
          When scSave = *on;
             Validate ();
             If isValid = *off;
                Exsr prsctl;
             Endif;
             Leave;
          //*** Begin Add *** 10/17/17 ********************************
          When showScreen = *Off;
             Exsr prsctl;
             Leave;
          //*** End   Add *** 10/17/17 ********************************
          When scCancel = *on;
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
          Write rcdPanel;
          Write rcdDetail;
          Exfmt rcdDetail;
          Read rcdPolPnl;
          Read rcdPanel;
       EndSl;

       Endsr;

       //*** Begin Add *** 10/17/17 ************************
      //===================================================================
      // GetPeoInfo - Get the PEO information
      //===================================================================
         Begsr GetPeoInfo;
            // Get the PEO information
            Chain (xxCo#:xxFnd:peoEmp#:peoDiv) pem01;
            If %Found(wmeml);
               scWPEONam  = EMname;
               If EMmAd1 <> ' ';
                  scWPEOAdd1 = EMmAd1;
                  scWPEOAdd2 = EMmAd2;
                  scWPEOCity = EMmCty;
                  scWPEOSt   = EMmSt;
                  scWPEOZip  = EMmZip;
               Else;
                  scWPEOAdd1 = EMpAd1;
                  scWPEOAdd2 = EMpAd2;
                  scWPEOCity = EMpCty;
                  scWPEOSt   = EMpSt;
                  scWPEOZip  = EMpZip;
               EndIf;
            Endif;
         Endsr;
       //*** End   Add *** 10/17/17 ************************
      //===================================================================
      // PreDsp - Pre-processing before displaying (every time)
      //===================================================================
         Begsr PreDsp;
            // Get endorsement additional information
            //*** Begin Add *** 10/17/17 ************************
            If showScreen = *Off;
               ExSr GetPeoInfo;
            Else;
            //*** End   Add *** 10/17/17 ************************
               clear PFM01;
               Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxFrm1) Wxfmp1;
               If %Found(Wxfmp1);
                  scWPEONam = F3EMNM;
                  scWPEOAdd1 = F3EAD1;
                  scWPEOAdd2 = F3EAD2;
                  scWPEOCity = F3C301;
                  scWPEOSt  = F3EMST;
                  scWPEOZip  = F3FEIN;
                  //*** Begin Add *** 10/17/17 ************************
                  If scWPEONam = ' ' Or scWPEOAdd1 = ' ';
                     ExSr GetPeoInfo;
                  EndIf;
                  //*** End   Add *** 10/17/17 ************************
               // Move the custom fields to the screen
               Else;
                  scWPEONam = *Blanks;
                  scWPEOAdd1 = *Blanks;
                  scWPEOAdd2 = *Blanks;
                  scWPEOCity = *Blanks;
                  scWPEOSt = *Blanks;
                  scWPEOZip  = *zeros;
                  //*** Begin Add *** 10/17/17 ************************
                  ExSr GetPeoInfo;
                  //*** End   Add *** 10/17/17 ************************
               Endif;
            //*** Begin Add *** 10/17/17 ************************
            Endif;
            //*** End   Add *** 10/17/17 ************************
         Endsr;
      //====================================================================
      // PrsCtl - Process control after no errors found (Call Submitter)
      //====================================================================
         Begsr PrsCtl;
            clear PFM01;
            Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxFrm1) Wxfmp1;

               // Move the additional information fields to the file
               F3EMNM = scWPEONam;
               F3EAD1 = scWPEOAdd1;
               F3EAD2 = scWPEOAdd2;
               F3C301 = scWPEOCity;
               F3EMST = scWPEOSt;
               F3FEIN = scWPEOZip;

            If %found (Wxfmp1);
               Update pfm01;
            Else;
               F3co#  = xxCo#;
               F3fnd  = xxFnd;
               F3fyr  = xxFyr;
               F3emp# = xxEmp#;
               F3div  = xxDiv;
               F3form = xxForm;
               Write pfm01;
            Endif;
         Endsr;
      //====================================================================

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

       Select;
       When inmode = 'D';
          dspVisb = *Off;
       When inmode = 'A';
          dspVisb = *On;
       When inmode = 'C';
          dspVisb = *On;
       Endsl;

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);

       // Get the client id
       GetClientID(##CltId);

       tabClick = *Off;
       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // Validate - Edit/Add State Code
       //===================================================================
     p Validate        b
     d Validate        pi

      /Free
      /End-Free

     p Validate        e
       //===================================================================

      * Utility policy procedure definitions
      /COPY SCOPYPOLPI
