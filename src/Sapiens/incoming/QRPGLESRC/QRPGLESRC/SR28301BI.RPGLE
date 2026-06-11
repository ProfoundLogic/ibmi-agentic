     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/TITLE SR28301BI - Additional Entry Program for WC280301.B
     h**********************************************************************
     h*
     h*  Description:  Collects additional information for form WC280301.B
     h*
     h*  Called from: SR28301B
     h*  Calls to:    None
     h*
     h* Created   : 02/03/15
     h* Programmer: C:295
     h* Project   : WC2315
     h**********************************************************************
     h* Revisions:
      //                                                               *
      //   03/23/16 C:308 Project: Elevate                             *
      //                  Genericize the toolbar programming         *
      //                                                               *
      //   01/07/16 C:65  Project: WC2401                              *
      //                  Recompile                                    *
     h*
      //   09/17/15 C:302 Project: WC2254                              *
      //                  Recompile                                    *
     h**********************************************************************
     FSr28301bf cf   e             workstn handler('PROFOUNDUI(HANDLER)')

     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      *****Begin Add*****  03/23/16  *************************************
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
      ***** End Add *****  03/23/16  *************************************

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
     d xxForm          c                   Const('WC280301B ')
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
     d SR28301BI       pr                  ExtPgm('SR28301BI')
     d inprds                        40
     d incall                         1
     d inco#                          3
     d infnd                          3
     d inemp#                         9
     d indiv                          5
     d infyr                          3
     d*
     d SR28301BI       pi
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
          Exsr ExfmtScreens;

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

      //===================================================================
      // PreDsp - Pre-processing before displaying (every time)
      //===================================================================
         Begsr PreDsp;
            // Get endorsement additional information
            clear PFM01;
            Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxFrm1) Wxfmp1;
            If %Found(Wxfmp1);
               scNum1 = F3a701;
               scRat1 = F3a521;
               scPrm1 = F3a921;
               scNum2 = F3a702;
               scRat2 = F3a522;
               scPrm2 = F3a922;
            // Move the custom fields to the screen
            Else;
               scNum1 = *zeros;
               scRat1 = *zeros;
               scPrm1 = *zeros;
               scNum2 = *zeros;
               scRat2 = *zeros;
               scPrm2 = *zeros;
            Endif;
         Endsr;
      //====================================================================
      // PrsCtl - Process control after no errors found (Call Submitter)
      //====================================================================
         Begsr PrsCtl;
            clear PFM01;
            Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv:xxFrm1) Wxfmp1;

               // Move the additional information fields to the file

               F3a701 = scNum1;
               F3a521 = scRat1;
               F3a921 = scPrm1;
               F3a702 = scNum2;
               F3a522 = scRat2;
               F3a922 = scPrm2;

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
