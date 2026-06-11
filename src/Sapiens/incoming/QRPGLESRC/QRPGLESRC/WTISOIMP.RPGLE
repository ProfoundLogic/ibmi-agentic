     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTISOIMP - Prompt for ISO Score Import
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     H*****************************************************************
     H/TITLE WTISOIMP - Prompt for ISO Score Import
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h* 04/26/17 C: 079 Project: 114WXY                               *
     h*                 Remove docroot folder path                    *
     h*                                                               *
     h* 08/18/16 C: 079 Project: 114WXY                               *
     h*                 Append docroot on folder path                 *
     h*                                                               *
     h*****************************************************************

     fWtisoimp  cf   e             workstn extfile('WTISOIMP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fStactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     fSsisp     if   e           k disk    Rename(pis01:pis0p)
     fXmlprel1  if a e           k disk

     dpsds           esds                  extname(szq1p)

      * ISO Score Import
     d ISOScrImpPr     pr

      * ISO Score Import Submit
     d ISOScrSubm      pr                  ExtPgm('S4412C1')
     d outFile                       60

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

       //*** Beg Add *** 08/18/16 ****************************
       //*** Beg Del *** 04/26/17 ****************************
       //**** * Retrieve doc root and suffix
       //**** d DocRoot         pr                  ExtPgm('RTVDOCROOT')
       //**** d   xxRoot_                     10

       //**** d SA030R          pr                  ExtPgm('SA030R')
       //****   d   xxSuffix                     3
       //**** d   xxFlrp                      80
       //*** End Del *** 04/26/17 ****************************
       //*** End Add *** 08/18/16 ****************************

     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrEmpLr
     d/copy wPrUsrprms

     d intCo#          s              3  0
      //*** Beg Add *** 08/18/16 **************************
       //*** Beg Del *** 04/26/17 ****************************
       //***D xxRoot          s             10
       //***D xxSufx          s              3
       //***D xxFlrP          s             80
       //*** End Del *** 04/26/17 ****************************
      //*** End Add *** 08/18/16 **************************

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // ISOScrImpPr - ISO Navigator Receive Program Submitter
       //===================================================================
     p ISOScrImpPr     b
     d ISOScrImpPr     pi

      /Free

       ISOScrSubm(XPxml);

       BtnCancel = *on;
      /End-free

     p ISOScrImpPr     e

       //===================================================================
       // LoadInitVal - Intialize the Screen fields
       //===================================================================

     p LoadInitVal     b
     d LoadInitVal     pi

      /Free
       scFlrName = *blanks;
       scFileName = *blanks;
       scEmAdr = *blanks;

       // Write record to XMLPREP
       Read pis0p;
       //*** Beg Add *** 08/18/16 ****************************
       //*** Beg Del *** 04/26/17 ****************************
       // Get Doc Root

       //*** DocRoot(xxRoot);
       //***   SA030R(xxSufx:xxFlrP);
       //***   If xxFlrP <> ' ';
       //***      xxFlrP = '/' + %trim(xxFlrP);
       //***   Endif;
       //*** End Del *** 04/26/17 ****************************
       //*** End Add *** 08/18/16 ****************************
       XPxml = %trim(ISifil)+'_'+%char(%Int( %Char( %date():*ISO0)));
       //*** Beg Chg *** 08/18/16 ****************************
       //*****XPxmlf = ISifld;
       //*** Beg Chg *** 04/26/17 ****************************
       //***XPxmlf = '/' + %trim(xxRoot) + %trim(xxFlrP) + '/' +
       //***         %trim(ISifld);
       XPxmlf = ISifld;
       //*** End Chg *** 04/26/17 ****************************
       //*** End Chg *** 08/18/16 ****************************
       XPmail = ISemad;
       XPtype = 'ISO Import';
       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = Timeto6(SystemTime());
       XPDt   = Dateto7(SystemDate());

       Write xp100;
      /End-free

     p LoadInitVal     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until cancel button is pressed.
       Dou BtnCancel  = *On ;

          Read pis0p;
          scFlrName = ISifld;
          scFileName = ISifil;
          scEmAdr = ISemad;

          // Display Screen
          Exsr ExfmtScreens;

          Select;

          When BtnSubmit = *on;
             BtnSubmit = *off;
                ISOScrImpPr();
                Leave;
          Other;
          Endsl;
       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 1050;
          APnlHeight = TabHeight + 50;

          Write rcdSysTbl;
          Write RcdTabISOI;

          Exfmt RcdISOSImp;

          Read rcdSysTbl;
          Read RcdTabISOI;

       EndSl;

       Endsr;
      /End-free
     p Main            e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Load initial values
       LoadInitVal();

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);

      /End-Free

     p ProgramStart    e
