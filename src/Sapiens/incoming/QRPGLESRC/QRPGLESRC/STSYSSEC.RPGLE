     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE STSYSSEC  - PROMPT FOR SYSTEM SECURITY REPORT
     H***********************************************************************
     H* REVISIONS:                                                         *
     H*                                                                    *
     H***********************************************************************
      *                                                                    *
     fSTSYSSEC  cf   e             workstn extfile('STSYSSEC')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fStActpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')

     fxmlprel1  uf   e           k disk
     dpsds           esds                  extname(szq1p)

      ** Screen variables
     d intCo#          s              5p 0 inz
     d outMode         s              1    inz(*blank)
     d outRRN          s              9
     d outPrefix       s              2
     d outstring       s             10
     d outCall         s             10
     d outCo#          s              3
     d outFnd          s              3
     d inReturn        s            250    inz
     d outXML          s             60    inz(*blanks)
     d xmFile          s             61    inz(*blanks)
     d isvalid         s              1
     d strSecGp        s              9
      *
      *
     d/copy sPrGenPop
      *
     d StSysSecP       pr                  Extpgm('STSYSSEC')
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d StSysSecP       pi
     d  inPrds                       40A   const
     d  inCall                        1A
     d
     d STLETTER        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)

     d S4400C          pr                  ExtPgm('S4400C')
     d   xmFile_                     61    const

      * Prototypes
     d/copy sPrUsrPrms
     d/copy sPrCompany
     d/copy sPrDate
     d/copy sPrLetter

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Validate Panel Sreen fields
     d ValidPnlPH      pr

      * Process the XML generation
     d Processpr       pr

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // LoadInitVal - Intialize the Screen fields
       //===================================================================
     p LoadInitVal     b
     d LoadInitVal     pi

      /Free
      /End-Free
     p LoadInitVal     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Display Screen
          Exsr ExfmtScreens;

          // If Submit on screen
          If scBtnSbmt = *On;
             ValidPnlPH();
          EndIf;

          // If all edit pass continue further
          If isvalid = *On;
             Processpr();
          EndIf;

          //Do nothing - catch tab click event
          If TabClick = *on;
             tabClick = *off;
          Endif;

          //If Btnrefresh is click
          If BtnRefresh = *On;
             BtnRefresh =*Off;
          EndIf;

          //If Btncancel is click
          If scBtnCncl = *On;
             scBtnCncl =*Off;
             ExitNow = *On;
          EndIf;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

          When TabIndex = 0;
             TabHeight = 600;
             APnlHeight = TabHeight + 50;

             Write RcdSysTbl;
             Write RcdTabSysS;
             Exfmt RcdRptSysS;
             Read RcdSysTbl;
             Read RcdTabSysS;

       EndSl;
       msUser = *Blanks;
       erUser = *Off;

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
       LoadInitVal();

       // Load Action panel with the Company Name.
       intCo# = GetUserInfo(q1user);
       scCompName = GetCompNam(intCo#);

       tabClick = *Off;

       Return;
      /End-Free
     p ProgramStart    e
       //===================================================================
       // ValidPnlPH - Validate Panel Screen Fields
       //===================================================================
     p ValidPnlPH      b
     d ValidPnlPH      pi

      /Free
       isvalid = *Off;
       If scBtnSbmt = *On;
             Select;
             When scUser = *Zeros and scSecGrp = *Zeros;
                isvalid = *On;
             When scUser <> *Zeros and scSecGrp <> *Zeros;
               erUser = *On;
               msUser = 'SYM0509';

             Other;
                isvalid = *On;
             EndSl;
       EndIf;

      /End-Free

     p ValidPnlPH      e
       //===================================================================
       // Processpr - Continue with the process
       //===================================================================
     p Processpr       b

      /Free
        // Get the XML File Name from STLETTER
        outCall   = 'XMLREQUEST';
        outXML    = *Blanks;
        inReturn  = 'System Security Report';
        outMode   = *Blanks;
        outRrn    = *Blanks;
        OutPrefix = *Blanks;
        OutString = 'SysSecRpt';
        OutCo#    = *Blanks;
        OutFnd    = *Blanks;

        StLetter(outCall:outXml:inReturn:outMode:OutRrn:
         OutPrefix:OutString:OutCo#:OutFnd);

        // Update the record with User or Group
        Chain (outXml) xp100;
        If %Found(xmlprel1);
           If scUser <> *Zeros;
              XPtext1 = %Char(scUser);
              XPtext2 = *Blanks;
           Else;
              XPtext1 = *Blanks;
              If scSecGrp = *zeros;
                 XPtext2 = 'All Groups';
              Else;
                 strSecGp = %Char(scSecGrp);
                 XPText2 = %trim(%editc(scSecGrp:'X'));
              Endif;
           Endif;

           XPPgm  = Q1PGM;
           XPUser = Q1USER;
           XPTime = Timeto6(SystemTime());
           XPDt   = Dateto7(SystemDate());

           Update xp100;
           PopOneBtn('Report has been submitted':'');
           ExitNow = *On;
           xmFile = outXml + '$';
           S4400C(xmFile);
        EndIf;

      /End-Free

     p Processpr       e
