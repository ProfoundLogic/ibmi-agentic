     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title MTRPTEOBRP - EOMB Batch Reprint
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h*****************************************************************
     h* Created: 4/24/17 C: 079 Project: MC410                        *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     h*                                                               *

     fMtrpteobrpcf   e             workstn extfile('MTRPTEOBRP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     f*
     f* User Parameter Master File
     fSmupl     IF   E           K DISK
     f*

     dpsds           esds                  extname(szq1p)

     d msgf            s             10    inz('FSMESG')
     d msgfLib         s             10    inz('*LIBL')

     d inBchDt         s              8
     d intBchDt        s              8  0
     d inEombTyp       s              1
     d inOutq          s             10
     d inMode          s              1

     d charMsg         s            100
     d outRrn#         s              9
     d inRrn           s              9  0
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(Upco#)
     d strOutq         s                   Like(Upoutq)
     d inSelect        s           1024
      *
     d inKc            s              1
     d inKl            s              1
     d inErr           s              1
     d inCo#           s              3
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy sPrUsrPrms
     d/copy sPrDate
    
      * Get client ID
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Submit the EOMB Batch Reprint Process
     d EobRpProc       pr

      * EOMB Batch Reprint Submit
     dEobRpSbm         pr                  ExtPgm('F0345C')
     d inCo#                          3
     d inBchDt                        8
     d inEombtyp                      1
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
       // EobRpProc - EOMB Batch Reprint Submit
       //===================================================================
     p EobRpProc       b
     d EobRpProc       pi

      /Free

       inCo# = %EditC(intCo#:'X');

       If intBchDt <> 0;
          intBchDt= dtYYMD(intBchDt);
          inBchDt = %EditC(intBchDt:'X');
       Else;
          inBchDt = *Blanks;
       EndIf;

       inEombTyp = scEombTyp;

       inOutq     = strOutq;
       inMode     = 'B';

       EobRpSbm(inCo#:inBchDt:inEombTyp:inOutQ:inMode);

      /End-Free

     p EobRpProc       e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       scBchDt    = 0;
       scEombTyp  = *Blanks;

       sccompany# = %Char(getUserInfo(q1user));

      /End-Free
     p LoadInVal       e
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

          Select;
          When BtnSubmit = *on;
             // Clear Errors
             ClrErr();
             // Validate Edits
             ValidPnl();
             If isValid = *Off;
                EobRpProc();
                Leave;
             EndIf;
          When BtnCancel = *on;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Intialize the Screen fields
             LoadInVal();
             // Clear Errors
             ClrErr();
             // Load Action panel with the Company Name.
             intCo# = getUserInfo(q1user);
             scCompName = GetCompNam(intCo#);
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

          Write rcdSysTbl;
          Write RcdTabRprt;

          Exfmt RcdEobRprt;

          Read rcdSysTbl;
          Read RcdTabRprt;

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

       // Load Action panel with the Company Name.
       intCo# = getUserInfo(q1user);
       scCompName = GetCompNam(intCo#);

       // Retrieve Output Queue of the User.
       Chain(n) q1user pup01;
       If %Found(Smupl);
          strOutq = Upoutq;
       EndIf;

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

       isValid    = *off;
       msgBchDt   = *blanks;
       erBchDt    = *off;
       msgEobTyp  = *blanks;
       erEobTyp   = *off;

      /End-Free

     p ClrErr          e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

       // Validation Check for Batch Date.

       intBchDt = dtCheck(scBchDt);
       If intBchDt   = 0;
          erBchDt    = *On;
          msgBchDt   = 'MCR0500';
          isValid    = *on;
       Endif;

       // Validation Check for Account Type.

       If scEombTyp = *Blanks;
          erEobTyp   = *On;
          msgEobTyp  = 'MCR0503';
          isValid    = *On;
       EndIf;

      /End-Free
     p ValidPnl        e
