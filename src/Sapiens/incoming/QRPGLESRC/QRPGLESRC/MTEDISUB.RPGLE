     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title MTEDISUB - Medical EDI Resubission
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h*****************************************************************
     h* Created: 4/18/18 C: 079 Project: MC421                        *
     h*****************************************************************
     h* Revisions:                                                    *
     h*****************************************************************
     f*
     fMtedisub  cf   e             workstn extfile('MTEDISUB')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     f*
     f* User Parameter Master File
     fSmupl     IF   E           K DISK
     f*
     f* WCMC EOB Header File
     ffaeol31   uf   e           k Disk
     f                                     InfDs(DBFd1)
     f* 837 Log Recordr File
     ffelgl1    if   e           k Disk
     f*
     f* WCMC Case Lockout File
     Ffxlop     uf   e           k Disk

     dpsds           esds                  extname(szq1p)

     d intCo#          s                   Like(Upco#)
     d xxAst           s                   Like(EOaSt)
      *
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy sPrUsrPrms
     d/copy sPrDate
     d DBFd1           DS
     d  DBrrn1               397    400B 0
    
      * Main Procedure Prototype
     d Mainpgm         pr                  ExtPgm('MtEdiSub')
     d                                1
     d                                1
     d                                9  0
     d                                1
    
      * Main Procedure Interface
     d Mainpgm         pi
     d   inkc                         1
     d   inkl                         1
     d   inEOB#                       9  0
     d   OutRtCd                      1
    
      * Update 837 EDI Log Record
     d UpdEDILog       pr                  ExtPgm('F6250R')
     d   inCo#_                       3
     d   inSub#_                      5
     d   inFnd_                       3
     d   inFYR_                       3
     d   inCase_                      7
     d   inEOB_                       9
     d   inRcn_                       3
     d   inActF_                      1
     d   inRtCd_                      1
     d   inCo#         s              3
     d   inSub#        s              5
     d   inFnd         s              3
     d   inFyr         s              3
     d   inCase        s              7
     d   inEOB         s              9
     d   inRcn         s              3
     d   inActF        s              1
     d   inRtCd        s              1
    
      * Update EDI flag Prototype
     d UpdEdiFlag      pr                  ExtPgm('FA740R')
     d                                9  0
     d                                1
     d inRrn           s              9  0
    
      * Standalone fields
    d inError         s               n
    d msError         s            100a
    
      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Process the screen
     d Process         pr

      * Process states that use 837
     d Process837      pr

      * Process Florida
     d ProcessFL       pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Program End
     d ProgramEnd      pr

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       // End Of Program
       ProgramEnd();

      /End-Free
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free

       sccompany# = %Char(getUserInfo(q1user));
       inCorrect = *on;
       msError = 'Please select an action from the list';

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
          When BtnProcess = *on;
             ClrErr();
             ValidPnl();
             If inError = *Off;
                Process();
                inkc = *on;
                Leave;
             Endif;
          When BtnCancel = *on;
             inkl = *on;
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
          Write RcdTabEDI;

          Select;
          When xxAst = 'FL';
             Exfmt RcdEDIFL;
          Other;
             Exfmt RcdEDI837;
          Endsl;

          Read rcdSysTbl;
          Read RcdTabEDI;

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

       tabClick = *Off;

       // Get the EOB
       Chain (intCo#:inEOB#) peo01;
       If %Found (faeol31);
          xxAst = EOast;
       Else;
          xxAst = *blanks;
          ProgramEnd();
       Endif;

       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // Process - Process the EDI Resubmission
       //===================================================================
     p Process         b
     d Process         pi

      /Free

       Select;
       When xxAst = 'FL';
          ProcessFL();

       Other;
          Process837();
       Endsl;

        // Update EDI flags in header files
       inRrn = DBrrn1;
       Update peo01;
       callp UpdEDIFlag(inrrn:outRtCd);


      /End-Free

     p Process         e
       //===================================================================
       // ProcessFL - Process Florida
       //===================================================================
     p ProcessFL       b
     d ProcessFL       pi

      /Free

       Select;
       When scResubFL = 'W';
          EOtape = 'W';
       When scResubFL = 'C';
          If EOtape = 'Y';
             EOsbDt = *blanks;
          Else;
             EOtape = 'R';
          Endif;
       When scResubFL = 'R';
          EOtape = 'B';
       When scResubFL = 'A';
          EOtape = 'A';
       Endsl;

      /End-Free

     p ProcessFL       e
       //===================================================================
       // Process837 - Process States that use 837
       //===================================================================
     p Process837      b
     d Process837      pi

      /Free

       // Get the 837 EDI Log Record for this EOB
       Chain (EOco#:EOsub#:EOfnd:EOeob#) plg01;
       If %Found (felgl1);
          inCo# = %Editc(LGco#:'X');
          inSub# = %Editc(LGsub#:'X');
          inFnd  = %Editc(LGfnd:'X');
          inFyr  = %Editc(LGfyr:'X');
          inCase = %Editc(LGcase:'X');
          inEOB  = %Editc(LGeob#:'X');
          inRcn  = %Editc(LGrcn:'X');
          inRtCd = ' ';

       // Update the EDI log file base on selection
          Select;
          When scResub837 = 'C';
             inActF = 'D';
          When scResub837 = 'R';
             If LGmtC = '00' and LGsts <> 'TA' and LGsts <> 'TE';
                inActF = ' ';
             Else;
                inActF = 'C';
             Endif;
             Endsl;
          UpdEDILog(inCo#:inSub#:inFnd:inFyr:inCase:inEOB:inRcn:
                 inActF:inRtCd);
       Endif;
      /End-Free

     p Process837      e
       //===================================================================
       // ProgramEnd - Final Procedure
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi

      /Free

          Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase) plo01;
          If %Found(fxlop);
             Delete plo01;
          Endif;

       *InLr = *on;

       Return;

      /End-Free

     p ProgramEnd      e
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free
       inError = *off;
      /End-Free

     p ClrErr          e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free
       If xxAst = 'FL' and scResubFL = ' ';
          inError = *on;
       Endif;
       If xxAst <> 'FL' and scResub837 = ' ';
          inError = *on;
       Endif;


      /End-Free
     p ValidPnl        e
