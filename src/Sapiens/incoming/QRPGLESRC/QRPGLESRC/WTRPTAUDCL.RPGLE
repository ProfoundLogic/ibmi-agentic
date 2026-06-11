     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WTRPTAUDCL - Promt for Audit Change List
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTAUDCL - Prompt for Audit Change List
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h* 06/06/2017 c:62 Ticket 47584                                  *
     h*                 Correct the policy year edit                  *
     h*                                                               *
     h*****************************************************************

     fWtrptaudclcf   e             workstn extfile('WTRPTAUDCL')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fWmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fSmcol     if   e           k disk    Rename(pco01:pco00)
     fWmupl     if   e           k disk    Rename(pup01:pup00)
     fWdf1l     if   e           k disk    Rename(pf101:pf100)
     fWmeml     if   e           k disk    Rename(pem01:pem00)
     fWmemp     if   e           k disk    Rename(pem01:pem0p)
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     f*** Begin Add *** 06/06/17 **********************************
     fWdell1    if   e           k disk
     f*** End   Add *** 06/06/17 **********************************

     dpsds           esds                  extname(szq1p)
     dstrOutq          s                   Like(Upoutq)
     dintCo#           s              3  0
     d inMode          s              1
     d inCall          s              1
     d outRrn#         s              9
     d empRrn          s              9  0
     d inSelect        s           1024
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELfyr)
     d outCo#          s              3
     d outFnd          s              3
     d outYr           s              3
     d outEmp#         s              9
     d outDiv          s              5
     d outType         s              1
     d outOUTQ         s             10
     d*outMode         s              1
     d isValidS        s               n   Inz(*off)

     d
      *
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrAgent
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs

      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall                         1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Audit Change List
     d AudChgPr        pr

      * Employer Search Proc.
     d EmployerSrch    pr

      * Audit Change List Submit
     d AudChgSubm      pr                  ExtPgm('W7600C')
     d outCo#                         3
     d outFnd                         3
     d outYr                          3
     d outEmp#                        9
     d outDiv                         5
     d outType                        1
     d outOUTQ                       10
     d outMode                        1    const

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
       clear RcdAudChg;
      /End-free

     p LoadInitVal     e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       // Intialize the Screen fields
       LoadInitVal();

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       scFnd  = userGrp(q1user);

       // Retrieve Output Queue of the User.
       Chain(n) q1user pup00;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       sctype = 'E';
       If ScEmpNum = *zeros;
          isEMPnbr = '0';
       EndIf;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       //Policy Years
       scPolYr1 = 0;
       scPolYr2 = 0;
       If scEmpNum = 0;
          Chain(n) (intCo#:scFnd) pf100;
          If %Found(wdf1l);
             scPolYr1 = F1fyr;
             fyrChoices = 'F1co# = ' + %char(F1co#) + ' AND F1fnd = '
              + %char(F1fnd);
          Endif;
          flgYear1 = *on;
          flgYear2 = *off;
       Else;
          scPolYr2 = ELfyr;
          fyrChoice1 = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
           + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
           + ' AND ELdiv = ' + %char(ELdiv);

          flgYear1 = *off;
          flgYear2 = *on;
       Endif;
      /End-Free

     p SetCriteria     e
       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       isEMPnbr = '0';
       inMode  = 'E';
       inCall  = '2';
       clear dsSrchCrit;
       clear inSelect;
       dsEmpGrp  = scFnd;
       dsEmpName = scEmpName;
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(inMode:inCall:outRrn#:inSelect);

       If outRrn# <> '000000000';

          empRrn=%Dec(outRrn#:9:0);
          Chain(n) empRrn pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;

             Chain(n) (intCo#:intFnd:intEmp#:intDiv) pem00;
             If %Found(WMEML);
                scEmpNum     = EMemp#;
                scEmpName  = EMname;
                isEMPnbr = '1';
                scDivision = EMdiv;
             EndIf;

          EndIf;

       EndIf;

      /End-Free

     p EmployerSrch    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until cancel button is pressed.
       Dou BtnCancel  = *On ;
          SetCriteria();

          // Display Screen
          Exsr ExfmtScreens;

          Select;

          When BtnEmpSrch=*On;
             BtnEmpSrch = *off;
             EmployerSrch();
          When BtnSubmit = *on;
             BtnSubmit = *off;
             ValidPnl();
             If isValidS = *off;
                AudChgPr();
                Leave;
             Endif;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             // Reload the program start values again.
             ProgramStart();
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

          Write rcdComPnl;
          Write RcdTabAudC;

          Exfmt RcdAudChg;

          Read rcdComPnl;
          Read RcdTabAudC;

       EndSl;

       Endsr;
      /End-free
     p Main            e
       //===================================================================
       // ValidPnl - Validate Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free
       erPolYr = *off;
       erEmpNum = *off;
       erDiv1 =*off;
       erDiv2 = *off;
       msPolYr = *blanks;
       msEmpNum = *blanks;
       msDiv1 = *blanks;
       msDiv2 = *blanks;
       isValidS = *off;

       // Validation Check for Group
       If scfnd = *Zeros;
          erFnd      = *On;
          msFnd      = 'WCR0503';
          isValidS   = *On;
       EndIf;

       //Validate the fund year entered

       //*** Begin Chg *** 06/06/17 ***************************
       //*If*scEmpNum = 0;
       //***Chain(n) (UPco#:scFnd:scPolYr1) pf100;
       //***If*not*%found;
       //******erPolYr = *on;
       //******msPolYr = 'WCR0517';
       //******isValidS = *on;
       //***Endif;*
       //*Endif;***
       //**********
       //*If*scEmpNum = 0;
       //***Chain(n) (UPco#:scFnd:scPolYr2) pf100;
       //***If*not*%Found;
       //******erPolYr = *on;
       //******msPolYr = 'WCR0517';
       //******isValidS = *on;
       //***Endif;*
       //*Endif;***
       If scEmpNum = 0;
         Chain(n) (UPco#:scFnd:scPolYr1) pf100;
         If not %found;
            erPolYr = *on;
            msPolYr = 'WCR0517';
            isValidS = *on;
         Endif;
       Else;
         Chain(n) (UPco#:scFnd:scPolYr2) pel01;
         If not %Found;
            erPolYr = *on;
            msPolYr = 'WCR0517';
            isValidS = *on;
         Endif;
       Endif;
       //*** End   Chg *** 06/06/17 ***************************

       //Validate Employer Entered

       If scEmpNum <> 0;
          Chain (UPco#:scFnd:scEmpNum:scDivision) Pem00;
          If not %Found;
             erEmpNum = *on;
             msEmpNum = 'WCR0538';
             isValidS= *on;
          Endif;
       Endif;


      /End-free

     p ValidPnl        e

     p AudChgPr        b
     d AudChgPr        pi

      /Free
       outCo# = %EditC(intCo#:'X');
       outFnd = %EditC(scFnd:'X');
       If scEmpNum = 0;
          outYr = %EditC(scPolYr1:'X');
       Else;
          outYr = %EditC(scPolYr2:'X');
       Endif;
       outEmp# = %EditC(scEmpNum:'X');
       outDiv = %EditC(scDivision:'X');
       AudChgSubm(outCo#:outFnd:outYr:outEmp#:outDiv:
        scType:UPoutq:'D');

       BtnCancel = *on;
      /End-free

     p AudChgPr        e
