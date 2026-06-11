     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE WTRPTAGLT - AGENT LIST
     H*****************************************************************    **
     H*  DESCRIPTION:  This program prompts for selection criteria to run *
     H*                the Agent List Report.                             *
     H*                                                                   *
     H*  CALLED FROM:                                                     *
     H*    MENU                                                           *
     H*                                                                   *
     H*  CALLS TO:                                                        *
     H*                                                                   *
     H*                                                                   *
     H*  REWRITE:               C:298     PROJECT: WC2472                 *
     H*********************************************************************
     H* REVISIONS:
     H*                                                                    *
     H***********************************************************************
     H* INDICATORS:
     H***********************************************************************
      *                                                                    *
     fWTRPTAGLT cf   e             workstn extfile('WTRPTAGLT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fWmfnl     if   e           k disk    Rename(pfn01:pfn00)
     fSmcol     if   e           k disk    Rename(pco01:pco00)
     fWmupl     if   e           k disk    Rename(pup01:pup00)

     d/copy scopypr01d
     d/copy sPrCompany
     d/copy wPrEmpLr
     d/copy wPrUsrprms
     d/copy sPrDate
     d/copy wPrPolErr

     d AgtLstSubm      pr                  Extpgm('W7340C')
     d  outCo#                        3
     d  outFnd                        3
     d  outZip                        3
     d  outtype                       1
     d  outOutq                      10
     d  outMode                       1    const
     d  outSel                        1
     d  outCity                      25
     d  outAgtSt                      1
     d  outIncCanA                    1

      * Agent List Proc.
     d AgtLstProc      pr

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Validate Panel Sreen Values
     d ValidPnlPH      pr

      **
     d outCo#          s              3
     d intCo#          s              3  0
     d  outOutq        s             10
     d isValidS        s               n   Inz(*off)
     d outZip          s              3
     d outFnd          s              3
      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

     p AgtLstProc      b
     d AgtLstProc      pi

      /Free
       outZip = %EditC(scZipCd:'X');
       outFnd = %EditC(scFnd:'X');
       AgtLstSubm(outCo#:outFnd:outZip:scAGENT:outOutq:'D':scSelBy:
        scCity:scAgtSt:scIncCanA);

       BtnCancel = *on;
      /End-free

     p AgtLstProc      e
       //===================================================================
       // LoadInitVal - Intialize the Screen fields
       //===================================================================
     p LoadInitVal     b
     d LoadInitVal     pi

      /Free

       Clear RcdAgtLst;
       isValidS = *off;

      /End-Free
     p LoadInitVal     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou BtnCancel  = *On ;

          SetCriteria();
          Exsr ExfmtScreens;

          Select;

          When BtnSubmit = *On;
             ValidPnlPH();
             If isValidS = *Off;
                AgtLstProc();
                Leave;
              Endif;

           When BtnRefresh = *On;
              BtnRefresh =*Off;
              // Intialize the Screen fields
              LoadInitVal();
              // Reload the program start values again.
              ProgramStart();
           Other;
           Endsl;
       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

          APnlHeight = 830;

          Write rcdComPnl;
          Write RcdTabAgtL;
          Exfmt RcdAgtLst;
          Read rcdComPnl;
          Read RcdTabAgtL;

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
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       outCo# = %EditC(intCo#:'X');
       scFnd  = userGrp(q1user);

       scIncCanA = 'N';
       scAgent   = 'A';

       // Retrieve Output Queue of the User.
       Chain q1user pup00;
       If %Found(Wmupl);
          outOutq = UPoutq;
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

      /End-Free

     p SetCriteria     e
       //===================================================================
       // ValidPnlPH - Validate Panel 1 Screen Fields
       //===================================================================
     p ValidPnlPH      b
     d ValidPnlPH      pi

      /Free

       erFnd = *off;
       erZipCd = *off;
       erCity = *off;
       erAgtSt = *off;
       erIncCanA = *off;
       msFnd = *blanks;
       msZipCd = *blanks;
       msCity = *blanks;
       msAgtSt = *blanks;
       msIncCanA = *blanks;
       isValidS = *off;

       If scSelBy = '1' and scZipCd = *Zeros;
          erZipCd = *On;
          msZipCd = 'WCR0503';
          isValidS = *On;
       Endif;

       If scSelBy = '2' and scCity = *Blanks;
          erCity = *On;
          msCity = 'WCR0503';
          isValidS = *On;
       Endif;

      /End-free

     p ValidPnlPH      e

