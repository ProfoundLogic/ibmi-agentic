     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*Caller)
     H/define CONVERT
     H*
     H/TITLE WTLCKCLM - Lock Claim Record
     H*****************************************************************    **
     H*  DESCRIPTION:  This program would lock the Claim Record and
     H*                release the lock on pressing Exit.                *
     H*                                                                   *
     H*  CALLED FROM:                                                     *
     H*    MENU                                                           *
     H*                                                                   *
     H*  CALLS TO:                                                        *
     H*                                                                   *
     H*                                                                   *
     H*  REWRITE:               C:298     PROJECT: C114WXY                *
     H*********************************************************************
     H* REVISIONS:
     H*                                                                    *
     H* 03/24/22 C:333 Project: SY434                                      *
     H*                - Update Grids for Sorting Filtering Upgrade        *
     H*                                                                    *
     H***********************************************************************
     H* INDICATORS:
     H***********************************************************************
      *                                                                    *
     fWTLCKCLM  cf   e             workstn extfile('WTLCKCLM')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflLckClm:RrnClm)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fWmcmp     uf   e             disk    Rename(pcm01:pcm0p)
     f                                     InfDs(dbfds1)

     d/copy wPrGenPop
     d/copy scopypr01d
     d/copy sPrCompany
     d/copy wPrEmpLr
     d/copy wPrUsrprms
     d/copy sPrDate
     d/copy wPrPolErr

     d dbfds1          ds
     d  dbrrn1               397    400b 0


      * Clear Subfile
     d ClearSubfil     pr

     d* Load Subfile with Claims
     d LoadClm         pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      **
     d  outCo#         s              3
     d  intCo#         s              3  0
     d  outPgmId       s             10
     d  inDft          s              1
     d  inChg          s              1
     d  outDate        s              8
     d  outType        s              1
     d  outOutq        s             10
     d  outDdTyp       s              1
     d  outPrtSSN      s              1
     d  outLossDt      s              8
     d  outExdflg      s              1
      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // ClearSubfil - Clear subfile
       //===================================================================
     p ClearSubfil     b
     d ClearSubfil     pi

      /Free
       dspClm = *Off;
       dspClmCtl = *Off;
       clrClm = *On;
       RrnClm = 0;
       Write rcdlckclmR;
      /End-Free
     p ClearSubfil     e

       //===================================================================
       // Load Subfile
       //===================================================================
     p LoadClm         b
     d LoadClm         pi

      /free

       // clear the subfile.
       clear sflLckClm;
       clrClm  = *Off;
       dspClm = *On;
       dspClmCtl = *On;
       clear dbRrn1;

       Setll *start wmcmp;
       Read pcm0p;
       Dow Not %Eof(wmcmp);

          //Write claim record to subfile

          scComp# = CMco#;
          scGrpNo = CMfnd;
          scYr = CMfyr;
          scCase = CMcase;

          scClmRrn = dbrrn1;

          rrnClm = rrnClm + 1;

          Write sfllckClm;
          Read pcm0p;
       Enddo;

       rrnClm = 1;

      /End-Free
     p LoadClm         e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
      /End-Free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
         Dou BtnExit = *On ;

           ClearSubfil();

           // Load Claim Records
           LoadClm();

           // Process Claim Record Screen
           Exsr ExfmtScreens;

           Readc sfllckclm;

           Dow not %eof;

              Select;

              When scChgSt = *On;
                 scChgSt = *off;
                 Chain scClmrrn pcm0p;
                 If %found(wmcmp);
                    PopOneBtn('This Record has been locked.':
                     'Claim Key Information':'Unlock');
                    Unlock Wmcmp;
                 Endif;
              Other;
              Endsl;
              Readc sfllckclm;
           Enddo;

           If BtnExit = *On;
              Unlock Wmcmp;
              leave;
           Endif;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================

       BegSr ExfmtScreens;

          APnlHeight = 830;
          Write rcdComPnl;
          Write rcdTabLckC;
          Exfmt rcdLckClmR;
          Read rcdComPnl;
          Read rcdTabLckC;

       Endsr;

      /end-free
     p Main            e
