     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H***********************************************************************
     H/TITLE FTBILLPRXH - Bill Import PerformRx Upload Transn Import History
     H DFTNAME(FTBILLPRXH)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('FTBNDDIR')
     h***********************************************************************
      *===============================================================*
      *  Created   : 04/12/16                                         *
      *  Programmer: 309                                              *
      *  Project   : 114MXO                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *                                                               *
      * ------------------------------------------------------------- *
      *===============================================================*

     ffTBILLPRXHCF   E             workstn extfile('FTBILLPRXH')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflBlImHst:rrnBLImHst)
     f*
     ffak9p     if   e             disk
     ffak9l1    if   e           k disk    rename(pk901:pk902)
     f                                     infds(dbfds1)

    d* data structures
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  rrnfak9p             397    400b 0

    d* standalone variables
     d process         s               n   inz(*off)
     d company         s              3p 0
     d uc              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d lc              c                   'abcdefghijklmnopqrstuvwxyz'
     d inCmd           s           2750
     d inCmdLen        s             15  5
      *

    d* procedure definitions
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

    d* Clear Subfiles
     d ClrSubFiles     pr

    d* load Transcation Import History screen
     dloadHstScreen    pr

    d* Main Processing
     d main            pr

    d* Program Start
     d programStart    pr
     d processMe                       n
      *

     d/copy sPrUsrPrms

     d FTBILLPRXHR     pr                  Extpgm('FTBILLPRXH')
     d  @@call                        1

     d FTBILLPRXHR     pi
     d  @@call                        1

     *---------------------------------------*
     * Main line code                        *
     *---------------------------------------*
      /Free

       ProgramStart(process);
       If process;
         Main();
       Endif;

       *inlr = *on;
       Return;
      /end-free
      /////////////////////////////////////////////////////////////////////

      //===================================================================
      // ClrSubFiles - Clears SubFiles
      //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnfak9p=0;
       rrnBLImHst=0;
       scRrnBLIMH= 0;

       clrHstSub = *On;
       dspHstSub = *Off;
       dspHstCtl = *Off;

       Write RcdBLImHst;
      /End-Free
     P ClrSubfiles     E

      //===================================================================
      // loadHstScreen - Load Transcation Import History screen
     //===================================================================
     PloadHstScreen    B
     dloadHstScreen    PI
      /free

      // Set subfile indicators
       clrHstSub = *Off;
       dspHstSub = *On;
       dspHstCtl = *On;

      // Read file and load the subfile
       Setll *loval Pk902;
       Read Pk902;
       Dow Not %Eof(fak9l1);
         Clear sflBlImHst;
         scMbr#    = K9MBR  ;

         Monitor;
           SCUpldDat = %Char(%Date(K9ULDT:*cymd):*usa/);
         On-error;
           SCUpldDat = '0001/01/01';
         Endmon;

         ScUpldTim  = K9ULTM ;

         Monitor;
           SCPrcsdDat = %Char(%Date(K9PRDT:*cymd):*usa/);
         On-error;
           SCPrcsdDat  = '0001/01/01';
         Endmon;

         scPrcsdTim= K9PRTM ;

         scRrnBLIMH = rrnfak9p;
         rrnBLImHst = rrnBLImHst + 1;

         If rrnBLImHst = 1;
           scHstRowSl = 'A';
         Else;
           clear scHstRowSl;
         Endif;
         Write sflBlImHst;
         Read pk902;
       Enddo;

       rrnBLImHst = 1;
      /end-free
     PloadHstScreen    E

     //===================================================================
     // Main - Main processing
     //===================================================================
     p Main            b
     d Main            pi
      /free

      // Process until exit button is pressed.
       Dou ExitNow = *On;

         // Clear Sub-files
          ClrSubfiles();

         // Load Transcation Import History screen
          loadHstScreen ();
          Exsr ExfmtScreens;

          Select;
         // Do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

         // Refresh screen
          When btnRefresh = *On;
             btnRefresh =*Off;

          Other;
          Endsl;
       Enddo;

      //===================================================================
      // Write Screen Formats;
      //===================================================================
       BegSr ExfmtScreens;
         Select;
         When TabIndex = 0;
           TabHeight = 1050;

           Write rcdTabPan;
           Exfmt RcdBLImHst;
           Read rcdTabPan;
         EndSl;
       Endsr;
      /end-free
     p Main            e

      //===================================================================
      // ProgramStart - Initial Program tasks
      //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n
      /Free

       TabIndex = 0;

       processMe = *on;
      // get company of the user
       company = GetUserInfo(q1user);
       Return;

      /end-free
     P ProgramStart    E

