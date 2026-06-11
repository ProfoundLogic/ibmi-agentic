     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h*
     h*  Description: Response program to check Claim Handling Instructions.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*
     h*  Called from: Ajax Call
     h*  Calls to:
     h*
     h*  Created on 09/04/2014 by C:279 for Project: 114WOE
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************

     fwmcmp     if   e             disk    rename(pcm01:pcm0p)

     d psds          esds                  extname(szq1p)

      /copy sPrAtrium
      * entry
     dClaimHandl01     pr                  ExtPgm('WTCLMHDI01')
     d                                9  0
     D                            32767
     d                                1

     dClaimHandl01     pi
     d inRrn                          9  0
     D outJSON                    32767
     d RtnCode                        1

      * work variable
     d intDivCnt       s              3  0

      /copy sCpyClmHdI
      *=============================================================================================
      * Main Processing
      *=============================================================================================
      /free

       Chain(n) inRrn pcm0p;

       outJSON = ' ';
       isInstExist(CMco#:CMfnd:CMemp#:CMdiv);

       BuildJSON(outJSON :'Claim Handling Instructions' :
        %EditC(intDivCnt:'X'));

       RtnCode = ' ';
       Return;

      /end-free
       //---------------------------------------------------------------------
       // Claim Handling Instruction Exist
       //---------------------------------------------------------------------
     pisInstExist      b                   Export
     disInstExist      pi              n
     dintCo#                          3  0
     dintFnd                          3  0
     dintEmp#                         9  0
     dintDiv                          5  0

     disRecFnd         s               n
      /free

       intDivCnt = 0;
       isRecFnd  = *off;


       // declare cursor
       Exec Sql Declare Cursor Scroll Cursor For
        Select Count(*) from WDCHP Where CHco# = :intco# and CHfnd = :intFnd and
        CHemp# = :intEmp# and CHdiv = :intDiv;

       // open cursor
       Exec SQL Open Cursor;

       // read from cursor
       Exec SQL Fetch Cursor Into :intDivCnt;

       // If record exist
       If SQLCOD >= 0 and SQLCOD <> 100 and intDivCnt > 0;
          isRecFnd = *on;
       Endif;

       // close cursor
       Exec SQL close Cursor;

       Return isRecFnd;

      /end-free

     pisInstExist      e
