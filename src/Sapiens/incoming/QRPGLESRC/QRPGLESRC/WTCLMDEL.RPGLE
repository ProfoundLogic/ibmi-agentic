     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WTCLMDEL - Claim Deletion
     H DFTNAME(WTCLMDEL)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 01/18/13                                         *
      *  Programmer: C:279                                            *
      *  Project   : SY266                                            *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwpigenpop cf   e             workstn extfile('WPIGENPOP')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fsdcml     if   e           k disk    rename(pcm01:pcm00)

     d @@can           s               n
     d process         s               n   inz(*off)
     d dartsKey        s             40
     d xxRrn           s              9  0

     d* main Processing
     d main            pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d* Darts exist
     d DARTSExist      pr              n

     d/copy wPrUsrPrms
     d/copy sPrString
    
     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9 0          wmcmp
     c                   Parm                    @@can

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = @@rrn;
       Endif;

       ProgramStart(process);

       If process;
          Main();
       Endif;

       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       // Check for Darts and payroll
       if DARTSExist = *on;
          clear scOkCnMsg;
          clear scBtn1Txt;
          clear scBtn2Txt;
          scBtn1Txt = 'Continue';
          scBtn2Txt = 'Cancel';
          scOkCnTtl = 'Information';
          scOkCnMsg = 'There are active DARTS Diary items attached to this ' +
           'claim. It is suggested that they be reviewed before the claim ' +
           'is closed.';

          btnOk = *off;
          btnCancel = *off;
          dou btnOk = *on or btnCancel = *on;
             exfmt rcdTwoBtn;
             select;
             when btnOk = *on;
                @@can = *off;
             other;
                @@can = *on;
             endsl;
          enddo;
       endif;

      /end-free
     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // Chain to Employer master for Program.
       chain xxRrn PCM0P;

       // Process only if valid employer record found.
       if not %found;
          processme = *Off;
          return;
       endif;

       scPLeft = 360;
       scPTop  = 215;
       scMLeft = 370;
       scMTop  = 260;
       scOkLeft = 515;
       scOkTop  = 340;
       scCnLeft = 625;
       scCnTop  = 340;

       processMe = *on;
       return;

      /end-free
     P ProgramStart    E
      //===================================================================
       // Determine if DARTS records exist
      //===================================================================
     P DARTSExist      b
     D DARTSExist      pi              n

      /free

       // Establish Key
       clear dartsKey;
       dartsKey = %EditC(CMco#:'X') + %EditC(CMfnd:'X') + %EditC(CMfYr:'X') +
        %EditC(CMcase:'X');
       Chain dartsKey pcm00;
       if %found(SDCML);
          Return *On;
       else;
          Return *Off;
       endIf;

       return *off;
      /end-free
     P DARTSExist      e
