     Hcopyright('(c) 2019 Sapiens, Inc. All rights reserved. -
     H Sapiens is a trademark of Sapiens, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE  W4236R - Bank Information Reconciliation Prompt
     H*****************************************************************
     H*                                                               *
     H*  Program Description:                                         *
     H*    This program is the prompt the starts the bank information *
     H*    reconciliation process.                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  Called From: Menu                                            *
     H*  Calls to:    W4236C                                         *
     H*                                                               *
     H*****************************************************************
     H* Created on 12/03/2019 by c:62 for Project 102WID              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FW4236F    cf   e             workstn
     F                                     Handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmupl     if   e           k disk    Rename(pup01:pup00)
     fsmcol     if   e           k disk    Rename(pco01:pco00)

     d fids01        e ds                  ExtName(SZQ2P)
     d psds          esds                  ExtName(SZQ1P)

     d SrCenter        pr                  ExtPgm('SRCENTER')
     d  srDfld                       80
     d  srDlen                        3  0

     d Submitter       pr                  ExtPgm('W4236C')
     d  inCompany                     3

     D inPrDs          s             40
     D srDfld          s             80
     d srDlen          s              3  0
     d inCompany       s              3

      /free

       // Get the user parameter file
       Chain Q1user pup00;
       If Not %Found(wmupl);
          UPco# = 0;
       EndIf;

       // Get Company Information
       Chain UPco# pco00;
       If %Found(smcol);
          srDfld = COname;
          srDlen = 40;
          SrCenter(srDfld:srDlen);
          scCompName = srDfld;
       Else;
          scCompName = ' ';
       EndIf;

       tabHeight  = 425;
       aPnlHeight = tabHeight + 60;
       Write rcdComPnl;
       Exfmt RcdTabBank;
       Read rcdComPnl;

       // Submit the job
       If btnSubmit = *On;
          inCompany = %EditC(UPco#:'X');
          Submitter(inCompany);
       EndIf;

       // End Program
       *inLR = *On;

      /end-free
