     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title STAUTODRYB - Auto Diary Trigger Processing (Batch)
     h dftname(STAUTODRYB)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Created   : 05/22/15                                              *
      *  Programmer: C:279                                                 *
      *  Project   : 114WKA                                                *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *                                                                    *
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
      *====================================================================*
     fsmtrplog  uf a e           k disk    rename(ptr01:ptr0log)

      * work variables
     d outCo#          s              3    inz('000')
     d outFnd          s              3    inz('000')

      /copy sPrTrigger
      /copy sPrDate

      /free

       // Percentage of Specific Retention
       ProcessTrigger('CLMRETEN':outCo#:outFnd);

       // Total Claim Payments
       ProcessTrigger('PAYMENT2':outCo#:outFnd);

       // Compensation Weeks Paid
       ProcessTrigger('WEEKSPD':outCo#:outFnd);

       // Policy Issuance Pending
       ProcessTrigger('POLPEND':outCo#:outFnd);

       // New Business Quote Status
       ProcessTrigger('NQTESTS':outCo#:outFnd);

       // Renewal Business Quote Status
       ProcessTrigger('RQTESTS':outCo#:outFnd);

       // Save details in log file for records already processed.
       Exsr SaveLogRec;

       *inlr = *on;

       //===================================================================
       // Save details in log file for records already processed.
       //===================================================================
       BegSr SaveLogRec;

       // Check if last run detail exist in log file (key - 000/'Batch')
       Chain (0:'Batch') ptr0log;
       If %found(SMTRPLOG);
          TRiDt  = Dateto7(SystemDate());
          Update ptr0log;
       Else;
          TRco#  = 0;
          TRname = 'Batch';
          TRiDt  = Dateto7(SystemDate());
          Write ptr0log;
       Endif;

       Endsr;

      /end-free
