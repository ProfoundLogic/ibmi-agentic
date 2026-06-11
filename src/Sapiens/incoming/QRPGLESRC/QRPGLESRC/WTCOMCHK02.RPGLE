     h Copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WTCOMCHK02 - Process Group Validation
     h dftname(WTCOMCHK02)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define   PROFOUNDUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
    *====================================================================*
    *  Created    : 08/05/14                                             *
    *  Programmer : C:279                                                *
    *  Project    : 114WNC                                               *
    *  Description: Check Schedule Maintenance Validation                *
    *                                                                    *
    *====================================================================*
    *  Revisions:                                                        *
     *  07/20/20 C: 079 Project: WC2749                                   *
     *                  Recompile                                         *
    *                                                                    *
    * ------------------------------------------------------------------ *
    *  NOTE: Please add comments to the top of Revisions.                *
    * ------------------------------------------------------------------ *
    *====================================================================*

     fwtcomchk01cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflErrMsg:rrnErrMsg)
     fwmbil7    if   e           k disk    rename(pbi01:pbi07)
      *  Group master
     fwmfnl     if   e           k disk    rename(pfn01:pfn0l)
      *  Group Auxiliary      key=CO#/GRP
     fwdf2l     if   e           k disk    rename(pf201:pf20l)
     fwxgsp     if   e             disk    rename(pgs01:pgs0p)

     d pdDs          esDs                  extname(szq1p)
     d bnkCdDA         ds            10    dtaara(bnkCd)
     d  BankCode               1     10

     d clientID        s              3    inz('000')
     d isBnkTyp        s              1    inz('N')
     d intCo#          s              3  0
     d isError         s               n
     d strCktp         s                   like(BICktp)

      * Proto-types

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      * Program Start
     d ProgramStart    pr

      * Main Processing
     d Main            pr

     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sprDate
     d/copy wPrUsrPrms

     c     *entry        plist
     c                   parm                    @@errFnd          1

      /free

       ProgramStart();

       Main();

       *inlr = *on;
       Return;
      /end-free

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /free

       isError = *off;
       @@errFnd = ' ';
       intCo# = UserCo#(q1user);

       GetClientID(clientID);

      /end-free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       // Load screen fields
       Exsr EditRecords;
       If isError = *on;
          Exsr ErrorDisplay;
       Else;
          // load bank code
          IN *lock bnkCdDA;
          BankCode = strCkTp;
          OUT bnkCdDA;
       Endif;

       Return;

       //===================================================================
       // Error Display
       //===================================================================
       BegSr ErrorDisplay;

       Exfmt rcdEdit;
       @@errFnd = 'Y';

       Endsr;

       //===================================================================
       // WriteRec
       //===================================================================
       BegSr WriteRec;

       isError = *on;
       rrnErrMsg = rrnErrMsg + 1;
       Write sflErrMsg;

       Endsr;
       //===================================================================
       // Edit the group records
       //===================================================================
       BegSr EditRecords;

       dspErrMsg = *off;
       clrErrMsg = *on;
       Write rcdEdit;
       dspErrMsg = *on;
       clrErrMsg = *off;
       rrnErrMsg = 0;
       btnOk = *off;

       Read pgs0p;
       Dow not %eof(wxgsp);
          If gsIeFl = 'I';
             scGrpErr = GSFnd;
             scErrMsg = ' ';

             Setll (intCo#:'WC':GSfnd:*hival) pbi07;
             Reade (intCo#:'WC':GSfnd) pbi07;
             If not %eof(wmbil7);
                // Bank code must be same for all groups selected
                If isBnkTyp <> 'Y';
                   isBnkTyp = 'Y';
                   strCkTp  = BICkTp;
                Else;
                   If strCkTp <> BICkTp;
                   // scErrMsg = 'The Groups selected do not have the same +
                   //  Bank Code.';
                   // Exsr writeRec;
                      strCkTp  = ' ';
                   Endif;
                Endif;
             Endif;

             // Group must be valid
             Chain (intCo#:GSfnd) pfn0l;
             If not %found;
                scErrMsg = 'The Group is not valid.';
                Exsr writeRec;
             Endif;

             // Group expiration date < system date
             IF FNExDt > 0 and FNExDt < Dateto7(SystemDate());
                scErrMsg = 'The Group is expired as of date.';
                Exsr writeRec;
             Endif;

             // Verify Group Auxiliary record
             Chain (intCo#:GSfnd) pf20l;
             If not %Found;
                scErrMsg = 'The Group Auxiliary record is not found.';
                Exsr writeRec;
             Else;
                // Group Auxiliary Process Claims Flag check
                If F2ClCk <> 'Y';
                   scErrMsg = 'The Group is not setup to process claim +
                    payments.';
                   Exsr writeRec;
                Endif;
             Endif;

          Endif;

          Read pgs0p;
       Enddo;
       rrnErrMsg = 1;
       Endsr;

      /end-free
     p Main            e
