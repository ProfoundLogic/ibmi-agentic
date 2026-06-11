     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/TITLE LODSMFCP - Load data for SMFCP (Folder Control)
     H option(*nodebugio:*srcstmt) bnddir('STBNDDIR')
     H dftactgrp(*no)
     //==========================================================
      // Notes: Add new folder entry at bottom: XXYYYYZZZZZZ
      //   XX     = 2A Prefix
      //   YYYYY  = 5A Folder Name
      //   ZZZZZZ = 40 Folder Description
     //==========================================================
     // Created: 02/16/16    C:136   Project: SY371
     //==========================================================
      // Revisions:
      //
     //==========================================================
     Fsmfcl     uf a e           k disk    rename(pfc01:pfc00)

     D#ofRcds          c                   100
     DaddKey           s             60    dim(#ofRcds) ctdata perrcd(1)
     Di1               s              3  0
     DsysTime          s                   like(FCtime)
     DsysDate          s                   like(FCdt)
     D psds          esds                  extname(szq1p)
     D/copy sPrDate

      /Free
       sysTime = TimeTo6(SystemTime);
       sysDate = DateTo7(SystemDate);
       For i1 = 1 to #ofRcds;
          Chain (%subst(addKey(i1):1:2)) pfc00;
          Select;
          When addKey(i1) = *blanks;
             Leave;
          When not(%found);
             Exsr WriteRecord;
          Endsl;
       Endfor;

       *Inlr = *On;
       //====================================================================
       // Write records to SMFCP
       //====================================================================
       Begsr WriteRecord;

       // Note: FCco# is not used
         FCpre  = %subst(addKey(i1):1:2);
         FCflrs = %subst(addKey(i1):3:5);
         FCdesc = %subst(addKey(i1):8:40);
         FCstat = 'A';
         FCuser = Q1User;
         FCpgm  = Q1Pgm;
         FCtime = sysTime;
         FCdt   = sysDate;
         FCiUsr = FCuser;
         FCiPgm = FCpgm;
         FCiDt  = FCdt;
         Write pfc00;

       Endsr;

      /End-free
**  100 A  - All qualifying output
DDDDRPTDeductible Dtl Report
DIDDINVDeductible Invoice
