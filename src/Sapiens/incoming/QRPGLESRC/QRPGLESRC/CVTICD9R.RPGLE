     H copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE CVTICD9R
     H dftactgrp(*no) actgrp(*CALLER)
     H**************************************************************************
     H*  Description:  Diagnosis Code Conversion Program
     H*
     H*  Created: 09/08/14   Programmer C:280   Project:147MDC
     H**************************************************************************
     H*  Revisions:
     H*
     H**************************************************************************
     H*
     FFXICD9P   IF   E             DISK
     H*
     H* FXICD9 - ICD9 Upload File
     H*
     FSMDIL     UF A E           K DISK    rename(pdi01:smpdi)
     H*
     H* Diagnosis Code Physical File (ICD9) by icd9/type
     H*
     FFMDIL     UF A E           K DISK    rename(pdi01:fmpdi)
     H*
     H* Diagnosis Code Physical File (ICD9) by icd0/type
     H*
     D*
     Dxxloc            s              3  0
     Ddate             s              7s 0
     Dtime             s              6s 0
     D@@icd9           s              5
     D*
     DPSDS           ESDS                  Extname(SZQ1P)
     D*
     C*----------------------------------------------------------------------
     C*  Main Line Process
     C*----------------------------------------------------------------------
     C*
      /free

      // Date & Time

           time  = %Dec(%Time(): *iso);
           date  = %dec(%date():*USA);

      // Read Input File

           read fxicd9p;

           dow not %eof(fxicd9p);

      // Remove Decimal

           xxloc = 0;
           xxloc = %scan('.':iccode);

           if xxloc > 0;
            @@icd9  = %subst(iccode:1:xxloc-1);
            @@icd9  = %trim(@@icd9) + %subst(iccode:xxloc+1);
           else;
            @@icd9 = iccode;
           endif;

      // Get SMDIP File

           chain (@@icd9:ictype) smdil;

           if not %found(smdil);

      // Add Record

            clear smpdi;

            diicd9  = @@icd9;
            didsc1 = icshort;
            didsc2 = iclong;
            distat = 'A';
            diuser = Q1user;
            dipgm  = Q1pgm;
            ditime = time;
            didt   = date;
            diiusr = Q1user;
            diidt  = date;
            dilost = ictype;
            dids1  = iccode;
            dispecific = icspecific;

            write smpdi;

           else;

            didsc1 = icshort;
            didsc2 = iclong;
            distat = 'A';
            diuser = Q1user;
            dipgm  = Q1pgm;
            ditime = time;
            didt   = date;
            dilost = ictype;
            dids1  = iccode;
            dispecific = icspecific;

            update smpdi;

           endif;

      // Get FMDIP File

           chain (@@icd9:ictype) fmdil;

           if not %found(fmdil);

      // Add Record

            clear fmpdi;

            diicd9  = @@icd9;
            didsc1 = icshort;
            didsc2 = iclong;
            distat = 'A';
            diuser = Q1user;
            dipgm  = Q1pgm;
            ditime = time;
            didt   = date;
            diiusr = Q1user;
            diidt  = date;
            dilost = ictype;
            dids1  = iccode;
            dispecific = icspecific;

            write fmpdi;

           else;

            didsc1 = icshort;
            didsc2 = iclong;
            distat = 'A';
            diuser = Q1user;
            dipgm  = Q1pgm;
            ditime = time;
            didt   = date;
            dilost = ictype;
            dids1  = iccode;
            dispecific = icspecific;

            update fmpdi;

           endif;

           read fxicd9p;

           enddo;

        *inlr = *on;

      /end-free
