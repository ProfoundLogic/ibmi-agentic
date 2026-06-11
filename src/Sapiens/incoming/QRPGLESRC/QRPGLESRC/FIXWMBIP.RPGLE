     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
      *=================================================================================
      * Fix EFT flag
      * Project : WC2044 C:282  Date: 4/17/12.
      *=================================================================================
     H* Revised:                                                                       *
     H*                                                                                *
      *=================================================================================
     fwmbip     uf   e             disk    Rename(pbi01:pbi0p)
      *
     dintTime          s                   like(BItime)
     dintDate          s                   like(BIdt)
     d psds          esds                  extname(szq1p)
      *
     d/copy sPrDate
      *
      /free

       //Processing starts here

       intTime = TimeTo6(SystemTime);
       intDate = DateTo7(SystemDate);

       // Update the record in file WMBIP

        Setll 1  pbi0p;
        Read     pbi0p;
        Dow Not %EOF(WMbip);

            If (BIcktp = 'EFT');

               BIcktp = *blanks;
               BIeftc = 'E';
               BIuser = q1User;
               BIpgm  = q1Pgm ;
               BItime = intTime;
               BIdt   = intDate;
               //Update record in file WMBIP
               Update pbi0p;

           EndIf;

        // Read the next record of file WMBIP.
        Read     pbi0p;
        EndDo;

       //Processing ends here.
       *InLr = *On;

      /end-free
