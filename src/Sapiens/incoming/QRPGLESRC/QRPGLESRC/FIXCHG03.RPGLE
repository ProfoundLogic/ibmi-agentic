     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
      *=================================================================================
      * Update CLMCHG03 entry in WTGTP
      * Project : 133WMZ C:282  Date: 5/15/12.
      *=================================================================================
     H* Revised:                                                                       *
     H*                                                                                *
      *=================================================================================
     fwtgtp     uf   e             disk    Rename(pgt01:pgt0p)
      *
     dintTime          s                   like(GTtime)
     dintDate          s                   like(GTdt)
     d psds          esds                  extname(szq1p)
      *
     d/copy sPrDate
      *
      /free

       //Processing starts here

       intTime = TimeTo6(SystemTime);
       intDate = DateTo7(SystemDate);

       // Update the record in file WTGTP

        Setll 1  pgt0p;
        Read  pgt0p;
        Dow Not %EOF(WTgtp);
           If (GTtbid = 'AXKY') And
              (GTkey = 'CLMCHG03');
              GTdesc = 'Claim Flag Changes';
              GTuser = q1User;
              GTpgm  = q1Pgm ;
              GTtime = intTime;
              GTdt   = intDate;
       //Update record in file WTGTP
              Update pgt0p;
           EndIf;

       // Read the next record of file WTGTP.
        Read  pgt0p;
        EndDo;

       //Processing ends here.
       *InLr = *On;

      /end-free
