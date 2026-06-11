     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *=================================================================================
      * Load Bank codes to generic tables WTGIP, WTGTP
      * Project : 114WDE C:255  Date: 3/12/12.
      *=================================================================================
     H* Revised:                                                                       *
     H*                                                                                *
      *=================================================================================
     fwtgtp     uf a e           k disk
     fwtgip     uf a e           k disk
     fwmupl     if   e           k disk
      *
     DintTime          s                   like(GItime)
     DintDate          s                   like(GIdt)
     DintCo#           s                   like(GIco#)
     dstrDescription   c                   'Processing'
     d*
     d psds          esds                  extname(szq1p)
     d
     d/copy sPrDate
     d/copy wprUsrPrms
      *
      /free

       //Processing starts here
       intTime = TimeTo6(SystemTime);
       intDate = DateTo7(SystemDate);
       intCo#  = UserCo#(Q1user);

       // Populate system table file WTGIP
       GIco#  = intCo#;
       GItbid = 'BNK';
       GIdesc = 'EFT SPECIAL PROCESSING BANK CODES';
       GIstat = 'A';
       GIuser = Q1user;
       GIpgm  = Q1pgm ;
       GIdt  =  intDate;
       GItime = intTime;
       GIfn01=  'VALUE';

       //Write record in file WTGIP
       Write     pgi01 ;

       //Populate system table file WTGTP
       GTco#  = intCo#;
       GTtbid = 'BNK';
       GTkey  = 'WF0';
       GTdesc = 'Wells Fargo - CSI Special';
       GTdesc =  %trim(GTdesc) + ' ' + strDescription;
       GTstat = 'A';
       GTuser = Q1user ;
       GTpgm  = Q1pgm;
       GTdt  =  intDate;
       GTtime=  intTime;
       GTfg01=  'U';
       GTfg02=  ' ';

       //Write record in file WTGTP
       Write     pgt01   ;

       //Processing ends here.
       *InLr = *On;
      /end-free
