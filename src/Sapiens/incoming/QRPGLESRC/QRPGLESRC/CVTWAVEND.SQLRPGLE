     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
      *=================================================================================
      * Update Waiver Endorsements with Endorsement Type "waiver"
      * Project : 133WQP C:291  Date: 08/20/18.
      *=================================================================================
     H* Revised:                                                                       *
     H*                                                                                *
      *=================================================================================
     fsmcop     if   e             disk
     fwtgtl     uf a e           k disk
     fwtgil     uf a e           k disk
      *
     DstrKey           s                   like(GTkey)
     DstrTbId          s                   like(GItbid)

     DintTime          s                   like(GItime)
     DintDate          s                   like(GIdt)

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

       Read pco01;
       Dow not %eof(SMcop);
          strTbId = 'ENTP';
          Chain (coco#:strTbId) pgi01;
          If not %Found(wtgtl);
             //Populate system table file WTGIP
             GIco#  = coco#;
             GItbid = strTbId;
             GIdesc = 'Endorsement Type';
             GIstat = 'A';
             GIuser = Q1user;
             GIpgm  = Q1pgm ;
             GIdt  =  intDate;
             GItime = intTime;
             GIfn01=  'Value';
             write pgi01;
          Endif;

          strKey = 'Waiver';
          Chain (coco#:strTbId:strKey) pgt01;
          If not %Found(wtgtl);
             //Populate system table file WTGTP
             GTco#  = coco#;
             GTtbid = strTbId;
             GTkey  = strKey;
             GTdesc = 'Waiver Endorsement';
             GTstat = 'A';
             GTuser = Q1user ;
             GTpgm  = Q1pgm;
             GTdt  =  intDate;
             GTtime=  intTime;
             //Write record in file WTGTP
             Write pgt01;
          Endif;

          //Read Next Co# from SMCOP
          Read pco01;
       Enddo;

        Exec SQL
         Update wtenp
          set ENType = 'Waiver', ENuser = :q1user,
          ENpgm = :q1pgm, ENDt = :intDate, ENtime = :intTime
          where substring(ENform,1,9) in (
          'WC000313 ',
          'WC040306 ',
          'WC420304A',
          'WC420304B',
          'WC430305 ',
          'WC480604 '
          );

       //Processing ends here.
       *InLr = *On;
      /end-free

