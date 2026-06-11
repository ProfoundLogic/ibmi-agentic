     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
      *=================================================================================
      * WI CX Reasons for the endorsement.
      * Project : WC2896 C:291  Date: 03/07/23.
      *=================================================================================
     H* Revised:                                                                       *
     H*                                                                                *
      *=================================================================================
     fsmcop     if   e             disk
     fwtgtl     uf a e           k disk
     fwtgil     uf a e           k disk
      *
     d X               s              3  0
     d strKey          s                   like(GTkey)
     d strTbId         s                   like(GItbid)

     d intTime         s                   like(GItime)
     d intDate         s                   like(GIdt)

     d key             S             10    DIM(6) CTDATA PERRCD(1)              KEY, Description
     d desc            S             40    DIM(6) ALT(key)

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
          strTbId = 'WICX';
          Chain (coCo#:strTbId) pgi01;
          If not %Found(wtgil);
             //Populate system table file WTGIP
             GIco#  = coCo#;
             GItbid = strTbId;
             GIdesc = 'WI Cancel Reasons';
             GIstat = 'A';
             GIuser = Q1user;
             GIpgm  = Q1pgm ;
             GIdt  =  intDate;
             GItime = intTime;
             GIfn01=  'CODE';
             write pgi01;
          Endif;

          For X = 1 to 6;
             strKey = key(X);
             Chain (coCo#:strTbId:strKey) pgt01;
             If not %Found(wtgtl);
                //Populate system table file WTGTP
                GTco#  = coCo#;
                GTtbid = strTbId;
                GTkey  = strKey;
                GTdesc = desc(X);
                GTstat = 'A';
                GTuser = Q1user ;
                GTpgm  = Q1pgm;
                GTdt  =  intDate;
                GTtime=  intTime;
                //Write record in file WTGTP
                Write pgt01;
             Endif;
          Endfor;

          //Read Next Co# from SMCOP
          Read pco01;
       Enddo;

       //Processing ends here.
       *InLr = *On;
      /end-free
** WTGTP KEYS & Description
01        Retiring From Business or Out of Bus
02        Completed Operations (No Emp/Exp/Oper)
03        Cancelled By Employer
08        Change of Interest or Ownership/Bus Sold
09        Coverage Placed Elsewhere
13        Misrepresentation of Info on Application
