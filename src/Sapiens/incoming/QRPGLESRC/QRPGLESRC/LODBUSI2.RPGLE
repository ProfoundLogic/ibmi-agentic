     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
      *=========================================================================
     H* Update the generic table file for Type of Business to include
     H* new type of business category.
     H*
     H* Project : WC2608 C:291  Date: 04/10/20.
     H*
     H* Called from: SYPOSTPGM2
     H* Calls to:    None
     H*
     H*=========================================================================
     H* Revisions:
     H*
     H*****************************************************************
     H*
     fsmcop     if   e             disk
     fwtgtl     uf a e           k disk

     dstrKey           s                   like(GTkey)
     dstrTbId          s                   like(GTtbid)

     dintTime          s                   like(GTtime)
     dintDate          s                   like(GTdt)
     d X               s              3  0

     d psds          esds                  extname(szq1p)

     d/copy sPrDate
     d/copy wprUsrPrms

     d key             S             10    DIM(14) CTDATA PERRCD(1)              KEY, category
     d catg            S              1    DIM(14) ALT(key)

      /free
       //Processing starts here
       intTime = TimeTo6(SystemTime);
       intDate = DateTo7(SystemDate);

       Read pco01;
       Dow not %eof(SMcop);
          strTbId = 'BUSI';
          For X = 1 to 14;
             strKey = key(X);
             Chain (coco#:strTbId:strKey) pgt01;
             If %found(wtgtl);
                If GTFG01 = ' ';
                   GTFG01 = catg(X);
                   GTuser = Q1user ;
                   GTpgm  = Q1pgm;
                   GTdt  =  intDate;
                   GTtime=  intTime;
                   Update pgt01;
                Endif;
             Endif;
          Endfor;
          Read pco01;
       Enddo;

       *InLr = *On;
       Return;

      /end-free

** WTGTP KEYS & Category
1         1
2         2
3         3
4         4
5         4
6         2
7         4
8         4
9         4
A         3
B         4
C         4
D         2
E         4
