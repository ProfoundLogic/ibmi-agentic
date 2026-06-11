     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXARMENU - Fix A/R menu to call group select
     h dftname(FIXARMENU)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h/define   profoundui
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/28/14                                         *
      *  Programmer: C:279                                            *
      *  Project   : 129WSV                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fmmopp     uf   e             disk    rename(pop01:pop0p)
     fatnavp    uf   e             disk    rename(atnavr:pav0p)

      /free

       // main program processing

       // process MMOPP file
       Read pop0p;
       Dow not %eof;
          If %Trim(%subst(OPexcl:1:10)) = 'W7380R';
             OPexcl = 'W7944C                 (''' + 'W7380R''' + ')';
             update pop0p;
          Endif;
          Read pop0p;
       Enddo;
       Unlock MMOPP;

       // process ATNAVP file
       Read pav0p;
       Dow not %eof;
          If %scan('W7380R':ANactn) > 0;
            If %scan('MTH':ANactn) > 0;
             ANactn = '/profoundui/auth/start?pgm=PDSOBJ/STLAUNCH&p1=W7944C    +
                      &l1=10&p2=MTH&l2=3&p3=%28%27W7380R%27%29*End&l3=19';
            Else;
             ANactn = '/profoundui/auth/start?pgm=PDSOBJ/STLAUNCH&p1=W7944C    +
                      &l1=10&p2=LIV&l2=3&p3=%28%27W7380R%27%29*End&l3=19';
            Endif;
            update pav0p;
          Endif;
          Read pav0p;
       Enddo;
       Unlock ATNAVP;

       *inlr = *on;
