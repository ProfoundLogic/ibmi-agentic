     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXPNCMENU - Add PNC Processing to menu
     h dftname(FIXPNCMENU)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h/define   profoundui
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 07/07/14                                         *
      *  Programmer: C:053                                            *
      *  Project   : 048WXW                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fmmopp     uf a e             disk    rename(pop01:pop0p)
     fatnavp    uf a e             disk    rename(atnavr:pav0p)

     d CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d CltId                          3

      /free

       // main program processing

       RtvCltID(CltId);
       If CltId <> '048';
          *inlr = *on;
          return;
       EndIf;




       // process MMOPP file
             OPmnam = 'OPER-PROC ';
             OPmYN = 'N';
             OPapp = 'LIV';
             OPexcl = 'PCL001R1';
             OPyno = 'N';
             OPmmsc = 000;
             OPmins = 010;
             OPty01 = 'A';
             OPty02 = 'P';
             OPty03 = 'P';
             OPty04 = 'M';
             OPmntt = 'PNC Positive Pay File Processing';
             OPoptn = '99PN';
             OPstat = 'A';
             write pop0p;
       Unlock MMOPP;

       // process ATNAVP file
             ANsect = 'N';
             ANitem = 999999;
             ANseq  = 13;
             ANparent = 663;
             ANtype = '1';
             ANactntype = '0';
             ANtext = 'PNC Positive Pay File Processing';
             ANwindow = '0';
             ANonce = '0';
             ANactn = '/profoundui/auth/start?pgm=PDSOBJ/STLAUNCH&p1=PCL001R1  +
                      &l1=10&p2=LIV&l2=3&p3=%28%29*End&l3=6';
             write pav0p;
       Unlock ATNAVP;

       *inlr = *on;
