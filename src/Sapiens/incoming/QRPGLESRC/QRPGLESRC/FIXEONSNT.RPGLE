     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXEONSNT- Fix Network Sent Flag in file FAEOP.
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
     h*===============================================================*
     h*  Created   : 09/23/14                                         *
     h*  Programmer: C:296                                            *
     h*  Project   : 114MPQ                                           *
     h*===============================================================*
     h*  Revisions:                                                   *
     h*                                                               *
     h*                                                               *
     h* ------------------------------------------------------------- *
     h*  NOTE: Please add comments to the top of Revisions.           *
     h* ------------------------------------------------------------- *
     h*===============================================================*
     f*
     ffaeap     if   e             disk
     ffaeol4    uf   e           k disk

     d* Stand alone variables.
     d*
     dpsds           esds                  extname(szQ1p)

     C*
     C* Main program processing
      /Free
       Exsr SrMain;

       Begsr SrMain;
         Exsr SrUpdFAeop;
         *inlr = *on;
       Endsr;

       Begsr SrUpdFAeop;
         Read pea01;

         Dow not %eof;
           If EArcn = 0 and EAnSnt = 'R';
             chain (EAco# :EAsub# :EAfnd :EAfyr :EAcase :EAeob#) peo01;
                If %found(faeol4);
                   EOnsnt = EAnsnt;
                   EOpgm  = Q1pgm;
                   EOtime = %Dec(%Time():*hms);
                   EOdt   = %Dec(%Date():*cymd);
                   Update peo01;
                Endif;
           Endif;
         Read pea01;
         Enddo;

       EndSr;
      /End-Free

