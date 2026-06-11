     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXFAEOP- Populate Network Sent Flag from FAEAP.
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
     h*===============================================================*
     h*  Created   : 02/23/15                                         *
     h*  Programmer: C:296                                            *
     h*  Project   : 114MRD                                           *
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
     dclientID         s              3A   inz(*blanks)
     d*
     dpsds           esds                  extname(szQ1p)
     d*
     d*---------------------------------------------------------------
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d*---------------------------------------------------------------

     C*
     C* Main program processing
      /Free
       Exsr SrMain;

       Begsr SrMain;

         GetClientID(clientID);

         If clientID = '114';
            Exsr SrUpdFAeop;
         Endif;
         *inlr = *on;
       Endsr;

       Begsr SrUpdFAeop;
         Read pea01;

         Dow not %eof(faeap);
           If EArcn = 0 and (EAnSnt = 'S' or EAnSnt = 's');
             chain (EAco# :EAsub# :EAfnd :EAfyr :EAcase :EAeob#) peo01;
                If %found(faeol4);
                   EOnsnt = 'S';
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

