     h**********************************************************************
     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to verify user group authority
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: STUSRDRY
     h*  Calls to:
     h*
     h*  Created on 09/09/2014 by C:279 for Support: 85602
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
      /copy sPrAtrium
      * Entry
     dDefaultPoints    pr                  ExtPgm('STUSRDRY01')
     d                                9  0
     d                            32767
     d                                1

     dDefaultPoints    pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d charPoints      s             40    inz
     d intDashPos      s              5  0 inz
     d intEndPos       s              5  0 inz
     d intRrn#         s              3  0 inz
     d intLen          s              5  0 inz
     d intStartPos     s              5  0 inz
     d charValue       s             41    inz
     d charGrp         s              1

      /free
       //                          1   5
       // input data is in format: ,x
       // x - Rrn#

       intRrn#  = 0;
       charGrp  = ' ';
       outJSON  = ' ';


       BuildJSON(outJSON :'outTemp' :'A');

       RtnCode = ' ';
       Return;
      /end-free
