     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hCopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: JSON Delivery gateway program.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: Portal
     h*  Calls to: Various
     h*
     h*  Created on 04/17/2017 by C:65  for SY390 Portal project.
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrString

      * *ENTRY
     DJsonGateway      pr                  ExtPgm('JGATEPGM')
     D                              100a
     D                              100a
     D                             9000a
     D                                 a   len(16000000)
     D                                1a
      *
     DJsonGateway      pi
     D action                       100a
     D user                         100a
     D criteria                    9000a
     D outJSON                         a   len(16000000)
     D rtnCode                        1a
      *
     d JSONPgm         pr                  ExtPgm(strPgm)
     d   inUser                     100a
     d   inCriteria                9000a
     D   inJSON                        a   len(16000000)
     D   inRtnCode                    1a

     D strPgm          s             10

      *
      /Free

       Select;
       When action = 'List Groups';
          strPgm = 'JLISTGRP';
          JSONPgm(user:criteria:outJSON:rtnCode);
       Other;
          rtnCode = '1';
          Return;
       Endsl;

       Return;
      /End-Free

