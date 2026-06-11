     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hCopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: JSON Delivery program to list groups.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: JGatePgm
     h*  Calls to:
     h*
     h*  Created on 04/17/2017 by C:65  for SY390 Portal project.
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmfnl     if   e             disk    rename(pfn01:pfn00)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrString

      * *ENTRY
     DJsonListGroups   pr                  ExtPgm('JLISTGRP')
     D                              100a
     D                             9000a
     D                                 a   len(16000000)
     D                                1a
      *
     DJsonListGroups   pi
     D user                         100a
     D criteria                    9000a
     D outJSON                         a   len(16000000)
     D rtnCode                        1a
      *
     d numEdtCde       c                   const('N')

      *
      /Free

       Setll 0 pfn00;
       Read(n) pfn00;

       outJSON = ' ';

       If %eof;
          rtnCode = '1';
          Return;
       Endif;

       outJSON = '{ "list" : ' + x'BA' + ' { ';

       Dow not %eof;

          outJSON = %trim(outJSON) + ' "group" : ' + %editc(FNFnd : 'X') +
           '", "name" : "' + %trim(ExtrAlphaNum(FNName)) + '" }';

          Read(n) pfn00;

          If not %eof;
             outJSON = %trim(outJSON) + ',';
          Endif;

       Enddo;

       outJSON = %trim(outJSON) + ' ' + x'BB' + ' } ';

       rtnCode = ' ';
       Return;
      /End-Free

