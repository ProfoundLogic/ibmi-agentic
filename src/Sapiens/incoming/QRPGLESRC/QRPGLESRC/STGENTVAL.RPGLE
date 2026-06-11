     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h BNDDIR('WTBNDDIR')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description:   Generic AJAX program to validate Toolbar actions
     h*               - Activation group should be *CALLER
     h*
     h*  Called from:
     h*  Calls to:
     h*
     h*  Created on 03/31/2016 by C:308 for Project Genericize Toolbar
     h*  Programming
     h**********************************************************************
     h* Revisions:
     h**********************************************************************
     d intPos          S              2P 0
     d intPos1         S              2P 0
     d inEnvironment   S              2    Inz
     d strTValPgm      S             10
      *
     dGenerToolVal     pr                  ExtPgm('STGENTVAL')
     d                                9  0
     d                              512
     d                            32767
     d                                1
      *
     dGenerToolVal     pi
     d inRRN                          9  0
     d inData                       512
     d outJSON                    32767
     d outRtnCode                     1
      *
     dValidator        pr                  ExtPgm(strTValPgm)
     d inRRN                          9  0
     d inData                       512
     d outJSON                    32767
     d outRtnCode                     1

      /Free
       intPos = *zero;
       intPos1 = *zero;
       inEnvironment = *Blank;
       intPos = %Scan(',':inData);
       If intPos > 0;
          intPos = intPos + 1;
          intPos1 = %Scan(',':inData:intPos);
          intPos1 = intPos1 - 1;
          If intPos1 > 2;
             inEnvironment = %SubSt(inData:intPos:intPos1);
          EndIf;
       EndIf;

       outJSON = *Blank;
       outRtnCode = *Blank;
       Select;

          When inEnvironment = *Blank or  inEnvironment = 'Q';
             strTValPgm = 'WTPOLTVAL';

          When inEnvironment = 'C';
             strTValPgm = 'WTCLMTVAL';

          When inEnvironment = 'SP';
             strTValPgm = 'ALPOLTVAL';

          When inEnvironment = 'SC';
             strTValPgm = 'CMCLMTVAL';

       EndSl;
       Validator(inRRN:inData:outJSON:outRtnCode);
       Return;
      /End-Free
