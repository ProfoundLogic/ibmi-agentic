     hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h BNDDIR('WTBNDDIR')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description:   Generic AJAX program to call Toolbar Wrapper Program
     h*               - Activation group should be *CALLER
     h*
     h*  Called from:
     h*  Calls to:
     h*
     h*  Created on 03/22/2016 by C:308 for Project Genericize Toolbar
     h*  Programming
     h**********************************************************************
     h* Revisions:
     h**********************************************************************
     d strTWrpPgm      S             10
    *
     d GenerToolWrap   pr                  ExtPgm('STGENTWRP')
     d                                9  0
     d                               20
     d                              256
     d                                2
      *
     d GenerToolWrap   pi
     d inRRN                          9  0
     d inOption                      20
     d inMode                       256
     d inEnvironment                  2
      *
      * Policy Toolbar Wrapper program
     dWrapperp         pr                  ExtPgm(strTWrpPgm)
     d inRRN                          9  0
     d inOption                      20
     d inMode                       256
     d inEnv                          2
      *
      * Claim Toolbar validate program
      /Free

       Select;

          When inEnvironment = *Blank or  inEnvironment = 'Q';
             strTWrpPgm = 'WTPOLTWRP';

          When inEnvironment = 'C';
             strTWrpPgm = 'WTCLMTWRP';

          When inEnvironment = 'SP';
             strTWrpPgm = 'ALPOLTWRP';

          When inEnvironment = 'SC';
             strTWrpPgm = 'CMCLMTWRP';

       EndSl;
       Wrapperp(inRRN:inOption:inMode:inEnvironment);
       Return;
      /End-Free
