     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to default the agent rate based on agent.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLQTE
     h*  Calls to:
     h*
     h*  Created on 10/11/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     FWMAAL1    IF   E           K DISK    rename(PAA01:PAA01)

      /copy sPrAtrium

      * *ENTRY
     DQuoteAgentRate   pr                  ExtPgm('WTPOLQTE11')
     D                              512
     D                            32767
     D                                1
      *
     DQuoteAgentRate   pi
     D inData                       512
     D outJSON                    32767
     D rtnCode                        1
      *
     D                 DS
     D  data                   1    512
     D  inCo#                  1      3  0
     D  inFnd                  4      6  0
     D  inAgent                7     12
     D*
     D xxRate          s              3  0 inz
     D agent#          s              5  0 inz
     D agtStr          s              5    inz
     C*
     C* Agent Assignment File Key List
     C*
     C     keyWMAAL1     Klist
     C                   Kfld                    inCo#
     C                   Kfld                    inFnd
     C                   Kfld                    agent#
     C*

      /Free

       data = inData;
       xxRate = 0;

       agtStr = %subst(inAgent:1:%scan(' ':inAgent:1)-1);
       agent# = %int(%trim(agtStr));

       If agent# <> 0 and inCo# <> 0 and inFnd <> 0;

          Chain(n) keyWMAAL1 PAA01;

          If %found;

             xxRate = AADRT#;

          Endif;

       Endif;

       outJSON = ' ';

       If xxRate <> 0;
          BuildJSON(outJSON :'agentRate' : %char(xxRate));
       Else;
          BuildJSON(outJSON :'agentRate' : ' ');
       Endif;

       rtnCode = ' ';
       Return;
      /End-Free

