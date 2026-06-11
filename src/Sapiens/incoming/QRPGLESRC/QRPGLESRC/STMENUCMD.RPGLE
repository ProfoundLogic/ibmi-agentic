     h*Copyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h*StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h DFTACTGRP(*no)
     h**********************************************************************
     h*
     h*  Description: This program only be use for commands that do not
     h*               require screen results.
     h*
     h*  Called from: Menu
     h*  Calls to:
     h*
     h*  Created on 06/11/2013 by C:282 for Redmine: 2785
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     Fstmcl     if   e           k disk    Rename(pmc01:pmc00)
      *
     d charErrDesc     c                   'Your system is not properly +
     d                                      configured for this menu option. +
     d                                      Please contact the system +
     d                                      administrator.'
      *
     d @@ProcessID     s             25A
     d charProcessID   s             25A
     d flgButton       s              1A

      /copy sPrGenPop
      /copy sPrRunCmd

     c     *Entry        Plist
     c                   Parm                    @@ProcessID

      /Free

       charProcessID = @@ProcessID;
       flgButton = '*';

       Chain (charProcessID) pmc00;
       If Not %Found(stmcl);
          flgButton = PopOneBtn(charErrDesc:'Invalid Menu Option':'Ok');
       Else;
          If MCbtn1 = *Blank;
                MCbtn1 = 'Execute';
          EndIf;
          If MCbtn2 = *Blank;
                MCbtn2 = 'Cancel';
          EndIf;
          If MCmsg = *Blank;
                MCmsg = 'Press ''' + %trim(MCbtn1) + ''' to process.';
          EndIf;

          Select;
          When MCbtn3 <> *Blank;
               flgButton = PopThreeBtn(MCmsg:MCtitle:MCbtn1:MCbtn2:MCbtn3);
          Other;
               flgButton = PopTwoBtn(MCmsg:MCtitle:MCbtn1:MCbtn2);
          EndSl;

          Select;
          When flgButton = '1';
             RunCmd(MCcmd1:256);
          When flgButton = '0';
             RunCmd(MCcmd2:256);
          When flgButton = ' ';
             RunCmd(MCcmd3:256);
          EndSl;

       Endif;

       *InLr = *On;

      /End-Free
