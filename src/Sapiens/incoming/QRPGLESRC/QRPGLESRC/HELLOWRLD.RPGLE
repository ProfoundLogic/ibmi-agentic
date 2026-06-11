     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('WTBNDDIR')
      //====================================================================
     H/TITLE HELLOWRLD - Test exit program logic.
      //====================================================================
      //  Description:
      //
      //  This program is a test for exit programs
      //
      //  Created:  04/22/16  C:302  Project: 25WUK
      //====================================================================
      //  Revisions:

     d Title           s             50

     d HelloWrld       pr
     d                               60

     d HelloWrld       pi
     d   XMLFileName                 60

      /Free
      /copy wprGenPop
       Title = 'Exit program HELLO WORLD';
       PopOneBtn(XMLFileName:Title);
       *inlr = *on;
       Return;

      /End-Free

