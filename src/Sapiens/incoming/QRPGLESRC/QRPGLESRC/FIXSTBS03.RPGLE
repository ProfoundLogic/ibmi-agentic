     HCopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H****************************************************************
     H*  FIXSTBS03 -  Update STSTP/STbs03 for a state.
     H*
     H*  Description: This pgm will update field STSTP/STbs03 for
     H*               a passed state.
     H*
     H*  Created    : 05/11/2017  Programmer: 180  Project: GA038
     H*
     H*  Called By  : FIXSTBS03L
     H*  Calls To   :
     H*
     H****************************************************************
     H*  Revisions:
     H****************************************************************
     F*
     FSTSTL1    UF   E           K DISK

     D* *Entry
     D FIXSTBS03       pr                  ExtPgm('FIXSTBS03')
     D                               10A
     D                                7A

     D FIXSTBS03       pi
     D   InKey                       10A
     D   Inmsg                        7A

      *
      * Stand Alone
      *
     D State           s             10A


      *
      * Main
      *
      /Free
       State = Inkey;
       Chain Inkey STSTL1;
       If %found(STSTL1);
          STbs03 = 'Y';
          ST7002 = inMsg;
          Update PST01;
       Endif;
       *inlr = *on;
      /End-Free
