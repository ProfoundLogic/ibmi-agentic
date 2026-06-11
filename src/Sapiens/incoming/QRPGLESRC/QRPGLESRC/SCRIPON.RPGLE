     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H*====================================================================
     H/TITLE SCRIPON - Turn on system flag for Scriptura Department letters
     H*====================================================================
     H*  Description:
     H*
     H*  Access System Control file record 'SY' to set SYFL11 position 7
     H*   to 'Y' for Scriptura Department Letters.
     H*
     H*  Created:  04/22/16  C:65  Project: SY251
     H*====================================================================
     H*  Revisions:
     H*
     H*====================================================================
     H* System Control file
     FSSYLF     uf   e           k disk
     D*
     D @@Flag          s              1
     D kkKey           s                   like(SYKEY)
     D*
      /free

       //* Access System Control file 'SY' record
       kkKey = 'SY';
       Chain(n) (kkKey) ssy01;

       If %subst(SYFl11:7:1) <> 'Y';

          Chain (kkKey) ssy01;
          SYFl11 = %subst(SYFl11:1:6) + 'Y' + %subst(SYFl11:8);
          Update ssy01;

       Endif;

       Return;
      /end-free
