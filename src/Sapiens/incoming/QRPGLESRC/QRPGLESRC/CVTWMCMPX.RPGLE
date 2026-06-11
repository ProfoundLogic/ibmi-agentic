     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*bnddir('WTBNDDIR')
      *================================================================
     H* Convert WMCMPX
     H*
     H* CREATED: 11/25/2013 C: 282 Project: 129WSC
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H*****************************************************************
      * Client Control Master File
     fwmcmp     uf   e             disk
      *
     d EOF             s               n   inz( *OFF )
      *
      /Free
       // Read each record from WMCMP.

       Setll 1 pcm01;
       DoW Not EOF;
          Read pcm01;
          If %EOF(wmcmp);
             Leave;
          Endif;                   // %EOF(wmcmp)

       // Upadte the subrogation flag with 3rd party Ind.
          CMSUBR = *Blanks;
          CMsubr = CM3rd;
          Update pcm01;

       EndDo;
       *InLr = *On;
      /End-Free
