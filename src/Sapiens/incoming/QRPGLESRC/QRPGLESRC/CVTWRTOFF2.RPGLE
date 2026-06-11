     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Convert Write off fields to Group Auxiliary
     h*
     h*  Called from: CVTWRTOFF
     h*  Calls to:    None
     h*
     h*  Created on 11/22/2013 by C:279 for Project - 129WSB
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************

     fwdf2l     uf   e           k disk    rename(pf201:pf200)
     fsmcop     if   e             disk    rename(pco01:pco0p)

      /free

       // Main processing
       Read pco0p;

       Setll (COco#) pf200;
       Reade (COco#) pf200;
       Dow not %eof;
          If F2thrl = 0;
             F2thrl = COthrl;
          Endif;

          If F2thrh = 0;
             F2thrh = COthrh;
          Endif;

          If F2dbcd = *blanks;
             F2dbcd = COdbcd;
          Endif;

          If F2pbcd = *blanks;
             F2pbcd = COpbcd;
          Endif;

          If F2fbcd = *blanks;
             F2fbcd = COfbcd;
          Endif;

          Update pf200;

          Read pf200;
       Enddo;
       Unlock WDF2L;

       *inlr = *on;

      /end-free
