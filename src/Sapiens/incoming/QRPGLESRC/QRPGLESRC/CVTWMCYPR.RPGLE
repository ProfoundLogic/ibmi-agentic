     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/Title 'Convert City name (CTCTY) in WMCYP to title case'
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H*****************************************************************
     H*                                                               *
     H* CREATED: 12/10/13  C:267 Redmine: 1509 FMIC                   *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/10/13 C:267 Redmine: 1509                                  *
     H*          Convert City name to upper/lower case.               *
     H*          Example: MIAMI BEACH ==> Miami Beach                 *
     H*                                                               *
     H*****************************************************************

     FWMCYP     uf   e             disk    rename(pcy01:pcy0p)

     D syRunSQL        PR                  ExtPgm('SYRUNSQL')
     D  stmt                        550    const

     D/copy sPrString

     D*entry
     D CVTWMCYP        PR                  ExtPgm('CVTWMCYP')

     D CVTWMCYP        PI

      /Free
       Dou %eof(WMCYP);
          Read WMCYP;
          If not %eof(WMCYP);
             CYCTY = TitleCase(CYCTY);
             Update pcy0p;
          Endif;
       Enddo;

       syRunSQL('select CYCTY from WMCYP');

       *inlr = *on;
       Return;
      /End-free

