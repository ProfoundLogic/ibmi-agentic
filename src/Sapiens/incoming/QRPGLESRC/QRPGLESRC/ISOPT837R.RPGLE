     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/Define CONVERT
      //****************************************************************
      //  ISOPT837R - Build OPTIMPT from OPTDOCLST
      //****************************************************************
      //                                                               *
      //  Called from: ISOPTDS837                                      *
      //  Calls to   : None                                            *
      //                                                               *
      //****************************************************************
      //  Created on 01/23/2018 by C:079       Project: 114MJB         *
      //****************************************************************
      //  Revisions:                                                   *
      //                                                               *
      //****************************************************************
      //  Indicators used:                                             *
      //                                                               *
      //****************************************************************
      //
      // Document Listing File
      //
     foptdoclst uf   e             disk
      //
      // Output File OPTIMPT
      //
     foptimpt   o    e           k disk
      //================================================================
      // Standalone fields
      //================================================================
      //  Plist definitions
      //================================================================
     c     *entry        Plist
     c                   Parm                    @@DocType         8
      //================================================================
      //  Klist definitions
      //================================================================
      //
      //================================================================
      //  *Like DEFN statements
      //================================================================
       //
       // Execute Main Subroutine
       //
      /Free
       Read pdo01;
       Dow Not %Eof(optdoclst);
          If %Subst(docdata:128:5) = '*STMF';
             OPdcId = %Subst(docdata:76:50);
             OPdocT = @@Doctype;
             Write pop01;
          Else;
             Delete pdo01;
          Endif;
          Read pdo01;
       Enddo;
       //
       *inlr = *on;
       Return;
      /End-Free
