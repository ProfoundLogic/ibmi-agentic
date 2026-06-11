     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h dftactgrp(*no) actgrp(*caller)
     H option(*nodebugio:*srcstmt)
    H*****************************************************************
    H/TITLE W8250R - Claim Import Driver Program
    H*****************************************************************
    H*                                                               *
    H* CALLED FROM: W8250C - Claim Import Listing Build              *
    H* CALLS TO:    W8251C - Claim Import Preparation                *
    H*                                                               *
    H*****************************************************************
    H* Created on 03/14/2017 by c:62 for Project 114WCT              *
    H*****************************************************************
    H* Revisions:                                                    *
    H*                                                               *
    H*****************************************************************
     * Document Listing
     fdoclist   if   e           k disk    Rename(pdl01:pdl0p)
    D*=====================================================================
    D* Prototypes
    D*=====================================================================
     d* Check Directory Contents
     d ChkDirContents  pr                  ExtPgm('RMVEMPDRC2')
     d   inPath                     100
    D*=====================================================================
    D* Stand Alone Fields
    D*=====================================================================
     D @@path          s            100
     d inPath          s            100
     *================================================================
     *   Entry Parameter
     *================================================================
     C     *entry        plist
     C                   parm                    @@path
     C
      /Free

       // Read Thru the Document Listing
       Read pdl0p;
       DoW Not %Eof(doclist);
          // Only process documents not directories
          If %Subst(DLitem:1:12) = '*DIR' And
             %Subst(DLitem:13:100) <> '.' And
             %Subst(DLitem:13:100) <> '..';
             // Call the program to test directory
             inPath  = %Trim(@@path) + '/' + %Trim(%Subst(DLitem:13:100));
             ChkDirContents(inPath);
          EndIf;
          Read pdl0p;
       EndDo;

       *inlr=*on;
       Return;
      /End-Free
