     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('STBNDDIR')
     H*=====================================================================
     H/TITLE LODRNWLTR - Load Renewal Letter
     H*=====================================================================
     H*  Created: 04/29/22    C:33   Project: WC2670
     H*=====================================================================
     H* Revisions:
     H*
     H*=====================================================================
     d psds          esds                  ExtName(szq1p)
     d date7           s              7  0
     d time6           s              6  0
      /copy sPrDate
       Exec Sql Set Option Commit = *None;
       time6 = TimeTo6(SystemTime());
       date7 = DateTo7(SystemDate());
       Exec sql
        update STSP4P
        set (S4rPath, S4rDoc, S4rLangD, S4user, S4pgm, S4dt, S4time)
         = (S4path, S4doc, S4langD, :q1User, :q1pgm, :date7, :time6);
       *inlr = *on;
