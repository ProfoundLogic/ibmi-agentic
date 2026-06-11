     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*bnddir('WTBNDDIR')
      *================================================================
     H* Convert WMCXP
     H*
     H* CREATED: 11/25/2013 C: 282 Project: 129WSC
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H*****************************************************************
      * Client Control Master File
     fsxcpf     if   e           k disk    rename(scp01:scp0p)
      * Client Control Master File
     fwmcxl     uf   e           k disk    rename(pcx01:pcx00)
      *
     d EOF             s               n   inz( *OFF )
     d intCO#          s              3  0
     d intFnd          s              3  0
      *
      * Key List on Client Control Master File
     c     keyWmcxl      klist
     c                   kfld                    intCO#
     c                   kfld                    intFnd
      *
      /Free
       Setll *Start sxcpf;
       DoW Not EOF;
          Read scp0p;
          If %EOF(sxcpf);
             Leave;
          Endif;                   // %EOF(sxcpf)

          intCo# = XCco#;
          intFnd = XCfnd;

          Chain keyWmcxl pcx00;
          If %Found(wmcxl);
             CXaprm = XCrm04;
             CXlgrh = XC10A2;
             Update pcx00;
          EndIf;                   // %Found(Wmxcl)

       EndDo;
       *InLr = *On;
      /End-Free
