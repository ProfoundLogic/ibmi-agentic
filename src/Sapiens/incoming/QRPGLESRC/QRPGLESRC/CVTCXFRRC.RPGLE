     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*bnddir('WTBNDDIR')
      *================================================================
     H* FROI Remark Code Conversion
     H*
     H* CREATED: 07/19/17   C: 053 Project: WC2499
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H*                                                               *
     H*****************************************************************
      * Client Control Master File
     fwmixp     if   e             disk    rename(pix01:pix0p)
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
     c****               kfld                    intFnd
      *
      /Free
       Setll *Start wmixp;
       DoW Not EOF;
          Read pix0p;
          If %EOF(wmixp);
             Leave;
          Endif;                   // %EOF(wmixp)

          intCo# = IXco#;

          Chain keyWmcxl pcx00;
          If %Found(wmcxl);
             If IXhcpr <> *blanks;
                CXhcpr = IXhcpr;
                Update pcx00;
             EndIf;
          EndIf;                   // %Found(Wmxcl)

       EndDo;
       *InLr = *On;
      /End-Free
