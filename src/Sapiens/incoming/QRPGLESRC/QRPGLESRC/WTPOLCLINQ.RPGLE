     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
    H*********************************************************************
     H/title WTPOLCLINQ - Claim Inquiry
    H*********************************************************************
     FSTDEFAULT CF   E             WORKSTN Handler('PROFOUNDUI(HANDLER)')
     D*entry
     DSTDefaultEntry   pr                  ExtPgm('STDEFAULT')
     D @@rrn                          9
     D @@mode                         1
     D @@next                        10
     DSTDefaultEntry   pi
     D @@rrn                          9
     D @@mode                         1
     D @@next                        10
      /Free
       Exfmt RcdError;
       @@next = 'WTPOLDTL';
       *inLr = *On;
       Return;
      /End-Free
