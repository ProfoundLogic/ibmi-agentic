     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
    H*********************************************************************
     H/title WTDEFAULT - Under Construction
    H*********************************************************************
     FSTDEFAULT CF   E             WORKSTN Handler('PROFOUNDUI(HANDLER)')
     D*entry
     DSTDefaultEntry   pr                  ExtPgm('STDEFAULT')
     D @@rrn                          9
     D @@mode                         1
     D @@call                         1
     DSTDefaultEntry   pi
     D @@rrn                          9
     D @@mode                         1
     D @@call                         1
      /Free
       Exfmt RcdError;
       *inLr = *On;
       Return;
      /End-Free
