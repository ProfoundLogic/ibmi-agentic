     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE F8069R  - License Panel Rate Code - Prompt Program
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
    H*****************************************************************
    H*    Program: F8069R                                            *
    H*    Created: 04/30/2013 PROJ: 147MAS   C:284                   *
    H*    Description: License Panel Rate Code                       *
    H*                                                               *
    H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FF8069F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     d* License Panel Rate Code Sumbitter
     d SubLicRtCde     pr                  Extpgm('F8069C')

      /Free
       Cancel = *off;
       Enter = *off;
       dow Cancel = *off and Enter = *off;

          Exfmt rcdRtcde;
          Select;
             when Enter = *on;
                SubLicRtCde();

             when Cancel = *on;
                *Inlr = *on;

             other;
                error = *on;
          endsl;

       enddo;
       *Inlr = *on;
      /End-Free
