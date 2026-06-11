     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/Title W0875RW - Work With EDI History Log Wrapper
     H*****************************************************************
     H* Created: 09/27/12  C:279   Project: WC2115                    *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     F* Files Used

     D @@co#           s              3
     D @@fnd           s              3
     D @@fyr           s              3
     D @@case          s              7
     D @@rrn           s              9
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D xxco#           s              3  0
     D xxfnd           s              3  0
     D xxfyr           s              3  0
     D xxcase          s              7  0
     D xxrrn           s              9  0

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D w0875r          pr                  extPgm('W0875R')
     D @@kc                                like(@@kc)
     D @@kl                                like(@@kl)
     D @@call                              like(@@call)
     D @@mode                              like(@@mode)
     D xxco#                               like(xxco#)
     D xxfnd                               like(xxfnd)
     D xxfyr                               like(xxfyr)
     D xxcase                              like(xxcase)
     D xxrrn                               like(xxrrn)

    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@call
     C                   parm                    @@mode
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@rrn

      /free

       clear xxco#;
       clear xxfnd;
       clear xxfyr;
       clear xxrrn;
       clear xxcase;

       If @@co# <> ' ';
          xxco# = %int(@@co#);
       Endif;

       If @@fnd <> ' ';
          xxfnd = %int(@@fnd);
       Endif;

       If @@fyr <> ' ';
          xxfyr = %int(@@fyr);
       Endif;

       If @@case <> ' ';
          xxcase = %int(@@case);
       Endif;

       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       w0875r(@@kc:@@kl:@@call:@@mode:xxco#:xxfnd:xxfyr:xxcase:xxRrn);

       Return;

      /end-free
