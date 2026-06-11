     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/Title W0875RW - Work With EDI History Log Wrapper
     H*****************************************************************
     H* Created: 10/03/12  C:190   Project: WC2116                    *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     F* Files Used

     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9
     D xxrrn           s              9  0

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D W0042R          pr                  extPgm('W0042R')
     D @@kc                                like(@@kc)
     D @@kl                                like(@@kl)
     D @@call                              like(@@call)
     D @@rrn                               like(xxrrn)

    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn

      /free

       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       W0042R(@@kc:@@kl:@@call:xxRrn);

       Return;

      /end-free
