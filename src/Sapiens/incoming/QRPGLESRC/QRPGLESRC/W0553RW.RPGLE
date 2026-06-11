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
     D @@qte           s              1
     D @@mode          s              1
     D xxrrn           s              9  0

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D W0553R          pr                  extPgm('W0553R')
     D @@kc                                like(@@kc)
     D @@kl                                like(@@kl)
     D @@call                              like(@@call)
     D @@rrn                               like(xxrrn)
     D ##qte                               like(@@qte)
     D @@mode                              like(@@mode)

    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@qte
     C                   parm                    @@mode

      /free

       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       w0553r(@@kc:@@kl:@@call:xxRrn:@@Qte:@@Mode);

       Return;

      /end-free
