     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*====================================================================
     H/Title CMS Log History Wrapper
     H*====================================================================
     H* Created: 09/27/12  C:279   Project: WC2115
     H*====================================================================
     H* Revisions:
     H*
     H*====================================================================
     F* Files used

     D @@rrn           s              9
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D xxrrn           s              9  0

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D w7485r          pr                  extPgm('W7485R')
     D @@kc                                like(@@kc)
     D @@kl                                like(@@kl)
     D @@call                              like(@@call)
     D xxrrn                               like(xxrrn)

    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@call
     C                   parm                    @@rrn

      /free

       clear xxrrn;
       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       w7485r(@@kc:@@kl:@@call:xxRrn);

       Return;

      /end-free
