     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H*  Work with Additinal Claimaints Wrapper                           *
     H*********************************************************************
     H* Created: 09/27/12   C:279  Project: WC2115                        *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H*********************************************************************
     F* Files Used

     D xxrrn           s              9  0
     D @@rrn           s              9
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D w0572r          pr                  extPgm('W0572R')
     D @@call                              like(@@call)
     D xxRrn                               like(xxRrn)
     D @@kc                                like(@@kc)
     D @@kl                                like(@@kl)

    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@rrn
     C                   parm                    @@kc
     C                   parm                    @@kl
      /free

       clear xxRrn;
       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       w0572r(@@call:xxRrn:@@kc:@@kl);

       Return;

      /end-free
