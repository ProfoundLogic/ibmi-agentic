     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/Title W0614RW - Work with Account Current A/R by Policy Wrapper
     H*****************************************************************
     H* Created: 10/28/15  C:300   Support: 37942                     *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     F* Files Used

     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@fnd           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fyr           s              3
     D ##fnd           s              3  0
     D ##emp#          s              9  0
     D ##div           s              5  0
     D ##fyr           s              3  0

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D W0614R          pr                  extPgm('W0614R')
     D @@call                         1
     D @@kc                           1
     D @@kl                           1
     D ##fnd                          3  0
     D ##emp#                         9  0
     D ##div                          5  0
     D ##fyr                          3  0

    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr

      /free

       If @@fnd <> ' ';
          ##fnd = %int(@@fnd);
       Endif;

       If @@emp# <> ' ';
          ##emp# = %int(@@emp#);
       Endif;

       If @@div <> ' ';
          ##div = %int(@@div);
       Endif;

       If @@fyr <> ' ';
          ##fyr = %int(@@fyr);
       Endif;

       W0614R(@@call:@@kc:@@kl:##fnd:##emp#:##div:##fyr);

       Return;

      /end-free
