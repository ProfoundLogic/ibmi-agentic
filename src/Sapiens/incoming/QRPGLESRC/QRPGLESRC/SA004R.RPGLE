     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H***************************************************************
     H*  SA004R - This program accepts an 80 character field as an  *
     H*           entry parm.  It then right justifies the string   *
     H*           and passes it back.                               *
     H*                                                             *
     H*  Created: 10/13/99  C:87                                    *
     H*                                                             *
     H*  Revisions:                                                 *
     H*                                                             *
     H***************************************************************

     D @@string        s             80
     D dummy           s                   LIKE(@@string)
     D position        s              3  0
     D length          c                   CONST(%size(@@string))

     C     *entry        plist
     C                   parm                    @@string

     C     ' '           checkr    @@string      position                 60

     C                   if         *in60 = *on
     C                   eval      %subst(dummy : (length - position)+1)
     C                                = @@string
     C                   eval      @@string = dummy
     C                   endif

     C                   seton                                        lr
     C                   return
