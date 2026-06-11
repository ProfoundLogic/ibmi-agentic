     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H***************************************************************
     H*  Created:                                                   *
     H*             04/17/01  C:69                                  *
     H*  Revisions:                                                 *
     H*                                                             *
     H***************************************************************
     Fslnpf     if   e             disk
     F
     Duse              s              1
     D
     C     *entry        plist
     C                   parm                    use
     C
     C     1             chain     slnpf                              99
     C                   if        *in99 = *off
     C                   movel     lnuse         use
     C                   endif
     C
     C                   seton                                        lr
