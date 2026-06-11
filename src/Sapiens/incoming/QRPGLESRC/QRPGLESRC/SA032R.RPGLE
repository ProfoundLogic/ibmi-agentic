     H* (C) Copyright Fiserv Insurance Solutions, 2006
     H***************************************************************
     H*  Created:                                                   *
     H*             01/05/05  C:02     Pjct: Vdarts                *
     H*  Revisions:                                                 *
     H*                                                             *
     H*                                                             *
     H***************************************************************
     Fsdrpf     if   e             disk
     F
     Ddrrtn            s              1
      *
     C     *entry        plist
     C                   parm                    dral04
     C                   parm                    drrtn
     **
     C                   read      sdrpf                                  99
     C                   if        *in99 = *on                                   (notfound)
     c                   movel     '1'           drrtn
     c                   else
     c                   movel     *blanks       drrtn
     C                   endif
     C
     C                   seton                                        lr
