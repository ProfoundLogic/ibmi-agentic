     H* (C) Copyright Progressive Data Solutions, 2002
     H***************************************************************
     H*  Created:                                                   *
     H*             04/11/01  C:69                                  *
     H*  Revisions:                                                 *
     H*                                                             *
     H***************************************************************
     Fsdrpf     if   e             disk
     F
     Duse              s              1
     D
     C     *entry        plist
     C                   parm                    use
     C
     C     1             chain     sdrpf                              99
     C                   if        *in99 = *off
     C                   movel     drfl12        use
     C                   endif
     C
     C                   seton                                        lr
