     H* (C) Copyright Progressive Data Solutions, 1998
     H***************************************************************
     H*  Created:                                                   *
     H*             04/28/98  C:69                                  *
     H*  Revisions:                                                 *
     H*                                                             *
     H***************************************************************
     Fssypf     uf   e             disk
     F
     C                   read      ssypf                                  99
     C                   if        *in99 = *off
     C                   movel     'Y'           syscif
     C                   movel     'Y'           syofif
     C                   movel     'Y'           syfxif
     C                   update    ssy01
     C                   endif
     C
     C                   seton                                        lr
