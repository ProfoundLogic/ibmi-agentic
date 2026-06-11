     H* (C) Copyright Progressive Data Solutions, 2004
     H*
     H* SA038R
     H***************************************************************
     H* Return "Y" if Experience Module is installed
     H***************************************************************
     H* REVISIONS:
     H*
     H*
     H* 06/16/04  C:61  PROJECT WC1173
     H*                 CREATED
     H***************************************************************


     Dexpmod           s              1


     C     *entry        plist
     C                   parm                    expmod

     c                   eval      expmod = *blank

     c                   call      'srchkmbr'
     c                   parm      'EA001R'      objname          10
     c                   parm      '*PGM'        objtype           7
     c                   parm      *blanks       objmbr           10
     c                   parm      *blank        err               1

     c                   if        err = '0'
     c                   eval      expmod = 'Y'
     c                   endif

     C                   seton                                        lr

