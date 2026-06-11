     H* (C) Copyright Progressive Data Solutions, 2004
     H***************************************************************
     H* Given:
     H*   Folder name.  Example WCPOLSV3
     H*
     H* Return:
     H*   If at data center:  Folder name with prefix.
     H*             Example PDS/132/WCPOLSV3
     H*
     H*   Otherwise: Return Folder name unchanged.
     H*             Example WCPOLSV3
     H*
     H***************************************************************
     H*  Revisions:
     H*
     H*
     H*  created 07/08/04  C:61  SY100
     H***************************************************************
      *
     Fssypf     if   e             disk


     Dfolder           s             80
     Dfolder_save      s             80
     Dsuffix           s              3

     DtheSuffix        ds             3
     D syfl08                         1
     D syfl09                         1
     D syfl10                         1


     C     *entry        plist
     C                   parm                    folder

     c                   movel     folder        xxpds             4

     C                   if        xxpds <> 'PDS/'

     C                   eval      folder_save = folder
     C                   read      ssypf                                  99

     C                   if        *in99 = *off
     C                   movel     theSuffix     suffix

     C                   if        %parms > 0 and %subst(syfl11:2:1) = 'Y'

     C                   if        suffix <> *blanks
     C                   eval      folder = 'PDS/'
     c                                      + suffix
     c                                      + '/'
     c                                      + folder_save
     C                   endif

     C                   endif

     C                   endif

     C                   endif

     C                   seton                                        lr

