     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('STBNDDIR')
     H***************************************************************
     H*  Created:                                                   *
     H*             07/31/00  C:69                                  *
     H*  Revisions:                                                 *
     H*                                                               *
     H*  04/10/18  C:33  Account for PDSQA                            *
     H*                                                               *
     H*  01/21/13  C:287 SEQ#: SY266_OVR                              *
     H*                  Add default activation group compiler options*
     H*                                                             *
     H*  05/18/04 c:62 Project SY100                                *
     H*                Add parm to indicate if this is a Data Center*
     H*                call.  If so then return the folder prefix.  *
     H*                                                             *
     H***************************************************************
     Fssypf     if   e             disk
     F
     Dpds_folder       c                   'PDS/'
     Dfolder_prefix    s             80
     Dsuffix           s              3
     DtheSuffix        ds             3
     D syfl08                         1
     D syfl09                         1
     D syfl10                         1
     D*
     D***  Begin Add  ***  04/10/18  ************************************
     DisIncludePrefix  s              1n   inz(*off)
     D/copy sPrPrprty
     D***  End   Add  ***  04/10/18  ************************************

     C     *entry        plist
     C                   parm                    suffix
     C                   parm                    folder_prefix
     C*
     C
     C                   read      ssypf                                  99
     C                   if        *in99 = *off
     C                   movel     theSuffix     suffix
     C***  Begin Add  ***  04/10/18  ************************************
      /Free
       isIncludePrefix = %subst(syfl11:2:1) = 'Y';
       If SysProperty('InstanceEnvironment') = 'PDSQA';
          suffix = '099';
          isIncludePrefix = *on;
       Endif;
      /End-free
     C***  End   Add  ***  04/10/18  ************************************
     C***  Begin Chg  ***  04/10/18  ************************************
     C**********         if        %parms > 1 and %subst(syfl11:2:1) = 'Y'
     C                   if        %parms > 1 and isIncludePrefix
     C***  End   Chg  ***  04/10/18  ************************************
     C                   if        suffix <> *blanks
     C                   eval      folder_prefix = pds_folder + suffix
     C                   endif
     C                   endif
     C                   endif
     C
     C                   seton                                        lr
