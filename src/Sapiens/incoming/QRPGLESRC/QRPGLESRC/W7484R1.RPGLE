     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H/title Import Response File prompt
     H*====================================================================
     H*  Created:07/16/09  C:198  Project: WC1819 Phase 3
     H*  Called by: W7484C
     H*  Calls to : None
     H*====================================================================
     H*  Revisions:
     H*
     H*  11/19/10   C:198  Project: WC1879 Priss 76/94/96
     H*                    Added 2 fields --> Policy Number and claim
     H*                    number.
     H*
     H*  10/22/10   C:198  Project: WC1879 Priss 68
     H*                    Fixed the issue where applied DOB, applied gender
     H*                    applied MSP effective/expiration date.
     H*
     H*====================================================================
     F*** Beg Chg *** 10/22/10 ****************************************
     f***wxr0p     if   e             disk    rename(wxr0p:pr001)
     fwxr0p     uf   e             disk    rename(wxr0p:pr001)
     F*** End Chg *** 10/22/10 ****************************************
     f                                     prefix(r0:2)
     fwdr2p     if a e             disk
     D*====================================================================
    D* Stand Alone Fields
     D*====================================================================
     D@@exdt           s              8
     D@@co#            s              3
     D*** Beg Add *** 10/22/10 ********************************************
     Dxxchg            s              1    inz('N')
     D*** End Add *** 10/22/10 ********************************************
     D*====================================================================
     D* Constants
     D*====================================================================
     D*
     D*====================================================================
     D* Data Structures
     D*====================================================================
     D*
     dr2data           ds
     dr2rin                    1      4
     dr2dcn#                   5     19
     dr2type                  20     20  0
     dr2hic#                  21     32
     dr2ss#                   33     41  0
     dr2lsn                   42     81
     dr2fsn                   82    111
     dr2mid                  112    112
     dr2sex                  113    113  0
     dr2dtob                 114    121  0
     dr2plaa                 122    130  0
     dr2off#                 131    139
     d*** Beg Add *** 11/19/10 *******************************************
     dr2cap#                 140    169
     dr2clm#                 170    199
     d*** End Add *** 11/19/10 *******************************************
     d*** Beg Chg *** 11/19/10 ***** moved 60 positions down *************
     dr2fil1                 200    219
     dr2ihic#                220    231
     d*** changed r2iss# into a  9A fields as per CMS 3.1 updates
     dr2iss#                 232    240
     dr2ilsn                 241    280
     dr2ifsn                 281    310
     dr2imid                 311    311
     dr2isex                 312    312  0
     dr2idtob                313    320  0
     dr2mefd                 321    328  0
     dr2mexd                 329    336  0
     dr2pity                 337    337
     dr2fil2                 338    357
     dr2code                 358    359
     dr2ecd1                 360    364
     dr2ecd2                 365    369
     dr2ecd3                 370    374
     dr2ecd4                 375    379
     dr2ecd5                 380    384
     dr2ecd6                 385    389
     dr2ecd7                 390    394
     dr2ecd8                 395    399
     dr2ecd9                 400    404
     dr2ecd10                405    409
     dr2ccd1                 410    411
     dr2ccd2                 412    413
     dr2ccd3                 414    415
     dr2ccd4                 416    417
     dr2ccd5                 418    419
     dr2ccd6                 420    421
     dr2ccd7                 422    423
     dr2ccd8                 424    425
     dr2ccd9                 426    427
     dr2ccd10                428    429
     dr2fil3                 430    460
     d*** End Chg *** 11/19/10 *******************************************
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@exdt
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
    C*================================================================
    C*  Mainline
    C*================================================================
     C*
     C*** Beg Add *** 10/22/10 ****************************************
     C                   read      pr001                                  80
     C                   dow       *in80 = *off
     C*** Beg Chg *** 11/19/10 ****************************************
     C*****              if        %subst(r0r0p:252:1) = ' '
     C*****              eval      %subst(r0r0p:252:1) = '0'
     C                   if        %subst(r0r0p:312:1) = ' '
     C                   eval      %subst(r0r0p:312:1) = '0'
     C*** End Chg *** 11/19/10 ****************************************
     C                   eval      xxchg = 'Y'
     C                   endif
     C*** Beg Chg *** 11/19/10 ****************************************
     C*****              if        %subst(r0r0p:253:8) = ' '
     C*****              eval      %subst(r0r0p:253:8) = '00000000'
     C                   if        %subst(r0r0p:313:8) = ' '
     C                   eval      %subst(r0r0p:313:8) = '00000000'
     C*** End Chg *** 11/19/10 ****************************************
     C                   eval      xxchg = 'Y'
     C                   endif
     C*** Beg Chg *** 11/19/10 ****************************************
     C*****              if        %subst(r0r0p:261:8) = ' '
     C*****              eval      %subst(r0r0p:261:8) = '00000000'
     C                   if        %subst(r0r0p:321:8) = ' '
     C                   eval      %subst(r0r0p:321:8) = '00000000'
     C*** End Chg *** 11/19/10 ****************************************
     C                   eval      xxchg = 'Y'
     C                   endif
     C*** Beg Chg *** 11/19/10 ****************************************
     C****               if        %subst(r0r0p:269:8) = ' '
     C****               eval      %subst(r0r0p:269:8) = '00000000'
     C                   if        %subst(r0r0p:329:8) = ' '
     C                   eval      %subst(r0r0p:329:8) = '00000000'
     C*** End Chg *** 11/19/10 ****************************************
     C                   eval      xxchg = 'Y'
     C                   endif
     C                   if        xxchg = 'Y'
     C                   update    pr001
     C                   eval      xxchg = 'N'
     C                   endif
     C                   read      pr001                                  80
     C                   enddo
     C     1             setll     pr001
     C*** End Add *** 10/22/10 ****************************************
     c                   read      pr001                                  90
     C*
     c                   dow       *in90 = *off
     C*
     c                   select
     c                   when      %subst(r0r0p:1:4) = 'NGRH'
     c                   eval      @@exdt = %subst(r0r0p:21:8)
     c                   when      %subst(r0r0p:1:4) = 'NGRD'
     c                   eval      r2data = r0r0p
     C                   write     pr201
     c                   endsl
     C*
     c                   read      pr001                                  90
     C*
     c                   enddo
     C*
     c                   Return
