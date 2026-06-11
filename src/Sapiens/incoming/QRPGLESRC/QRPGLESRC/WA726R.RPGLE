     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*****************************************************************
    H/TITLE WA726R - API/to accept co#/fnd/"# yrs for extract
    H*****************************************************************
    H*                                                               *
    H*  Description:                                                 *
    H*    Accepts CO#, Group and number of years for extarct.
    H*    Return Begining Policy year.
    H*                                                               *
    H*  Called from:                                                 *
    H*  W8901R                                                       *
    H*  W8902R                                                       *
    H*  W8903R                                                       *
    H*  W8904R                                                       *
    H*  W8905R                                                       *
    H*  W8906R                                                       *
    H*  W8907R                                                       *
    H*  W8915R2                                                      *
    H*  W8914R                                                       *
    H*                                                               *
    H*  Calls to:                                                    *
    H*    None                                                       *
    H*                                                               *
    H*****************************************************************
    H*  Revisions:                                                   *
    H*                                                               *
    H*  11/25/10  C:220 WC1856                                       *
    H*                  Created.                                     *
    H*                                                               *
    H*****************************************************************

    F*---------------------------------------------------------------------
    F* Files
    F*---------------------------------------------------------------------
     F***** co#/fnd/stpo
     FWDF1L1    if   e           k Disk
    F*---------------------------------------------------------------------
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D*
     D @@co#           s              3
     D @@fnd           s              3
     D @@yr#           s              3
     D @@gfyr          s              3
     D @@rtcd          s              1
     D @@date          s              8  0
     D @@months        s              3  0
     D @@err           s              1
     D*
     D xxco#           s              3  0
     D xxfnd           s              3  0
     D xxyr#           s              3  0
     D xxgfyr          s              3  0
     D xxstpo          s              7  0
     D xxmth           s              5  0
     D xxpdt           s              7  0
     D*
     D srdte           s              8  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     c*
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c*
     c     f1ky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxstpo
     c*
     c     f1ky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c*
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c*
     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@fnd
     c                   parm                    @@yr#
     c                   parm                    @@gfyr
     c                   parm                    @@rtcd
    c*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c****** Initialzied the work variables from entry parameters *****
     c*
     c                   movel     @@co#         xxco#
     c                   movel     @@fnd         xxfnd
     c                   movel     @@yr#         xxyr#
     c*
     c                   TIME                    DSDTTM
     c
     c*
    c*================================================================
    c*  Normal Programming Area
    c*================================================================
    c*
     c                   eval      @@rtcd='N'
    c*
     c                   if        xxyr# = 0
     c     f1ky1         chain     pf101                              60
    c                   if        *in60 = *off
    c                   movel     f1fyr         xxgfyr
     c                   eval      @@rtcd = 'Y'
     c*
    c                   endif
    c                   else
     c*
    c* Find past date for starting policy year
     c*
    c                   eval      xxmth =  xxyr# * 12
     c
     c                   movel     dsdate        @@date
     c*
     c                   call      'SRPMTH8'
     c                   parm                    @@date
     c                   parm      xxmth         @@months
     c                   parm                    @@err
     c*
     c                   if        @@err = '0'
     c*
     c                   z-add     @@date        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxpdt
     c*
     c* Initialized the policy start date sub key.
     c*
     c                   eval      xxstpo = xxpdt
     c                   endif                                                   <-----+ : :
     c*
     c* Findout the oldest policy year form fund details file
     c*
     c     f1ky          setgt     pf101
     c     f1ky1         readpe    pf101                                  60
     c*
     c                   if        *in60 = *off
    c                   movel     f1fyr         xxgfyr
     c                   eval      @@rtcd = 'Y'
     c*
     c                   else                                                    <-----+ : :
     c*
     c     f1ky          setll     pf101
     c     f1ky1         reade     pf101                                  60
     c*
     c                   if        *in60 = *off
    c                   movel     f1fyr         xxgfyr
     c                   eval      @@rtcd = 'Y'
     c*
     c                   else
     c                   eval      @@rtcd = 'N'
     c                   endif
     c*
     c                   endif                                                   <-----+ : :
     c                   endif                                                   <-----+ : :
    c*
    c                   movel     xxgfyr        @@gfyr
    c*
     c                   seton                                        lr
    
