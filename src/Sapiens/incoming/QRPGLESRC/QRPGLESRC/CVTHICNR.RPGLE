     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
    H*
     H/TITLE CVTHICNR - Move HICN# from WMCMP to WMCDP
    H*
     H*****************************************************************
    H* Description:                                                  *
     H* This program will be used to move HICN# from claims master    *
     H* to claims demographic file.                                   *
    H*                                                               *
    H*  Called from: command line                                    *
    H*                                                               *
    H*  Calls to: None.                                              *
     H*****************************************************************
     H*  Created by:                                                  *
     H*                                                               *
     H*  12/13/10  C:198 Project: WC1963                              *
     H*                                                               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     Fwmcdl     uf   e           k disk
     F                                     infds(cdds)
     Fwmcmla    uf   e           k disk
     F                                     infds(cmds)
     Fqsysprt   o    f  132        printer oflind(*inof)
    D*================================================================
    D*  Standalone fields
    D*================================================================
     D rcd_found       S               n   inz
     D xxnewhic        S               n   inz
     D xxcount         S             10  0 inz
     D xxname          S              5A   inz
     D xxssn           S                   like(cdss#)
     D xxrrn           S              9  0
     D xxhica          S                   like(cmhic#)
     D xxhicb          S                   like(cmhic#)
     D x1hica          S                   like(cmhic#)
     D x1hicb          S                   like(cmhic#)
    D*================================================================
    D*  Constants
    D*================================================================
     Dxxtitl           C                   'HICN CONVERSION PGM CVTHICNR'
    D*================================================================
    D*  Data Structures
    D*================================================================
     D cdds            ds
     D  cdrrn                397    400b 0
     D cmds            ds
     D  cmrrn                397    400b 0
     Dxxdcn            DS
     Dxxco#                    1      3
     Dxxhyp1                   4      4    inz('-')
     Dxxfnd                    5      7
     Dxxhyp2                   8      8    inz('-')
     Dxxfyr                    9     11
     Dxxhyp3                  12     12    inz('-')
     Dxxcase                  13     19
    C*================================================================
    C*  Main Processing
    C*================================================================
     C                   except    Header
     C                   read      pcd01                                98
     C                   dow       *in98 = *off
     C     cdss#         setll     pcm01
     C     cdss#         reade     pcm01                                  99
     C                   eval      xxcount = 1
     C                   dow       *in99 = *off
     C*
     C                   if        cmhic# <> ' '
     C*
     C   of              except    Header
     C*
     C                   if        *inof = *on
     C                   eval      *inof = *off
     C                   endif
     C*
     C                   eval      rcd_found = *on
     C*
     C                   eval      xxnewhic = *on
     C*
     C                   if        xxcount = 1
     C                   movel     'WMCMP'       xxname
     C                   movel     cmss#         xxssn
     C                   else
     C                   movel     *blanks       xxssn
     C                   movel     *blanks       xxname
     C                   endif
     C                   move      cmco#         xxco#
     C                   move      cmfnd         xxfnd
     C                   move      cmfyr         xxfyr
     C                   move      cmcase        xxcase
     C*
     C                   z-add     cmrrn         xxrrn
     C*
     C                   eval      x1hicb = ' '
     C                   movel     cdhicn        x1hicb
     C*
     C                   eval      xxhicb = ' '
     C                   movel     cmhic#        xxhicb
     C*
     C                   movel     cmhic#        cdhicn
     C*
     C                   eval      cmhic# = ' '
     C*
     C                   eval      xxhica = ' '
     C                   movel     cmhic#        xxhica
     C*
     C                   eval      x1hica = ' '
     C                   movel     cdhicn        x1hica
     C*
     C                   except    prtdata
     C*
     C                   update    pcm01
     C                   if        xxcount = 1
     C                   update    pcd01
     C                   endif
     C                   eval      xxcount = xxcount + 1
     C*
     C                   endif                                                  cmhic# <> ' '
     C*
     C   of              except    Header
     C*
     C                   if        *inof = *on
     C                   eval      *inof = *off
     C                   endif
     C*
     C     cdss#         reade     pcm01                                  99
     C                   enddo                                                  *in99 = *off
     C*
     C                   if        xxnewhic  = *on
     C                   movel     'WMCDP'       xxname
     C                   movel     *BLANKS       xxssn
     C                   movel     *BLANKS       xxdcn
     C                   z-add     cdrrn         xxrrn
     C                   eval      xxhicb = ' '
     C                   eval      xxhica = ' '
     C                   movel     x1hicb        xxhicb
     C                   movel     x1hica        xxhica
     C*
     C                   except    prtdata
     C*
     C                   except    space#
     C                   eval      xxnewhic = *off
     C                   endif
     C*
     C                   read      pcd01                                  98
     C                   enddo                                                  *in98 = *off
     C*
     C                   if        rcd_found = *off
     C                   except    fot2
     C                   endif
     C*
     C                   except    fot1
     C*
     C                   eval      *inlr = *on
    C*================================================================
     O*** Qsysprt
     OQsysprt   e            header      1  1
     O                       xxtitl              70
     O          e            header      1  2
     O                                            5 'File'
     O                                           17 'SSN'
     O                                           50 'Claim Number'
     O                                           65 'HIC# before'
     O                                           80 'HIC# after'
     O                                          113 'RRN'
     O*
     O          e            prtdata        1
     O                       xxname               5
     O                       xxssn               17
     O                       xxdcn               50
     O                       xxhicb              65
     O                       xxhica              80
     O                       xxrrn              113
     O*
     O          e            Space#         1
     O*
     O          e            Fot1           1
     O                                           78 ' *** End  Of  Report *** '
     O*
     O          e            Fot2           1
     O                                           78 ' *** No Records Found *** '
     O*
     O*
