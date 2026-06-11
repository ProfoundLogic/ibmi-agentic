     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/title Print Error log
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H* ============================================================= *
     H*BLDERRPT  - PRINT ERROR LOG OF BUILDING MC data                *
     H*                                                               *
     H*  DESCRIPTION:  This program Prints a listing of a file error  *
     H*                during the BLDFSDATC program                   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    BLDFSDATC                                                  *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    BLDFSDATA                                                  *
     H*    BLDERRPT                                                   *
     H*                                                               *
     H*  CREATED: 08/24/10  PROGRAMMER: C:220  PROJECT: FS324         *
     H*                                                               *
     H* ============================================================= *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* ============================================================= *
     f*  AUDIT REPORT
     fqsysprt   o    f  132        printer oflind(*inob)
     ffxcpp     if   e             disk
     f*
     dfromLib          s             10
     dFromFile         s             10
     dxxMsgText        s            105
     dcount            s              7  0
     d*
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
      *
     d* Constants
     d*
     d ErrUnkn         c                   const('Error-Unknown type, must chec- " "
     d                                     k and if possible - Please CORRECT')
      *  ============================================================= *
      *
     DcontactInfo      s             80    inz
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
      *
     c     *entry        plist
     c                   parm                    FromLib
     c
     c                   time                    dsdttm
     c
     c*
     c                   Except    Head
     c*
     c                   if        *inob = *on
     c                   eval      *inob = *off
     c                   Except    Head
     c                   endif
     c
     c                   callp     RtvBrnd('N':brandName)
     c                   eval      contactInfo = 'Contact '
     c                                         + %trim(brandName)
     c                                         + ' support help desk'
     c                                         + ' for instructions.'
     c
     c                   eval      count = 1
     c                   read      pcp01                                  60
     c                   dow       *in60 = *off
     c
      *  Error on File Save before Copies
     c                   movel(P)  cpfile        FromFile
     c                   movel(P)  cperrtx       xxMsgText
     c                   Except    det
     c
     C*  Overflow for next page
     c                   if        *inob = *on
     c                   eval      *inob = *off
     c                   Except    Head
     c                   endif
     c
     c                   read      pcp01                                  60
     c
     c*  Increment counter if error file is not empty
     c                   if        *in60 = *off
     c                   eval      count = count + 1
     c                   endif
     c
     c                   enddo
     c
     c                   Except    dets
     c                   eval      *inlr = *on
     c*
     c
     c                   return
     c
     oqsysprt   e            head           1 06
     o                       dstime               8 '  :  :  '
     o                                           44 'Copy V5R2PNDTA data in'
     o                                           69 'recent ALDON environment'
     o                       dsdate              98 '  /  /    '
     o          e            head           1
     o                       q1pgm               10
     o                                           93 'PAGE'
     o                       page1         3     98
     o          e            head           1
     o                       q1user              10
     o          e            head           1
     o                                           77 'ERROR REPORT ON COPY V5R2P-
     o                                              NDTA FILES TO RECENT ALDON -
     o                                              ENVIRONMENT'
     o          e            head           2
     o                                           45 '   '
     o          e            head           1
     o                                            8 'From Lib'
     o                                           22 'File Name'
     o                                           52 'Error Description for file'
     o          e            head           2
     o                                            8 '--------'
     o                                           22 '---------'
     o                                           52 '--------------------------'
      *
     o          e            det            4
     o                       FromLib             10
     o                       FromFile            23
     o                       xxMsgText          131
     o          e            dets        1
     o                                           51 'Total files not copied du-
     o                                              e to errors :'
     o                       count               59
      *
