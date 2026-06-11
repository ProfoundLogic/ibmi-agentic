     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE CVTTERA - Program to include terrorism charges in assessment *
    H*********************************************************************
    H*  DESCRIPTION:  This program will update existing terrrism flag    *
    H*                for once and update the new flags with the corresp *
    H*                field values                                       *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    WCPOSTCV                                                       *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  09/15/11 C:136 WA114 77974                                       *
    H*                 Don't update RRTERA if 'Y' for backwards          *
    H*                 compatibility. Post load pgms shouldn't           *
    H*                 update maint fields.                              *
    H*                                                                   *
    H*  CREATED: 06/24/10  PROGRAMMER: C:227  Project: WC1873            *
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*********************************************************************
     Fwdrrp     uf   e             disk
    D*
     D psds          esds                  extname(szq1p)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYDTPL
     C*
     C* Main program
     C*
     C                   time                    dsdttm
     C*
     C* Update the terrorism flags
     C*
     C                   read      prr01                                  99
     C                   dow       *in99 = *off

     C                   if        rrtera = 'X'
     C                   leave
     C                   else
     C                   select
     C*
     C                   when      rrtera = 'Y'
     C*
     C                   eval      rr1atr = 'B'
     C                   eval      rr2atr = 'B'
     C                   eval      rr3atr = 'B'
     C                   eval      rr4atr = 'B'
     C                   eval      rr5atr = 'B'
     C                   eval      rr6atr = 'B'
     C                   eval      rr7atr = 'B'
     C*
     C                   when      rrtera = ' '
     C*
     C                   eval      rr1atr = ' '
     C                   eval      rr2atr = ' '
     C                   eval      rr3atr = ' '
     C                   eval      rr4atr = ' '
     C                   eval      rr5atr = ' '
     C                   eval      rr6atr = ' '
     C                   eval      rr7atr = ' '
      *** beg add ***** 09/15/11 ************************************
     C                   eval      rrtera = 'X'
      *** end add ***** 09/15/11 ************************************
     C*
     C                   endsl
     C*
      *** beg del ***** 09/15/11 ************************************
     C***********        eval      rrtera = 'X'
     C***********        movel(p)  q1user        rruser
     C***********        movel(p)  q1pgm         rrpgm
     C***********        z-add     dsdate        srdti
     C***********        call      'SRCVTDAT'    dtcymd
     C***********        movel     srdto         rrdt
     C***********        move      dstime        rrtime
      *** end del ***** 09/15/11 ************************************
     C*
     C                   update    prr01
     C*
     C                   endif                                                  rrtera <> 'X'
     C*
     C                   read      prr01                                  99
     C*
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*
