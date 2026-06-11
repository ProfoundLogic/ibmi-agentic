     Hcopyright('(c) Copyright Fiserv, Inc., 2004 - 2007')
     H*****************************************************************
     h/title WCTEMP  - convert policy number                          *
     H*****************************************************************
     H*                                                               *
     H* 05/21/04 C:132 PROJECT: 71WFC                                 *
     H*                Reformat Policy Number using Emplyer#, Group   *
     H*                and Policy Year by removing the leading zeros  *
     H*                and insert dashes between Group #, Employer #  *
     H*                and Policy Year. Then sending back reformated  *
     H*                Policy number either Left Justified or Right   *
     H*                Justified depenging on the value passed into   *
     H*                program in QQJLR Parameter                     *
     H*                                                               *
      *
     d qqemp#          s              9
     d qqfnd           s              3
     d qqfyr           s              3
     d qqjlr           s              1
     d qqpol#          s             17
     d z               s              3  0
     d y               s              3  0
     d zaemp#          s              9
     d zafnd           s              3
     d zafyr           s              3
     d zpemp#          s              9
     d zpfnd           s              3
     d zpfyr           s              3
     d zzplcy          s             17    inz
     d zrplcy          s             17    inz
      *
     c     *entry        plist
     c                   parm                    qqemp#                         EMPLOYER #
     c                   parm                    qqfnd                          GROUP #
     c                   parm                    qqfyr                          POLICY YEAR
     c                   parm                    qqjlr                          JUSTIFY LEFT/RT
     c                   parm                    qqpol#                         Reformated Pol#
      *
      * Create the Policy number without leading zeros.
      *
     c                   move      *blanks       zzplcy
     c                   move      *blanks       zrplcy
     c                   move      qqemp#        zaemp#
     c                   move      qqfnd         zafnd
     c                   move      qqfyr         zafyr
      *
     c                   eval      z = *zeros
     c     '0'           check     zaemp#        z                      69
     c                   if        (z > 1)
     c                   eval      zpemp# =
     c                             %subst(zaemp#:z)
     c                   else
     c                   move      zaemp#        zpemp#
     c                   endif
      *
     c                   eval      z = *zeros
     c     '0'           check     zafnd         z                      69
     c                   if        (z > 1)
     c                   eval      zpfnd =
     c                             %subst(zafnd:z)
     c                   else
     c                   movel     zafnd         zpfnd
     c                   endif
      *
     c                   eval      z = *zeros
     c     '0'           check     zafyr         z                      69
     c                   if        (z > 1)
     c                   eval      zpfyr =
     c                             %subst(zafyr:z)
     c                   else
     c                   movel     zafyr         zpfyr
     c                   endif
      *
     c                   eval       zzplcy = *blanks
     c                   eval       zzplcy = %trim(zpfnd) + '-' + %trim(zpemp#)
     c                              + '-' + %trim(zpfyr)
      *
      * Now justify the new field
     c                   eval       qqpol# = *blanks
      * Left Justify
     c                   if        qqjlr = 'L' or qqjlr = ' '
     c                   eval      qqpol# = zzplcy
     c                   endif
      * Right Justify
     c                   if        qqjlr = 'R'

     c     ' '           checkr    zzplcy        y

     c                   eval       zrplcy = *blanks
     c                   eval      %subst(zrplcy:17-y+1) =
     c                             %subst(zzplcy:1:y)
     c                   eval      qqpol# = zrplcy

     c                   endif
      * Return To caller
     c                   return
     c* ============================================================ *
