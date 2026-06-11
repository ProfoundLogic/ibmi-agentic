      **  Copyright Progressive Data Solutions, Inc., 2003                                         *
      **********************************************************************************************
      **     Program Description:                                                                  *
      **                                                                                           *
      **     WCMS - Convert WMRHP claim reserve buckets for PDSQuest.                              *
      **                                                                                           *
      **                                                                                           *
      **     Programmer:  C15                                                                      *
      **     Created:    02/27/2003                                                                *
      **********************************************************************************************
      **     Revisions:                                                                            *
      **                                                                                           *
      **       /  /       C:                                                                       *
      **                                                                                           *
      **********************************************************************************************
     h
     fwmfnl     if   e           k disk                                         group file
     fwmrhl     uf   e           k disk                                         rsv history file
      **********************************************************************************************
     d                 DS
     drsvcd                    1     20    Dim(10)
     drhrt01                   1      2
     drhrt02                   3      4
     drhrt03                   5      6
     drhrt04                   7      8
     drhrt05                   9     10
     drhrt06                  11     12
     drhrt07                  13     14
     drhrt08                  15     16
     drhrt09                  17     18
     drhrt10                  19     20
      *
     d                 DS
     drsvamt                   1     90  2 Dim(10)
     drhsu01                   1      9  2
     drhsu02                  10     18  2
     drhsu03                  19     27  2
     drhsu04                  28     36  2
     drhsu05                  37     45  2
     drhsu06                  46     54  2
     drhsu07                  55     63  2
     drhsu08                  64     72  2
     drhsu09                  73     81  2
     drhsu10                  82     90  2
      *
      *
     d                 DS
     dnewrsvcd                 1     20    Dim(10)
     dnewrt01                  1      2
     dnewrt02                  3      4
     dnewrt03                  5      6
     dnewrt04                  7      8
     dnewrt05                  9     10
     dnewrt06                 11     12
     dnewrt07                 13     14
     dnewrt08                 15     16
     dnewrt09                 17     18
     dnewrt10                 19     20
      *
     d                 DS
     dnewrsvamt                1     90  2 Dim(10)
     dnewsu01                  1      9  2
     dnewsu02                 10     18  2
     dnewsu03                 19     27  2
     dnewsu04                 28     36  2
     dnewsu05                 37     45  2
     dnewsu06                 46     54  2
     dnewsu07                 55     63  2
     dnewsu08                 64     72  2
     dnewsu09                 73     81  2
     dnewsu10                 82     90  2
      *
      *
     d svfnd           s              3  0
     d i               s              2  0                                      counter
     d x               s              2  0
     d inCompany#      s                   Like(RHco#)
     d inGroup#        s                   Like(RHfnd)
      *

     c     *entry        plist
     c                   parm                    inCompany#
     c                   parm                    inGroup#

      *---------------------------------------------------------------------------------------------
      * Key list for the Reserve History file
      *
     c     rhky          klist
     c                   kfld                    inCompany#
     c                   kfld                    inGroup#
      * Key list for the Group Master file
      *
     c     fnky          klist
     c                   kfld                    rhco#
     c                   kfld                    rhfnd
      *
      *---------------------------------------------------------------------------------------------
      * initial read of file
      *
     c                   If        inGroup# = 0
     c                   read      wmrhl                                  60
     c                   Else
     c     rhky          Chain     prh01                              60
     c                   EndIf
     c                   dow       not *in60
      *
     c                   clear                   newrsvamt
      *
      * Load the new order of reserve codes, if the group # changed.
      *
     c                   if        svfnd <> rhfnd
     c                             or *in98 = *on
     c     fnky          chain     wmfnl                              60
      *
     c                   eval      newrt01 = fnrs01
     c                   eval      newrt02 = fnrs02
     c                   eval      newrt03 = fnrs03
     c                   eval      newrt04 = fnrs04
     c                   eval      newrt05 = fnrs05
     c                   eval      newrt06 = fnrs06
     c                   eval      newrt07 = fnrs07
     c                   eval      newrt08 = fnrs08
     c                   eval      newrt09 = fnrs09
     c                   eval      newrt10 = fnrs10
     c                   setoff                                           98
     c                   eval      svfnd = rhfnd
      *
     c                   endif
      *------
      *
     c                   do        10            i
      *
     c                   eval      x = 1
     c     rsvcd(i)      lookup    newrsvcd(x)                            99
      *
     c                   if        *in99 = *off
     c                   eval      x = 1
     c     *blanks       lookup    newrsvcd(x)                            99
     c   99              eval      newrsvcd(x) = rsvcd(i)
     c                   seton                                            98
     c                   endif
      *
     c                   if        *in99 = *on
     c                   eval      newrsvamt(x) = (newrsvamt(x) + rsvamt(i))
     c                   endif
      *
     c                   enddo                                                  do 10
      *
     c                   exsr      updrsv
      *
      *----
     c                   If        inGroup# = 0
     c                   read      wmrhl                                  60    read next record
     c                   Else
     c     rhky          ReadE     prh01                                  60
     c                   EndIf
      *----
     c                   enddo                                                  *in60 = *off
      *--------
      *  End of pgm
     c                   eval      *inlr = *on                                  forced end of pgm
      *=============================================================================================
      *
     c     updrsv        begsr
      *
      *  Resequence the reserve buckets
      *
     c                   do        10            i
      *
     c                   eval      rsvcd(i) = newrsvcd(i)
     c                   eval      rsvamt(i) = newrsvamt(i)
      *
     c                   enddo                                                  do 10
      *
     c                   update    prh01
      *
     c                   endsr                                                  updclaim
      **********************************************************************************************
