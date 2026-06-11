     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

    H/TITLE WA097R - API - SSN/FEIN User Authoriy
    H*********************************************************************
     H* REVISED:
     H*
     H* 08/02/11 C:061 PROJECT WC2003
     H*                Created.
     H*
    H*********************************************************************

      * SSN/FEIN Default Table
     Fwtsfl     if   e           k disk

      * User Parameters
     Fwmupl     if   e           k disk

     Dsrccsc           PR                  extpgm('SRCCSC')
     D userType                       1    const
     D userLevel                      3    const
     D clntType                       1    const
     D clntLevel                      3    const
     D type                           1    const
     D failed                         1    const
     D clntType2                      1    const options(*nopass)
     D clntType3                      1    const options(*nopass)
     D clntType4                      1    const options(*nopass)
     D clntType5                      1    const options(*nopass)
     D clntType6                      1    const options(*nopass)

     D psds          esds                  extname(szq1p)

     D #1ULevel        s              3
     D #1CLevel        s              3

      **************************************************************
      * KLIST
      **************************************************************

      * SSN/FEIN Default Table

     c     sfky          klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    @@rpid


      **************************************************************
      * PLIST
      **************************************************************

     c     *entry        plist
     c                   parm                    @@co#             3
     c                   parm                    @@rpid           10
     c                   parm                    @@dft             1
     c                   parm                    @@chg             1


      **************************************************************
      * Main
      **************************************************************

     c                   move      @@co#         kkco#
     c                   eval      @@dft = *blank
     c                   eval      @@chg = '0'

     c     sfky          chain     psf01

     c                   if        (%found) and (sfflag <> *blanks)
     c                   eval      @@dft = sfflag
     c     q1user        chain     pup01

     c                   if        (%found)

     C                   move      uplvl         #1ULevel          3
     C                   move      sflvl         #1CLevel          3

     C                   callp     srccsc(uptype:
     C                                    #1ULevel:
     C                                    sfty01:
     C                                    #1CLevel:
     C                                    '1':
     C                                    @@chg:
     C                                    sfty02:
     C                                    sfty03:
     C                                    sfty04:
     C                                    sfty05:
     C                                    sfty06)

     c                   endif

     c                   endif

     c                   return
     c                   eval      *inlr = *on

