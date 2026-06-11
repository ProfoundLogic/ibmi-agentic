     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')

      /title WA077R - Check if Deductible Discount Code used by policy
      *****************************************************************
      * Called by: W0155R - WW Deductible Discount Codes
      *
      *****************************************************************
      * Revised:
      *
      * 04/04/10  C:61  Project: 133WHX
      *                 Created.
      *
      *****************************************************************

      * Employer Policy Year
     Fwdell     if   e           k disk

      * Rate Set Code
     Fwmrsl     if   e           k disk


      **************************************************************
      * KLIST
      **************************************************************
      *
      * Employer Policy Year
      *
     C     elky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
      *
      * Rate Set Code
      *
     C     rsky          klist
     C                   kfld                    elco#
     C                   kfld                    elrtcd


      **************************************************************
      * PLIST
      **************************************************************

     C     *entry        plist
     C                   parm                    @@co#             3 0
     C                   parm                    @@fnd             3 0
     C                   parm                    @@st              2
     C                   parm                    @@code            3
     C                   parm                    @@efdt            7 0
     C                   parm                    @@rtcd            1


      **************************************************************
      * MAIN
      **************************************************************

     c                   eval      @@rtcd = *blank
     c     elky          setll     pel01
     c     elky          reade     pel01

     c                   dow       not(%eof)

     c                   if        ((@@code = ELDCDE) or (@@code = ELDCD2))
     c                              and (@@efdt <= ELPFDT)

     c     rsky          chain     prs01

     c                   if        %found and (@@st = rsjrst)
     c                   eval      @@rtcd = 'F'
     c                   leave
     c                   endif

     c                   endif

     c     elky          reade     pel01
     c                   enddo

     c                   move      *on           *inlr

