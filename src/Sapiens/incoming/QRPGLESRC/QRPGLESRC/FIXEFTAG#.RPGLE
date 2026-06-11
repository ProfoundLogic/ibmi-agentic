     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title FIXEFTAG# - Fix Agent# in Employer EFT agent type records
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * CALLED FROM: SYPOSTPGM2                                       *
      * CALLED TO:                                                    *
      *                                                               *
      * Created 12/27/11  C:282  Project: WC2005                      *
      *****************************************************************
      * Revision:                                                     *
      *                                                               *
      *****************************************************************
     fwmefp     uf   e             disk
      *
      *****************************************************************
      * Main Loigc
      *****************************************************************
      * Read the file WMEFP for each record.
     c     1             setll     pef01
     c                   read      pef01                                  60
      *
     c                   dow       *in60 = *off
      *
     c                   if        eftype = 'E' and
     c                             efeatp = 'AG' and
     c                             efagt# = 0
      *
      * Call API WA002R to retrieve agent.
     c                   call      'WA002R'
     c                   parm                    efco#
     c                   parm                    effnd
     c                   parm                    efemp#
     c                   parm                    efdiv
     c                   parm      *zeros        xxfyr             3 0
     c                   parm      *zeros        xxagt#            5 0
     c                   parm      *blanks       xxanam           60
     c                   parm      *zeros        xxprod            5 0
     c                   eval      efagt# = xxagt#
      *
      * Update the file WMEFP
     c                   update    pef01
     c                   endif                                                  eftype = 'E'
      *
     c                   read      pef01                                  60
     c                   enddo                                                  *in60 = *off
      *
     c                   eval      *inlr = *on
