     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     h/title W7763R  : Written vs Collected premium
     h*=================================================================================
     h*  Description:
     h*  Produces a report/workfile of written and collected premium numbers. Written premium
     h*  is pulled from the CEU.  Collected premium, fees billed, and fees paid are pulled from A/R.
     h*
     h*  Called from: W7761C
     h*  Calls to:
     h*
     h*=================================================================================
    h*  Created on 06/01/2009 by c:62 for Project 114WRL
     h*=================================================================================
     h* Revisions:
     h*
     h* 08/26/14 C:307 Project: SY354
     h*                -Implement Routing Page Changes.
     h*
     h*  06/21/10  C:191 Project: 114WVX
     h*                   Written vs Collected Report Changes
     h*
     h* 01/28/10  C:249  Project: SY215                               *
     h*           C:189  Retrieve brand Name                          *
     h*                                                               *
     h*=================================================================================
      * Written Prem vs Collected Prem
     fwfwcl     if   e           k disk
     fsmorl     if   e           k disk
      * Print file
     fw7763o    o    e             printer usropn oflind(*in01)
     *---------------------------------------------------------------------
     * Stand Alone Fields
     *---------------------------------------------------------------------
     d xxco#           s              3  0
     d xxfgrp          s              3  0
     d xxtgrp          s              3  0
     d xxGrpRng        s             10    inz(*blanks)
     d xxque           s             10
     d xxfil#          s                   like(ORFIL#)
     d xxtype          s             15
     d xxCat           s              1
     d x8asod          s              8  0
     d x7asod          s              7  0
     d charJan1        s              7
     d x7Jan1          s              7  0
     d x7date          s              7  0
     d svfyr           s                   like(WCFYR)
     d svfnd           s                   like(WCFND)
     d svemp#          s                   like(WCEMP#)
     d svdiv           s                   like(WCDIV)
     d companyNm       s             40
     d groupName       s             25
     d upusnm          s             50
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d xxtitle         s             60
     d xxPrtVb         s              3
     d xxFilVb         s              3
     d line1           s            198
     d line2           s            120
     d eewrpr          s             11  2
     d eesurb          s             11  2
     d eeclpr          s             11  2
     d eesurp          s             11  2
     d eeprmb          s             11  2
     d eeassb          s             11  2
     d eetval          s             11  2
     d ggwrpr          s             11  2
     d ggsurb          s             11  2
     d ggclpr          s             11  2
     d ggsurp          s             11  2
     d ggprmb          s             11  2
     d ggassb          s             11  2
     d ggtval          s             11  2
     d ttwrpr          s             11  2
     d ttsurb          s             11  2
     d ttclpr          s             11  2
     d ttsurp          s             11  2
     d ttprmb          s             11  2
     d ttassb          s             11  2
     d tttval          s             11  2
     d*** BEGIN CHG ****** 06/21/10 ************************************
     d*********** xxpol#          s             25
     d xxinnm          s             25
     d*** END   CHG ****** 06/21/10 ************************************
     d xxtval          s             11  2
     d xxrrn           s              9  0
     d ##str           s             80
     d ##len           s              3  0
     *---------------------------------------------------------------------
     * Arrays and Data Structure
     *---------------------------------------------------------------------
     d psds          esds                  extname(szq1p)
     *---------------------------------------------------------------------
     * Constants
     *---------------------------------------------------------------------
      *** beg chg ***** 06/25/10 ***************************************
     d*title*****      c                   'Written Premium vs Collected -
     d***********                          Premium'
     d title           c                   'Month End A/R Report'
      *** end chg ***** 06/25/10 ***************************************
      *** Beg Add *** 01/28/10 ****************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
      *** End Add *** 01/28/10 ****************************************
     *---------------------------------------------------------------------
     * Procedure Interfaces
     *---------------------------------------------------------------------
     d*entry
     d W7763R          pr
     d  @@co#                         3
     d  @@fgrp                        3
     d  @@tgrp                        3
     d  @@asod                        8
     d  @@output                      1
     d W7763R          pi
     d  @@co#                         3
     d  @@fgrp                        3
     d  @@tgrp                        3
     d  @@asod                        8
     d  @@output                      1

     d/copy sPrString
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrGroup
     d/copy wPrEmplr
     d/copy wPrUsrPrms
      *=========================================================================
      * klist
      *=========================================================================
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#
      *=========================================================================
      * Main
      *=========================================================================
     c                   read      pwc01

     c                   eval      svfnd  = wcfnd
     c                   eval      svemp# = wcemp#
     c                   eval      svdiv  = wcdiv

     c                   write     polHeader1
     c                   write     polHeader2

     c                   dow       not %eof

     c                   if        svfnd  <> wcfnd  or
     c                             svemp# <> wcemp# or
     c                             svdiv  <> wcdiv
     c                   exsr      srEmpTotals
     c                   endif

     c                   if        svfnd <> wcfnd
     c                   exsr      srGrpTotals
     c                   endif

     C                   eval      xxrrn  = getEmpYrRR(wcfyr:wcemp#:wcdiv:
     C                                            wcfnd:wcco#)
     C*** BEGIN CHG ****** 06/21/10 ************************************
     C*************      eval      xxpol# = getEmpPol#(xxrrn)
     C*************      eval      xxtval = wcprmb + wcassb
     C                   eval      xxtval = wcbal
     C                   eval      xxinnm = wcenam
     C*** END   CHG ****** 06/21/10 ************************************
     c                   exsr      accTotals

     c                   if        *in01
     c                   write     polHeader1
     c                   write     polHeader2
     c                   setoff                                       01
     c                   endif

     c                   write     polDetail

     c                   read      pwc01
     c                   enddo
     c*
     c                   exsr      srEmpTotals
     c                   exsr      srGrpTotals
     c                   exsr      srTotal

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfil# = '0';
       Chain orky POR01;
       If %found;
          Write Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   seton                                        lr
      *=========================================================================
      *    *inzsr
      *=========================================================================
     c     *inzsr        begsr

     c                   move      @@asod        x8asod
     c                   eval      x7asod = dtCYMD(x8asod)
     c                   move      x7asod        charJan1
     c                   move      '0101'        charJan1
     c                   move      charJan1      x7Jan1

     c                   open      w7763o
     c                   move      *all'_'       line1
     c                   move      *all'_'       line2
     c                   exsr      routing

     c                   clear                   eewrpr
     c                   clear                   eesurb
     c                   clear                   eeclpr
     c                   clear                   eesurp
     c                   clear                   eeprmb
     c                   clear                   eeassb
     c                   clear                   eetval

     c                   clear                   ggwrpr
     c                   clear                   ggsurb
     c                   clear                   ggclpr
     c                   clear                   ggsurp
     c                   clear                   ggprmb
     c                   clear                   ggassb
     c                   clear                   ggtval

     c                   clear                   ttwrpr
     c                   clear                   ttsurb
     c                   clear                   ttclpr
     c                   clear                   ttsurp
     c                   clear                   ttprmb
     c                   clear                   ttassb
     c                   clear                   tttval

     c                   endsr
      *=========================================================================
      * Routing
      *=========================================================================
     c     routing       begsr

     c                   eval      *in08 = *off
     c                   eval      *in09 = *on
     c                   eval      sys_Date  = SystemDate
     c                   eval      sys_Date  = SystemDate
     c                   eval      sys_Time  = SystemTime
     c                   eval      xxco#     = UserCo#(q1user)
     c                   eval      upusnm    = GetUsrName(q1user)
     C*
     c                   eval      companyNm = GetCompNam(xxco#)
     c                   eval      ##str=%trim(companyNm)
     c                   call      'SRCENTER'
     c                   parm                    ##str
     c                   parm      40            ##len
     c                   movel(p)  ##str         companyNm
     C*
     c                   eval      xxque     = GetUsrOutq(q1user)
     c                   move      @@fgrp        xxfgrp
     c                   move      @@tgrp        xxtgrp

     c                   if        xxfgrp = 0 and
     c                             xxtgrp = 0
     c                   movel     '*all'        xxGrpRng
     c                   else
     c                   eval      xxGrpRng = @@fgrp + ' - ' + @@tgrp
     c                   endif

     c                   if        xxtgrp = 0
     c                   eval      xxtgrp = *hival
     c                   endif

     c                   select
     c                   when      @@output = 'P'
     c                   eval      xxPrtVb = 'Yes'
     c                   eval      xxFilVb = 'No'
     c                   when      @@output = 'F'
     c                   eval      xxPrtVb = 'No'
     c                   eval      xxFilVb = 'Yes'
     c                   when      @@output = 'B'
     c                   eval      xxPrtVb = 'Yes'
     c                   eval      xxFilVb = 'Yes'
     c                   endsl
     *
     * Get the Distribution Information
     *
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     c                   clear                   xxtitle
     c                   eval      xxfil#   = '0'
     c     orky          chain     por01                              60

     c                   if        ORTITL = *blank
     c                   call      'SRCENTER'
     c                   parm      title         ##str
     c                   parm      60            ##len
     c                   else
     c                   eval      ##str=%trim(ORTITL)
     c                   call      'SRCENTER'
     c                   parm                    ##str
     c                   parm      60            ##len
     c                   endif
      *
     c                   movel(p)  ##str         xxtitle
      *
     * Print the Distribution Information
     *
     c                   if        not *in60
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         write     route
      ***** End Del ***** 08/26/14 ************************************
     c                   setoff                                       01
     c                   endif

     c                   endsr
      *=========================================================================
      * AccTotals - Accumulate Totals
      *=========================================================================
     c     AccTotals     begsr

     c                   add       wcwrpr        eewrpr
     c                   add       wcsurb        eesurb
     c                   add       wcclpr        eeclpr
     c                   add       wcsurp        eesurp
     c                   add       wcprmb        eeprmb
     c                   add       wcassb        eeassb
     c                   add       xxtval        eetval

     c                   add       wcwrpr        ggwrpr
     c                   add       wcsurb        ggsurb
     c                   add       wcclpr        ggclpr
     c                   add       wcsurp        ggsurp
     c                   add       wcprmb        ggprmb
     c                   add       wcassb        ggassb
     c                   add       xxtval        ggtval

     c                   add       wcwrpr        ttwrpr
     c                   add       wcsurb        ttsurb
     c                   add       wcclpr        ttclpr
     c                   add       wcsurp        ttsurp
     c                   add       wcprmb        ttprmb
     c                   add       wcassb        ttassb
     c                   add       xxtval        tttval

     c                   endsr
      *=========================================================================
      *  srEmpTotals
      *=========================================================================
     c     srEmpTotals   begsr

     c*** BEGIN DEL ****** 06/21/10 ************************************
     c* Do not print employer totals, but still clear related fields to retaing correct processing
     c*************      if        *in01
     c*************      write     polHeader1
     c*************      write     polHeader2
     c*************      setoff                                       01
     c*************      endif
      *************
     c*************      write     empTotals
     c*** END   DEL ****** 06/21/10 ************************************

     c                   clear                   eewrpr
     c                   clear                   eesurb
     c                   clear                   eeclpr
     c                   clear                   eesurp
     c                   clear                   eeprmb
     c                   clear                   eeassb
     c                   clear                   eetval

     c                   eval      svemp# = wcemp#
     c                   eval      svdiv  = wcdiv

     c                   endsr
      *=========================================================================
      *  srGrpTotals
      *=========================================================================
     c     srGrpTotals   begsr

     c                   if        *in01
     c                   write     polHeader1
     c                   write     polHeader2
     c                   setoff                                       01
     c                   endif

     c                   write     grpTotals

     c                   clear                   ggwrpr
     c                   clear                   ggsurb
     c                   clear                   ggclpr
     c                   clear                   ggsurp
     c                   clear                   ggprmb
     c                   clear                   ggassb
     c                   clear                   ggtval

     c                   eval      svfnd = wcfnd

     c                   endsr
      *=========================================================================
      *  srTotal
      *=========================================================================
     c     srTotal       begsr

     c                   write     polHeader1
     c                   write     totHeader
     c                   write     totDetail

     c                   endsr
