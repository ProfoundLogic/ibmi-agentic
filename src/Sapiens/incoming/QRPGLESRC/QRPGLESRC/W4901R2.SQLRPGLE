     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *============================================================================================*
      /title W4901R1 - Create Summary File                                                         *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      * Revisions                                                                                  *
      *                                                                                            *
      * 08/26/14 C:307 Project: SY354                                                              *
      *                -Implement Routing Page Changes.                                            *
      *                                                                                            *
      * Created by C:136 for Support item WA139 65431                                              *
      *============================================================================================*
     fqsysprt3  o    f  154        printer oflind(*inof)
      *=========================================================================
      * Field definitions
      *=========================================================================
     d true            c                   const( '1' )
     d false           c                   const( '0' )
     d sqlSttOk        c                   const( '00000' )
     d sqlSttNoRow     c                   const( '02000' )
     d sqlSttDupKey    c                   const( '23505' )
     d moreRows        s               n
     d recCount        s              9  0
     d xxco#           s              3  0
     d xxfnd           s              3  0
     d xxemp#          s              9  0
     d xxdiv           s              5  0
     d xxfyr           s              3  0
     d xxname          s             40
     d xxTitle         s             50    inz('  CEU Comparison Reports ')
     d xoInpr          s             11  2
     d xoYdwr          s             11  2
     d xoYdea          s             11  2
     d xoUnea          s             11  2
     d xoMdea          s             11  2
     d xoPywr          s             11  2
     d xoPyea          s             11  2
     d xnInpr          s             11  2
     d xnYdwr          s             11  2
     d xnYdea          s             11  2
     d xnUnea          s             11  2
     d xnMdea          s             11  2
     d xnPywr          s             11  2
     d xnPyea          s             11  2
     d xtInpr          s             13  2
     d xtYdwr          s             13  2
     d xtYdea          s             13  2
     d xtUnea          s             13  2
     d xtMdea          s             13  2
     d xtPywr          s             13  2
     d xtPyea          s             13  2
     d companyNm       s             60
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d psds          esds                  extname(szq1p)
      *=========================================================================
      * Prototypes
      *=========================================================================
     d/copy sprcompany
     d/copy sprdate
     d/copy sprstring
     d/copy wprUsrPrms
      *=========================================================================
     c/exec SQL
     c+ declare e1 cursor for
     c+  select a.EUCO#, a.EUFND, a.EUEMP#,
     c+         a.EUDIV, a.EUFYR, a.EUNAME,
     c+         a.EUINPR,
     c+         a.EUYDWR, a.EUYDEA, a.EUUNEA,
     c+         a.EUMDEA, a.EUPYWR, a.EUPYEA
     c+    from QTEMP/WFEUP2O a
     c+      exception join QTEMP/WFEUP2N b
     c+      on a.EUCO#  = b.EUCO#  and
     c+         a.EUFND  = b.EUFND  and
     c+         a.EUEMP# = b.EUEMP# and
     c+         a.EUDIV  = b.EUDIV  and
     c+         a.EUFYR  = b.EUFYR
     c+    order by a.EUCO#, a.EUFND, a.EUEMP#,
     c+             a.EUDIV, a.EUFYR
     c+ for read only
     c/end-exec
     c/exec SQL
     c+ declare e2 cursor for
     c+  select a.EUCO#, a.EUFND, a.EUEMP#,
     c+         a.EUDIV, a.EUFYR, a.EUNAME,
     c+         a.EUINPR,
     c+         a.EUYDWR, a.EUYDEA, a.EUUNEA,
     c+         a.EUMDEA, a.EUPYWR, a.EUPYEA
     c+    from QTEMP/WFEUP2N a
     c+      exception join QTEMP/WFEUP2O b
     c+      on a.EUCO#  = b.EUCO#  and
     c+         a.EUFND  = b.EUFND  and
     c+         a.EUEMP# = b.EUEMP# and
     c+         a.EUDIV  = b.EUDIV  and
     c+         a.EUFYR  = b.EUFYR
     c+    order by a.EUCO#, a.EUFND, a.EUEMP#,
     c+             a.EUDIV, a.EUFYR
     c+ for read only
     c/end-exec
     c/exec SQL
     c+ declare j1 cursor for
     c+  select a.EUCO#, a.EUFND, a.EUEMP#,
     c+         a.EUDIV, a.EUFYR, a.EUNAME,
     c+         a.EUINPR, a.EUYDWR, a.EUYDEA,
     c+         a.EUUNEA,
     c+         a.EUMDEA, a.EUPYWR, a.EUPYEA,
     c+         b.EUINPR, b.EUYDWR, b.EUYDEA,
     c+         b.EUUNEA,
     c+         b.EUMDEA, b.EUPYWR, b.EUPYEA
     c+    from QTEMP/WFEUP2O a join QTEMP/WFEUP2N b
     c+      on a.EUCO#  = b.EUCO#  and
     c+         a.EUFND  = b.EUFND  and
     c+         a.EUEMP# = b.EUEMP# and
     c+         a.EUDIV  = b.EUDIV  and
     c+         a.EUFYR  = b.EUFYR
     c+    where a.EUINPR <> b.EUINPR or
     c+          a.EUYDWR <> b.EUYDWR or
     c+          a.EUYDEA <> b.EUYDEA or
     c+          a.EUUNEA <> b.EUUNEA or
     c+          a.EUMDEA <> b.EUMDEA or
     c+          a.EUPYWR <> b.EUPYWR or
     c+          a.EUPYEA <> b.EUPYEA
     c+    order by a.EUCO#, a.EUFND, a.EUEMP#,
     c+             a.EUDIV, a.EUFYR
     c+    for read only
     c/end-exec
      *================================================================================
      * Main
      *================================================================================
     c                   exsr      initialize
     c                   eval      recCount = 0
     c                   exsr      openE1
     c                   dow       moreRows = true
     c                   exsr      processE1
     c                   enddo
     c                   exsr      closeE1

     c                   eval      recCount = 0
     c                   exsr      openE2
     c                   dow       moreRows = true
     c                   exsr      processE2
     c                   enddo
     c                   exsr      closeE2

     c                   eval      recCount = 0
     c                   exsr      openJ1
     c                   dow       moreRows = true
     c                   exsr      processJ1
     c                   enddo
     c                   exsr      closeJ1
     c                   exsr      srTotals

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
          Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   eval      *inlr = *on
      *=================================================================================
      *    initialize
      *=================================================================================
     c     initialize    begsr
     C                   eval      xxco#  = UserCo#(q1user)
     C                   eval      companyNm = CenterFld(GetCompNam(xxco#):
     C                                                %len(companyNm))
     C     *hms          move      q1time        sys_time
     C     *usa          move      *date         sys_date
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endsr
      *=================================================================================
      * openE1
      *=================================================================================
     c     openE1        begsr
     c/exec sql
     c+                  open      e1
     c/end-exec
     c                   if        sqlStt = sqlSttOK
     c                   eval      moreRows = true
     c                   eval      *inof = *on
     c                   eval      xxTitle = 'Unmatched Policies in Old vs New -
     c                             CEU Reports'
     c                   else
     c                   eval      moreRows = false
     c                   endif
     c                   endsr
      *=================================================================================
      * openE2
      *=================================================================================
     c     openE2        begsr
     c/exec sql
     c+                  open      e2
     c/end-exec
     c                   if        sqlStt = sqlSttOK
     c                   eval      moreRows = true
     c                   eval      *inof = *on
     c                   eval      xxTitle = 'Unmatched Policies in New vs Old -
     c                             CEU Reports'
     c                   else
     c                   eval      moreRows = false
     c                   endif
     c                   endsr
      *=================================================================================
      * openJ1
      *=================================================================================
     c     openJ1        begsr
     c/exec sql
     c+                  open      j1
     c/end-exec
     c                   if        sqlStt = sqlSttOK
     c                   eval      moreRows = true
     c                   eval      *inof = *on
     c                   eval      xxTitle = 'Premium Difference in Old vs New -
     c                             CEU Reports'
     c                   else
     c                   eval      moreRows = false
     c                   endif
     c                   endsr
      *=================================================================================
      * closeE1
      *=================================================================================
     c     closeE1       begsr
     c                   if        recCount = 0
     c                   except    noRecs
     c                   endif
     c/exec sql
     c+                  close     e1
     c/end-exec
     c                   endsr
      *=================================================================================
      * closeE1
      *=================================================================================
     c     closeE2       begsr
     c                   if        recCount = 0
     c                   except    noRecs
     c                   endif
     c/exec sql
     c+                  close     e2
     c/end-exec
     c                   endsr
      *=================================================================================
      * closeJ1
      *=================================================================================
     c     closeJ1       begsr
     c                   if        recCount = 0
     c                   except    noRecs
     c                   endif
     c/exec sql
     c+                  close     j1
     c/end-exec
     c                   endsr
      *=================================================================================
      * processE1
      *=================================================================================
     c     processE1     begsr
     c                   if        *inof = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c/exec SQL
     c+                  fetch next from e1 into :xxco#,
     c+                                          :xxfnd,
     c+                                          :xxemp#,
     c+                                          :xxdiv,
     c+                                          :xxfyr,
     c+                                          :xxname,
     c+                                          :xoInpr,
     c+                                          :xoYdwr,
     c+                                          :xoYdea,
     c+                                          :xoUnea,
     c+                                          :xoMdea,
     c+                                          :xoPywr,
     c+                                          :xoPyea
     c/end-exec
     c                   select
     c                   when      sqlStt = sqlSttOK
     c                   add       1             recCount
     c                   eval      xtInpr =  xtInpr - xoInpr
     c                   eval      xtYdwr =  xtYdwr - xoYdwr
     c                   eval      xtYdea =  xtYdea - xoYdea
     c                   eval      xtUnea =  xtUnea - xoUnea
     c                   eval      xtMdea =  xtMdea - xoMdea
     c                   eval      xtPywr =  xtPywr - xoPywr
     c                   eval      xtPyea =  xtPyea - xoPyea
     c                   if        *inof = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c                   except    polInfo
     c                   except    prmInfo1
     c                   when      sqlStt = sqlSttNoRow
     c                   eval      moreRows = *off
     c                   endsl
     c                   endsr
      *=================================================================================
      * processE2
      *=================================================================================
     c     processE2     begsr
     c                   if        *inof = *on
     c                   except    head
     c                   endif
     c/exec SQL
     c+                  fetch next from e2 into :xxco#,
     c+                                          :xxfnd,
     c+                                          :xxemp#,
     c+                                          :xxdiv,
     c+                                          :xxfyr,
     c+                                          :xxname,
     c+                                          :xnInpr,
     c+                                          :xnYdwr,
     c+                                          :xnYdea,
     c+                                          :xnUnea,
     c+                                          :xnMdea,
     c+                                          :xnpywr,
     c+                                          :xnPyea
     c/end-exec
     c                   select
     c                   when      sqlStt = sqlSttOK
     c                   add       1             recCount
     c                   eval      xtInpr =  xtInpr + xnInpr
     c                   eval      xtYdwr =  xtYdwr + xnYdwr
     c                   eval      xtYdea =  xtYdea + xnYdea
     c                   eval      xtUnea =  xtUnea + xnUnea
     c                   eval      xtMdea =  xtMdea + xnMdea
     c                   eval      xtPywr =  xtPywr + xnPywr
     c                   eval      xtPyea =  xtPyea + xnPyea
     c                   if        *inof = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c                   except    polInfo
     c                   except    prmInfo1
     c                   when      sqlStt = sqlSttNoRow
     c                   eval      moreRows = *off
     c                   endsl
     c                   endsr
      *=================================================================================
      * processJ1
      *=================================================================================
     c     processJ1     begsr
     c                   if        *inof = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c/exec SQL
     c+                  fetch next from j1 into :xxco#,
     c+                                          :xxfnd,
     c+                                          :xxemp#,
     c+                                          :xxdiv,
     c+                                          :xxfyr,
     c+                                          :xxname,
     c+                                          :xoInpr,
     c+                                          :xoYdwr,
     c+                                          :xoYdea,
     c+                                          :xoUnea,
     c+                                          :xoMdea,
     c+                                          :xoPywr,
     c+                                          :xoPyea,
     c+                                          :xnInpr,
     c+                                          :xnYdwr,
     c+                                          :xnYdea,
     c+                                          :xnUnea,
     c+                                          :xnMdea,
     c+                                          :xnPywr,
     c+                                          :xnPyea
     c/end-exec
     c                   select
     c                   when      sqlStt = sqlSttOK
     c                   add       1             recCount
     c                   eval      xtInpr =  xtInpr + (xnInpr - xoInpr)
     c                   eval      xtYdwr =  xtYdwr + (xnYdwr - xoYdwr)
     c                   eval      xtYdea =  xtYdea + (xnYdea - xoYdea)
     c                   eval      xtUnea =  xtUnea + (xnUnea - xoUnea)
     c                   eval      xtMdea =  xtMdea + (xnMdea - xoMdea)
     c                   eval      xtPywr =  xtPywr + (xnPywr - xoPywr)
     c                   eval      xtPyea =  xtPyea + (xnPyea - xoPyea)
     c                   if        *inof = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c                   except    polInfo
     c                   except    prmInfo
     c                   when      sqlStt = sqlSttNoRow
     c                   eval      moreRows = *off
     c                   endsl
     c                   endsr
      *==================================================================================
      *==================================================================================
      * srTotals
      *==================================================================================
     c     srTotals      begsr
     c                   except    totHeader
     c                   except    totals
     c                   endsr
      *==================================================================================
     oqsysprt3  e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     o**********                                  8 'XXXXXXXX'
     o**********                                 20 'XXXXXX'
     o**********                                 34 'XX      XX'
     o**********                                 45 'XXXXXXXXXX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XXX     XX'
     o**********                                 77 'XXXXXX'
     o**********e            route          1
     o**********                                  9 'XXXXXXXXX'
     o**********                                 21 'XXXXXXXX'
     o**********                                 34 'XX      XX'
     o**********                                 45 'XXXXXXXXXX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XXXX    XX'
     o**********                                 78 'XXXXXXXX'
     o**********e            route          1
     o**********                                 10 'XX      XX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XXXX    XX'
     o**********                                 79 'XX      XX'
     o**********e            route          1
     o**********                                 10 'XX      XX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX XX   XX'
     o**********                                 71 'XX'
     o**********e            route          1
     o**********                                  9 'XXXXXXXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX XX   XX'
     o**********                                 71 'XX'
     o**********e            route          1
     o**********                                  8 'XXXXXXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX  XX  XX'
     o**********                                 71 'XX'
     o**********e            route          1
     o**********                                  4 'XXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX  XX  XX'
     o**********                                 79 'XX    XXXX'
     o**********e            route          1
     o**********                                  5 'XXXXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX   XX XX'
     o**********                                 79 'XX    XXXX'
     o**********e            route          1
     o**********                                  6 'XX XXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX   XX XX'
     o**********                                 79 'XX      XX'
     o**********e            route          1
     o**********                                  7 'XX  XXX'
     o**********                                 22 'XX      XX'
     o**********                                 34 'XX      XX'
     o**********                                 41 'XX'
     o**********                                 52 'XX'
     o**********                                 67 'XX    XXXX'
     o**********                                 79 'XX      XX'
     o**********e            route          1
     o**********                                  8 'XX   XXX'
     o**********                                 21 'XXXXXXXX'
     o**********                                 33 'XXXXXXXX'
     o**********                                 41 'XX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XX    XXXX'
     o**********                                 78 'XXXXXXXX'
     o**********e            route          3
     o**********                                  9 'XX    XXX'
     o**********                                 20 'XXXXXX'
     o**********                                 32 'XXXXXX'
     o**********                                 41 'XX'
     o**********                                 56 'XXXXXXXXXX'
     o**********                                 67 'XX     XXX'
     o**********                                 77 'XXXXXX'
      ***** End Del ***** 08/26/14 ************************************
     o          e            route          2
     o                                           19 'REPORT TITLE:'
     o                                        +   1 'CEU Comparison Report'
      ***** Beg Del ***** 08/26/14 ************************************
     o**********e            route          2
     o**********                                 19 'PROGRAM NAME:'
     o**********                              +   1 'W4901R2'
      ***** End Del ***** 08/26/14 ************************************
     o          e            route          1 60
     o                                              '(C) Copyright StoneRiver,'
     o                                        +   1 'Inc.'
     o                       *year            +   1

     o          e            head           1 06
     o                       sys_time
     o**********             companyNm          117
     o**********             sys_date           152
     o**********e            head           1
     o**********             q1pgm
     o**********             xxTitle            112
     o**********                                148 'PAGE:'
     o**********             page          z    152
     o          e            head           1
     o                                            3 'Grp'
     o                                           15 'Employer#'
     o                                           23 'Div'
     o                                           29 'Yr'
     o                                           45 'Employer Name'
     o          e            head           1
     o                                           26 'Inforce'
     o                                           44 'YTD Written'
     o                                           62 'YTD Earned'
     o                                           80 'Unearned'
     o                                           98 'MTD Earned'
     o                                          116 'Prior Yr Written'
     o                                          134 'Prior Yr Earned'
     o          e            polInfo     1  1
     o                       xxfnd         z      3
     o                       xxemp#        z     15
     o                       xxdiv         z     23
     o                       xxfyr         z     29
     o                       xxname              72
     o          e            prmInfo     1
     o                                            6 'Old'
     o                       xoInpr        j     27
     o                       xoYdwr        j     45
     o                       xoYdea        j     63
     o                       xoUnea        j     81
     o                       xoMdea        j     99
     o                       xopywr        j    117
     o                       xoPyea        j    135
     o          e            prmInfo     1  1
     o                                            6 'New'
     o                       xnInpr        j     27
     o                       xnYdwr        j     45
     o                       xnYdea        j     63
     o                       xnUnea        j     81
     o                       xnMdea        j     99
     o                       xnpywr        j    117
     o                       xnPyea        j    135
     o          e            prmInfo1    1  1
     o                       xoInpr        j     27
     o                       xoYdwr        j     45
     o                       xoYdea        j     63
     o                       xoUnea        j     81
     o                       xoMdea        j     99
     o                       xopywr        j    117
     o                       xoPyea        j    135
     o          e            totHeader   1  2
     o                       sys_time
     o                       companyNm          117
     o                       sys_date           152
     o          e            totHeader      1
     o                       q1pgm
     o                                          112 'Summary of Premium Differe-
     o                                              nces'
     o                                          148 'PAGE:'
     o                       page          z    152
     o
     o
     o          e            totHeader      1
     o                                           26 'Inforce'
     o                                           44 'YTD Written'
     o                                           62 'YTD Earned'
     o                                           80 'Unearned'
     o                                           98 'MTD Earned'
     o                                          116 'Prior Yr Written'
     o                                          134 'Prior Yr Earned'
     o          e            totals      1
     o                       xtInpr        j     26
     o                       xtYdwr        j     44
     o                       xtYdea        j     62
     o                       xtUnea        j     80
     o                       xtMdea        j     98
     o                       xtpywr        j    116
     o                       xtPyea        j    134
     o          e            noRecs      1  1
     o                                           85 '*** No records meet the -
     o                                              selection criteria. ***'
