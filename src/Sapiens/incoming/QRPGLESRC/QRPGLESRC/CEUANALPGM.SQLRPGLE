      * (C) Copyright Fiserv Insurance Solutions, Inc., 2008                                       *
      *============================================================================================*
      /title CUEANALPGM - Analysis for QFEUP MTD Errors                                            *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      *     qqmode = '1' or '2'
      *       - '1' builds a summary file (CEUANBLD) that summarizes YDWR, MDWR,
      *             YDEA, and MDEA numbers
      *       - '2' builds an error file populated if Prior Mth YDEA + Current Mth MDEA <> Current
      *             Mth YDEA or if Cumulative MDWR <> YDWR.
      *     qqecym = cym of last month in run.
      *============================================================================================*
     fceuanbld  uf a e           k disk    prefix(c)
     fceuanrst  uf a e           k disk

     d moreRows        s               n   inz(*on)
     d xxfcym          s              5  0
     d xxecym          s              5  0
     d svco#           s                   like(CEUCO#)
     d svfnd           s                   like(CEUFND)
     d svemp#          s                   like(CEUEMP#)
     d svdiv           s                   like(CEUDIV)
     d svfyr           s                   like(CEUFYR)
     d svrcym          s                   like(CEURCYM)
     d prmdea          s                   like(CEUMDEA)
     d prydea          s                   like(CEUYDEA)
     d prmdwr          s                   like(CEUMDEA)
     d prydwr          s                   like(CEUYDEA)
     d totmdwr         s                   like(CEUYDWR)
     d totydwr         s                   like(CEUYDWR)
     d svydwr          s                   like(CEUYDWR)
     d totydea         s                   like(CEUYDEA)
     d flag            s              1

     c     *entry        plist
     c                   parm                    qqmode            1
     c                   parm                    qqecym            5

     c                   move(p)   qqecym        xxfcym
     c                   move      '01'          xxfcym
     c                   move(p)   qqecym        xxecym

     c                   if        qqmode = '1'

     c                   read      peu01
     c                   dow       not(%eof)
     c                   delete    peu01
     c                   read      peu01
     c                   enddo

c    c/exec SQL
     c+ declare ceucursor cursor
     c+   for select euco#, eufnd, euemp#, eudiv, eufyr, eurcym,
     c+              sum(eumdea) mdea, sum(euydea) ydea,
     c+              sum(eumdwr) mdwr, sum(euydwr) ydwr
     c+         from qfeup
     c+         where eurcym >= :xxfcym and
     c+               eurcym <= :xxecym
     c+         group by euco#, eufnd, euemp#, eudiv, eufyr, eurcym
     c+         order by euco#, eufnd, euemp#, eudiv, eufyr, eurcym
     c/end-exec

     c/exec SQL
     c+ open ceucursor
     c/end-exec

     c                   dow       moreRows = *on

     c/exec SQL
     c+ fetch next from ceucursor
     c+   into :ceuco#,
     c+        :ceufnd,
     c+        :ceuemp#,
     c+        :ceudiv,
     c+        :ceufyr,
     c+        :ceurcym,
     c+        :ceumdea,
     c+        :ceuydea,
     c+        :ceumdwr,
     c+        :ceuydwr
     c/end-exec

     c                   if        sqlstt <> '00000'
     c                   eval      moreRows = *off
     c                   else
     c                   write     peu01
     c                   endif

     c                   enddo

     c/exec SQL
     c+ close ceucursor
     c/end-exec

     c                   endif

     c                   if        qqmode = '2'

     c                   read      peu02
     c                   dow       not(%eof)
     c                   delete    peu02
     c                   read      peu02
     c                   enddo

     c                   read      peu01
     c                   eval      svco# = ceuco#
     c                   eval      svfnd = ceufnd
     c                   eval      svemp# = ceuemp#
     c                   eval      svdiv = ceudiv
     c                   eval      svfyr = ceufyr
     c                   eval      svrcym = ceurcym
     c                   eval      prmdea = ceumdea
     c                   eval      prydea = ceuydea
     c                   eval      prmdwr = ceumdwr
     c                   eval      prydwr = ceuydwr
     c                   eval      totmdwr = ceumdwr
     c                   eval      totydwr = ceuydwr
     c                   dow       not(%eof)

     c                   if        ceuco# <> svco# or
     c                             ceufnd <> svfnd or
     c                             ceuemp# <> svemp# or
     c                             ceudiv <> svdiv or
     c                             ceufyr <> svfyr
     c                   eval      eudiff = totydwr - totmdwr
     c                   if        totydwr <> totmdwr
     c                   eval      flag = '1'
     c                   exsr      writeOut
     c                   endif
     c                   if        ceuydwr <> ceumdwr
     c                   eval      flag = '1'
     c                   eval      svco# = ceuco#
     c                   eval      svfnd = ceufnd
     c                   eval      svemp# = ceuemp#
     c                   eval      svdiv = ceudiv
     c                   eval      svfyr = ceufyr
     c                   eval      svrcym = ceurcym
     c                   exsr      writeOut
     c                   endif
     c                   eval      eudiff = ceuydea - ceumdea
     c                   if        ceuydea <> ceumdea
     c                   eval      flag = '2'
     c                   eval      svco# = ceuco#
     c                   eval      svfnd = ceufnd
     c                   eval      svemp# = ceuemp#
     c                   eval      svdiv = ceudiv
     c                   eval      svfyr = ceufyr
     c                   eval      svrcym = ceurcym
     c                   exsr      writeOut
     c                   endif
     c                   eval      prmdea = ceumdea
     c                   eval      prydea = ceuydea
     c                   eval      prmdwr = ceumdwr
     c                   eval      prydwr = ceuydwr
     c                   eval      svco# = ceuco#
     c                   eval      svfnd = ceufnd
     c                   eval      svemp# = ceuemp#
     c                   eval      svdiv = ceudiv
     c                   eval      svfyr = ceufyr
     c                   eval      svrcym = ceurcym
     c                   eval      totmdwr = ceumdwr
     c                   eval      totydwr = ceuydwr
     c                   endif

     c                   if        svrcym <> ceurcym
     c                   eval      svrcym = ceurcym
     c                   eval      totydwr = prydwr + ceumdwr
     c                   eval      eudiff = totydwr - ceuydwr
     c                   if        totydwr <> ceuydwr
     c                   eval      flag = '1'
     c                   exsr      writeOut
     c                   endif
     c                   eval      totydea = prydea + ceumdea
     c                   eval      eudiff = totydea - ceuydea
     c                   if        totydea <> ceuydea
     c                   eval      flag = '2'
     c                   exsr      writeOut
     c                   endif
     c                   eval      prydwr  = ceuydwr
     c                   eval      prydea  = ceuydea
     c                   eval      totmdwr = totmdwr + ceumdwr
     c                   eval      totydwr = ceuydwr
     c                   endif

     c                   read      peu01
     c                   enddo

     c                   endif

     c                   eval      *inlr = *on
     p*===========================================================================
     p* writeOut - writes out errors to file
     p*===========================================================================
     c     writeOut      begsr

     c                   select
     c                   when      flag = '1'
     c                   eval      EUERR = 'YDWR'
     c                   when      flag = '2'
     c                   eval      EUERR = 'MDEA'
     c                   endsl
     c                   eval      euco# = svco#
     c                   eval      eufnd  = svfnd
     c                   eval      euemp# = svemp#
     c                   eval      eudiv  = svdiv
     c                   eval      eufyr  = svfyr
     c                   eval      eurcym = svrcym
     c                   write     peu02
     c
     c                   endsr
