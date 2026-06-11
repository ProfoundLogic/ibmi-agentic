     h* (c) 2011 StoneRiver, Inc. All rights reserved.
     h* StoneRiver is a trademark of StoneRiver, Inc.
      *
      * Mode <> 'D' - prints all duplicate Active payrolls to report.
      *               Does not delete payrolls.
      *
      * Mode =  'D' - will delete inactive duplicate payrolls
      *               prints deleted payrolls to report. Count does
      *               not equal number of deleted records
      *               prints duplicate active payrolls to report
      *
      * If you need to call this program with Mode = 'D' , delete the
      * condition  "where epstat = 'A'" from line no 47 . This condition has been
      * added for KRMS
      *
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp('QILE')
     h optimize(*none)

     fqsysprt   o    f   80        printer oflind(*inof)
     fqsysprt1  o    f   80        printer oflind(*inoa)
     d kco#            s              3  0
     d kfnd            s              3  0
     d kemp#           s              9  0
     d kdiv            s              5  0
     d kfyr            s              3  0
     d kclas           s              5  0
     d kspec           s              1
     d kext            s              5
     d kefdt           s              7  0
     d kcnt            s              7  0
     d moreRows        s               n   inz(*on)
     d title           s             40
     d title1          s             40
     d deleteInactive  pr
     d printDeletes    pr
     d count                               like(kcnt)
     d printActives    pr
     d printDupes      pr

     c     *entry        plist
     c                   parm                    qqlib            10
     c                   parm                    qqmode            1

     c                   eval      title  = qqlib
     c                   eval      title1 = qqlib

     c/exec SQL
     c+  declare dupPayroll scroll cursor for
     c+     select epco#, epfnd, epemp#, epdiv, epfyr, epclas, epspec, epext,
     c+            epefdt, count(*)
     c+     from wmepp  where epstat = 'A'
     c+     group by epco#, epfnd, epemp#, epdiv, epfyr, epclas, epspec, epext,
     c+              epefdt
     c+     order by epco#, epfnd, epemp#, epdiv, epfyr, epclas, epspec, epext,
     c+              epefdt
     c+  for read only
     c/end-exec

     c/exec SQL
     c+    open  dupPayroll
     c/end-exec

     c                   eval      moreRows = *on
     c                   eval      *inoa    = *on
     c                   eval      *inof    = *on
     c                   dow       moreRows = *on

     c/exec SQL
     c+    fetch next from dupPayroll
     c+               into :kco#, :kfnd, :kemp#, :kdiv, :kfyr, :kclas,
     c+                    :kspec, :kext ,:kefdt, :kcnt
     c/end-exec

     c                   select
     c                   when      SQLstt = '02000'
     c                   eval      moreRows = *off
     c                   when      kcnt > 1
     c                   callp     deleteInactive
     c                   endsl

     c                   enddo

     c/exec SQL
     c+    close dupPayroll
     c/end-exec

     c                   seton                                        lr

     oqsysprt   e            head        6  2
     o                       title               60
     o          e            head           2
     o                                           +2 'Grp'
     o                                           +2 'Employer#'
     o                                           +2 '  Div'
     o                                           +2 'Fyr'
     o                                           +2 'Class'
     o                                           +2 ' '
     o                                           +2 ' Eff Dt'
     o                                           +2 '  Count'
     o          e            detail         1
     o                       kfnd          3     +2
     o                       kemp#         3     +2
     o                       kdiv          3     +2
     o                       kfyr          3     +2
     o                       kclas         3     +2
     o                       kspec               +2
     o                       kext                +2
     o                       kefdt         3     +2
     o                       kcnt          3     +2
     oqsysprt1  e            head1       6  2
     o                       title1              60
     o          e            head1          2
     o                                           +2 'Grp'
     o                                           +2 'Employer#'
     o                                           +2 '  Div'
     o                                           +2 'Fyr'
     o                                           +2 'Class'
     o                                           +2 ' '
     o                                           +2 '      Eff Dt'
     o                                           +2 '      Count'
     o          e            detail1        1
     o                       kfnd          3     +2
     o                       kemp#         3     +2
     o                       kdiv          3     +2
     o                       kfyr          3     +2
     o                       kclas         3     +2
     o                       kspec               +2
     o                       kext                +2
     o                       kefdt         3     +2
     o                       kcnt          3     +2
      *========================================================================
      * deleteInactive - if pgm is called in mode = 'D', it will delete the
      *                  inactive payrolls and write them to the delete report
      *                  If an inactive reocrd doesn't exist, it will write
      *                  to the Active Duplicate report. If not called in
      *                  'D' mode, it prints a report of all duplicates.
      *========================================================================
     p deleteInactive  b
     d deleteInactive  pi
     d deleteCnt       s                   like(kcnt)

     c                   if        qqmode = 'D'
     c                   eval      deleteCnt = 0

     c/exec SQL
     c+ select count(*) into :deleteCnt from WMEPP
     c+      where epco#  = :kco#  and
     c+            epfnd  = :kfnd  and
     c+            epemp# = :kemp# and
     c+            epdiv  = :kdiv  and
     c+            epfyr  = :kfyr  and
     c+            epclas = :kclas and
     c+            epspec = :kspec and
     c+            epext  = :kext  and
     c+            epefdt = :kefdt and
     c+            epstat = 'I'
     c+   group by epco#,
     c+            epfnd,
     c+            epemp#,
     c+            epdiv,
     c+            epfyr,
     c+            epclas,
     c+            epspec,
     c+            epext,
     c+            epefdt
     c/end-exec

     c/exec SQL
     c+ delete from wmepp
     c+   where epco#  = :kco#  and
     c+         epfnd  = :kfnd  and
     c+         epemp# = :kemp# and
     c+         epdiv  = :kdiv  and
     c+         epfyr  = :kfyr  and
     c+         epclas = :kclas and
     c+         epspec = :kspec and
     c+         epext  = :kext  and
     c+         epefdt = :kefdt and
     c+         epstat = 'I'
     c/end-exec

     c                   if        SQLstt    = '00000'                          successful delete
     c                   callp     printDeletes(deleteCnt)
     c                   endif
     c                   callp     printActives

     c                   else
     c                   callp     printDupes
     c                   endif

     p deleteInactive  e
      *========================================================================
      * printDeletes
      *========================================================================
     p printDeletes    b
     d printDeletes    pi
     d deleteCnt                           like(kcnt)
     c                   eval      title = %trimr(title) + ' Deleted Payrolls'
     c                   if        *inof
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c                   eval      kcnt = deleteCnt
     c                   except    detail
     p printDeletes    e
      *========================================================================
      * printActives
      *========================================================================
     p printActives    b
     d printActives    pi
     d activeCnt       s                   like(kcnt)
     c                   eval      activeCnt = 0
     c/exec SQL
     c+ select count(*) into :activeCnt from WMEPP
     c+      where epco#  = :kco#  and
     c+            epfnd  = :kfnd  and
     c+            epemp# = :kemp# and
     c+            epdiv  = :kdiv  and
     c+            epfyr  = :kfyr  and
     c+            epclas = :kclas and
     c+            epspec = :kspec and
     c+            epext  = :kext  and
     c+            epefdt = :kefdt and
     c+            epstat = 'A'
     c+   group by epco#,
     c+            epfnd,
     c+            epemp#,
     c+            epdiv,
     c+            epfyr,
     c+            epclas,
     c+            epspec,
     c+            epext ,
     c+            epefdt
     c/end-exec

     c                   eval      title1 = %trimr(title1)
     c                                    + ' Duplicate Active Payrolls'
     c                   if        *inoa
     c                   except    head1
     c                   eval      *inoa = *off
     c                   endif
     c                   if        activeCnt > 1
     c                   eval      kcnt = activeCnt
     c                   except    detail1
     c                   endif
     p printActives    e
      *========================================================================
      * printDupes
      *========================================================================
     p printDupes      b
     d printDupes      pi
     c                   eval      title = %trimr(title)
     c                                   + ' Duplicate Payrolls'
     c                   if        *inof
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     c                   except    detail
     p printDupes      e
      *=========================================================================
