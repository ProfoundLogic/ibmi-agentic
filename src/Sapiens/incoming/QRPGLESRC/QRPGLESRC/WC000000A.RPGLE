     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE WC000000A - Create endorsement WC000000.A
    H*********************************************************************
    H* Created: 11/08/05    Programmer: C:33    Project: 139WAJ          *
    H*********************************************************************
    H* Revisions                                                         *
    H*                                                                   *
     H* 12/08/10 C:61  Project: 132WOM  Priss 35                          *
     H*                         Write DISCLOSR form prior WC000000A.      *
    H*                                                                   *
     H* 09/17/10 C:220 Project: 132WOM                                    *
     H*                         Write WC000000A files in 4 ways           *
    H*                         - For fund 105, AFG policies              *
    H*                         - For fund 106, AFICA policies            *
    H*                         - For fund 107, AFN polices               *
    H*                         - For existing processing                 *
    H*                                                                   *
    H*********************************************************************
     FWC000000A o    e             printer oflind(*in98)
     F                                     infds(fids01)
     D fids01        e ds                  extname(szq4p)
     D psds          esds                  extname(szq1p)
     D*
    D**** Beg Add *** 09/17/10 *******************************************
     D xcplb           c                   const('*LIBL')
     D xcvrz           c                   const(00.200)
     D xchrz           c                   const(00.200)
     D Y               s              1s 0
     D qqfnd           s              3
     D xxfnd           s              3
    D**** End Add *** 09/17/10 *******************************************
    C*
    C* Add an entry parameter to acctept curre
    C*
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C*
     C                   call      'RTVCLTID'
     C                   parm                    xxclt             3
     C*
     C                   movel     qqfnd         xxfnd
     C*
    C* Process policies in 4 ways
    C**** Beg Add *** 09/17/10 *******************************************
     C                   select
     C* Process AFG polices
     C                   when      xxfnd = '105' and xxclt = '132'
     C* Print QUICKREF
     C                   eval      xpnam = 'PGQKRF01'
     C                   write     wcpsg0001
     C                   write     newpage01
     C**** Beg Add *** 12/08/10 *******************************************
     *
      * Print DISCLOSR
     C                   eval      xpnam = 'PGDSCL01'
     C                   write     wcpsg0001
     C                   write     newpage01
     *
     **** End Add *** 12/08/10 *******************************************
    C*
     C                   eval      Y = 1
     C* Print 6 pages AFN for policies
     C                   do        6
    C*
     C                   eval      xpnam = 'PGFN00' + %editc(Y:'Z')
     C                   write     wcpsg0001
     C                   write     newpage01
    C*
     C                   eval      Y = Y + 1
     C                   enddo
     **** Beg Del *** 12/08/10 *******************************************
     *******
      * Print DISCLOSR
    C*******            eval      xpnam = 'PGDSCL01'
    C*******            write     wcpsg0001
    C*******            write     newpage01
     **** End Del *** 12/08/10 *******************************************
    C*
     C* Process AFICA polices
    C*
     C                   when      xxfnd = '106' and xxclt = '132'
     C* Print QUICKREF
     C                   eval      xpnam = 'PGQKRF01'
     C                   write     wcpsg0001
     C                   write     newpage01
     C**** Beg Add *** 12/08/10 *******************************************
     *
      * Print DISCLOSR
     C                   eval      xpnam = 'PGDSCL01'
     C                   write     wcpsg0001
     C                   write     newpage01
     *
     **** End Add *** 12/08/10 *******************************************
    C*
     C                   eval      Y = 1
     C* Print 6 pages AFICA for policies
     C                   do        6
    C*
     C                   eval      xpnam = 'PGCA00' + %editc(Y:'Z')
     C                   write     wcpsg0001
     C                   write     newpage01
    C*
     C                   eval      Y = Y + 1
     C                   enddo
     **** Beg Del *** 12/08/10 *******************************************
     *******
      * Print DISCLOSR
    C*******            eval      xpnam = 'PGDSCL01'
    C*******            write     wcpsg0001
    C*******            write     newpage01
     **** End Del *** 12/08/10 *******************************************
    C*
     C* Process AFG polices
    C*
     C                   when      xxfnd = '107' and xxclt = '132'
     C* Print QUICKREF
     C                   eval      xpnam = 'PGQKRF01'
     C                   write     wcpsg0001
     C                   write     newpage01
     C**** Beg Add *** 12/08/10 *******************************************
     *
      * Print DISCLOSR
     C                   eval      xpnam = 'PGDSCL01'
     C                   write     wcpsg0001
     C                   write     newpage01
     *
     **** End Add *** 12/08/10 *******************************************
    C*
     C                   eval      Y = 1
     C* Print 6 pages for AFN policies
     C                   do        6
    C*
     C                   eval      xpnam = 'PGFG00' + %editc(Y:'Z')
     C                   write     wcpsg0001
     C                   write     newpage01
    C*
     C                   eval      Y = Y + 1
     C                   enddo
     **** Beg Del *** 12/08/10 *******************************************
     *******
      * Print DISCLOSR
    C*******            eval      xpnam = 'PGDSCL01'
    C*******            write     wcpsg0001
    C*******            write     newpage01
     **** End Del *** 12/08/10 *******************************************
    C*
     C                   other
     C* Process rest of the polices
    C**** End Add *** 09/17/10 *******************************************
     C                   eval      page# = 1
     C                   write     headLine
     C                   write     headOdd
     C                   write     ctrLine1
     C                   write     wc0000001
     C                   write     footer
    C*
     C                   write     newPage
    C*
     C                   eval      page# = 2
     C                   write     headLine
     C                   write     headEven
     C                   write     ctrLine
     C                   write     wc0000002
     C                   write     footer
    C*
     C                   write     newPage
    C*
     C                   eval      page# = 3
     C                   write     headLine
     C                   write     headOdd
     C                   write     ctrLine
     C                   write     wc0000003
     C                   write     footer
    C*
     C                   write     newPage
    C*
     C                   eval      page# = 4
     C                   write     headLine
     C                   write     headEven
     C                   write     ctrLine
     C                   write     wc0000004
     C                   write     footer
    C*
     C                   write     newPage
    C*
     C                   eval      page# = 5
     C                   write     headLine
     C                   write     headOdd
     C                   write     ctrLine
     C                   write     wc0000005
     C                   write     footer
    C*
     C                   write     newPage
    C*
     C                   eval      page# = 6
     C                   write     headLine
     C                   write     headEven
     C                   write     ctrLine
     C                   write     wc0000006
     C                   write     footer
    C*
    C**** Beg Add *** 09/17/10 *******************************************
     C                   endsl
    C**** End Add *** 09/17/10 *******************************************
    C*
     C                   move      *on           *inlr
