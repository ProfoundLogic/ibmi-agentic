     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=========================================================================
      /title W4931R - FL Premium with Deductible Report
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *=========================================================================
      *  CALLED FROM: W4931C
      *
      *  CREATED 08/19/06   C:136   PROJECT: 139WEE
      *=========================================================================
      * REVISIONS:
      *
      * 08/05/15 C:307 Project: SY354                                 *
      *                -Adding 'Route To:'                            *
      *                                                               *
     h* 08/26/14 C:307 Project: SY354
     h*                -Implement Routing Page Changes.
     h*
      *  01/28/10  C:249 Project: SY215
      *            C:189 Retrieve brand Name
      *                                                               *
      *  08/24/06
      *     TEMPORARY CHANGE TO USE TEMPORARY LIBRARY PDSFILMD INSTEAD OF PDSTST
      *
      *=========================================================================
     fwfeul1    if   e           k disk
     fwdehl6    if   e           k disk
     fsmorl     if   e           k disk
     fwffdp     o    e             disk
     fqsysprt   o    f   80        printer oflind(*inof) usropn
    d*=========================================================================
    d* Stand Alone Fields - TOP
    d*=========================================================================
     d kkco#           s                   like(EUCO#)
     d svfnd           s                   like(EUFND)
     d svemp#          s                   like(EUEMP#)
     d svdiv           s                   like(EUDIV)
     d svfyr           s                   like(EUFYR)
     d kkrpdt7         s                   like(EHDT)
     d xxWrtnWDed      s                   like(EUYDWR)
     d xxWrtnWoDed     s                   like(EUYDWR)
     d xxYTDEarnWDed   s                   like(EUYDWR)
     d xxYTDEarnWoDed  s                   like(EUYDWR)
     d xxManWDed       s                   like(EHWMPR)
     d xxManWoDed      s                   like(EHWMPR)
     d xxerr           s                   like(EHFAF)
     d xxRatio         s              9  6
     d wkDate          s               d
     d SqlStmtStr      s            256a
      *=========================================================================
      * Error Report Variables
      *=========================================================================
     d suffix          s              3
     d testLib         s             10
     d x               s              1  0
     d errMessage      s             40
     d firstWrite      s              1    inz('Y')
     d companyNm       s             60
     d userName        s             50
     d UPUSNM          s                   like(userName)
     d xxque           s                   like(@@que)
     d line            s             80
     d xxfil#          s                   like(ORFIL#)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d title           c                   'FL Premium W/O Deductible Error -
     d                                     Report'
      *=========================================================================
      * Mnemonics to increase readability
      *=========================================================================
     d true            c                   const( '1' )
     d false           c                   const( '0' )
     d sqlSttOk        c                   const( '00000' )
     d sqlSttNoRow     c                   const( '02000' )
     d sqlSttDupKey    c                   const( '23505' )
    d*=========================================================================
    d* Data Structures and Arrays
    d*=========================================================================
     d psds          esds                  extname(szq1p)
    d*=========================================================================
    d* Procedure Prototypes
    d*=========================================================================
     d*entry
     d W4931R          pr
     d  @@co#                         3
     d  @@rpdt                        8
     d  @@que                        10
     d W4931R          pi
     d  @@co#                         3
     d  @@rpdt                        8
     d  @@que                        10
     d writeDetail     pr
     d getWrtnWDed     pr
     d getWrtnWoDed    pr
     d writeError      pr
     d getSuffix       pr                  extpgm('SA030R')
     d  suffix                        3
     d/copy sprdate
     d/copy sprcompany
     d/copy sprstring
     d/copy wprusrprms
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    c*=========================================================================
      *  KLIST's
    c*=========================================================================
     c     euky          klist
     c                   kfld                    kkco#

     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#
    c*=========================================================================
      *  MAINLINE PROCESSING
    c*=========================================================================
     c                   move      @@co#         kkco#
     c                   eval      wkDate = cvtToDate(numDate(@@rpdt))
     c     *iso          move      wkDate        FDRPDT
     c     *cymd         move      wkDate        kkrpdt7
     c                   exsr      prepCursor
      *
     c     euky          reade     peu01
     c                   eval      svfnd  = EUFND
     c                   eval      svemp# = EUEMP#
     c                   eval      svdiv  = EUDIV
     c                   eval      svfyr  = EUFYR
     c                   dow       not(%eof(WFEUL1))
     c                   if        svfnd  <> EUFND  or
     c                             svemp# <> EUEMP# or
     c                             svdiv  <> EUDIV  or
     c                             svfyr  <> EUFYR
     c                   callp     writeDetail
     c                   eval      svfnd  = EUFND
     c                   eval      svemp# = EUEMP#
     c                   eval      svdiv  = EUDIV
     c                   eval      svfyr  = EUFYR
     c                   eval      xxYTDEarnWDed  =  EUYDEA
     c                   else
     c                   eval      xxYTDEarnWDed  = xxYTDEarnWDed + EUYDEA
     c                   endif
     c     euky          reade     peu01
     c                   if        %eof(WFEUL1)
     c                   callp     writeDetail
     c                   endif
     c                   enddo

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If firstWrite = 'Y';
       xxfil# = '0';
       Chain orky POR01;
       If %found;
          Except Route;
       Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   seton                                        lr
      *=========================================================================
      * prepCursor
      *=========================================================================
     c     prepCursor    begsr

     c*****              callp     getSuffix(suffix)
     c*****              eval      testLib = 'PDSTST' + suffix
     c                   eval      testLib = 'PDSFILMD'
     c                   eval      sqlStmtStr =
     c                              'select EHWMPR, EHWAPR -
     c                             from ' + %trim(testLib) + '/' + 'WDEHPWODED -
     c                             where EHCO#  = ? and -
     c                             EHFND  = ? and -
     c                             EHEMP# = ? and -
     c                             EHDIV  = ? and -
     c                             EHFYR  = ? and -
     c                             absval(EHWMPR - ?) <= 25'

     c/exec SQL
     c+                  declare   selectWoDed cursor
     c+                    for dynSqlStmt
     c/end-exec

     c/exec SQL
     c+                  prepare   dynSqlStmt
     c+                    from :sqlStmtStr
     c/end-exec

     c                   if        SqlStt <> sqlSttOk
     c                   eval      x=3
     c                   callp     writeError
     c                   seton                                        lr
     c                   return
     c                   endif

     c                   endsr
      *=========================================================================
      * Output files
      *=========================================================================
     o/copy scopyrout

     oqsysprt   e            route          1

     o*** Beg Chg *****  01/28/10  ************************************
     o*****     e            route          1 55
     o***                                           '(C) Copyright Fiserv ISD'
     o***                                     +   0 ', Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** End Chg *****  01/28/10  ************************************
    o*
    o* Report Heading
    o*
     o          e            head           1 06
     o                       sys_time
     o                       companyNm           70
     o                       sys_date            80
     o          e            head           2
     o                       q1pgm
     o                       ORTITL              70
     o                                           75 'PAGE:'
     o                       page          z     80
     o          e            head           0
     o                       line                80
     o          e            head           2
     o                                            5 'Grp'
     o                                           16 'Emp#'
     o                                           23 'Div'
     o                                           28 'Fyr'
     o                                           42 'Error reason'
     o          e            detail         1
     o                       svfnd         4      5
     o                       svemp#        4     16
     o                       svdiv         3     23
     o                       svfyr         4     28
     o                       errMessage          70
     p*=========================================================================
     p* writeDetail - write to WFFDP
     p*=========================================================================
     p writeDetail     b
     d writeDetail     pi

     c                   if        xxYTDEarnWDed <> 0
     c                   eval      FDCO#  = kkco#
     c                   eval      FDFND  = svfnd
     c                   eval      FDEMP# = svemp#
     c                   eval      FDDIV  = svdiv
     c                   eval      FDFYR  = svfyr
     c                   eval      FDSEQ# = FDSEQ# + 1
     c                   eval      FDEAWD = xxYTDEarnWDed
     c                   callp     getWrtnWDed
     c                   eval      FDWPWD = xxWrtnWDed
     c                   eval      FDWPWO = xxWrtnWoDed
     c                   if        FDWPWD <> 0
     c                   eval      FDRATI = FDWPWO / FDWPWD
     c                   else
     c                   eval      FDRATI = 0
     c                   endif
     c                   if        FDRATI <> 0
     c                   eval(h)   FDEAWO = FDEAWD * FDRATI
     c                   else
     c                   eval      FDEAWO = FDEAWD
     c                   endif
     c                   eval      FDMPWD = xxManWDed
     c                   eval      FDMPWO = xxManWoDed
     c                   eval      FDERRF = xxerr
     c                   eval      FDIUSR = Q1USER
     c                   eval      FDIPGM = Q1PGM
     c                   eval      FDIDT  = dateTo7(SystemDate)
     c                   eval      FDITIM = timeTo6(SystemTime)
     c                   write     PFD0P
     c                   endif

     p writeDetail     e
     p*=========================================================================
     p* getWrtnWDed - returns written premium with deductible discount
     p*=========================================================================
     p getWrtnWDed     b
     d getWrtnWDed     pi

     c     ehky          klist
     c                   kfld                    kkco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr

     c     ehky1         klist
     c                   kfld                    kkco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
     c                   kfld                    kkrpdt7

     c                   eval      xxerr       =  ' '
     c                   eval      xxManWDed   =  0
     c                   eval      xxManWoDed  =  0
     c                   eval      xxWrtnWDed  =  0
     c                   eval      xxWrtnWoDed =  0
     c     ehky          setll     peh01
     c     ehky          reade     peh01
     c                   dow       not(%eof(WDEHL6))
     c                   if        EHWAPR <> 0
     c                   if        EHWDCT <> 0 or
     c                             EHWDC2 <> 0
     c                   eval      xxWrtnWDed  =  EHWAPR
     c                   eval      xxManWDed   =  EHWMPR
     c                   callp     getWrtnWoDed
     c                   if        xxManWDed   =  xxManWoDed
     c                   leave
     c                   endif
     c                   else
     c                   eval      xxWrtnWDed  = EHWAPR
     c                   eval      xxWrtnWoDed = xxWrtnWDed
     c                   endif
     c                   endif
     c     ehky          reade     peh01
     c                   enddo

     c                   if        xxWrtnWDed  = 0
     c                   eval      xxerr       =  '1'
     c                   eval      x = 1
     c***                callp     writeError
     c                   endif

     c                   if        xxWrtnWDed <> 0 and
     c                             xxWrtnWoDed = 0
     c                   eval      xxerr       =  '2'
     c                   eval      x = 2
     c***                callp     writeError
     c                   endif

     p getWrtnWDed     e
     p*=========================================================================
     p* getWrtnWoDed - returns written premium without deductible discount
     p*=========================================================================
     p getWrtnWoDed    b
     d getWrtnWoDed    pi

     c/exec sql
     c+                    open selectWoDed
     c+                      using :kkco#,
     c+                            :svfnd,
     c+                            :svemp#,
     c+                            :svdiv,
     c+                            :svfyr,
     c+                            :xxManWDed
     c/end-exec

     c/exec sql
     c+                    fetch next
     c+                      from selectWoDed
     c+                      into  :xxManWoDed,
     c+                            :xxWrtnWoDed
     c/end-exec

     c                   if        sqlstt <> '00000'
     c                   eval      xxWrtnWoDed = 0
     c                   endif

     c/exec sql
     c+                    close selectWoDed
     c/end-exec

     p getWrtnWoDed    e
     p*=========================================================================
     p* WriteError - Writes Error Report
     p*=========================================================================
     p writeError      b
     d writeError      pi

     c                   if        firstWrite = 'Y'
     c                   open      qsysprt
     c                   eval      *inof = *on
     c                   eval      firstWrite = 'N'
     c                   eval      companyNm = GetCompNam(kkco#)
     c                   eval      companyNm = centerFld(companyNm)
     c     @@que         comp      *blank                             090908
     c                   eval      xxque    = @@que
     c                   eval      userName = getUsrName(q1user)
     c                   eval      UPUSNM   = userName
     c                   move      *all'_'       line
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     *
     * Get the distribution information for the error report.
     *
     c                   clear                   ORTITL
     c                   eval      xxfil#   = '0'
     c     orky          chain     por01                              60
     c                   if        ORTITL = *blank
     c                   eval      ORTITL = title
     c                   endif

     c                   eval      ORTITL = leftJstify(ORTITL)
     c                   if        not *in60
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endif

     c                   eval      ORTITL = centerFld(ORTITL)
     c                   endif

     c                   if        *inof = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif

     c                   select
     c                   when      x = 1
     c                   eval      errMessage = 'No written premium -
     c                             with ded'
     c                   when      x = 2
     c                   eval      errMessage = 'No written premium -
     c                             without ded'
     c                   when      x = 3
     c                   eval      errMessage = 'SQL prepare statement failed.'
     c                   endsl

     c                   except    detail

     p writeError      e
