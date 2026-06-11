     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE POPAGTHST - Populate Agent History.
     H*
     Fwdell     if   e           k disk
     Fwmahl     if a e           k disk

     d psds          esds                  extname(szq1p)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)

     D svfyr           s              3  0
     c*
     c     ahkey1        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c*  Reaf Employer Detail Records

     c                   move      *zeros        kkco#             3 0
     c                   move      *zeros        kkfnd             3 0
     c                   move      *zeros        kkemp#            9 0
     c                   move      *zeros        kkdiv             5 0

     c                   read      pel01                                  60

     c                   dow       *in60 = *off

     c     *hms          move      q1time        sys_time
     c     *usa          move      *date         sys_date

     c                   if        (elfnd <> kkfnd or elemp# <> kkemp# or
     c                              eldiv <> kkdiv)


     c                   eval      kkco# = elco#
     c                   eval      kkfnd = elfnd
     c                   eval      kkemp#= elemp#
     c                   eval      kkdiv= eldiv

     c     ahkey1        setll     pah01                              61
     c     ahkey1        reade(n)  pah01                                  61
     c  N61              eval      svfyr = ahfyr
     c                   endif

     c                   if        *in61 = *off AND svfyr <> elfyr
     c                   eval      ahfyr  = elfyr                               POLICY YEAR
     c                   eval      ahefdt = elpfdt                              EFFECTIVE DATE
     c                   eval      ahstat = 'A'                                 STATUS
     c                   eval      ahuser = q1user                              USER
     c                   eval      ahpgm  = q1pgm                               PROGRAM
     c     *cymd         move      sys_Date      ahdt                           DATE MAINTAINED
     c                   move      sys_time      ahtime                         TIME
     c                   eval      ahiusr = ahuser                              INITIAL USER
     c                   eval      ahipgm = ahpgm                               INITIAL PROGRAM
     c                   eval      ahidt  = ahdt                                DATE MAINTAINED
     c                   eval      ahitm  = ahtime                              INITIAL TIME

     c                   write     pah01

     C                   endif                                                  *in61 = *off

     C                   read      pel01                                  60
     c                   enddo

     c                   eval      *inlr = *on
     C*
