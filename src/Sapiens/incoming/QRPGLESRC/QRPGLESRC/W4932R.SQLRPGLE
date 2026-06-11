      * (C) Copyright Progressive Data Solutions, Inc., 2006
      *=========================================================================
      /title W4931R - FL Premium with Deductible Report
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *=========================================================================
      *  CALLED FROM: Command line
      *
      *  CREATED 08/19/06   C:136   PROJECT: 139WEE
      *=========================================================================
      * REVISIONS:
      *
      *=========================================================================
     fwdell     if   e           k disk
     fwablp     o    e             disk

     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d xxco#           s                   like(BLCO#)
     d xxfnd           s                   like(BLFND)
     d xxemp#          s                   like(BLEMP#)
     d xxdiv           s                   like(BLDIV)
     d xxfyr           s                   like(BLFYR)
     d psds          esds                  extname(szq1p)
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     c     elky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    xxfyr

     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    
     c     *entry        plist
     c                   parm                    @@rpdt            7 0

     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         BLDT
     c                   movel     dstime        BLTIME
     c                   eval      BLPGM  = Q1PGM
     c                   eval      BLUSER = Q1USER

     c/exec SQL
     c+                  declare selectPolicies cursor
     c+                    for select
     c+                          distinct euco#, eufnd, euemp#, eudiv, eufyr
     c+                      from wfeup
     c+                      order by euco#, eufnd, euemp#, eudiv, eufyr
     c/end-exec

     c/exec sql
     c+                  open selectPolicies
     c/end-exec

     c                   eval      SqlStt = '00000'
     c                   dow       SqlStt = '00000'
     c/exec sql
     c+                  fetch next
     c+                    from selectPolicies
     c+                    into :xxco#,
     c+                         :xxfnd,
     c+                         :xxemp#,
     c+                         :xxdiv,
     c+                         :xxfyr
     c/end-exec

     c                   if        SqlStt <> '00000'
     c                   leave
     c                   endif

     c     elky          chain     pel01
     c                   eval      BLCO#  = xxco#
     c                   eval      BLFND  = xxfnd
     c                   eval      BLEMP# = xxemp#
     c                   eval      BLDIV  = xxdiv
     c                   eval      BLFYR  = xxfyr
     c                   eval      BLFLG1 = 'Y'
     c                   eval      BLFLG2 = ' '
     c                   eval      BLFLG3 = 'Y'
     c                   eval      BLSTAT = ' '
     c                   eval      BLRFLG = 'N'
     c                   eval      BLPFDT = ELPFDT
     c                   write     pbl01
     c                   enddo

     c/exec sql
     c+                  close selectPolicies
     c/end-exec

     c                   seton                                        lr
