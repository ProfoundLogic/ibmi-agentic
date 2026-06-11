     hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp('QILE')
     *=================================================================================
    h/TITLE W8146R - PA SECURITY FUND PROCESSING                                      |
     *=================================================================================
     *  DESCRIPTION:                                                                  |
     *  CALLED FROM: W7944R (GROUP SELECT)                                            |
     *  CALLS TO: W8146C (SUBMITTER)                                                  |
     *  CREATED: 07/27/09      C:136     PROJECT: 114WUK                              |
     *================================================================================|
     *  REVISIONS                                                                     |
     *================================================================================
     fW8146F    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     d/copy scopypr01d
     *================================================================================
     * Stand Alone Fields - TOP
     *================================================================================
     d optnm           c                   const('PA Security Fund Processing')
     d comapi          c                   const('WA001R')
     d @@call          s              1
     d @@prds          s             40
     d xxcall          s                   like(@@call)
     d ##co#           s              3  0
     d #@co#           s              3
     d ##fnam          s             40
     d ##fnd           s              3  0
     d ##lvl           s              3  0
     d ##typ           s              1
     d ##outq          s             10
     d ##rndt          s              8
     d @@inkc          s                   like(@@call)
     d @@inkl          s                   like(@@call)
     d currDate        s               d   datfmt(*jul)
     d curJulDt        s              5
     d seq#            s              2  0
     *================================================================================
     * Data Structures
     *================================================================================
     d parmData        ds           200
     d runDtArea       ds            50
     d  runJulDt                      5
     d  runSeq#                       2
     *================================================================================
     * Prototypes
     *================================================================================
     d/copy sprdate
     *================================================================
     *  PLIST definitions
     *================================================================
     c     *entry        plist
     c                   parm      *inkc         @@inkc
     c                   parm      *inkl         @@inkl
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
     c                   parm                    ##lvl
     c                   parm                    ##typ
     c                   parm                    ##outq
     *================================================================
     *  KLIST definitions
     *================================================================
     c/copy scopypr01c
     *====================================================================
     * DSCAPI - Call API for getting descriptions (use 91 on error)
     *====================================================================
     c     DSCAPI        begsr
     c                   call      comapi        compl                  91
     c                   endsr
     *====================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *====================================================================
     c     ERRCK1        begsr
     c                   eval      scrndt = dtCheck(scrndt)
     c                   if        scrndt = 0
     c                   seton                                        71  90
     c                   endif
     c                   endsr
     *====================================================================
     * KJDO   - Processing for F10
     *====================================================================
     c     KJDO          begsr
     c                   endsr
     *====================================================================
     * OTHFKS - Processing for other functions keys
     *====================================================================
     c     OTHFKS        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     c                   endsr
     *====================================================================
     * PGMEND - End of program processing
     *====================================================================
     c     PGMEND        begsr
     c                   seton                                        lr
     c                   endsr
     *====================================================================
     * PGMSTR - Used for any start of program processing (one-time)
     *====================================================================
     c     PGMSTR        begsr
     c     *dtaara       define    waxmlda       parmData
     c                   dow       *in90 = *off
     c                   in        parmData
     c                   if        parmData <> *blanks
     c                   exfmt     errWdw
     c                   if        *inkc = *on
     c                   return
     c                   endif
     c                   if        *inkg = *on
     c                   iter
     c                   endif
     c                   endif
     c                   eval      *in90 = *on
     c                   enddo
     c                   endsr
     *====================================================================
     * PREDSP - Pre-processing before displaying (every time)
     *====================================================================
     c     PREDSP        begsr
     c                   endsr
     *====================================================================
     * PRSCTL - Process control after no errors found (Call Submitter)
     *====================================================================
     c     PRSCTL        begsr
      *
      * Populate W8146DTA with the current date and latest sequence number.
      * This will be read by WAXMLR to populate WZP1P/P1DHTRBCHN.
      *
     c     *dtaara       define    w8146dta      runDtArea
     c                   eval      currDate = SystemDate
     c     *jul0         movel     currDate      curJulDt
     c     *lock         in        runDtArea
     c                   if        runJulDt <> curJulDt
     c                   eval      runJulDt = curJulDt
     c                   eval      runSeq#  = '01'
     c                   else
     c                   move      runSeq#       seq#
     c                   eval      seq# = seq# + 1
     c                   move      seq#          runSeq#
     c                   endif
     c                   out       runDtArea
      *
      * Populate WAXMLDA.  WAXMLR uses the info.
      *
     c                   move      ##co#         #@co#
     c                   move      scrndt        ##rndt
     c     *lock         in        parmData
     c                   movel     scrndt        parmData
     c                   out       parmData
     c                   call      'W8146C'
     c                   parm                    #@co#
     c                   parm                    ##rndt
     c                   parm                    ##outq
     c                   move      *on           *inkc
     c                   endsr
