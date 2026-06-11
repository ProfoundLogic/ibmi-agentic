     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *********************************************************************
    H/TITLE
     *********************************************************************
     *  DESCRIPTION: Check Reconcillation Import Prompt                  *
     *                                                                   *
     *  CALLED FROM: Menu                                                *
     *                                                                   *
     *  CALLS TO:                                                        *
     *                                                                   *
     *  CREATED: 01/05/06      C:167     PROJECT: 136WBP                 *
     *********************************************************************
     *  REVSIONS:                                                        *
     *                                                                   *
     *********************************************************************
    Hdebug(*yes) option(*srcstmt:*nodebugio)
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Fw4250f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
     Fwmfnl     if   e           k disk
     Fsmcol     if   e           k disk
     Fwmupl     if   e           k disk
     *
     D/COPY SCOPYPR01D
     D sczp            s              3  0
     D srdfld          s             80
     D srdlen          s              3  0
     *
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D optnm           c                   const('Title')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D qqco#           s              3
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     C/COPY SCOPYDTPL
     *
     *================================================================
     *  PLIST definitions
     *================================================================
     *
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     *
     *================================================================
     *  KLIST definitions
     *================================================================
     *
     C/COPY SCOPYPR01C
     *
     *====================================================================
     * DSCAPI - Call API for getting descriptions (use 91 on error)
     *====================================================================
     *
     C     DSCAPI        begsr
     *
     C                   call      comapi        compl                  91
     *
     C                   endsr
     *
     *====================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *====================================================================
     *
     C     ERRCK1        begsr
     C                   endsr
     *
     *====================================================================
     * KJDO   - Processing for F10
     *====================================================================
     *
     C     KJDO          begsr
     C                   endsr
     *
     *====================================================================
     * OTHFKS - Processing for other functions keys
     *====================================================================
     *
     C     OTHFKS        begsr
     *
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
      *   process all command keys
     C                   if        *in11 = *on
      *   F03 = end of job
     C                   if        *inkc = *on
     C                   eval      *inlr = *on
     C                   endif
      *
     C                   endif
     *
     C                   endsr
     *
     *====================================================================
     * PGMEND - End of program processing
     *====================================================================
     *
     C     PGMEND        begsr
     C                   endsr
     *
     *====================================================================
     * PGMSTR - Used for any start of program processing (one-time)
     *====================================================================
     *
     C     PGMSTR        begsr
     *
     C                   if        %parms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
     *
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     *
     C                   movel     ##data        scprds
     *
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
     *
     C                   endsr
     *
     *====================================================================
     * PREDSP - Pre-processing before displaying (every time)
     *====================================================================
     *
     C     PREDSP        begsr
     *
     C                   eval      *in90 = *off
     C                   exsr      dscapi
     C                   eval      scconm = ##conm
     *
     C                   endsr
     *
     *====================================================================
     * PRSCTL - Process control after no errors found (Call Submitter)
     *====================================================================
     *
     C     PRSCTL        begsr
      *
      *   call the clp to submit the list.
     C                   move      ##co#         qqco#
     C                   call      'W4250C'
     C                   parm                    qqco#
      *
     C                   endsr
