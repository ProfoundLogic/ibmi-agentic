     hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp('QILE')
     h*****************************************************************
     h/TITLE W8005R5 - Fee invoice deletion
     h*****************************************************************
     h* PROGRAM DESCRIPTION:                                          *
     h*   This program delete fee invoices                            *
     h*                                                               *
     h* CALLED FROM:                                                  *
     h*   Menu                                                        *
     h* CALLS TO:                                                     *
     h*   NONE                                                        *
     h* PROGRAMMER: C:190                                             *
     h* CREATED: 05/20/10               PROJECT: WC1851               *
     h*****************************************************************
     h* INDICATORS:                                                   *
     h*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h* 08/29/11 C:65  Project:  WC1851                               *
     h*                Convert deletions to a submitted job.          *
     h*                                                               *
     h* 05/31/11 C:65  WC1851  PRISS 188/191                          *
     h*                Ensure A/R type is populated on records.       *
     h*                Combine a/r batch records for reversals.       *
     h*                                                               *
     h* 05/20/11 C:189 WC1851  PRISS 187                              *
     h*                Invoice Deletion process creating outrageously *
     h*                high dollar amounts.                           *
     h*                The offset batch is not getting trenafserred   *
     h*                for Originally transferred batch               *
     h*                                                               *
     h* 02/15/11 C:061 WC1851                                         *
     h*               -Use new subroutine DeleteARBtch to process     *
     h*                deletions.                                     *
     h*                                                               *
     h*****************************************************************
     fW8005F5   cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     fwmexl     if   e           k disk
     fwmfnl     if   e           k disk
     fwmfnp     if   e             disk    rename(pfn01 : pfn0p)
     fwmemp     if   e             disk    rename(pem01 : pem0p)
     fwdfcl4    if   e           k disk
     DsysDate7         S              7  0
     DsysTime6         s              6  0
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d optnm           c                   const('Fee Invoices Deletion')
     d comapi          c                   const('WA001R')
     d @@prds          s             25
     d ##co#           s              3  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d found           s               n   inz(*off)
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d svfnd           s                   like(exfnd)
     d kkfnd           s                   like(exfnd)
     d kkemp#          s                   like(ememp#)
     d kkpodt          s              7  0
     d kkdiv           s              5  0
     d rrnGrp          s              9  0
     d rrnEmp          s              9  0
     d error           s              1
     d inkc            s              1
     d inkl            s              1
     d xxpodt8         s              8  0
      *
      ******* end add ****** 02/15/11 *********************************
     *---------------------------------------------------------------------
     * Data Structures and Arrays
     *---------------------------------------------------------------------
     *---------------------------------------------------------------------
     * Prototypes
     *---------------------------------------------------------------------
     d/copy wprgroup
     d/copy wpremplr
     c/copy scopypr41c
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

     c     *entry        plist
     c                   parm                    @@prds
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
     c                   parm                    ##lvl             3 0
     c                   parm                    ##type            1
     c                   parm                    ##outq           10
     *================================================================
     *  KLIST definitions
     *================================================================

     c     exky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd

     c     fcky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd
     c                   kfld                    kkpodt

     c     fcky1         klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd
     c                   kfld                    kkpodt
     c                   kfld                    kkemp#

     c     fcky2         klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd
     c                   kfld                    kkpodt
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     C*
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

     c                   eval      found = validGrp(scfnd)
     c                   eval      *in71 = not(found)
     c                   eval      *in90 = *in71
     c                   if        scemp# <> *zeros and not(*in90)
     c                   move(p)   scdiv         xxdiv             5 0
     c                   eval      found = validEmp(scemp# :xxdiv:rrnemp:scfnd)
     c                   eval      *in72 = not(found)
     c                   eval      *in90 = *in72
     c                   endif
      *
      *
     c  n90              if        svfnd <> scfnd
     c                   eval      svfnd = scfnd
     c                   eval      kkfnd = scfnd
     c                   write     scr01
     c     exky          chain     pfn01
     c                   eval      scname = fnname
     c                   endif                                                  scfnd <> svfnd
      *
      * Validate Post Date.
      *
     c  n90              if        scpodt = *zeros
     c                   eval      *in74 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif

     c  n90              if        scpodt <> 0
     c                   z-add     scpodt        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scpodt
     c                   eval      KKpodt = dtCYMD(scpodt)
     c                   if        srerr = '1'
     c                   eval      *in76 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   endif
      *
     c*    If Employer field is blank then delete all the records of group with provided
     c*    Post date
     c  n90              if        scemp# = *zeros

     c     fcky          setll     pfc01
     c     fcky          reade     pfc01                                  60

     c* Check for existence of data in WDFCP
     c                   if        *in60 = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif                                                  *in60 = *on

     c                   endif                                                  scemp# = 0

     c*    If Employer field is not blank and Div is blank, then delete all records of
     c*    this employer.
     c  n90              if        scemp# <> *zeros and scdiv = *blanks
     c                   eval      kkemp# = scemp#
     c     fcky1         setll     pfc01
     c     fcky1         reade     pfc01                                  60

     c* Check for existence of data in WDFCP
     c                   if        *in60 = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif                                                  *in60 = *on

     c                   endif                                                  scemp# <> 0,scdiv=bl

     c*    If Employer and Div field is not blank, then delete specific record.
     c  n90              if        scemp# <> *zeros and scdiv <> *blanks
     c                   eval      kkemp# = scemp#
     c                   movel     scdiv         kkdiv
     c     fcky2         setll     pfc01
     c     fcky2         reade     pfc01                                  60

     c* Check for existence of data in WDFCP
     c                   if        *in60 = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif                                                  *in60 = *on

     c                   endif
      *
      * Submit job now that all errors are passed.
      *
     c                   move      ##co#         ZZco#
     c                   move      kkfnd         ZZfnd
     c                   move      scemp#        ZZemp#
     c                   move      scdiv         ZZdiv
     c                   move      scpodt        ZZpodt
      *
     c                   call      'W8005C5'
     c                   parm                    ZZco#             3
     c                   parm                    ZZfnd             3
     c                   parm                    ZZemp#            9
     c                   parm                    ZZdiv             5
     c                   parm                    ZZpodt            8
      *
     c                   endsr                                                  errck1
    c*====================================================================
    c* KJDO   - Processing for F10
    c*====================================================================
     c     KJDO          begsr
     c                   select
     c                   when      hfld = 'SCFND'
     c                   exsr      GroupSearch
     c                   when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     c                   exsr      EmplrSearch
     c                   endsl
     c                   endsr
    c*====================================================================
    c* OTHFKS - Processing for other functions keys
    c*====================================================================
     c     OTHFKS        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     c                   endsr
    c*====================================================================
    c* PGMEND - End of program processing
    c*====================================================================
     c     PGMEND        begsr
     c                   endsr
    c*====================================================================
    c* PGMSTR - Used for any start of program processing (one-time)
    c*====================================================================
     c     PGMSTR        begsr
     c                   movel(p)  optnm         ##data
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel     ##data        scprds
     c                   endsr


    c*====================================================================
    c* PREDSP - Pre-processing before displaying (every time)
    c*====================================================================
     c     PREDSP        begsr
     c                   endsr
    c*====================================================================
    c* Search groups
    c*====================================================================
     c     GroupSearch   begsr
     c                   eval      rrnGrp = GrpSrch(inkc:inkl:scFnd:##Co#)
     c                   if        rrnGrp <> *zero
     c     rrnGrp        chain     WMFNP
     c                   eval      scFnd  = FNFND
     c                   eval      scName = FNNAME
     c                   seton                                        32
     c                   endif                                                  WMFNP
     c                   if        rrnGrp = *zero
     c                   seton                                        31
     c                   endif                                                  WMFNP
     c                   endsr
    c*====================================================================
    c* Search employers
    c*====================================================================
     c     EmplrSearch   begsr
     c                   if        scFnd = *zero
     c                   exsr      GroupSearch
     c                   endif
     c                   clear                   rrnEmp
     c                   if        scFnd <> *zero
     c                   eval      rrnEmp = EmpSrch(inkc:inkl:error:
     c                                              scFnd:##Co#)
     c                   if        rrnEmp <> *zero
     c     rrnEmp        chain     WMEMP
     c                   eval      scEmp# = EMEMP#
     c                   eval      scDiv  = %editc(EMDIV:'3')
     c                   movel     scDiv         scDiv
     c                   setoff                                       31
     c                   seton                                        32
     c                   endif                                                  rrnEMP
     c                   endif                                                  scfnd <> 0
     c                   if        rrnEmp = *zero
     c                   seton                                        32
     c                   endif                                                  rrnEMP
     c                   endsr
    c*====================================================================
    c* PRSCTL - Process control after no errors found
    c*====================================================================
     c     PRSCTL        begsr
     c                   endsr
    c*====================================================================
