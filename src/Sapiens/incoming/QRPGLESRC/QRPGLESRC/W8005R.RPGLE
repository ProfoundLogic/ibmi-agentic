     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp('QILE')
     h*****************************************************************
     h/TITLE W8005R - Fee invoice process prompt
     h*****************************************************************
     h* PROGRAM DESCRIPTION:                                          *
     h*   This prompt program to invoke claim fee/collateral          *
     h*   invoice and detail report                                   *
     h*                                                               *
     h* CALLED FROM:                                                  *
     h*   NONE                                                        *
     h*                                                               *
     h* CALLS TO:                                                     *
     h*   W8005C                                                      *
     h*                                                               *
     h* PROGRAMMER: C:189                                             *
     h* CREATED: 01/23/10               PROJECT: WC1851               *
     h*****************************************************************
     h* INDICATORS:                                                   *
     h*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     h*****************************************************************
     h* Revisions:                                                    *
     H*                                                               *
     H* 08/02/11 C:061 Project WC2003                                 *
     H*                Default value of prompt for print of SSN.
     h*                                                               *
     h* 02/15/11  C:061 Project: WC1851                               *
     h*                 - Add 'Print/File/Both' prompt.               *
     h*                                                               *
     h* 05/20/10  C:190 Project: WC1851   Additional changes          *
     h*                                                               *
     h* 02/24/10  C:33  Project: WC1851   25WQL Priss #9              *
     h*           C:136 Re-write.  No comments.                       *
     h*****************************************************************

     fW8005F    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     fwtabl     if   e           k disk
     fwmexl     if   e           k disk
     fwmfnl     if   e           k disk
     fwmfnp     if   e             disk    rename(pfn01 : pfn0p)
     fwmemp     if   e             disk    rename(pem01 : pem0p)
     fstctl1    if   e           k disk
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d optnm           c                   const('Fee Invoices')
     d comapi          c                   const('WA001R')
     d @@call          s              1
     d @@prds          s             25
     d xxcall          s                   like(@@call)
     d ##co#           s              3  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d singleMode      s               n   inz(*off)
     d found           s               n   inz(*off)
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d sysDt           s               d
     d postDate        s               d
     d x7podt          s              7  0
     d sysDt7          s              7  0
     d xxyrm1          s              5  0
     d xxyrm2          s              5  0
     d ctr             s              2  0
     d kkky            s             10
     d svfnd           s                   like(exfnd)
     d kkfnd           s                   like(exfnd)
     d rrnGrp          s              9  0
     d rrnEmp          s              9  0
     d error           s              1
     d inkc            s              1
     d inkl            s              1
     *---------------------------------------------------------------------
     * Data Structures and Arrays
     *---------------------------------------------------------------------
     d                 ds
     d  comNts                 1      8
     d                                     dim(4)
     d   sccom1                1      2
     d   sccom2                3      4
     d   sccom3                5      6
     d   sccom4                7      8
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
     c                   parm                    @@call
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
     c     tbky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkky
     c     exky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkfnd
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
      ******* beg add ****** 02/15/11 *********************************
     c                   eval      *in90 = *off
      ******* end add ****** 02/15/11 *********************************

     C                   if        DtCheck(scLsDt) <> *zero
     C                   eval      scLsDt = DtCheck(scLsDt)
     C                   endif
     C*
     C                   if        DtCheck(scPoDt) <> *zero
     C                   eval      scPoDt = DtCheck(scPoDt)
     C                   endif
     C*
     C                   if        DtCheck(scPrDt) <> *zero
     C                   eval      scPrDt = DtCheck(scPrDt)
     C                   endif

     c                   if        singleMode
     c                   eval      found = validGrp(scfnd)
     c                   eval      *in71 = not(found)
     c                   eval      *in90 = *in71
     c                   if        not(*in90)
     c                   move(p)   scdiv         xxdiv             5 0
     c                   eval      found = validEmp(scemp# :xxdiv:rrnemp:scfnd)
     c                   eval      *in72 = not(found)
     c                   eval      *in90 = *in72
     c                   endif
      *
      * Populate default values in single mode only because these values are kept
      * at the group level and this process uses group select.
      *
     c  n90              if        svfnd <> scfnd
     c                   eval      svfnd = scfnd
     c                   eval      kkfnd = scfnd
     c                   write     scr01
     c     exky          chain     pfn01
     c                   eval      scname = fnname
     c     exky          chain     pex01
     c                   if        %found
     c                   if        scpodt = 0
     c     sysDt         adddur    EXDFID:*days  postDate
     c     *usa          move      postDate      scpodt
     c                   write     scr01
     c                   eval      *in90 = *on
     c                   endif                                                  scpodt = 0
     c                   if        scddrm = *blank
     c                   eval      scddrm = EXFIRM
      *** beg add ***** 05/20/10 *****************************************
     c                   clear                   scdflg
     c                   if        scddrm <> *blank
     c                   eval      scdflg = 'Y'
     c                   endif
      *** end add ***** 05/20/10 *****************************************
     c                   write     scr01
     c                   eval      *in90 = *on
     c                   endif                                                  scddrm = *blank
      ******* beg del ****** 08/02/11 *********************************
     c*******            if        scassf = *blank
     c*******            eval      scassf = EXASSF
     c*******            write     scr01
     c*******            eval      *in90 = *on
     c*******            endif                                                  scassf = *blank
      ******* end del ****** 08/02/11 *********************************
     c                   endif                                                  pex01 %found
     c                   endif                                                  scfnd <> svfnd
     c                   endif                                                  singleMode
     c   90              leavesr
      *
      * Validate previous balance date if populated.
      *
      *** beg dlt ***** 05/20/10 *****************************************
     c**n90***           if        scpbdt <> 0
     c********           z-add     scpbdt        srdti
     c********           call      'SRCVTDAT'    datchk
     c********           movel     srdto         scpbdt
     c********           if        srerr = '1'
     c********           eval      *in74 = *on
     c********           eval      *in90 = *on
     c********           leavesr
     c********           endif
     c********           endif
      *** end add ***** 05/20/10 *****************************************
      *
      * Load current date to loss date if not populated.
      *
     c                   if        not(*in90)
     c                   z-add     sclsdt        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         sclsdt
     c                   if        srerr = '1'
     c                   eval      *in75 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   endif
      *
      * Validate A/R Post Date, if populatd. Defaults in single mode only.
      *
     c  n90              if        scpodt <> 0
     c                   z-add     scpodt        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scpodt
     c                   if        srerr = '1'
     c                   eval      *in76 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
      *
      *  The post date cannot exist in a prior month
      *
     c                   eval      x7podt = dtCYMD(scpodt)
     c     *CYMD         move      sysDt         sysDt7
     c                   movel     x7podt        xxyrm1
     c                   movel     sysDt7        xxyrm2
     c                   if        xxyrm1 < xxyrm2
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   endif                                                  scpodt <> 0

     c                   if        not(*in90)
     c                   z-add     scprdt        srdti
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scprdt
     c                   if        srerr = '1'
     c                   eval      *in78 = *on
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   endif
      *** beg add ***** 05/20/10 *****************************************
     c  n90              move      *zeros        xxdate            8 0
     c                   eval      xxdate = DateTo8(SystemDate)
     C                   if        scprdt < xxdate
     C                   eval      *in79 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
      *** end add ***** 05/20/10 *****************************************

     c  n90              do        4             ctr
     c                   if        comNts(ctr) <> *blank
     c                   eval      kkky = comNts(ctr)
     c     tbky          chain     pbc01
     c                   select
     c                   when      %found and BCSTAT <> 'I'
     c                   other
     c                   z-add     80            n                 2 0
     c                   add       ctr           n
     c                   eval      *in(n) = *on
     c                   eval      *in90  = *on
     c                   leavesr
     c                   endsl
     c                   endif
     c                   enddo

     c  n90              if        scdflg <> *blanks and scdflg <> 'Y'
     c                   seton                                        85  90
     c                   leavesr
     c                   endif                                                  scdflg = *blanks
      ******* beg add ****** 02/15/11 *********************************
      *
     C     SCDFLG        IFEQ      'Y'
     C                   CALL      'W7944R1'                            99
     C                   PARM      '1'           #1CALL            1
     C                   PARM      'EXFIRM'      #1TYPE           10
     C                   PARM      '0'           #1ERR             1

     C     #1ERR         IFEQ      *ON
     C     SCDDRM        ANDEQ     *BLANKS
     C                   SETON                                        86  90
     C                   leavesr
     C                   ENDIF

     C                   ENDIF

      ******* end add ****** 02/15/11 *********************************
      *
      *  DARTS Remark field must be valid
      *
     c  n90              if        scddrm <> *blanks
     c     scddrm        chain     pct01                              60
     c                   if        *in60
     c                   seton                                        86  90
     c                   endif
     c                   endif
      ******* beg add ****** 02/15/11 *********************************

      * Notify user the DARTS remark code is only used for groups w/o a
      *  default remark code set-up.

     C  N90              IF        (SCDDRM <> *BLANKS) and
     C                             SVddrm = *BLANKS
     C     *LIKE         DEFINE    SCDDRM        SVDDRM
     C                   MOVEL     SCDDRM        SVDDRM
     C                   SETON                                        69  90
     C                   leavesr
     C                   ENDIF

      * zzz
      ******* end add ****** 02/15/11 *********************************
      *
      *  Validate the social security flag
      *
     c  n90              if        scassf <> 'Y' and
     c                             scassf <> 'N' and
     c                             scassf <> '4'
     c                   seton                                        87  90
     c                   endif
      *
      *  Validate the the exclusion flag
      *
     c  n90              if        scexcl <> 'Y' and
     c                             scexcl <> ' '
     c                   seton                                        88  90
     c                   endif
    c*
     c                   endsr
    c*====================================================================
    c* KJDO   - Processing for F10
    c*====================================================================
     c     KJDO          begsr
     c                   select
     c                   when      hfld = 'SCFND'
     c                   exsr      GroupSearch
     c                   when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     c                   exsr      EmplrSearch
     c                   when      hfld = 'SCCOM1' or
     c                             hfld = 'SCCOM2' or
     c                             hfld = 'SCCOM3' or
     c                             hfld = 'SCCOM4'
     c                   exsr      CommentSearch
     c                   when      hfld = 'SCDDRM'
     c                   exsr      DartRmkSearch
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
     c                   if        %parms >= 1
     c                             and @@prds <> *blanks
     c                   movel(p)  @@prds        ##data
     c                   else
     c                   movel(p)  optnm         ##data
     c                   endif
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel     ##data        scprds
     c                   if        %parms >= 2
     c                   movel     @@call        xxcall
     c                   else
     c                   clear                   xxcall
     c                   endif
     c                   if        xxcall = 'S'
     c                   eval      singleMode = *on
     c                   eval      *in65  = *on
     c                   endif
     c                   call      'SA030R'
     c                   parm                    suffix            3
     c                   eval      sclib1 = 'PDSRPT' + suffix
     c                   clear                   svfnd
     c                   eval      scuser = Q1USER
     c                   eval      sysDt  = SystemDate
     c                   eval      sclsdt = dateTo8(sysDt)
      ******* beg add ****** 08/02/11 *********************************

      * Get "SSN Print" default value

     c                   move      ##co#         #1co#

     c                   call      'WA097R'
     c                   parm                    #1co#             3
     c                   parm      'W8005R'      #1rpid           10
     c     scassf        parm      *blank        #1y4n             1
     c     *in44         parm      *blank        #1chg             1

      ******* end add ****** 08/02/11 *********************************
     c                   endsr


    c*====================================================================
    c* PREDSP - Pre-processing before displaying (every time)
    c*====================================================================
     c     PREDSP        begsr
     c                   eval      scPFB = 'B'
     c                   endsr
    c*====================================================================
    c* PRSCTL - Process control after no errors found (Call Submitter)
    c*====================================================================
     c     PRSCTL        begsr
     c                   move      ##co#         qqco#             3
     c                   move      scfnd         qqfnd             3
     c                   move      scemp#        qqemp#            9
     c                   move      scdiv         qqdiv             5
     c                   move      sclsdt        qqlsdt            8
     c                   move      scpodt        qqpodt            8
     c                   move      *zeros        qqpbdt            8
     c                   move      scprdt        qqprdt            8
     c                   move      ##outq        qqoutq           10
     c                   move      scassf        qqassf            1
     c                   move      sccom1        qqcom1            2
     c                   move      sccom2        qqcom2            2
     c                   move      sccom3        qqcom3            2
     c                   move      sccom4        qqcom4            2
     c                   move      scddrm        qqddrm            5
     c                   move      scdflg        qqdflg            1
     c                   move      xxcall        qqcall            1
     c                   move      scexcl        qqexcl            1
     c                   call      'W8005C'
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqemp#
     c                   parm                    qqdiv
     c                   parm                    qqpbdt
     c                   parm                    qqlsdt
     c                   parm                    qqpodt
     c                   parm                    qqprdt
     c                   parm                    qqcom1
     c                   parm                    qqcom2
     c                   parm                    qqcom3
     c                   parm                    qqcom4
     c                   parm                    qqoutq
     c                   parm                    qqdflg
     c                   parm                    qqddrm
     c                   parm                    qqassf
     c                   parm                    qqcall
     c                   parm                    qqexcl
      ******* beg add ****** 02/15/11 *********************************
     c                   parm      scpfb         ##pfb             1
      ******* end add ****** 02/15/11 *********************************
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
     c                   setoff                                       31
     c                   seton                                        32
     c                   endif                                                  rrnEMP
     c                   endif                                                  scfnd <> 0
     c                   if        rrnEmp = *zero
     c                   seton                                        32
     c                   endif                                                  rrnEMP
     c                   endsr
    c*====================================================================
    c* DARTS remark search
    c*====================================================================
     c     DartRmkSearch begsr
     c                   move      *blanks       qqrmcd            5
     c                   call      'SC305R'
     c                   parm      '2'           ##call            1
     c     *inkc         parm                    ##kc              1
     c     *inkl         parm                    ##kl              1
     c     qqrmcd        parm      *blanks       ##rmcd            5
     c                   parm      *blanks       ##desc           40
     c                   parm      *blank        ##rtcd            1
     c     qqrmcd        ifne      *blanks
     c                   movel     qqrmcd        scddrm
     c                   seton                                        39
     c                   else
     c                   seton                                        38
     c                   endif
     c                   endsr
     *====================================================================
     * CommentSearch
     *====================================================================
     c     commentSearch begsr
     c                   call      'W2191R'
     c                   parm      '2'           qqcall            1
     c                   parm                    qqkc              1
     c                   parm                    qqkl              1
     c                   parm      *blanks       qqkey            10
     c                   select
     c                   when      hfld = 'SCCOM1'
     c                   eval      sccom1 = qqkey
     c                   if        sccom1 = *blank
     c                   eval      *in33 = *on
     c                   else
     c                   eval      *in34 = *on
     c                   endif
     c                   when      hfld = 'SCCOM2'
     c                   eval      sccom2 = qqkey
     c                   if        sccom2 = *blank
     c                   eval      *in34 = *on
     c                   else
     c                   eval      *in35 = *on
     c                   endif
     c                   when      hfld = 'SCCOM3'
     c                   eval      sccom3 = qqkey
     c                   if        sccom3 = *blank
     c                   eval      *in35 = *on
     c                   else
     c                   eval      *in36 = *on
     c                   endif
     c                   when      hfld = 'SCCOM4'
     c                   eval      sccom4 = qqkey
     c                   if        sccom4 = *blank
     c                   eval      *in36 = *on
     c                   else
     c                   eval      *in37 = *on
     c                   endif
     c                   endsl
     c                   endsr
