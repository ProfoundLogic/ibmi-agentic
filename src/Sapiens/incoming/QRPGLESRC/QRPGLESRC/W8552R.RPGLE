     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H* ================================================================= *
    H/title Agent History Update for Multi Employers (Update)
    H* ================================================================= *
    H*  Program ID:   W8552R                                             *
    H*                                                                   *
    H*  CALLED FROM:  W8551C                                             *
    H*                                                                   *
    H*  CALLS TO:     None                                               *
    H*                                                                   *
    H*  CREATED: 09/07/07      C:132     PROJECT: WC1478                 *
    H*  Rewrite: 12/10/07      C:33      PROJECT: WC1478                 *
    H* ================================================================= *
    H*  Revisions:                                                       *
    H*                                                                   *
     H* 07/18/22  C:33   Support: 102942                                  *
     H*                  Use a valid time in mantenance fields.           *
    H*                                                                   *
     H* 12/13/18  C:33   Support: 75027                                   *
     H*                  When running in Quote, do not create a transfer  *
     H*                   quote status, just update the existing with the *
     H*                   new agent.                                      *
    H*                                                                   *
     H* 08/30/10  C:257  Support : SEQ73849                               *
     H*                  Fix the issue with report not printing correct   *
     H*                  header information and details                   *
     H*                                                                   *
     H* 02/20/09  C:33   Project: WC1478    WA071/67113                   *
     H*                  Edit agent information against cx dates.         *
     H*                                                                   *
     H*  02/10/09  C:33  Project: WC1478   WA071/67113                    *
    H*                  Update quote tracking, if needed.                *
     H*                                                                   *
     H*  08/13/08  C:33  Project: WC1478                                  *
    H*                  Update report sort.                              *
     H*                                                                   *
     H*  05/06/08  C:180   Preventive - Fix group total break.            *
    H*                                                                   *
     H*  05/05/08  C:180   PRISS 35 - Correct typo error on error message.*
     H*                             - Don ot process agent when agent     *
     H*                                not found.                         *
    H* ================================================================= *
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fsmorl     if   e           k disk    rename(por01:por00)
     fWDELL3    if   e           k disk    rename(pel01:pel03)
     fWMFNL     IF   E           k DISK    rename(pfn01:pfn00)
     fWMAAL1    if   e           k disk
     fWMARL     if   e           k disk    rename(par01:par00)
     fWMAPL     if   e           k disk    rename(pap01:pap00)
     fWMRSL     if   e           k disk    rename(prs01:prs00)
     fWMAHL1    if   e           k disk    infds(AHfds1)
     fWMAHL7    if   e           k disk    rename(pah01:pah07) infds(AHfds7)
     fWMAHP     uf a e             disk    rename(pah01:pah0p)
     fWFAHP     uf a e           k disk    rename(pah01:pah0f)
     fWAQTL7    if   e           k disk    rename(pqt01:pqt07) infds(QTfds7)
     fWAQTP     uf a e             disk    rename(pqt01:pqt0p)
    f*
     fqsysprt   o    f  132        printer oflind(*inof) usropn
    f*
     D AHfds1          DS
     D  AHrrn1               397    400B 0
     D AHfds7          DS
     D  AHrrn7               397    400B 0
     D QTfds7          DS
     D  QTrrn7               397    400B 0
     *
     *================================================================
     * CONSTANTS
     *================================================================
     d x0Title         c                   const('Agent History Update -
     d                                     Report')
     *
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
      *
     d @@Co#           s              3
     d @@FrFEIN        s              9
     d @@FrMod         s              3
     d @@ToFEIN        s              9
     d @@ToMod         s              3
     d @@AgtEffDt      s              8
     d @@FrRt#         s              3
     d @@ToRt#         s              3
     d @@FrProd        s              5
     d @@ToProd        s              5
     d @@FrGrp         s              3
     d @@ToGrp         s              3
     d @@PfDt          s              8
     d @@PtDt          s              8
     d @@OutQ          s             10
     d @@PrtOnly       s              1
     d xxCo#           s              3  0
     d xxFrFEIN        s              9  0
     d xxFrMod         s              3  0
     d xxToFEIN        s              9  0
     d xxToMod         s              3  0
     d x8AgtEffDt      s              8  0
     d xxFrRt#         s              3  0
     d xxToRt#         s              3  0
     d xxFrProd        s              5  0
     d xxToProd        s              5  0
     d xxFrGrp         s              3  0
     d xxToGrp         s              3  0
     d xxQue           s             10
     d x8AHEFDT        s              8  0
     *
     d testing         s              1n   inz(*off)
     *
     d grpQual         s              1n
     d empQual         s              1n
     d prtRoute        s              1n
     d kkprt           s              1
     d sysDt7          s              7  0 inz
     d sysDt8          s              8  0 inz
     d sysTm6          s              6  0 inz
     d***  Begin Add  ***  07/18/22  ********************
     d sysTimeStamp    s               z   inz
     d***  End   Add  ***  07/18/22  ********************
     d UPUSNM          s             10
     d xxFnd           s              3  0 inz
     d xxMsg           s             75
     d xxMsg2          s             75
     d xpfyr           s              3  0 inz
     d xxFrAgt#        s              5  0
     d xxToAgt#        s              5  0
     d x7AgtEffDt      s              7  0 inz
     d x7PfDt          s              7  0 inz
     d x7PtDt          s              7  0 inz
     d x8PfDt          s              8  0 inz
     d x8PtDt          s              8  0 inz
     d xxJrSt          s                   like(RSJRST)
     d xxStat          s                   like(AHSTAT)
     d xqStat          s                   like(QTSTAT)
     d xxAgt#          s                   like(QTAGT#)
     DisQuoteEnv       s              1n   inz
     DisLiveEnv        s              1n   inz
     d newAgt#         s                   like(xxFrAgt#)
     d currentAgt#     s                   like(xxFrAgt#)
     d effectAgt#      s                   like(xxFrAgt#)
     d recordAgt#      s                   like(xxFrAgt#)
     d currentDt       s                   like(AHEFDT)
     d effectDt        s                   like(AHEFDT)
     d recordDt        s                   like(AHEFDT)
     d*
     d saveAHEMP#Old   s                   like(AHEMP#Old)
     d saveAHDIVOld    s                   like(AHDIVOld)
     d saveAHFYROld    s                   like(AHFYROld)
     d saveAHAGT#Old   s                   like(AHAGT#Old)
     d saveAHPRODOld   s                   like(AHPRODOld)
     d saveAHRT#Old    s                   like(AHRT#Old)
     d saveAHEFDTOld   s                   like(AHEFDTOld)
     DstrEnv           s              4a
     *
     d prtOpen         s              1n   inz('0')
     d ErrCnt          s              5  0 inz                                  Errors Listed
     d RcdAdd          s              7  0 inz                                  Records added
     d RcdByp          s              7  0 inz                                  Records bypassed
     d RcdCnt          s              7  0 inz                                  Records Read
     *
     d psds          esds                  extname(szq1p)
     d fids01        e ds                  extname(szq2p)
     d*
     d @@RatSt         ds
     d  xxRatSt01              1      2
     d  xxRatSt02              3      4
     d  xxRatSt03              5      6
     d  xxRatSt04              7      8
     d  xxRatSt05              9     10
     d  xxRatSt06             11     12
     d  xxRatSt07             13     14
     d  xxRatSt08             15     16
     d  xxRatSt09             17     18
     d  xxRatSt10             19     20
     d ratSt                          2    dim(10) overlay(@@RatSt)             Rating States
     d*
     d/copy sPrDate
     d/copy sPrString
     d/copy wPrAgent
     d*
     DAddRecord        pr
     D outAsOfDt                           like(AHEFDT) const
     d*
     DGetAgtEffDt      pr                  like(AHEFDT)
     D outCo#                              like(AHCO#)  const
     D outFnd                              like(AHFND)  const
     D outEmp#                             like(AHEMP#) const
     D outDiv                              like(AHDIV)  const
     D outFyr                              like(AHFYR)  const
     D outAgt#                             like(AHAGT#) const
     D outAsOfDt                           like(AHEFDT) const
     d*
     DGetQteTrkSts     pr                  like(QTSTAT)
     d*
     DMsgInfo          pr
     D msg1                          75
     D msg2                          75
     d*
     DNewInfo          pr
     d*
     DOldInfo          pr
     D
     DRtvEnv           pr                  extpgm('RTVENV')
     D env                            4a
     d*
     DValidAgt         pr             1n
     D outFnd                              like(AHFND)  const
     D outAgt#                             like(AHAGT#) const
     d*
     DValidProd        pr             1n
     D outFnd                              like(AHFND)  const
     D outAgt#                             like(AHAGT#) const
     D outProd                             like(AHPROD) const
     d*
     DValidRt#         pr             1n
     D outCo#                              like(AHCO#)  const
     D outFnd                              like(AHFND)  const
     D outRt#                              like(AHRT#)  const
     D outEfDt                             like(AHEFDT) const
     *
     *================================================================
     *   KEY LISTS
     *================================================================
     *
     *   Company
     c     keySMCOL      klist
     c                   kfld                    xxCo#
     *
     *   Output Routing
     c     keySMORL      klist
     c                   kfld                    q1pgm
     c                   kfld                    kkprt
     *
     *   Employer Detail file
     c     keyWDELL3     klist
     c                   kfld                    xxCo#
     c                   kfld                    xxFnd
     *
     c     key1WDELL3    klist
     c                   kfld                    xxCo#
     c                   kfld                    xxFnd
     c                   kfld                    x7PfDt
     *
     *   Group Master
     c     keyWMFNL      klist
     c                   kfld                    xxCo#
     *
     c     key1WMFNL     klist
     c                   kfld                    xxCo#
     c                   kfld                    xxFnd
     *
     *   Rate Set Code file
     c     keyWMRSL      klist
     c                   kfld                    xxCo#
     c                   kfld                    ELRTCD

     c     keyWMAHL7     klist
     c                   kfld                    xxCo#
     c                   kfld                    xxStat

     c     keyWFAHP      klist
     c                   kfld                    xxCo#
     c                   kfld                    xxFnd

     c     keyWAQTL7     klist
     c                   kfld                    xxCo#
     c                   kfld                    xxFnd
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    xqStat
     c                   kfld                    xxAgt#
    c*
    c*====================================================================
    c* PARM LISTS  - List of Parms used in program
    c*====================================================================
      *
     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@FrFEIN
     c                   parm                    @@FrMod
     c                   parm                    @@ToFEIN
     c                   parm                    @@ToMod
     c                   parm                    @@AgtEffDt
     c                   parm                    @@FrRt#
     c                   parm                    @@ToRt#
     c                   parm                    @@FrProd
     c                   parm                    @@ToProd
     c                   parm                    @@FrGrp
     c                   parm                    @@ToGrp
     c                   parm                    @@RatSt
     c                   parm                    @@PfDt
     c                   parm                    @@PtDt
     c                   parm                    @@OutQ
     c                   parm                    @@PrtOnly
      *
    c*====================================================================
    c* MAINLINE - Main processing for program
    c*====================================================================
     c
      * Setup for first time
     c                   exsr      PgmStr
      *
      *  Start with incoming Group and process through ending Group.
     *
     c     key1WMFNL     setll     pfn00                                        Co#, xxFnd
     c     keyWMFNL      reade     pfn00
     c                   dow       not %eof(WMFNL) and
     c                              FNFND <= xxToGrp
     c                   eval      grpQual = *on
     c                   exsr      NewGroup
     c                   exsr      CheckRt#
     c                   if        grpQual
     c                   exsr      ProcessGrp
     c                   endif
     c     keyWMFNL      reade     pfn00
     c                   enddo                                                  *in68 = *OFF
     *
     c                   exsr      pgmend
     *
    c*====================================================================
    c* Check Rate #
    c*====================================================================
     c     CheckRt#      begsr
     C*
     c                   if        xxToRt# <> *zero and
     c                              not ValidRt#(xxCo#:xxFnd:xxToRt#:
     c                                           x7AgtEffDt)
     C                   eval      grpQual = *off
     C                   eval      xxMsg = '''To'' Rate not valid for Group.'
     C                   except    grpMsg
     C                   endif
     C*
     c                   endsr
     *
    c*====================================================================
    c* Check Employer Rating State
    c*====================================================================
     c     CheckRtSt     begsr
     C*
     C                   if        @@RatSt <> *blanks
     c                   clear                   RSJRST
     c     keyWMRSL      chain     prs00                                        CO#,FND,FFEIN,FMOD
     c                   eval      xxJrSt = RSJRST
     c     xxJrSt        lookup    ratSt                                  60
     c                   if        not %found
     C                   eval      empQual = *off
     C                   endif
     c                   endif
     C*
     c                   endsr
     *
    c*====================================================================
    c* Process Message 1
    c*====================================================================
     c     Msg1          begsr
     C*
     c                   eval      x8AHEFDT = dtMDYY(AHEFDT)
     C                   callp     OldInfo
     C                   eval      xxMsg = '# Current record effective ' +
     c                                       'after the New Eff Date.  ' +
     c                                       'No record changes.'
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C                   eval      rcdByp = rcdByp + 1
     C*
     c                   endsr
     *
    c*====================================================================
    c* Process Message 7
    c*====================================================================
     c     Msg7          begsr
     C*
     C                   callp     OldInfo
     C                   eval      xxMsg = '# ''From'' agent is ' +
     c                                       'effective on the account ' +
     c                                       'in the past and or future'
     c                   eval      xxMsg2 = '   but not at the time of ' +
     c                                      'the New Eff Date.  ' +
     c                                      'No record changes.'
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C                   eval      rcdByp = rcdByp + 1
     C*
     c                   endsr
     *
    c*====================================================================
    c* Process Message 8
    c*====================================================================
     c     Msg8          begsr
     C*
     C                   eval      xxMsg = '+ Records effective after ' +
     c                                       'the New Eff Date indicate ' +
     c                                       'another agent exists.'
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     c                   endsr
     *
    c*====================================================================
    c* Process a new group
    c*====================================================================
     c     NewGroup      begsr
     C*
     c                   if        xxFnd <> FNFND and xxFnd <> *ZEROS
     c                   exsr      Totals
     c                   endif
     C*
     c                   eval      xxFnd = FNFND
     C                   except    head
     C                   except    head1
     c*
     c                   clear                   errCnt
     c                   clear                   rcdAdd
     c                   clear                   rcdByp
     c                   clear                   rcdCnt
     C*
     C                   eval      *inof = *off
     C*
     c                   endsr
     *
    c*====================================================================
    c* Process Groups
    c*====================================================================
     c     ProcessGrp    begsr
     c*
     C                   eval      xxFrAgt# = GetAgent#(xxCo#:xxFrFEIN:
    c                                                  xxFrMod:xxFnd)
     C                   eval      xxToAgt# = GetAgent#(xxCo#:xxToFEIN:
    c                                                  xxToMod:xxFnd)
     C*
     C                   if        xxFrAgt# > *ZERO and xxToAgt# > *ZERO
      *
     C     key1WDELL3    setll     pel03
     C     keyWDELL3     reade     pel03
     C                   dow       not %eof and
     C                              ELPFDT <= x7PtDt
     C                   eval      empQual = *on
     C                   exsr      CheckRtSt
     c                   if        empQual
     c                   exsr      ProcessEmp
     C                   endif
     C     keyWDELL3     reade     pel03
     C                   enddo
     C*
     C                   endif
     C                   endsr
     *
    c*====================================================================
    c* Process Employers
    c*====================================================================
     c     ProcessEmp    begsr
     c*
     c                   eval      currentAgt# = GetEmpAgt(xxCo#:xxFnd:
     c                                          ELEMP#:ELDIV:ELFYR:
     c                                          sysDt7)
     c                   eval      effectAgt# = GetEmpAgt(xxCo#:xxFnd:
     c                                          ELEMP#:ELDIV:ELFYR:
     c                                          x7AgtEffDt)
     c                   eval      recordAgt# = GetEmpAgt(xxCo#:xxFnd:
     c                                          ELEMP#:ELDIV:ELFYR)
     C*
     C                   if        currentAgt# = xxFrAgt# or
     C                             effectAgt#  = xxFrAgt# or
     C                             recordAgt#  = xxFrAgt#
     C                   eval      currentDt = GetAgtEffDt(xxCo#:xxFnd:
     c                                          ELEMP#:ELDIV:ELFYR:xxFrAgt#:
     c                                          sysDt7)
     C                   eval      effectDt  = GetAgtEffDt(xxCo#:xxFnd:
     c                                          ELEMP#:ELDIV:ELFYR:xxFrAgt#:
     c                                          x7AgtEffDt)
     c                   eval      recordDt  = GetAgtEffDt(xxCo#:xxFnd:
     c                                          ELEMP#:ELDIV:ELFYR:xxFrAgt#:
     c                                          *hival)
     C                   exsr      ProcessAgt
     C                   exsr      UpdQteTrack
     C                   eval      rcdCnt = rcdCnt + 1
     C                   endif
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Agents
    c*====================================================================
     c     ProcessAgt    begsr
     c*
     C                   select
     C                   when      currentAgt# = xxFrAgt# and
     C                              effectAgt# = xxFrAgt# and
     C                              recordAgt# = xxFrAgt#
     C                   exsr      Process1
     c*
     C                   when      currentAgt# = xxFrAgt# and
     C                              effectAgt# = xxFrAgt# and
     C                              recordAgt# <> xxFrAgt#
     C                   exsr      Process2
     c*
     C                   when      currentAgt# = xxFrAgt# and
     C                              effectAgt# <> xxFrAgt# and
     C                              recordAgt# = xxFrAgt#
     C                   exsr      Process3
     c*
     C                   when      currentAgt# = xxFrAgt# and
     C                              effectAgt# <> xxFrAgt# and
     C                              recordAgt# <> xxFrAgt#
     C                   exsr      Process4
     c*
     C                   when      currentAgt# <> xxFrAgt# and
     C                              effectAgt# = xxFrAgt# and
     C                              recordAgt# = xxFrAgt#
     C                   exsr      Process5
     c*
     C                   when      currentAgt# <> xxFrAgt# and
     C                              effectAgt# = xxFrAgt# and
     C                              recordAgt# <> xxFrAgt#
     C                   exsr      Process6
     c*
     C                   when      currentAgt# <> xxFrAgt# and
     C                              effectAgt# <> xxFrAgt# and
     C                              recordAgt# = xxFrAgt#
     C                   exsr      Process7
     c*
     C                   when      currentAgt# <> xxFrAgt# and
     C                              effectAgt# <> xxFrAgt# and
     C                              recordAgt# <> xxFrAgt#
     C                   exsr      Process8
     c*
     C                   endsl
     C*
     C                   if        *inof
     C                   except    head
     C                   except    head1
     C                   endif
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 1
    c*====================================================================
     c     Process1      begsr
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C                   callp     AddRecord(currentDt)
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C                   callp     AddRecord(effectDt)
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     C                   callp     AddRecord(recordDt)
     C*
     C                   endsl
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 2
    c*====================================================================
     c     Process2      begsr
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C                   callp     AddRecord(currentDt)
     C                   exsr      Msg8
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C                   callp     AddRecord(effectDt)
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     c                   eval      x8AHEFDT = *zero
     c                   eval      xxMsg = '* Invalid condition 2'
     C                   clear                   xxMsg2
     C                   callp     OldInfo
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     C                   endsl
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 3
    c*====================================================================
     c     Process3      begsr
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           sysDt7))
     C                   exsr      Msg7
     C                   exsr      Msg8
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           *hival))
     C                   exsr      Msg1
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     c                   eval      x8AHEFDT = *zero
     c                   eval      xxMsg = '* Invalid condition 3'
     C                   clear                   xxMsg2
     C                   callp     OldInfo
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     C                   endsl
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 4
    c*====================================================================
     c     Process4      begsr
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           sysDt7))
     C                   exsr      Msg7
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           sysDt7))
     C                   exsr      Msg1
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           sysDt7))
     C                   exsr      Msg7
     C*
     C                   endsl
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 5
    c*====================================================================
     c     Process5      begsr
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C                   callp     AddRecord(effectDt)
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           effectDt))
     C                   callp     OldInfo
     c                   eval      xxMsg = '# There are other agents ' +
     c                                       'effective between the ' +
     c                                       'New Eff Date and the last ' +
     c                                       'record.'
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     C                   callp     AddRecord(recordDt)
     C*
     C                   endsl
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 6
    c*====================================================================
     c     Process6      begsr
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C                   callp     AddRecord(effectDt)
     C                   exsr      Msg8
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C                   exsr      Msg7
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     C                   exsr      Msg7
     C*
     C                   endsl
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 7
    c*====================================================================
     c     Process7      begsr
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           *hival))
     C                   exsr      Msg1
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C                   eval      x8AHEFDT = dtMDYY(GetAgtEffDt(
     C                                           ELCO#:ELFND:ELEMP#:ELDIV:
     c                                           ELFYR:xxFrAgt#:
     c                                           *hival))
     C                   exsr      Msg1
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     c                   eval      x8AHEFDT = *zero
     c                   eval      xxMsg = '* Invalid condition 7'
     C                   callp     OldInfo
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     C                   endsl
     C*
     C                   endsr
     *
    c*====================================================================
    c* Process Condition 8
    c*====================================================================
     c     Process8      begsr
     c*
     c                   eval      x8AHEFDT = *zero
     c                   eval      xxMsg = '* Invalid condition 8'
     C                   callp     OldInfo
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     c*
     C                   select
     c*
     C                   when      currentDt <= effectDt and
     C                              effectDt <= recordDt
     C*
     C                   when      effectDt <= currentDt and
     C                              currentDt <= recordDt
     C*
     C                   when      currentDt <= recordDt and
     C                              recordDt <= effectDt
     C*
     C                   endsl
     C*
     C                   endsr
     *
     *================================================================
     * PGMEND - Program Ending
     *================================================================
     c     pgmend        begsr
     c*
     C                   exsr      Totals
     *
     c                   if        errCnt = *zeros and
     c                              RcdAdd = *zeros
     c                   except    totErr
     c                   endif
     c*
     c                   if        prtOpen
      /Free
       kkprt  = '0';
       Chain KeySmorl por00;
       If %found or testing;
          Except Route;
       Endif;
      /End-free
     c                   eval      prtOpen =  *OFF
     c                   close     qsysprt
     c                   endif
     *
     c                   exsr      UpdtNewRcds
     *
     c                   move      *on           *INLR
     c                   return
     *
     c                   endsr
     *
     *================================================================
     * PgmStr - Set-up and Initialize
     *================================================================
     c     PgmStr        begsr
     C*
     c                   move      @@Co#         xxCo#
     c                   move      @@FrFEIN      xxFrFEIN
     c                   move      @@FrMod       xxFrMod
     c                   move      @@ToFEIN      xxToFEIN
     c                   move      @@ToMod       xxToMod
     c                   move      @@AgtEffDt    x8AgtEffDt
     c                   move      @@FrRt#       xxFrRt#
     c                   move      @@ToRt#       xxToRt#
     c                   move      @@FrProd      xxFrProd
     c                   move      @@ToProd      xxToProd
     c                   move      @@FrGrp       xxFrGrp
     c                   move      @@ToGrp       xxToGrp
     c                   move      @@PfDt        x8PfDt
     c                   move      @@PtDt        x8PtDt
     *
     c                   select
     c                   when      @@PrtOnly = 'Y'
     c                   eval      testing = *on
     c                   endsl
     *
     C                   eval      sysDt7 = DateTo7(SystemDate)
     C                   eval      sysDt8 = DateTo8(SystemDate)
     C***  Begin Chg  ***  07/18/22  **************************
     C**********         eval      sysTm6 = TimeTo6(SystemTime)
       sysTimeStamp = %timestamp();
     C***  End   Chg  ***  07/18/22  **************************
     *
     *   Setup New Agent Effective Dates
     C                   eval      x7AgtEffDt = DtCYMD(x8AgtEffDt)
     *
     *   Setup Policy Effective Dates
     C                   eval      x7PfDt = DtCYMD(x8PfDt)
     C                   eval      x7PtDt = DtCYMD(x8PtDt)
     *
     c                   eval      xxFnd = xxFrGrp
     *
     c                   clear                   CONAME
     c     keySMCOL      chain     pco00                              60
     C                   eval      CONAME = CenterFld(CONAME:%len(CONAME))
     c*
     * If only Errors are to be listed, remove the EXSR SrRouting line below.
     c                   exsr      SrRouting                                        ZZZZZZZZ
     *
     c                   if        xxToGrp = *zero
     c                   eval      xxToGrp = *hival
     c                   endif
     c                   if        x7PtDt = *zero
     c                   eval      x7PtDt = *hival
     c                   endif
     *
     c                   eval      xqStat = GetQteTrkSts

      /Free
       RtvEnv(strEnv);
       isQuoteEnv = strEnv = '*QTE';
       isLiveEnv = strEnv = '*LIV';
      /End-free
     *
     c                   endsr
     *
     *================================================================
     * Print the information changes
     *================================================================
     c     PrtInfo       begsr
     C*
     C                   if        saveAHEMP#Old <> AHEMP#Old or
     C                             saveAHDIVOld  <> AHDIVOld  or
     C                             saveAHFYROld  <> AHFYROld  or
     C                             saveAHAGT#Old <> AHAGT#Old or
     C                             saveAHPRODOld <> AHPRODOld or
     C                             saveAHRT#Old  <> AHRT#Old  or
     C                             saveAHEFDTOld <> AHEFDTOld
     C                   except    extraLine
     C                   except    detlOld
     C                   endif
     C*
     C                   if        AHAGT#New <> *zero
     C                   except    detlNew
     C                   endif
     C*
     C                   if        AHMSG1 <> *blanks
     C                   eval      xxMsg = AHMSG1
     C                   except    detlMsg
     C                   endif
     C*
     C                   if        AHMSG2 <> *blanks
     C                   eval      xxMsg = AHMSG2
     C                   except    detlMsg
     C                   endif
     C*
     C                   eval      saveAHEMP#Old = AHEMP#Old
     C                   eval      saveAHDIVOld  = AHDIVOld
     C                   eval      saveAHFYROld  = AHFYROld
     C                   eval      saveAHAGT#Old = AHAGT#Old
     C                   eval      saveAHPRODOld = AHPRODOld
     C                   eval      saveAHRT#Old  = AHRT#Old
     C                   eval      saveAHEFDTOld = AHEFDTOld
     C*
     c                   endsr
     *
     *================================================================
     * SrRouting - Open QSYSPRT and then Print routing page, also for OF
     *================================================================
     c     SrRouting     begsr
     *
     *   Open qsysprt if prtOpen flag not set *ON.
     *
     c                   if        prtOpen <> *ON
     c                   eval      prtOpen =  *ON
     c                   open      qsysprt
     c                   endif
     C*
     C                   clear                   ORTITL
     *
     *   Check Routing file record
     *
     c                   move      '0'           kkprt
     c     keySMORL      chain     por00                              60
     c                   eval      prtRoute  = %found or testing
     *
     c                   if        *in60 = *off
     *
     *  User Name if no Distribution specified in routing
     *
     c                   if        ordis1 = *blanks
     c                             and ordis2 = *blanks
     c                             and ordis3 = *blanks
     c                             and ordis4 = *blanks
     c                   eval      ORDIS1 = q1User
     c                   endif
     *
     c                   else
     *
     c                   eval      ORPNAM = q1Pgm
     c                   eval      ORDIS1 = q1User
     *
     c                   endif                                                  *in60 = *off
     c
     c                   eval      UPUSNM = q1User
     C     @@Outq        comp      *blanks                            080809
     c                   if        @@OutQ <> *blanks
     C                   eval      xxQue  = @@OutQ
     C                   eval      OROUTQ = @@OutQ
     C                   endif
     *
     c                   if        ORTITL = *blanks
     C                   eval      ORTITL = x0Title
     c                   endif
     C*
     C                   eval      ORTITL = LeftJstify(%trim(ORTITL))
     C*
     c                   select
     c                   when      @@PrtOnly = 'Y'
     c                   eval      ORTITL = %trim(ORTITL) + ' (Report Only)'
     c                   endsl
     *
     C                   if        prtRoute
     C                   endif
     C*
     C                   eval      ORTITL = CenterFld(ORTITL:%len(ORTITL))
     C*
     c                   eval      *inof = *on
     *
     c                   endsr
     *
     *================================================================
     * Process totals
     *================================================================
     c     Totals        begsr
     *
     C                   clear                   saveAHEMP#Old
     C                   clear                   saveAHDIVOld
     C                   clear                   saveAHFYROld
     C                   clear                   saveAHAGT#Old
     C                   clear                   saveAHPRODOld
     C                   clear                   saveAHRT#Old
     C                   clear                   saveAHEFDTOld
     C*
     C     keyWFAHP      setll     pah0f
     c*
     C                   dou       %eof(WFAHP)
     c*
     c                   if        *inof
     c                   except    head
     c                   except    head1
     c                   eval      *inof = *off
     c                   endif
     *
     C     keyWFAHP      reade     pah0f
     C                   if        not %eof(WFAHP)
     C                   exsr      PrtInfo
     C                   endif
     C*
     C                   enddo
     C*
     C                   except    extraLine
     c*
     c                   except    total
     *
     C                   endsr
     *
     *================================================================
     * Update New Agent History records
     *================================================================
     c     UpdtNewRcds   begsr
     *
     C                   eval      xxStat = '*'
     C     keyWMAHL7     setll     pah07
     C*
     c                   dou       %eof(wmahl7)
     c     keyWMAHL7     reade     pah07
     C     AHrrn7        chain     pah0p
     c                   if        %found
     C                   eval      AHSTAT = 'A'
     C                   update    pah0p
     C                   endif
     C                   enddo
     *
     C                   endsr
     *
     *================================================================
     * Update Quote Tracking, if needed
     *================================================================
     c     UpdQteTrack   begsr
     *
     c                   eval      newAgt# = GetEmpAgt(xxCo#:xxFnd:
     c                                          ELEMP#:ELDIV:ELFYR)
     c                   if        recordAgt# <> newAgt#
     c                              and recordAgt# <> *zero
     c                              and newAgt# <> *zero
     c                   eval      xxAgt# = recordAgt#
     c     keyWAQTL7     setgt     pqt07
     c     keyWAQTL7     readpe    pqt07
     c                   if        not %eof
      /Free
       If isQuoteEnv;
          Chain QTrrn7 WAQTP;
          QTagt# = newAgt#;
          QTuser = q1User;
          QTpgm  = q1Pgm;
          QTtime = Timeto6(SystemTime());
          QTdt   = Dateto7(SystemDate());
          Update pqt0p;
       Else;
      /End-free
     c                   select
     c                   when      QTTYPE <> 'X' and QTTYPE <> 'T'
     c                   eval      QTXFER = QTTYPE
     c                   endsl
     c                   eval      QTTYPE = 'X'
     c                   exsr      WrtQteTrack
     c                   eval      QTTYPE = 'T'
     c                   eval      QTAGT# = newAgt#
     c                   exsr      WrtQteTrack
     c                   endif
     c                   endif
     c                   endif
     *
     C                   endsr
     *
     *================================================================
     * Write Quote Tracking record
     *================================================================
     c     WrtQteTrack   begsr
     *
     C                   eval      QTUSER = q1User
     C                   eval      QTPGM  = q1Pgm
     C                   eval      QTDT   = DateTo7(SystemDate)
     C                   eval      QTTIME = TimeTo6(SystemTime)
     *
     C                   eval      QTIUSR = QTUSER
     C                   eval      QTIPGM = QTPGM
     C                   eval      QTIDT  = QTDT
     C                   eval      QTITM  = QTTIME
     *
     C                   eval      QTWKDT = QTDT
     *
     C                   if        not testing
     C                   write     pqt0p
     C                   endif
     *
     C                   endsr
     *
     *================================================================
     *
     O/copy sCopyRout
     O          e            route       2  2
     O                                           24 'REPORT SELECTIONS:'
     *
     O          e            route          2
     O                                         +007 'COMPANY . . . . :'
     O                       xxCo#         z   +002
     *
     O          e            route          0
     O                                         +007 'Change Information:'
     O                                           37 'From'
     O                                           53 'To'
     O          e            route          2
     O                                           37 '____'
     O                                           53 '__'
     O          e            route          1
     O                                         +009 'AGENT FEIN/MOD:'
     O                       xxFrFEIN      x   +005
     O                       xxFrMod       z   +001
     O                       xxToFEIN      x   +004
     O                       xxToMod       z   +001
     O          e            route          1
     O                                         +009 'AGENT RATE    :'
     O                       xxFrRt#       z   +010
     O                       xxToRt#       z   +014
     O          e            route          2
     O                                         +009 'PRODUCER  . . :'
     O                       xxFrProd      z   +009
     O                       xxToProd      z   +012
     O          e            route          2
     O                                         +009 'NEW AGENT HISTORY EFF'
     O                                         +001 'DATE:'
     O                       x8AgtEffDt        +002 '  /  /    '
     *
     O          e            route          2
     O                                         +007 'Selection criteria:'
     O          e            route          2
     O                                         +009 'GROUP RANGE'
     O                                         +002 '. . . . . . . :'
     O                       xxFrGrp       z   +002
     O                                         +001 '-'
     O                       xxToGrp       z   +001
     O          e            route          2
     O                                         +009 'RATING STATE(S)'
     O                                         +002 '. . . . . :'
     O                       xxRatSt01         +002
     O                       xxRatSt02         +002
     O                       xxRatSt03         +002
     O                       xxRatSt04         +002
     O                       xxRatSt05         +002
     O                       xxRatSt06         +002
     O                       xxRatSt07         +002
     O                       xxRatSt08         +002
     O                       xxRatSt09         +002
     O                       xxRatSt10         +002
     O          e            route          2
     O                                         +009 'POLICY EFFECTIVE DATE'
     O                                         +001 'RANGE:'
     O                       x8PfDt            +002 '  /  /    '
     O                                         +001 '-'
     O                       x8PtDt            +001 '  /  /    '
     *
     O          e            route          1 60
     O                                         +002 '(c) Copyright'
     O                                         +001 'Fiserv, Inc.,'
     O                       *year             +001
     *
     O          e            head           1 03
     O                       sysTm6               8 '  :  :  '
     O                       CONAME              90
     O                       sysDt8             130 '  /  /    '
     *
     O          e            head           2
     O                       q1pgm               10
     O                       ortitl             100
     O                                          123 'PAGE:'
     O                       page          z    130
     *
     O          e            head1          2
     O                                         +002 'GROUP:'
     O                       XXFND             +002
     O                       FNNAME            +002
     *
     O          e            head1          1
     O                                           34 'CURRENT RECORD'
     O                                           87 '  NEW RECORD  '
     O          e            head1          1
     O                                           10 ' EMPLOYER'
     O                                           16 '  DIV'
     O                                           21 ' YR'
     O                                           28 'AGT #'
     O                                           35 ' PROD'
     O                                           40 'RATE'
     O                                           51 ' EFF DATE '
     O*
     O                                           63 ' EMPLOYER'
     O                                           69 '  DIV'
     O                                           74 ' YR'
     O                                           81 'AGT #'
     O                                           88 ' PROD'
     O                                           93 'RATE'
     O                                          104 ' EFF DATE '
      *
     O          e            grpMsg         1
     O                       xxMsg               80
      *
     O          e            detlOld        0
     O                       AHEMP#Old     3     10
     O                       AHDIVOld      3     16
     O                       AHFYROld      3     21
     O                       AHAGT#Old     3     28
     O                       AHPRODOld     3     35
     O                       AHRT#Old      3     40
     O                       AHEFDT8Old          51 '  /  /    '
     O          e            detlNew        1
     O                       AHEMP#New     3     63
     O                       AHDIVNew      3     69
     O                       AHFYRNew      3     74
     O                       AHAGT#New     3     81
     O                       AHPRODNew     3     88
     O                       AHRT#New      3     93
     O                       AHEFDT8New         104 '  /  /    '
     O          e            detlMsg        1
     O                       xxMsg              132
     *
     O          e            extraLine      1
     *
     O          e            toterr      2  1
     O                                         +001 'There were NO Errors and'
     O                                         +001 'NO Records Updated/Changed'
     O                                         +001 'Please Verify input data'
     O          e            toterr         1
     *
     O          e            total       1  1
     O                       RcdCnt        3   +002
     O                                         +001 'Records Read'
     O                       RcdAdd        3   +002
     O                                         +001 'Records Added'
     O                       ErrCnt        3   +002
     O                                         +001 'Errors Listed (*)'
     O                       RcdByp        3   +002
     O                                         +001 'Records Bypassed (#)'





    P*====================================================================
    P* Add records and inactivate future records for agent
    P*====================================================================
     PAddRecord        b                   export
    P*
     DAddRecord        PI
     D inAsOfDt                            like(AHEFDT) const

     d agtQual         s              1n   inz(*on)
     D x7AsOfDt        s                   like(AHEFDT)
     d toProd          s                   like(AHPROD)
     d toRt#           s                   like(AHRT#)
     d toEffDt         s                   like(AHEFDT)

     c     keyWMAHL1     klist
     c                   kfld                    AHCO#
     c                   kfld                    AHFND
     c                   kfld                    AHFYR
     c                   kfld                    xxFrAgt#
     c                   kfld                    AHEMP#
     c                   kfld                    AHDIV
     c                   kfld                    x7AsOfDt

     c     key1WMAHL1    klist
     c                   kfld                    AHCO#
     c                   kfld                    AHFND
     c                   kfld                    AHFYR
     c                   kfld                    xxFrAgt#
     c                   kfld                    AHEMP#
     c                   kfld                    AHDIV

     C                   eval      x7AsOfDt = GetAgtEffDt(ELCO#:ELFND:
     c                                         ELEMP#:ELDIV:ELFYR:xxFrAgt#:
     c                                         inAsOfDt)

     c                   clear                   AHEFDT
     c* Find record as of date requested
     c     keyWMAHL1     Setll     pah01
     c                   Dou       %eof or AHstat <> 'I'
     c                   Clear                   AHrrn1
     c     keyWMAHL1     Reade     pah01
     c                   Enddo
     c     AHrrn1        Chain(n)  wmahp
     c                   eval      x8AHEFDT = dtMDYY(AHEFDT)
     c                   select
     c                   when      %found and
     c                              AHSTAT = '*'
     c                   eval      agtQual = *off
     c                   when      %found and
     c                              xxFrRt# <> *zero and
     c                              AHRT# <> xxFrRt#
     c                   eval      agtQual = *off
     c                   eval      xxMsg = '*'
     c                   exsr      RateMsg
     c                   when      %found and
     c                              xxFrProd <> *zero and
     c                              AHPROD <> xxFrProd
     c                   eval      agtQual = *off
     c                   eval      xxMsg = '*'
     c                   exsr      ProdMsg
     c                   when      %found
     C                   select
     c                   when      xxToRt# <> *zero
     c                   eval      toRt# = xxToRt#
     c                   when      xxToRt# = *zero
     c                   eval      toRt# = AHRT#
     c                   endsl
     C                   select
     c                   when      xxToProd <> *zero
     c                   eval      toProd = xxToProd
     c                   when      xxToProd = *zero
     c                   eval      toProd = AHPROD
     c                   endsl
     c                   eval      toEffDt = x7AgtEffDt
     c* Check new detail
     c                   exsr      CheckNewRt#
     c                   exsr      CheckNewAgt
     c                   exsr      CheckNewProd
     c                   other
     c                   eval      agtQual = *off
     c                   endsl
     C*
     c                   if        agtQual
     c                   eval      rcdAdd = rcdAdd + 1
     c                   exsr      Inactivate
     c                   exsr      AddAgent
     c     key1WMAHL1    reade     pah01
     c                   dow       not %eof
     C                   if        AHSTAT <> 'I'
     c                              and AHSTAT <> '*'
     c                   select
     c                   when      xxFrRt# <> *zero and
     c                              AHRT# <> xxFrRt#
     c                   eval      xxMsg = '+'
     c                   exsr      RateMsg
     c                   when      xxFrProd <> *zero and
     c                              AHPROD <> xxFrProd
     c                   eval      xxMsg = '+'
     c                   exsr      ProdMsg
     c                   other
     c                   exsr      FutureMsg
     c                   endsl
     c                   endif
     c     key1WMAHL1    reade     pah01
     c                   enddo
     c                   else
     c                   eval      errCnt = errCnt + 1
     c                   endif

     C                   return
     *
    c*====================================================================
    c* Add record
    c*====================================================================
     c     AddAgent      begsr
     C*
     C                   eval      AHAGT# = xxToAgt#
     c                   eval      AHRT#  = toRt#
     C                   eval      AHPROD = toProd
     C                   eval      AHEFDT = toEffDt
     C                   eval      AHSTAT = '*'
     c                   exsr      MaintFlds
     c                   eval      AHIPGM = AHPGM
     c                   eval      AHIUSR = AHUSER
     c                   eval      AHIDT  = AHDT
     c                   eval      AHITM  = AHTIME
     C                   if        not testing
     c                   write     pah0p
     C                   endif
     c                   eval      x8AHEFDT = dtMDYY(AHEFDT)
     C                   callp     NewInfo
     C*
     c                   endsr
     *
    c*====================================================================
    c* Check New Agent
    c*====================================================================
     c     CheckNewAgt   begsr
     C*
     c                   if        not ValidAgt(xxFnd:xxToAgt#)
     c                   eval      xxMsg = '* Agent ' +
     c                                     %trim(%editc(AAAGT#:'Z')) +
     c                                     ' is not effective as of the +
     c                                     new Agent History effective +
     c                                     date.'
     c                   if        agtQual
     C                   callp     OldInfo
     c                   endif
     c                   eval      agtQual = *off
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     c                   endif
     C*
     c                   endsr
     *
    c*====================================================================
    c* Check New Producer
    c*====================================================================
     c     CheckNewProd  begsr
     C*
     c                   if        not ValidProd(xxFnd:xxToAgt#:toProd)
     c                   eval      xxMsg = '* Producer ' +
     c                                     %trim(%editc(AHPROD:'Z')) +
     c                                     ' is not valid for new agent.'
     c                   if        agtQual
     C                   callp     OldInfo
     c                   endif
     c                   eval      agtQual = *off
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     c                   endif
     C*
     c                   endsr
     *
    c*====================================================================
    c* Check New Rate #
    c*====================================================================
     c     CheckNewRt#   begsr
     C*
     c                   if        not ValidRt#(AHCO#:AHFND:toRt#:toEffDt)
     c                   eval      xxMsg = '* Rate ' +
     c                                     %trim(%editc(AHRT#:'Z')) +
     c                                     ' is not valid for group as' +
     c                                     ' of the New Eff Date.'
     c                   if        agtQual
     C                   callp     OldInfo
     c                   endif
     c                   eval      agtQual = *off
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     c                   endif
     C*
     c                   endsr
     *
    c*====================================================================
    c* Inactivate record
    c*====================================================================
     c     Inactivate    begsr
     C*
     C     AHrrn1        chain     pah0p
     c                   if        %found
     c                              and AHEFDT >= toEffDt
     C                   eval      AHSTAT = 'I'
     c                   exsr      MaintFlds
     C                   if        not testing
     C                   update    pah0p
     C                   endif
     C                   endif
     c                   eval      x8AHEFDT = dtMDYY(AHEFDT)
     C                   callp     OldInfo
     C*
     c                   endsr
     *
    c*====================================================================
    c* Maintance fields
    c*====================================================================
     c     MaintFlds     begsr
     C*
     C***  Begin Chg  ***  07/18/22  **************************
     C**********         eval      sysTm6 = sysTm6 + 1
       sysTimeStamp += %seconds(1);
       sysTm6 = TimeTo6(%time(sysTimeStamp));
     C***  End   Chg  ***  07/18/22  **************************
     C*
     C                   eval      AHUSER = q1User
     C                   eval      AHPGM  = q1Pgm
     C                   eval      AHDT   = sysDt7
     C                   eval      AHTIME = sysTm6
     C*
     c                   endsr
     *
    c*====================================================================
    c* Future agent history reocrd exists - Message
    c*====================================================================
     c     FutureMsg     begsr
     C*
     c                   eval      x8AHEFDT = dtMDYY(AHEFDT)
     c                   eval      xxMsg = '+ Agent History record ' +
     c                                     'exists after new ' +
     c                                     'effective date'
     C                   callp     OldInfo
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     c                   endsr
     *
    c*====================================================================
    c* From producer doesn't match - Message
    c*====================================================================
     c     ProdMsg       begsr
     C*
     c                   eval      xxMsg = %trim(xxMsg) +
     c                                     ' Producer ' +
     c                                     %trim(%editc(AHPROD:'Z')) +
     c                                     ' does not match change ' +
     c                                     '''From'' producer ' +
     c                                     %trim(%editc(xxFrProd:'Z')) +
     c                                     '.'
     C                   callp     OldInfo
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     c                   endsr
     *
    c*====================================================================
    c* From rate # doesn't match - Message
    c*====================================================================
     c     RateMsg       begsr
     C*
     c                   eval      xxMsg = %trim(xxMsg) +
     c                                     ' Rate ' +
     c                                     %trim(%editc(AHRT#:'Z')) +
     c                                     ' does not match change ' +
     c                                     '''From'' rate ' +
     c                                     %trim(%editc(xxFrRt#:'Z')) +
     c                                     '.'
     C                   callp     OldInfo
     C                   clear                   xxMsg2
     C                   callp     MsgInfo(xxMsg:xxMsg2)
     C*
     c                   endsr

     PAddRecord        e





    P*====================================================================
    P* Retrive the Agent History effective date as of a date passed in
    P*====================================================================
     PGetAgtEffDt      b                   export
    P*
     DGetAgtEffDt      PI                  like(AHEFDT)
     D inCo#                               like(AHCO#)  const
     D inFnd                               like(AHFND)  const
     D inEmp#                              like(AHEMP#) const
     D inDiv                               like(AHDIV)  const
     D inFyr                               like(AHFYR)  const
     D inAgt#                              like(AHAGT#) const
     D inAsOfDt                            like(AHEFDT) const

     c     keyWMAHL1     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    inFyr
     c                   kfld                    inAgt#
     c                   kfld                    inEmp#
     c                   kfld                    inDiv
     c*
     c     key1WMAHL1    klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    inFyr
     c                   kfld                    inAgt#
     c                   kfld                    inEmp#
     c                   kfld                    inDiv
     c                   kfld                    inAsOfDt

     c     key1WMAHL1    setgt     pah01
     c     keyWMAHL1     readpe    pah01
     c                   dow       not %eof and
     c                              AHSTAT = 'I'
     c     keyWMAHL1     readpe    pah01
     c                   enddo

     c                   select
     c                   when      not %eof
     C                   return    AHEFDT
     c                   when      %eof
     c                   return    *zeros
     c                   endsl

     PGetAgtEffDt      e





    P*====================================================================
    P* Get the value needed for Quote Tracking status (QTSTAT)
    P*====================================================================
     PGetQteTrkSts     b                   export
     DGetQteTrkSts     pi                  like(QTSTAT)

     D ##PDS           s              1n
     D ##Obj           s             10
     D ##Type          s              7
     D ##Lib           s             10

     C                   call      'SACHKPDS'
     C                   parm                    ##PDS
     C*
     C                   call      'RTVOBJLIB'
     C                   parm      'WMEMP'       ##Obj
     C                   parm      '*FILE'       ##Type
     C                   parm      *blanks       ##Lib
     C*
     C                   select
     C                   when      ##PDS and %subst(##Lib:10:1) = 'Q'
     C                   return    'Q'
     C                   when      %subst(##Lib:1:6) = 'PDSQTE'
     C                   return    'Q'
     C                   other
     C                   return    'W'
     C                   endsl

     PGetQteTrkSts     e





    P*====================================================================
    P* Load the message information
    P*====================================================================
     PMsgInfo          b                   export
    P*
     DMsgInfo          PI
     D msg1                          75
     D msg2                          75

     C                   eval      AHMSG1 = msg1
     C                   eval      AHMSG2 = msg2
     C*
     C                   write     pah0f

     PMsgInfo          e





    P*====================================================================
    P* Load the New Agent History information
    P*====================================================================
     PNewInfo          b                   export
    P*
     DNewInfo          PI

     C                   eval      AHEMP#NEW  = AHEMP#
     C                   eval      AHDIVNEW   = AHDIV
     C                   eval      AHFYRNEW   = AHFYR
     C                   eval      AHAGT#NEW  = AHAGT#
     C                   eval      AHPRODNEW  = AHPROD
     C                   eval      AHRT#NEW   = AHRT#
     C                   eval      AHEFDTNEW  = AHEFDT
     C                   eval      AHEFDT8NEW = DtMDYY(AHEFDT)
     C*
     C                   write     pah0f

     PNewInfo          e





    P*====================================================================
    P* Load the Old Agent History information
    P*====================================================================
     POldInfo          b                   export
    P*
     DOldInfo          PI

     C                   eval      AHEMP#OLD  = AHEMP#
     C                   eval      AHDIVOLD   = AHDIV
     C                   eval      AHFYROLD   = AHFYR
     C                   eval      AHAGT#OLD  = AHAGT#
     C                   eval      AHPRODOLD  = AHPROD
     C                   eval      AHRT#OLD   = AHRT#
     C                   eval      AHEFDTOLD  = AHEFDT
     C                   eval      AHEFDT8OLD = DtMDYY(AHEFDT)
     C*
     C                   clear                   AHEMP#NEW
     C                   clear                   AHDIVNEW
     C                   clear                   AHFYRNEW
     C                   clear                   AHAGT#NEW
     C                   clear                   AHPRODNEW
     C                   clear                   AHRT#NEW
     C                   clear                   AHEFDTNEW
     C                   clear                   AHEFDT8NEW
     C*
     C                   clear                   AHMSG1
     C                   clear                   AHMSG2

     POldInfo          e





    P*====================================================================
    P* Validate agent  r
    P*====================================================================
     PValidAgt         b                   export
    P*
     DValidAgt         PI             1n
     D inFnd                               like(AHFND)  const
     D inAgt#                              like(AHAGT#) const

     c     keyWMAAL1     klist
     c                   kfld                    AHCO#
     c                   kfld                    inFnd
     c                   kfld                    inAgt#

     c     keyWMAAL1     chain     paa01
     c*
     c                   select
     c                   when      not %found(WMAAL1) or
     C                             AASTAT = 'I'
     c                   return    *off
     c                   when      AAEFDT <> *zero
     c                              and x7AgtEffDt < AAEFDT
     c                   return    *off
     c                   when      AACNDT <> *zero
     c                              and x7AgtEffDt > AACNDT
     c                   return    *off
     c                   other
     c                   return    *on
     c                   endsl

     PValidAgt         e





    P*====================================================================
    P* Validate producer
    P*====================================================================
     PValidProd        b                   export
    P*
     DValidProd        PI             1n
     D inFnd                               like(AHFND)  const
     D inAgt#                              like(AHAGT#) const
     D inProd                              like(AHPROD) const

     c     keyWMAPL      klist
     c                   kfld                    AACO#
     c                   kfld                    AAFEIN
     c                   kfld                    AAMOD
     c                   kfld                    inProd

     c     keyWMAAL1     klist
     c                   kfld                    AHCO#
     c                   kfld                    inFnd
     c                   kfld                    inAgt#

     C                   CLEAR                   AAFEIN
     C                   CLEAR                   AAMOD
     c*
     c     keyWMAAL1     chain     paa01
     c     keyWMAPL      chain     pap00
     c*
     c                   select
     c                   when      not %found(WMAPL) or
     C                             APSTAT = 'I'
     c                   return    *off
     c                   when      APCXDT <> *zero
     c                              and x7AgtEffDt > APCXDT
     c                   return    *off
     c                   other
     c                   return    *on
     c                   endsl

     PValidProd        e





    P*====================================================================
    P* Validate rate #
    P*====================================================================
     PValidRt#         b                   export
    P*
     DValidRt#         PI             1n
     D inCo#                               like(AHCO#)  const
     D inFnd                               like(AHFND)  const
     D inRt#                               like(AHRT#)  const
     D inEfDt                              like(AHEFDT) const

     c     keyWMARL      klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    inRt#
     c                   kfld                    inEfDt
     c*
     c     key1WMARL     klist
     c                   kfld                    inCo#
     c                   kfld                    inFnd
     c                   kfld                    inRt#

     c*
     c     keyWMARL      setgt     par00
     c     key1WMARL     readpe    par00                                  60
     c                   dow       not %eof(WMARL) and ARSTAT = 'I'
     c     key1WMARL     readpe    par00                                  60
     c                   enddo
     c*
     c                   select
     c                   when      %eof(WMARL)
     c                   return    *off
     c                   other
     c                   return    *on
     c                   endsl

     PValidRt#         e





