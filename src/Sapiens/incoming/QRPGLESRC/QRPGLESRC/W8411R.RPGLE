    Hcopyright('(C) Copyright Fiserv, Inc., 2005')
    H*********************************************************************
    H/title S/R Premium Report
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 06/08/05      C:33      PROJECT: 135UDN                 *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*  11/13/06  C:132  Project: CI071 - 55762                          *
    H*                   Add Edit for Policy begin & ending dates for    *
    H*                   WDELP file in WriteRcds Procedure               *
    H*                                                                   *
    H*  11/08/06  C:132  Project: CI071 - 55716                          *
    H*                   Add Edit for Posting Date and valid A/R Code    *
    H*                   in CalcWrtPrem Procedure                        *
    H*                                                                   *
    H*  10/26/05  C:33   Project: 135UDN   PQ135/49335                   *
    H*                   Remove data write from employer history audit   *
    H*                    information.                                   *
    H*********************************************************************
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     Fwdell1    if   e           k disk
     F*wdell     if   e           k disk    rename(pel01:pel00)
     Fwdehl     if   e           k disk    rename(peh01:peh00)
     Fwasrl5    if   e           k disk    rename(psr01:psr05)
     Fwmepl1    if   e           k disk
     Fwmeal23   if   e           k disk    rename(pea01:pea23)
     Fwmrsl     if   e           k disk    rename(prs01:prs00)

     Fqxsqp     o    e             disk    rename(psq01:psq0p)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d xxCo#           s                   like(SQCO#)
     d xxFrRgn#        s                   like(SQRGN)
     d xxToRgn#        s                   like(SQRGN)
     d xxFrFyr         s                   like(SQFYR)
     d xxToFyr         s                   like(SQFYR)
     d x8AsOfDt        s              8  0
     d x8FrDt          s              8  0
     d x8ToDt          s              8  0
     d xxDtType        s              1

     d x7AsOfDt        s              7  0
     d x7FrDt          s              7  0
     d x7ToDt          s              7  0

     d calcFrDt        s              7  0
     d calcToDt        s              7  0

     d process         s              1
     d empHistDt       s               d

     d xxFnd           s                   like(FNFND) inz
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     d*entry
     d W8411R          pr
     d  @@co#                         3    const
     d  @@FrRgn#                      3    const
     d  @@ToRgn#                      3    const
     d  @@FrFyr                       3    const
     d  @@ToFyr                       3    const
     d  @@AsOfDt                      8    const
     d  @@FrDt                        8    const
     d  @@ToDt                        8    const
     d  @@DtType                      1    const
     d W8411R          pi
     d  @@co#                         3    const
     d  @@FrRgn#                      3    const
     d  @@ToRgn#                      3    const
     d  @@FrFyr                       3    const
     d  @@ToFyr                       3    const
     d  @@AsOfDt                      8    const
     d  @@FrDt                        8    const
     d  @@ToDt                        8    const
     d  @@DtType                      1    const

     d CalcManPrem     pr                  like(SQMAPR)
     d  process                       1    const
     d  fromDt                         d   const
     d  toDt                           d   const
     d  asOfDt                         d   const options(*nopass)
     d CalcWrtPrem     pr                  like(SQWPRM)
     d  process                       1    const
     d  fromDt                         d   const
     d  toDt                           d   const
     d  asOfDt                         d   const options(*nopass)
     d EmpHistExts     pr              n
     d  empHistDt                      d   const
     d GetJrSt         pr                  like(RSJRST)
     d GetWDF1P        pr
     d  co#                                const like(F1CO#)
     d  fnd                                const like(F1FND)
     d  fyr                                const like(F1FYR)
     d GetNextGrp      pr                  like(FNFND)
     d WriteQXSQP      pr
     d  co#                                const like(SQCO#)
     d  rgn                                const like(SQRGN)
     d  fnd                                const like(SQFND)
     d  fyr                                const like(SQFYR)
     d  emp#                               const like(SQEMP#)
     d  div                                const like(SQDIV)
     d  mapr                               const like(SQMAPR)
     d  wprm                               const like(SQWPRM)

     d/copy sPrString
     d/copy sPrDate

    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     keyWDELL1     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFrFyr

     C     key1WDELL1    klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
    C*
    C*====================================================================
    C* Mainline
    C*====================================================================

     C                   exsr      PgmStr

     C                   dou       xxFnd = *zero

     C                   eval      xxFnd = GetNextGrp

     C                   if        xxFnd <> *zero

     C                   exsr      ProcessEmps

     C                   endif

     C                   enddo

     C                   eval      *inlr = *on

    C*====================================================================
    C* Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PgmStr        begsr
    
     C                   move      @@co#         xxCo#
     C                   move      @@FrRgn#      xxFrRgn#
     C                   move      @@ToRgn#      xxToRgn#
     C                   move      @@FrFyr       xxFrFyr
     C                   move      @@ToFyr       xxToFyr
     C                   move      @@AsOfDt      x8AsOfDt
     C                   move      @@FrDt        x8FrDt
     C                   move      @@ToDt        x8ToDt
     C                   move      @@DtType      xxDtType

     C                   eval      x7AsOfDt = DtCYMD(x8AsOfDt)
     C                   eval      x7FrDt   = DtCYMD(x8FrDt)
     C                   eval      x7ToDt   = DtCYMD(x8ToDt)


     C                   select
     C                   when      xxToFyr <> *zero
     C                   eval      empHistDt = CvtToDate(x8AsOfDt)
     C                   eval      process = 'Y'
     C                   when      x7ToDt <> *zero and
     C                              xxDtType = 'R'
     C                   eval      empHistDt = CvtToDate(x8ToDt)
     C                   eval      process = 'R'
     C                   eval      calcFrDt = 0000101
     C                   eval      calcToDt = 2990101
     C                   when      x7ToDt <> *zero and
     C                              xxDtType = 'P'
     C                   eval      calcFrDt  = x7FrDt
     C                   eval      calcToDt  = x7ToDt
     C                   eval      empHistDt = CvtToDate(x8ToDt)
     C                   eval      process = 'P'
     C                   endsl

     C                   if        xxToFyr = *zero
     C                   eval      xxToFyr = *hival
     C                   endif
    
     C                   endsr
    
    C*====================================================================
    C* Process Employers
    C*====================================================================
    
     C     ProcessEmps   begsr

     C     keyWDELL1     setll     pel01

     C                   dou       %eof(wdell1)

     C     key1WDELL1    reade     pel01

     C                   if        not %eof(wdell1)

     C                   select
     C                   when      process = 'Y' and ELFYR <= xxToFyr
     C                   exsr      WriteRcds
     C                   when      process = 'P'
     C                   exsr      WriteRcds
     C                   when      process = 'R' and
     C                             (CvtToDate(ELPFDT) >= CvtToDate(x8FrDt)
     C                               and
     C                              CvtToDate(ELPFDT) <= CvtToDate(x8ToDt))
     C                   exsr      WriteRcds
     C                   endsl

     C                   endif

     C                   enddo

     C                   endsr
    
    C*====================================================================
    C* Determine if records should be written and write them
    C*====================================================================
    
     C     WriteRcds     begsr

     C***  Begin Del  ***  10/26/05  **************************************
     C**********         if        ELFAF <> 'A' or
     C**********                    not EmpHistExts(empHistDt)
     C***  End   Del  ***  10/26/05  **************************************

      ***** BEG ADD ***** 11/13/06 ********************************************
     C                   If        ELPFDT > *ZEROS AND ELPTDT > *ZEROS
      ***** END ADD ***** 11/13/06 ********************************************

     C                   select
     C                   when      process = 'Y'
     C                   eval      calcFrDt = ELPFDT
     C                   eval      calcToDt = ELPTDT
     C                   endsl

     C                   callp     GetWDF1P(ELCO#:ELFND:ELFYR)

     C                   callp     WriteQXSQP(ELCO#:EXDRGN:ELFND:ELFYR:
     C                                        ELEMP#:ELDIV:
     C                              CalcManPrem(process:
     C                                          CvtToDate(calcFrDt):
     C                                          CvtToDate(calcToDt):
     C                                          CvtToDate(x8AsOfDt)):
     C                              CalcWrtPrem(process:
     C                                          CvtToDate(calcFrDt):
     C                                          CvtToDate(calcToDt):
     C                                          CvtToDate(x8AsOfDt)))

      ***** BEG ADD ***** 11/13/06 ********************************************
     C                   endif                                                  End Elpfdt > *zeros
      ***** END ADD ***** 11/13/06 ********************************************

     C***  Begin Del  ***  10/26/05  **************************************
     C**********         endif
     C***  End   Del  ***  10/26/05  **************************************

     C                   endsr





    P*====================================================================
     P* Calculate the manual premium amount
    P*====================================================================
     p CalcManPrem     b
     d CalcManPrem     pi                  like(SQMAPR)
     d  process                       1    const
     d  fromDt                         d   const
     d  toDt                           d   const
     d  asOfDt                         d   const options(*nopass)

     d*premium         s                   like(SQMAPR) inz
     d premium         s            + 2    like(EPAMPR)
     d rate            s                   like(EPARAT) inz
     d asOf            s               d   inz(*hival)
     d toDate          s               d   inz
     d rptCYMD         s                   like(SREFDT)
     d saveSREFDT      s                   like(SREFDT) inz
     d saveSRCLAS      s                   like(SRCLAS) inz

     c     keyWASRL5     klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c                   if        %parms >= 4
     c                   eval      asOf = asOfDt
     c                   endif

     c     keyWASRL5     setll     psr05

     c                   dou       %eof(wasrl5)

     c     keyWASRL5     reade     psr05

     c                   if        not %eof(wasrl5)

     c                   if        SREFDT <> saveSREFDT or
     c                             SRCLAS <> saveSRCLAS
     c                   exsr      ClassBrk
     c                   endif

     c                   select
     c                   when      SRTODT <> *zero
     c                   eval      toDate = CvtToDate(SRTODT)
     c                   when      SRTODT = *zero
     c                   movel     SRRPMY        rptCYMD
     c                   move      01            rptCYMD
     c                   eval      toDate = GetLastDay(CvtToDate(rptCYMD))
     c                   if        toDate <> asOf and process = 'Y'
     c                   eval      asOf = CvtToDate(dtPast(DateTo8(asOf):1))
     c                   endif
     c                   endsl

     c                   select
     c                   when      process = 'Y' and
     c**                           (CvtToDate(SREFDT) < fromDt or
     c**                            CvtToDate(SREFDT) > toDt   or
     c                              toDate > asOf
     c                   iter
     c                   when      process = 'P' and
     c                             (toDate < fromDt or
     c                              toDate > toDt)
     c                   iter
     c                   when      process = 'R' and
     c                             (toDate < fromDt or
     c                              toDate > toDt)
     c                   iter
     c                   endsl

     c                   eval(h)   premium = premium + (SRAMTP * rate/100)

     c                   endif

     c                   enddo

     c                   return    premium
    
    C*====================================================================
    C* Class/Effective Date Break
    C*====================================================================
    
     C     ClassBrk      begsr

     c     keyWMEPL1     klist
     c                   kfld                    SRCO#
     c                   kfld                    SRFND
     c                   kfld                    SRFYR
     c                   kfld                    SREMP#
     c                   kfld                    SRDIV
     c                   kfld                    SRCLAS
     c                   kfld                    SRSPEC
     c                   kfld                    SREFDT

     c                   clear                   EPARAT
     c     keyWMEPL1     chain     pep01
     c                   eval      rate = EPARAT

     c                   eval      saveSRCLAS = SRCLAS
     c                   eval      saveSREFDT = SREFDT

     C                   endsr

     p CalcManPrem     e





    P*====================================================================
     P* Calculate the written premium amount
    P*====================================================================
     p CalcWrtPrem     b
     d CalcWrtPrem     pi                  like(SQWPRM)
     d  process                       1    const
     d  fromDt                         d   const
     d  toDt                           d   const
     d  asOfDt                         d   const options(*nopass)

     d premium         s                   like(SQWPRM) inz
     d asOf            s               d   inz(*hival)
     d fromDt7         s                   like(EAPODT)

     c     keyWMEAL23    klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR
     c                   kfld                    fromDt7

     c     key1WMEAL23   klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c                   eval      fromDt7 = DateTo7(fromDt)

     c                   if        %parms >= 4
     c                   eval      asOf = asOfDt
     c                   endif

     c     keyWMEAL23    setll     pea23

     c                   dou       %eof(wmeal23)

     c     key1WMEAL23   reade     pea23

     c                   if        not %eof(wmeal23)

     c                   select

      ***** BEG ADD ****** 11/08/06 ********************************************
      * Edit for valid Date and A/R Code
     c                   when      eapodt < 0010101 or EAARCD = '  '
     c                   iter
      ***** END ADD ****** 11/08/06 ********************************************

     c                   when      EAARCD <> F1SRBB
     c                   iter
     c                   when      process = 'Y' and
     c                             CvtToDate(EAPODT) > asOf
     c                   iter
     c                   when      process = 'P' and
     c                             (CvtToDate(EAPODT) < fromDt or
     c                              CvtToDate(EAPODT) > toDt)
     c                   iter
     c                   endsl

     c                   eval      premium = premium + (EAAMT)

     c                   endif

     c                   enddo

     c                   return    premium

     p CalcWrtPrem     e





    P*====================================================================
     P* Get lastest WDEHP audit record prior to or equal to the 'As Of Date'
    P*====================================================================
     p EmpHistExts     b
     d EmpHistExts     pi              n
     d  empHistDt                      d   const

     d xxfaf           s                   like(EHFAF)  inz('A')
     d empHistDt7      s                   like(EHEFDT)

     c     keyWDEHL      klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR
     c                   kfld                    xxfaf
     c                   kfld                    empHistDt7

     c     key1WDEHL     klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR
     c                   kfld                    xxfaf

     c                   eval      empHistDt7 = DateTo7(empHistDt)

     c     keyWDEHL      setgt     peh00
     c     key1WDEHL     readpe    peh00

     c                   if        not %eof(wdehl)
     c                   callp     WriteQXSQP(EHCO#:EXDRGN:EHFND:EHFYR:
     c                                        EHEMP#:EHDIV:EHWMPR:EHWAPR)
     c                   endif

     c                   return    not %eof(wdehl)

     p EmpHistExts     e





    P*====================================================================
     P*  Get the Juristication state
    P*====================================================================
     p GetJrSt         b
     d GetJrSt         pi                  like(RSJRST)

     c     keyWMRSL      klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD

     c                   clear                   RSJRST
     c     keyWMRSL      chain     prs00

     c                   return    RSJRST

     p GetJrSt         e





    P*====================================================================
     P*  Get the next group that meets the selected criteria
    P*====================================================================
     p GetNextGrp      b
     d GetNextGrp      pi                  like(FNFND)

     c     keyWMFNL      klist
     c                   kfld                    xxCo#
     c                   kfld                    xxFnd

     c     key1WMFNL     klist
     c                   kfld                    xxCo#

     c     keyWMEXL      klist
     c                   kfld                    FNCO#
     c                   kfld                    FNFND

     c*__________

     c     keyWMFNL      setgt     pfn00

     c                   dou       %eof(wmfnl) or FNFND <> *zero

     c                   clear                   FNFND
     c     key1WMFNL     reade     pfn00

     c                   if        FNTEST = 'Y'
     c                   clear                   FNFND
     c                   iter
     c                   endif

     c     keyWMEXL      chain     pex00

     c                   if        not %found(wmexl) or
     c                             (EXDRGN < xxFrRgn# or EXDRGN > xxToRgn#)
     c                   clear                   FNFND
     c                   iter
     c                   endif

     c                   enddo

     c                   return    FNFND

     p GetNextGrp      e





    P*====================================================================
     P*  Get the WDF1P record
    P*====================================================================
     p GetWDF1P        b
     d GetWDF1P        pi
     d  co#                                const like(F1CO#)
     d  fnd                                const like(F1FND)
     d  fyr                                const like(F1FYR)

     c     keyWDF1L      klist
     c                   kfld                    co#
     c                   kfld                    fnd
     c                   kfld                    fyr

     c                   clear                   F1SRBB
     c     keyWDF1L      chain     pf100

     c                   return

     p GetWDF1P        e





    P*====================================================================
     P* Write record to QXSQP
    P*====================================================================
     p WriteQXSQP      b
     d WriteQXSQP      pi
     d  co#                                const like(SQCO#)
     d  rgn                                const like(SQRGN)
     d  fnd                                const like(SQFND)
     d  fyr                                const like(SQFYR)
     d  emp#                               const like(SQEMP#)
     d  div                                const like(SQDIV)
     d  mapr                               const like(SQMAPR)
     d  wprm                               const like(SQWPRM)

     c                   clear                   psq0p

     c                   eval      SQCO#  = co#
     c                   eval      SQRGN  = rgn
     c                   eval      SQFND  = fnd
     c                   eval      SQFYR  = fyr
     c                   eval      SQEMP# = emp#
     c                   eval      SQDIV  = div
     c                   eval      SQJRST = GetJRST
     c                   eval      SQMAPR = mapr
     c                   eval      SQWPRM = wprm

     c                   write     psq0p

     c                   return

     p WriteQXSQP      e
