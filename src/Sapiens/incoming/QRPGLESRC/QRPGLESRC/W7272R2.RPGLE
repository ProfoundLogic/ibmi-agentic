     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     Hoption(*NODEBUGIO)
    H*********************************************************************
    H/TITLE W7272R1 - Loss Ratio by Job Class using PPC Premium
    H*********************************************************************
    H* Description:  Workfile for the Loss Ratio by Job Class Report     *
    H*                using PPC Premium.                                 *
    H*                                                                   *
    H* Created: 11/14/05    Programmer: C:33    Project: 135WFH          *
    H*********************************************************************
    H* Revisions:                                                        *
     H*
     H* 08/05/15 C:307 Project: SY354                                     *
     H*                -Adding 'Route To:'                                *
     H*                                                                   *
     H* 08/26/14 C:298 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
     H*                                                                   *
     H*  01/22/13  C:287 SEQ#: SY266_OVR
     H*                  Added module source to change compiler options of object.
    H*                                                                   *
    h*  01/28/10  C:249 Project: SY215                                   *
    h*            C:189 Retrieve brand Name                              *
    H*                                                                   *
     H* 05/19/09 C:180 SUPPORT  68553
     H*                Fix run time error RNQ0103
     H*
    H* 10/23/08 C:61  PROJECT 135WLP                                     *
    H*                Add "Hazard Group" to output file.                 *
    H*                                                                   *
      * 10/11/07 C:190 SUPPORT WA099 54504                            *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
      *                                                               *
    H* 07/11/07 C:136 Support: WA135 59043                               *
    H*                The state value wasn't being populated correctly   *
    H*                in WFLJP2. Made correction for employer/div        *
    H*                break so that the correct value would be pulled.   *
    H*                                                                   *
    H* 01/06/06 C:15  Support: WC135/50588                               *
    H*                Re-wrote pgm to process breaks correctly.          *
    H*                                                                   *
    H* 01/04/06 C:15  Support: WC135/50588                               *
    H*                Added code to use the prompt screen "As of" date   *
    H*                to determine which S/R records to include in the   *
    H*                premium calc.                                      *
    H*********************************************************************
     Fsmorl     if   e           k disk    rename(por01:por00)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
     Fwmepl     if   e           k disk    prefix(e1:2)
     Fwmepl4    if   e           k disk    rename(pep01:pep04)
     Fwasrl4    if   e           k disk    rename(psr01:psr04)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwdell     if   e           k disk    rename(pel01:pel00)
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     Fwdrrl     if   e           k disk    rename(prr01:prr00)
     Fwmprl1    if   e           k disk
     Fwmcmlz    if   e           k disk    rename(pcm01:pcm0z)
     Fwmrhl     if   e           k disk    rename(prh01:prh00)
     Fwdppl1    if   e           k disk
     Fwtddl2    if   e           k disk    rename(pdd01:pdd02)
     Fwfljp2    o    e             disk
     Fqsysprt   o    f  132        printer oflind(*inof)

     D*entry
     D W7272R2         PR                  extpgm('W7272R2')
     D  @@Co#                         3
     D  @@Fnd                         3
     D  @@Fyr                         3
     D  @@Prem                        1
     D  @@AsOf                        8
     D  @@TypO                        1
     D  @@Que                        10
     D W7272R2         PI
     D  @@Co#                         3
     D  @@Fnd                         3
     D  @@Fyr                         3
     D  @@Prem                        1
     D  @@AsOf                        8
     D  @@TypO                        1
     D  @@Que                        10

     D/copy sPrDate
     D/copy sPrString
     D/copy sPrCompany
     D/copy wPrGroup
     D/copy wPrEmplr

     D RtvCltID        PR                  extpgm('RTVCLTID')
     D  cltID                         3

     DCalcFreq         pr                  like(LJFREQ)
     D premium                             like(LJWPRM) const
     D claims                              like(classClaims) const

     DCalcLoss         pr                  like(LJLOSS)
     D premium                             like(LJWPRM) const
     D incurred                            like(classTotInc) const

     DDeductible       pr                  like(LJDEDA)

     D                 ds
     D  rhsu                   1     90  2 dim(10)
     D  rhsu01                 1      9  2
     D  rhsu02                10     18  2
     D  rhsu03                19     27  2
     D  rhsu04                28     36  2
     D  rhsu05                37     45  2
     D  rhsu06                46     54  2
     D  rhsu07                55     63  2
     D  rhsu08                64     72  2
     D  rhsu09                73     81  2
     D  rhsu10                82     90  2
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D title0          c                   'Loss Ratio by Job Class'
     D titlePPC        c                   'using PPC Premium'
     D foot1           c                   'RATIOS ARE DERIVED FROM'
     D foot2           c                   'MANUAL PREMIUM AMOUNTS.'
     D allGrps         s              1n
     D allYrs          s              1n
     D companyNm       s             40
     D classClaims     s                   like(LJ#CLM)
     D classPrem       s            + 2    like(EPAMPR)
     D classManPrem    s            + 2    like(EPAMPR)
     D classWrtPrem    s            + 2    like(EPAMPR)
     D classPyrl       s            + 2    like(EPEAPR)
     D classTotInc     s            + 2    like(RHSU01)
     D cltID           s              3
     D***** BEGIN CHANGE ***** 05/19/09 ******************************
     D*** count1          s              5  0
     D count1          s              6  0
     D***** END CHANGE   ***** 05/19/09 ******************************
     D eDays           s              5  0
     D emplrClaims     s                   like(LJ#CLM)
     D emplrDed        s                   like(LJDEDA)
     D emplrPrem       s            + 2    like(EPAMPR)
     D emplrManPrem    s            + 2    like(EPAMPR)
     D emplrWrtPrem    s            + 2    like(EPAMPR)
     D emplrPyrl       s            + 2    like(EPEAPR)
     D emplrTotInc     s            + 2    like(RHSU01)
     D fDays           s              5  0
     D footer          s             60
     D lrempr          s             13  2
     D lrfreq          s              5  2
     D lritot          s             13  2
     D lrloss          s              7  2
     D lrpyrl          s             13  2
     D lrtotc          s              7  0
     D l1empr          s             13  2
     D l1freq          s              5  2
     D l1itot          s             13  2
     D l1loss          s              7  2
     D l1pyrl          s             13  2
     D l1totc          s              7  0
     D prtFnd          s             10
     D prtFyr          s             10
     D rptFile         s               n
     D rptPrint        s               n
     D saveFnd         s                   like(FNFND)
     D saveFyr         s                   like(F1FYR)
     D premHdr         s             10
     D prtGrpNm        s             60
     D prtLoss         s              7  2
     D prtFreq         s              5  2
     D x7AsOf          s                   like(ELPFDT)
     D svAapr          s                   like(EPAAPR)
     D svAmpr          s                   like(EPAMPR)
     D svArat          s                   like(EPARAT)
     D svClas          s                   like(EPCLAS)
     D svEapr          s                   like(EPAAPR)
     D svEmpr          s                   like(EPEMPR)
     D svSafl          s                   like(EPSAFL)
     D svSpec          s                   like(EPSPEC)
     D xxCo#           s                   like(EPCO#)
    D*** Begin add *** 01/04/06 ******************************************
     D xxcym           s                   like(SRRPMY)
    D*** End add ***** 01/04/06 ******************************************
     D xxDiv           s                   like(ELDIV)
     D xxEfDt          s                   like(ELPFDT)
     D xxEmp#          s                   like(ELEMP#)
     D xxFnd           s                   like(EPFND)
     D xxFyr           s                   like(EPFYR)
     D xxFreq          s                   like(LJFREQ)
     D xxLoss          s                   like(LJLOSS)
     D xxMan           s                   like(EPAMPR)
     D xxPremium       s             11  0
     D xxProR          s              5  4
     D xxPrt           s              1
     D xxQue           s                   like(@@Que)
     D xxRate          s                   like(ELRTCD)
     D xxSt            s                   like(RSJRST)
     D x8AsOf          s              8  0
     D yrempr          s             13  2
     D yrfreq          s              5  2
     D yritot          s             13  2
     D yrloss          s              7  2
     D yrpyrl          s             13  2
     D yrtotc          s              7  0
     D sysDt           s               d
     D usaAsOf         s               d   datfmt(*usa)
     D usaDate         s               d   datfmt(*usa)
     D usaTime         s               t   timfmt(*usa)
     D FNrrn           s              9  0
     D EMrrn           s              9  0
     D ##From          s              8  0
     D ##To            s              8  0
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C                   exsr      sr1st
    C*
     C                   select
     C                   when      allGrps
     C     keyWMFNL      setll     pfn00
     C                   other
     C     key1WMFNL     setll     pfn00
     C                   endsl
    C*
     C                   dou       %eof(WMFNL)
    C*
     C                   select
     C                   when      allGrps
     C     keyWMFNL      reade     pfn00
     C                   other
     C     key1WMFNL     reade     pfn00
     C                   endsl
     C*
     C                   if        not %eof(WMFNL) and
     C                             (FNTEST <> 'Y' or not allGrps)
     C                   exsr      ProcessGrp
     C                   endif
     C*
     C                   eval      saveFyr = 0
     C*
     C                   enddo
    C*
     C                   if        rptPrint = *on
     C                   exsr      GroupCalcs
     C                   endif
    C*
     C                   move      *on           *inlr
    C*
     C                   eval      lrLoss = CalcLoss(lrEmpR:lrITot)
     C                   eval      lrFreq = CalcFreq(lrEmpR:lrTotC)
    C*
     C                   if        rptPrint = *on
     C                   except    head
     C                   move      *off          *inof
     C                   except    totlr
      /Free
       //***** Beg Add ***** 08/26/14 ******************************************
       xxPrt = '0';
       Chain  keySMORL POR00;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ******************************************
      /End-free
     C                   endif
    C*
    C*====================================================================
    C*   Set-up and Initialize
    C*====================================================================
     C     sr1st         begsr
    C*
    C*   KEY LIST FOR THE OVERRIDE FILE
    C*
     C     keySMORL      klist
     C                   kfld                    q1pgm
     C                   kfld                    xxPrt
    C*
    C*  USER PARAMETER FILE
    C*
     C     keyWMUPL      klist
     C                   kfld                    q1user
    C*
    C*
    C*  FUND MASTER
    C*
     C     keyWMFNL      klist
     C                   kfld                    xxCo#
    C*
     C     key1WMFNL     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
    C*
    C*  FUND DETAIL
    C*
     C     keyWDF1L      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
    C*
     C     key1WDF1L     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
    C*
    C*  EMPLOYER PAYROLL FILE
    C*
     C     keyWMEPL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    srEMP#
     C                   kfld                    srDIV
     C                   kfld                    srFyr
     C                   kfld                    srefdt
     C                   kfld                    srCLAS
    C*
     C     keyWMEPL4     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
    C*
     C     keyWASRL4     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
     C                   kfld                    xxEMP#
     C                   kfld                    xxDIV
     C                   kfld                    svCLAS
     C** **              kfld                    svSPEC
    C*
    C*  RATE SET CODE FILE
    C*
     C     keyWMRSL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxRate
    C*
    C*   KEY LIST FOR THE CLAIMS FILE
    C*
     C     keyWDRRL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxSt
     C                   kfld                    ELPFDT
    C*
     C     key1WDRRL     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxSt
    C*
    C*  PAYROLL CLASSIFICATION FILE
    C*
     C     keyWMPRL1     klist
     C                   kfld                    xxCo#
     C                   kfld                    ELRTCD
     C                   kfld                    EPCLAS
     C                   kfld                    EPEFDT
    C*
     C     key1WMPRL1    klist
     C                   kfld                    elco#
     C                   kfld                    ELRTCD
     C                   kfld                    EPCLAS
    C*
    C*  EMPLOYER DETAIL FILE
    C*
     C     keyWDELL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
    C*
     C     key1WDELL     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEMP#
     C                   kfld                    xxDIV
     C                   kfld                    xxFYR
    C*
    C*   KEY LIST FOR THE CLAIMS FILE
    C*
     C     keyWMCMLZ     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEMP#
     C                   kfld                    xxDIV
     C                   kfld                    xxFYR
     C                   kfld                    svCLAS
    C*
    C*   KEY LIST FOR THE RESERVE HISTORY FILE
    C*
     C     keyWMRHL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
     C                   kfld                    CMCASE
     C*
     C                   callp     RtvCltID(cltID)
    C*
     C                   move      @@Co#         xxCo#
     C                   move      @@Fnd         xxFnd
     C                   move      @@Fyr         xxFyr
     C                   move      @@AsOf        x8AsOf
    C*
     C                   eval      x7AsOf = DtCYMD(x8AsOf)
     C                   eval      usaAsOf = CvtToDate(x8AsOf)
    C*
     C                   select
     C                   when      xxFnd = *zeros
     C                   eval      prtFnd  = 'ALL GROUPS'
     C                   eval      allGrps = *on
     C                   other
     C                   eval      prtFnd = %editc(xxFnd:'Z')
     C                   endsl
    C*
     C                   select
     C                   when      xxFyr = *zeros
     C                   eval      prtFyr = 'ALL YEARS'
     C                   eval      allYrs = *on
     C                   other
     C                   eval      prtFyr = %editc(xxFyr:'Z')
     C                   endsl
    C*
     C                   if        @@TypO = 'P'
     C                             or @@TypO = 'B'
     C                   eval      rptPrint = *on
     C                   endif
     C                   if        @@TypO = 'F'
     C                             or @@TypO = 'B'
     C                   eval      rptFile = *on
     C                   endif
    C*
     C                   eval      *in09 = not(@@que = *blanks)
     C                   eval      *in08 =     @@que = *blanks
     C                   eval      xxQue = @@Que
     C*
     C                   select
     C                   when      @@prem = 'W'
     C                   eval      premHdr = CenterFld('WRITTEN':%len(premHdr))
     C                   when      @@prem = 'M'
     C                   eval      premHdr = CenterFld('MANUAL':%len(premHdr))
     C                   endsl
     C*
     C                   eval      footer = foot1 + ' ' + %trim(premHdr)
     C                                    + ' ' + foot2
    C*
     C                   eval      sysDt = SystemDate
     C                   eval      usaDate = sysDt
     C                   eval      usaTime = SystemTime
    C*
     C     keyWMUPL      chain     pup00
     C                   eval      companyNm = CenterFld(GetCompNam(xxCo#):
     C                                                   %len(companyNm)   )
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
    C*
    C*   GET THE DISTRIBUTION INFORMATION
    C*
     C                   if        rptPrint = *on
     C                   eval      xxPrt = '0'
     C                   clear                   ORTITL
     C     keySMORL      chain     por00
     C                   if        ORTITL = *blanks
     C                   select
     C                   when      cltID = '135'
     C                   eval      ORTITL = title0 + ' ' + titlePPC
     C                   other
     C                   eval      ORTITL = title0
     C                   endsl
     C                   endif
     C                   if        %found(SMORL)
     C                   eval      ORTITL = LeftJstify(ORTITL)
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
     C                   eval      ORTITL = CenterFld(ORTITL)
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Base Calculations for payroll and premium
    C*====================================================================
     C     BaseCalcs     begsr
    C*
     C                   exsr      NonCommon
    C*
    C* Determine whether estimated or audited payroll should be listed
    C*  based on payroll type flag (EPSAFL)
    C*
     C                   if        svSAFL <> 'A'
     C                   eval      emplrPyrl = emplrPyrl + svEAPR
     C                   else
     C                   eval      emplrPyrl = emplrPyrl + svAAPR
     C                   endif
    C*
    C* Calculate premium according to entered rate date.
    C*
     C                   if        ELFAF = 'A'
     C                   eval      emplrManPrem = svAMPR
     C                   else
     C                   eval      emplrManPrem = svEMPR
     C                   endif
    C*
    C*  Calculate proRata Premium
    C*
     C                   eval(h)   xxPremium = emplrManPrem * xxProR
     C                   eval      emplrManPrem = xxPremium
    C*
     C                   endsr
    C*
    C*====================================================================
    C* CLT135 Calculations for payroll and premium
    C*====================================================================
     C     C135Calcs     begsr
     C*
     C     keyWASRL4     setll     psr04
     C*
     C                   dou       %eof(WASRL4)
     C*
     C     keyWASRL4     reade     psr04
     C*
     C                   if        not %eof(WASRL4)

    c*** Begin add *** 01/04/06 ******************************************
     c                   movel     x7asof        xxcym
     c
     c                   if        srtodt <> 0 and srtodt <= x7asof
     c                              or srtodt = 0 and srrpmy <= xxcym
    c*** End add ***** 01/04/06 ******************************************

     C     keyWMEPL      chain     pep01

     C                   eval      emplrPyrl = emplrPyrl + SRAMTP
     C                   eval(h)   emplrManPrem = emplrManPrem +
     C                              (SRAMTP * e1ARAT / 100)
     C                   eval(h)   emplrWrtPrem = emplrWrtPrem +
     C                              ((SRAMTP * e1ARAT / 100) - Deductible)
     C                   endif
     C*

    c*** Begin add *** 01/04/06 ******************************************
     c                   endif
    c*** End add ***** 01/04/06 ******************************************

     C                   enddo
     C*
     C                   eval      emplrDed = emplrManPrem - emplrWrtPrem
     C*
     C                   endsr
    C*
    C*====================================================================
    C* Class code break
    C*====================================================================
     C     ClassBreak    begsr
     C*
     c***** Begin add **** 05/19/09 ***************************************
     c                   if        rptPrint = *on
     c***** End   add **** 05/19/09 ***************************************
     C                   eval      count1 = count1 + 1
     c***** Begin add **** 05/19/09 ***************************************
     c                   endif
     c***** End   add **** 05/19/09 ***************************************
     C*
     C                   select
     C                   when      cltID = '135'
     C                   exsr      C135Calcs
     C                   other
     C                   exsr      BaseCalcs
     C                   endsl
     C*
     C                   select
     C                   when      @@prem = 'M'
     C                   eval      classPrem = classManPrem
     C                   when      @@prem = 'W'
     C                   eval      classPrem = classWrtPrem
     C                   endsl
     C*
     C                   eval      xxLoss = CalcLoss(classPrem:classTotInc)
     C                   eval      xxFreq = CalcFreq(classPrem:classClaims)
     C*
     C                   move      xxLoss        prtLoss
     C                   move      xxFreq        prtFreq
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Employer Break
    C*====================================================================
     C     EmplrBreak    begsr
    C*
     C     key1WDELL     chain     pel00                              60
     C                   move      ELRTCD        xxRate
     C     keyWMRSL      chain     prs00                              60
     C                   move      RSJRST        xxSt
     C     keyWDRRL      setgt     prr00
     C     key1WDRRL     readpe    prr00                                  60
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Read employer payrolls
    C*====================================================================
     C     EmplPayrolls  begsr
    C*
     C                   exsr      TotIncurred
    C*
     C                   exsr      ClassBreak
    C*
     C                   if        rptFile = *on
     C                   exsr      LoadFile
     C                   endif
    C*
    C*  OUTPUT DETAIL LINE ITEM
    C*
     C                   if        rptPrint = *on
    C*
     C                   eval      classPyrl    = classPyrl    + emplrPyrl
     C                   eval      classManPrem = classManPrem + emplrManPrem
     C                   eval      classWrtPrem = classWrtPrem + emplrWrtPrem
     C                   eval      classClaims  = classClaims  + emplrClaims
     C                   eval      classTotInc  = classTotInc  + emplrTotInc
    C*
     C                   if        *inof = *on
     C                   except    note
     C                   except    head
     C                   eval      *inof = *off
     C                   endif
     C                   except    detail
    C*
    C*  UPDATE FUND AND FINAL TOTALS FOR CLASS
    C*
     C                   eval      yrpyrl = yrpyrl + classPyrl
     C                   eval      yrempr = yrempr + classPrem
     C                   eval      yrtotc = yrtotc + classClaims
     C                   eval      yritot = yritot + classTotInc
    C*
     C                   eval      count1       = 0
     C                   eval      classPyrl    = 0
     C                   eval      classPrem    = 0
     C                   eval      classManPrem = 0
     C                   eval      classWrtPrem = 0
     C                   eval      classClaims  = 0
     C                   eval      classTotInc  = 0
    C*
     C                   endif
    C*
     C                   eval      emplrPyrl    = 0
     C                   eval      emplrPrem    = 0
     C                   eval      emplrManPrem = 0
     C                   eval      emplrWrtPrem = 0
     C                   eval      emplrClaims  = 0
     C                   eval      emplrTotInc  = 0
    C*
      *** beg del ***** 07/11/07 *******************************************************
     C***********        eval      xxEmp# = EPEMP#
     C***********        eval      xxDiv  = EPDIV
      *** end del ***** 07/11/07 *******************************************************
     C                   eval      svclas = EPCLAS
    C*
     C******             if        EPCLAS <> svClas and svClas <> 0
     C******             exsr      ClassBreak
     C******             endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Group break
    C*====================================================================
     C     GroupBreak    begsr
    C*
     C                   eval      prtGrpNm = FNNAME
     C                   if        FNANAM <> *blank
     C                   eval      companyNm = CenterFld(FNANAM:
     C                                                   %len(companyNm))
     C                   endif
    C*
     C                   eval      l1pyrl = 0
     C                   eval      l1empr = 0
     C                   eval      l1totc = 0
     C                   eval      l1itot = 0
    C*
     C                   eval      saveFnd = FNFND
     C                   eval      xxFnd = FNFND
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Group Calculations
    C*====================================================================
     C     GroupCalcs    begsr
    C*
     C                   eval      l1Loss = CalcLoss(l1EmpR:l1ITot)
     C                   eval      l1Freq = CalcFreq(l1EmpR:l1TotC)
    C*
     C                   eval      lrpyrl = lrpyrl + l1pyrl
     C                   eval      lrempr = lrempr + l1empr
     C                   eval      lrtotc = lrtotc + l1totc
     C                   eval      lritot = lritot + l1itot
    C*
    C*  OUTPUT TOTALS
    C*
     C                   if        *inof = *on
     C                   except    head
     C                   endif
     C                   move      *off          *inof
     C                   except    totl1
    C*
     C                   endsr
    C*
    C*====================================================================
    C*  Load the Outfile                                                   *
    C*====================================================================
     C     LoadFile      begsr

     C                   select
     C                   when      @@prem = 'M'
     C                   eval      emplrPrem = emplrManPrem
     C                   when      @@prem = 'W'
     C                   eval      emplrPrem = emplrWrtPrem
     C                   endsl
    C*
     C                   clear                   plj01
    C*
     C                   eval      LJCO#  = EPCO#
     C                   eval      LJCNAM = LeftJstify(companyNm)
     C                   eval      LJFND  = xxFND
     C                   eval      LJFNAM = GetGrpName(xxFND:FNrrn:EPCO#)
     C                   eval      LJRNDT = DateTo8(sysDt)
     C                   eval      LJEMP# = xxEMP#
     C                   eval      LJDIV  = xxDIV
     C                   eval      LJENAM = GetEmpName(xxEMP#:xxDIV:EMrrn:
     C                                       xxFND:EPCO#)
     C                   clear                   EMRGN#
     C     EMrrn         chain     PEM0P
     C                   eval      LJFYR  = xxfyr
     C                   eval      LJRST  = xxSt
     C                   eval      LJRGN# = EMRGN#
     C                   eval      LJCLAS = svCLAS
     C                   eval      LJCLDS = PRDESC
     C                   eval      LJPYRL = emplrPyrl
     C                   eval      LJMAN  = emplrManPrem
     C                   eval      LJWPRM = emplrWrtPrem
     C                   eval      LJDEDA = emplrDed
     C                   eval      LJ#CLM = emplrClaims
     C                   eval      LJTOTI = emplrTotInc
     C                   eval      LJLOSS = CalcLoss(emplrPrem:emplrTotInc)
     C                   eval      LJFREQ = CalcFreq(emplrPrem:emplrClaims)
     C                   eval      LJASOF = x8AsOf
     C                   eval      LJPRFL = @@prem
     ***** beg add ***** 10/23/08 ****************************************
     C                   eval      LJhzgr = prhzgr
     ***** end add ***** 10/23/08 ****************************************
    C*
     C                   write     plj01
    C*
     C                   endsr
    C*
    C*====================================================================
    C*  Load save values                                                 *
    C*====================================================================
     C     LoadSaveVal   begsr
    C*
     C                   if        EPEMP# <> xxEmp#
     C                              or EPDIV  <> xxDiv
     C                   eval      xxemp# = epemp#
     C                   eval      xxdiv  = epdiv
     C                   exsr      EmplrBreak
     C                   endif
     ***** beg add ***** 10/23/08 ****************************************
     c                   eval      prhzgr = *blanks
     ***** end add ***** 10/23/08 ****************************************
    C*
     C     keyWMPRL1     setgt     ppr01
     C     key1WMPRL1    readpe    ppr01                                  60
    C*
     C                   eval      svClas = epclas
     C                   eval      svArat = eparat
     C                   eval      svSpec = epspec
     C                   eval      svSafl = epsafl
     C                   eval      svEapr = epeapr
     C                   eval      svAapr = epaapr
     C                   eval      svEmpr = epempr
     C                   eval      svAmpr = epampr
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Non-Common Expiration Dates
    C*====================================================================
     C     NonCommon     begsr
    C*
    C*  GET NUMBER OF DAYS ACTIVE
    C*
     C                   if        fncgfl = 'Y'
    C*
     C                   eval      xxDiv = *zeros
     C     keyWDELL      chain     pel00                              60
    C*
     C                   eval      ##From = DtMDYY(ELPFDT)
     C                   eval      ##To   = x8AsOf
    C*
     C                   if        ELPTDT < x7AsOf
     C                   eval      ##To   = DtMDYY(ELPTDT)
     C                   endif
    C*
     C                   eval      eDays  = DtDiff(##From:##To)
    C*
     C                   eval      xxProR = eDays / 365
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Process groups
    C*====================================================================
     C     ProcessGrp    begsr
    C*
    C*  Process a group break.
    C*
     C                   if        FNFND <> saveFnd
     C                   exsr      GroupBreak
     C                   endif
    C*
     C                   select
     C                   when      allYrs
     C     keyWDF1L      setll     pf100
     C                   other
     C     key1WDF1L     setll     pf100
     C                   endsl
    C*
     C                   dou       %eof(WDF1L)
    C*
     C                   select
     C                   when      allYrs
     C     keyWDF1L      reade     pf100
     C                   other
     C     key1WDF1L     reade     pf100
     C                   endsl
     C*
     C                   if        not %eof(WDF1L)
     C                   exsr      ProcessGrpYr
     C                   endif
     C*
     C                   enddo
     C*
     C                   endsr
    C*
    C*====================================================================
    C* Process group years
    C*====================================================================
     C     ProcessGrpYr  begsr
    C*
    C*  Process a group break.
    C*
     C                   if        F1FYR <> saveFyr
     C                   exsr      YearBreak
     C                   endif
    C*
    C*  Retrieve the number of employers and manual premium from
    C*   the Employer Payroll File
    C*
     C     keyWMEPL4     setll     pep04
    C*
     C                   dou       %eof(WMEPL4)
    C*
     C     keyWMEPL4     reade     pep04
    C*
     C********           if        not %eof(WMEPL4) and EPSTAT <> 'I'
     C                   if        not %eof(WMEPL4)
    C*
     C                   if        svclas = 0
     C                   exsr      LoadSaveVal
     C                   endif
    C*
     C                   if        EPCLAS <> svclas
     C                              or EPEMP# <> xxEmp#
     C                              or EPDIV  <> xxDiv
     C                   exsr      EmplPayrolls
     C                   exsr      LoadSaveVal
     C                   endif
    C*
     C                   endif
    C*
     C                   enddo
    C*
     C                   if        svclas <> 0
     C                   exsr      EmplPayrolls
     C                   eval      svclas = 0
     C                   if        rptPrint = *on
     C                   exsr      YearCalcs
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Get total incurred, accumulate totals
    C*====================================================================
     C     TotIncurred   begsr
    C*
     C     keyWMCMLZ     chain     pcm0z                              60
     C                   dow       *in60 = *off
    C*
     C                   if        CMIDT <= x7AsOf
    C*
     C                   eval      emplrClaims = emplrClaims + 1
    C*
    C*   ACCUMULATE RESERVE AMOUNTS
    C*
     C     keyWMRHL      chain     prh00                              60
     C                   dow       *in60 = *off
     C                              and RHEFDT <= x7AsOf
     C                   eval      emplrTotInc = emplrTotInc + %xfoot(rhsu)
     C     keyWMRHL      reade     prh00                                  60
     C                   enddo
    C*
     C                   endif
    C*
     C     keyWMCMLZ     reade     pcm0z                                  60
     C                   enddo
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Policy year break
    C*====================================================================
     C     YearBreak     begsr
    C*
    C*  DO THE FOLLOWING CALCULATIONS ONLY IF THIS FUND IS NOT SET
    C*  UP FOR NON-COMMON EXPIRATION DATE POLICIES
    C*
     C                   if        FNCGFL <> 'Y'
    C*
    C*  GET NUMBER OF FUND POLICY DAYS
    C*
     C                   eval      ##From = DtMDYY(F1STPO)
     C                   eval      ##To   = DtMDYY(F1ENPO)
    C*
     C                   eval      fDays = dtDiff(##From:##To)
    C*
     C                   if        fDays = 366 or fDays = 0
     C                   eval      fDays = 365
     C                   endif
    C*
    C*  GET NUMBER OF POLICY DAYS
    C*
     C                   eval      ##From = DtMDYY(F1STPO)
     C                   eval      ##To   = x8AsOf
    C*
     C                   if        F1ENPO < x7AsOf or F1STPO > x7AsOf
     C                   eval      ##To   = DtMDYY(F1ENPO)
     C                   endif
    C*
     C                   eval      eDays = dtDiff(##From:##To)
    C*
     C                   if        x7AsOf > F1ENPO
     C                   eval      eDays = fDays
     C                   endif
    C*
     C                   eval      xxProR = eDays / fDays
    C*
     C                   endif
    C*
    C*  INITIALIZE FIELDS
    C*
     C                   eval      xxEmp# = 0
     C                   eval      xxDiv  = 0
     C                   eval      svclas = 0
    C*
     C                   eval      yrpyrl = 0
     C                   eval      yrempr = 0
     C                   eval      yrtotc = 0
     C                   eval      yritot = 0
    C*
     C                   eval      saveFyr = F1FYR
     C                   eval      xxFyr   = F1FYR
    C*
     C                   if        rptPrint = *on
     C                   except    head
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Year Calculations
    C*====================================================================
     C     YearCalcs     begsr
    C*
     C                   eval      yrLoss = CalcLoss(yrEmpR:YrITot)
     C                   eval      yrfreq = CalcFreq(yrEmpR:yrTotC)
    C*
     C                   eval      l1pyrl = l1pyrl + yrpyrl
     C                   eval      l1empr = l1empr + yrempr
     C                   eval      l1totc = l1totc + yrtotc
     C                   eval      l1itot = l1itot + yritot
    C*
    C*  OUTPUT TOTALS
    C*
     C                   if        *inof = *on
     C                   except    head
     C                   endif
     C                   move      *off          *inof
     C                   except    totyr
    C*
     C                   endsr
    C*==================================================================*
     O/copy sCopyRout
    O**   SELECTION CRITERIA
     O          e            route          1
     O                                           20 'GROUP        :'
     O                       prtFnd            +  1
     O          e            route          1
     O                                           20 'POLICY YEAR  :'
     O                       prtFyr            +  1
     O          e            route          1
     O                                           20 'PREMIUM TYPE :'
     O                       @@Prem            +  1
     O          e            route          1
     O                                           20 'AS OF DATE   :'
     O                       usaAsOf           +  1
     O*** BEG CHG *****  01/28/10  ************************************
     O***       e            route          1 60
     O***                                      +  1 '(c) Copyright Fiserv,'
     O***                                           ' Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
     O          e            head           1 06
     O                       usaTime             08
     O                       companyNm           87
     O                       usaDate            132
     O          e            head           2
     O                       q1pgm               10
     O                       ortitl              96
     O                                          126 'PAGE:'
     O                       page          z    132
     O          e            head           2
     O                                              'GROUP:'
     O                       xxFnd         z   +  1
     O                       prtGrpNm          +  1
     O                                          121 'AS OF DATE:'
     O                       usaAsof            132
     O          e            head           2
     O                                           12 'POLICY YEAR:'
     O                       xxFyr         z   +  1
     O          e            head           1
     O                                           45 '# OF'
     O** **                                      79 'EARNED'
     O                       premHdr             85
     O                                           92 '# OF'
     O                                          120 'LOSS'
     O                                          128 'FREQ'
     O          e            head           2
     O                                           05 'CLASS'
     O                                           18 'DESCRIPTION'
     O                                           47 'EMPLOYERS'
     O                                           64 'TOTAL PAYROLL'
     O** **                                      83 'MANUAL PREMIUM'
     O                                           83 'PREMIUM'
     O                                           93 'CLAIMS'
     O                                          111 'TOTAL INCURRED'
     O                                          128 'RATIO   RATIO'
     O          e            detail         1
     O                       svclas        z     05
     O                       prdesc              37
     O                       count1        3     45
     O                       classPyrl     j     65
     O                       classPrem     j     84
     O                       classClaims   3     93
     O                       classTotInc   j    112
     O                       prtLoss       j    121
     O                       prtFreq       j    129
     O          e            note        1  1
     O                                           32 'RATIOS ARE DERIVED FROM '
     O                                           55 'MANUAL PREMIUM AMOUNTS.'
     O          e            totyr       1  1
     O                                           23 'YEAR TOTALS:'
     O                       yrpyrl        j     65
     O                       yrempr        j     84
     O                       yrtotc        3     93
     O                       yritot        j    112
     O                       yrloss        j    121
     O                       yrfreq        j    129
     O          e            totl1       1  1
     O                                           23 'GROUP TOTALS:'
     O                       l1pyrl        j     65
     O                       l1empr        j     84
     O                       l1totc        3     93
     O                       l1itot        j    112
     O                       l1loss        j    121
     O                       l1freq        j    129
     O          e            totlr       2  1
     O                                           23 'FINAL TOTALS:'
     O                       lrpyrl        j     65
     O                       lrempr        j     84
     O                       lrtotc        3     93
     O                       lritot        j    112
     O                       lrloss        j    121
     O                       lrfreq        j    129
     O          e            totlr          1 60
     O                       footer            + 10





    P*====================================================================
    P* Calculate the frequency ratio
    P*====================================================================
     PCalcFreq         b
    P*
     DCalcFreq         pi                  like(LJFREQ)
     D premium                             like(LJWPRM) const
     D claims                              like(classClaims) const

     D prem            s             11  2

     C                   eval(h)   prem = premium / 1000
    C*
     C                   if        prem <> *zero
     C                   return    claims / prem
     C                   else
     C                   return    *zero
     C                   endif

     PCalcFreq         e





    P*====================================================================
    P* Calculate the loss
    P*====================================================================
     PCalcLoss         b
    P*
     DCalcLoss         pi                  like(LJLOSS)
     D premium                             like(LJWPRM) const
     D incurred                            like(classTotInc) const

     D loss            s             11  3

     C                   if        premium <> *zero
     C                   eval(h)   loss = incurred / premium
     C                   else
     C                   eval      loss = *zero
     C                   endif
     C*
     C                   return    loss * 100

     PCalcLoss         e





    P*====================================================================
    P* Calculate deductible discount amount
    P*====================================================================
     PDeductible       b
    P*
     DDeductible       pi                  like(LJDEDA)

     D dedCode         s                   like(DDCODE)
     D dedPct          s                   like(DDPCT)
     D useDedPct       s              1n   inz(*off)

     C     keyWDPPL1     klist
     C                   kfld                    SRCO#
     C                   kfld                    SRFND
     C                   kfld                    SREMP#
     C                   kfld                    SRDIV
     C                   kfld                    SRFYR
     C                   kfld                    SREFDT

     C     key1WDPPL1    klist
     C                   kfld                    SRCO#
     C                   kfld                    SRFND
     C                   kfld                    SREMP#
     C                   kfld                    SRDIV
     C                   kfld                    SRFYR
    
     C     key2WDELL     klist
     C                   kfld                    SRCO#
     C                   kfld                    SRFND
     C                   kfld                    SREMP#
     C                   kfld                    SRDIV
     C                   kfld                    SRFYR

     C                   clear                   PPEDCT
     C                   clear                   PPMAPR
     C     keyWDPPL1     setgt     ppp01
     C     key1WDPPL1    readpe    ppp01
     C                   if        %eof(WDPPL1)
     C                   exsr      clearPEL00
     C     key2WDELL     chain     pel00
     C                   eval      PPEDCT = ELEDCT
     C                   eval      PPMAPR = ELMAPR
     C                   exsr      getDedPct
     C                   endif

     C                   select
     C                   when      useDedPct
     C                   return    (dedPct / 100) *
     C                              (SRAMTP * SVARAT / 100)
     C                   when      PPMAPR = *zero
     C                   return    *zero
     C                   other
     C                   return    (PPEDCT / PPMAPR) *
     C                              (SRAMTP * SVARAT / 100)
     C                   endsl

     C*====================================================================
     C* Clear EL records needed for other calcs
     C*====================================================================
     C     clearPEL00    begsr
     C*
     C                   clear                   ELEDCT
     C                   clear                   ELMAPR
     C                   clear                   ELDCDE
     C                   clear                   ELDCD2
     C                   clear                   ELDCEF
     C                   clear                   ELDCEX
     C                   clear                   EL2DEF
     C                   clear                   EL2DEX
     C*
     C                   endsr

     C*====================================================================
     C* Get the deductible percentage
     C*====================================================================
     C     getDedPct     begsr
     C*
     C                   eval      useDedPct = *on
     C*
     C     keyWTDDL2     klist
     C                   kfld                    SRCO#
     C                   kfld                    SRFND
     C                   kfld                    xxSt
     C                   kfld                    dedCode
     C                   kfld                    SREFDT
     C*
     C     key1WTDDL2    klist
     C                   kfld                    SRCO#
     C                   kfld                    SRFND
     C                   kfld                    xxSt
     C                   kfld                    dedCode
     C*
     C                   select
     C                   when      SREFDT >= ELDCEF and
     C                              (ELDCEX = 0 or SREFDT < ELDCEX)
     C                   eval      dedCode = ELDCDE
     C                   when      SREFDT >= EL2DEF and
     C                              (EL2DEX = 0 or SREFDT < EL2DEX)
     C                   eval      dedCode = ELDCD2
     C                   endsl
     C*
     C     keyWTDDL2     setgt     pdd02
     C     key1WTDDL2    readpe    pdd02
     C                   if        not %eof
     C*** BEG ADD *****  10/11/07  ************************************
     c                             and (ddstat = 'A')
     C*** END ADD *****  10/11/07  ************************************
     C                   eval      dedPct = DDPCT
     C                   endif
     C*
     C                   endsr

     PDeductible       e
