     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
    H*********************************************************************
    H/TITLE Agent History Update for Multi Employers (Prompt)
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*   Menu                                                            *
    H*  CALLS TO:                                                        *
    H*   W8550C                                                          *
    H*                                                                   *
    H*  Created: 08/30/07      C:132     PROJECT: WC1478                 *
    H*  Rewrite: 12/10/07      C:33      PROJECT: WC1478                 *
    H*********************************************************************
    H*  REVISIONS:                                                        *
     H*
     H* 02/15/22  C:33   Support: 100729                                  *
     H*                  Remove edit - producer chg requires same fein/mod*
     H*
     H* 04/28/17  C:33   Support: 46984                                   *
     H*                  Remove "from" producer edit for different agents *
     H*
     H* 07/28/24  C:300  Support: SEQ# 86336                              *
     H*                  Cove Risk QA - Commission adjustment             *
     H*                  Set the Print Only flag Default to 'N'.          *
     H*
     H* 03/01/10  C:246  Project: 139WNQ - PRISS 12                       *
     H*                  Fix the Primary Agent Default to 'Y'.            *
     H*                  Only one Agent Can be selected on Mass Agent Update
     H*
     H* 01/22/10  C:246  Project: 139WNQ                                   *
     H*                  Add 3 Agebts for Change request.                  *
     H*
     H* 02/20/09  C:33   Project: WC1478    WA071/67113                    *
     H*                  Edit agent information against cx dates.          *
     H*
     H* 04/23/08  C:180   PROJECT WC1478                                   *
     H*                   Fix state validation                             *
    H*********************************************************************
     FW8550F    cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     FWMFNP     IF   E             DISK    rename(pfn01:pfn0p)
     fWMAAL     if   e           k disk    rename(paa01:paa00)
     fWMAAP     if   e             disk    rename(paa01:paa0p)
     fWMAGP     if   e             disk    rename(pag01:pag0p)
     fWMARP     if   e             disk    rename(par01:par0p)
     fWMARL     if   e           k disk    rename(par01:par00)
     fWMAPP     if   e             disk    rename(pap01:pap0p)
     fWMAPL     if   e           k disk    rename(pap01:pap00)
     fWMPSL     if   e           k disk    rename(pps01:pps00)
     fWDRRL1    if   e           k disk
    F*
     D/COPY SCOPYPR01D
     D/COPY sPrDate
     D/COPY sPrString
     D/COPY wPrAgent
     D/COPY wPrGroup
     D/COPY wPrUsrPrms
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Agent History Update for +
     D                                            Multi Employers')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     d ##call          s              1
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     d ##kc            s              1
     d ##kl            s              1
     d ##rrn           s              9  0
     d ##rrn2          s              9  0
     D #@Co#           s              3    inz
     D ##FrFEIN        s              9    inz
     D ##FrMod         s              3    inz
     D ##ToFEIN        s              9    inz
     D ##ToMod         s              3    inz
     D ##AeDt          s              8    inz
     D ##FrRr#         s              3    inz
     D ##ToRt#         s              3    inz
     D ##FrProd        s              5    inz
     D ##ToProd        s              5    inz
     D ##FrGrp         s              3    inz
     D ##ToGrp         s              3    inz
     D ##RatSt         s             20    inz
     D ##PfDt          s              8    inz
     D ##PtDt          s              8    inz
     D ##OutQ          s             10    inz
     D ##PrtOnly       s              1    inz
     D ##scpagt        s              1    inz
     D ##scsagt        s              1    inz
     D ##sctagt        s              1    inz
     D err             s              1
     d toAPRST         s                   like(APRST)
     d toAPLFLG        s                   like(APLFLG)
     d toAPCXDT        s                   like(APCXDT)
     d toAAEFDT        s                   like(AAEFDT)
     d toAACNDT        s                   like(AACNDT)
     d frAPCXDT        s                   like(APCXDT)
     d frAAEFDT        s                   like(AAEFDT)
     d frAACNDT        s                   like(AACNDT)
     D x               s              3  0
     d xxefdt          s              7  0
     D xxfein          s              9  0 inz
     D xxfnd           s              3  0 inz
     D xxgrp           s              3  0 inz
     D xxmod           s              3  0 inz
     d xxprod          s              5  0
     d xxrt#           s              3  0
     d xxrtst          s              2
     d testPrompt      s              1n   inz(*off)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D #ofRatSt        c                   10
     d  scRatSt        ds
     d  scRatSt01              1      2
     d  scRatSt02              3      4
     d  scRatSt03              5      6
     d  scRatSt04              7      8
     d  scRatSt05              9     10
     d  scRatSt06             11     12
     d  scRatSt07             13     14
     d  scRatSt08             15     16
     d  scRatSt09             17     18
     d  scRatSt10             19     20
     d  ratSt                         2    dim(#ofRatSt) overlay(scRatSt)       Rating States
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     c     keyWMAAL      klist
     c                   kfld                    ##co#
     c                   kfld                    xxfein
     c                   kfld                    xxmod
     c                   kfld                    xxgrp
    C*
     c     keyWMARL      klist
     c                   kfld                    ##co#
     c                   kfld                    xxgrp
     c                   kfld                    xxrt#
     c                   kfld                    xxefdt
      *
     c     key1WMARL     klist
     c                   kfld                    ##co#
     c                   kfld                    xxgrp
     c                   kfld                    xxrt#
     C*
     c     keyWMAPL      klist
     c                   kfld                    ##co#
     c                   kfld                    xxfein
     c                   kfld                    xxmod
     c                   kfld                    xxprod
     C*
     c     keyWDRRL1     klist
     c                   kfld                    ##co#
     c                   kfld                    xxfnd
     c                   kfld                    xxrtst
     C*
     c     keyWMPSL      klist
     c                   kfld                    ##co#
     c                   kfld                    scToFEIN
     c                   kfld                    scToMod
     c                   kfld                    scToProd
     c                   kfld                    xxRtSt
    C*
     C                   Movel     'Y'           scpagt
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     c*
     c* ======================== *
      * Validate From Agent FEIN
     c* ======================== *
     c*
     c* If use a range of funds use WMAGP OR if from group zeros and no user group
     c* If have from/to group & the same OR if have user group & from group zeros-WMAAP
     c*
     c                   select
     c*
     c                   when      scFrFEIN = *zeros
     C                   seton                                        84  90
     c*
     c                   when      scFrGrp = *zeros or
     c                             scFrGrp <> scToGrp
     C                   if        not  ValidFein(scFrFEIN:scFrMod:##Co#)
     C                   seton                                        31  90
     c                   endif
     c*
     c                   when      scFrGrp <> *zeros and
     c                              scFrGrp = scToGrp
     c                   eval      xxgrp   = scFrGrp
     c                   eval      xxfein  = scFrFEIN
     c                   eval      xxmod   = scFrMod
     c     keyWMAAL      chain     paa00                              60
     c                   if        *in60 OR aastat = 'I'
     C                   seton                                        32  90
     c                   endif
     C                   eval      frAAEFDT = AAEFDT
     C                   eval      frAACNDT = AACNDT
     c*
     c                   endsl
     c*
     c* ====================== *
      * Validate TO Agent FEIN
     c* ====================== *
     c*
     c  n90              select
     c                   When      scToFein = *zeros
     C                   seton                                        33  90
     c*
     c                   when      scToGrp = *zeros or
     c                             scFrGrp <> scToGrp
     C                   if        not  ValidFein(scToFEIN:scToMod:##Co#)
     C                   seton                                        34  90
     C                   endif
     c*
     c                   when      scFrGrp <> *zeros and
     c                              scFrGrp = scToGrp
     c                   eval      xxgrp   = scFrGrp
     c                   eval      xxfein  = scToFEIN
     c                   eval      xxmod   = scToMod
     c     keyWMAAL      chain     paa00                              60
     c                   if        *in60 OR aastat = 'I'
     C                   seton                                        35  90
     c                   endif
     C                   eval      toAAEFDT = AAEFDT
     C                   eval      toAACNDT = AACNDT
     c*
     c                   endsl
     c*
     c* ================== *
      * Validate From Rate
     c* ================== *
     c*
     c  n90              select
     c*
     c                   when      scFrRt# <> *zeros and
     C                              (scFrFEIN <> scToFEIN or
     C                               scFrMod <> scToMod)
     C                   seton                                        39  90
     c*
     c                   when      scFrRt# <> *zeros and
     C                              scToRt# = *zeros
     C                   seton                                        37  90
     c*
     c                   when      scFrRt# <> *zeros and
     C                              scFrGrp = *zeros
     C                   seton                                        38  90
     c*
     c                   when      scFrRt# <> *zeros and
     C                              scEfDt = *zeros
     C                   seton                                        42  90
     c*
     c                   when      scFrRt# <> *zeros and
     C                              scFrGrp <> scToGrp
     C                   seton                                        41  90
     c*
     c                   when      scFrRt# <> *zeros
     c                   eval      xxrt#  = scFrRt#
     c                   eval      xxgrp  = scFrGrp
     c                   eval      xxefdt = DtCYMD(scEfDt)
     c     keyWMARL      setgt     par00
     c     key1WMARL     readpe    par00                                  60
     c                   dow       not %eof(WMARL) and ARSTAT = 'I'
     c     key1WMARL     readpe    par00                                  60
     c                   enddo
     c                   if        %eof(WMARL)
     C                   seton                                        43  90
     c                   endif
     c*
     c                   endsl
     c*
     c* ================ *
      * Validate To Rate
     c* ================ *
     c*
     C  n90              select
     c*
     c                   when      scToRt# <> *zeros and
     C                              scFrGrp = scToGrp
     c                   eval      xxrt#   = scToRt#
     c                   eval      xxgrp   = scFrGrp
     c                   eval      xxefdt = DtCYMD(scEfDt)
     c     keyWMARL      setgt     par00
     c     key1WMARL     readpe    par00                                  60
     c                   dow       not %eof(WMARL) and ARSTAT = 'I'
     c     key1WMARL     readpe    par00                                  60
     c                   enddo
     c                   if        %eof(WMARL)
     C                   seton                                        44  90
     c                   endif
     c*
     c                   endsl
     c*
     c* ====================== *
      * Validate From Producer
     c* ====================== *
     c  n90              select
     c*
     c***  Begin Del  ***  02/15/22  **************************
     c**********         when      scFrProd <> *zeros and
     C**********                    (scFrFEIN <> scToFEIN or
     C**********                     scFrMod <> scToMod)
     C**********         seton                                        48  90
     c***  End   Del  ***  02/15/22  **************************
     c*
     c                   when      scFrProd <> *zeros and
     C                              scToProd = *zeros
     C                   seton                                        45  90
     c*
     c                   when      scFrProd <> *zeros
     c                   eval      xxfein = scFrFEIN
     c                   eval      xxmod  = scFrMod
     c                   eval      xxprod = scFrProd
     c     keyWMAPL      chain     pap00                              60
     c                   if        not %found(WMAPL) or
     C                             APSTAT = 'I'
     C                   seton                                        49  90
     c                   endif
     C                   eval      frAPCXDT = APCXDT
     c*
     c                   endsl
     c*
     c* ==================== *
      * Validate To Producer
     c* ==================== *
     c*
     C  n90              select
     c*
     c*
     c                   when      scToProd <> *zeros
     c                   eval      xxfein = scToFEIN
     c                   eval      xxmod  = scToMod
     c                   eval      xxprod = scToProd
     c     keyWMAPL      chain     pap00                              60
     c                   if        not %found(WMAPL) or
     C                             APSTAT = 'I'
     C                   seton                                        50  90
     c                   endif
     C                   eval      toAPRST  = APRST
     C                   eval      toAPLFLG = APLFLG
     C                   eval      toAPCXDT = APCXDT
     c*
     c                   endsl
     c*
     c* ====================== *
      * If FEIN/Mods From & To equal, Rate# or Producer must be entered
     c* ====================== *
     C  n90              select
     C*
     c                   when      scFrFEIN = scToFEIN and
     C                              scFrMod = scToMod and
     C                              scFrRt# = *zeros and
     C                              scFrProd = *zeros
     C                   seton                                        36  90
     C*
     C                   endsl
     c*
     c* ====================== *
      * Validate Effective Date
     c* ====================== *
     C*
     C  n90              select
     C*
     c                   when      DtCheck(scEfDt) = *zeros
     C                   seton                                        51  90
     c*
     c                   endsl
     c*
     c  n90              if        scFrGrp <> *zeros and
     c                              scFrGrp = scToGrp
     c*
     c  n90              select
     c                   when      toAAEFDT <> *zero and
     c                              CvtToDate(SCEFDT) < CvtToDate(toAAEFDT)
     C                   seton                                        35  90
     c                   when      toAACNDT <> *zero and
     c                              CvtToDate(SCEFDT) > CvtToDate(toAACNDT)
     C                   seton                                        35  90
     c                   when      toAPCXDT <> *zero and
     c                              CvtToDate(SCEFDT) > CvtToDate(toAPCXDT)
     C                   seton                                        50  90
     c                   endsl
     c*
     c                   endif
     c*
     c* ====================== *
      * Validate Print Only flag
     c* ====================== *
     C*
     C  n90              select
     C*
     c                   when      scPrtOnly <> 'Y' and scPrtOnly <> 'N'
     c                   seton                                        20  90
     c*
     c                   endsl
     c*
     c* ====================== *
      * Validate Agents flag
     c* ====================== *
     C*
     c                   if        *in90 = *off
     c                   z-add     *zeros        Cnt               1 0
      *
     c                   if        scpagt = 'Y'
     c                   add       1             Cnt
     c                   endif
     c                   if        scsagt = 'Y'
     c                   add       1             Cnt
     c                   endif
     c                   if        sctagt = 'Y'
     c                   add       1             Cnt
     c                   endif
     C*
     c                   if        Cnt > 1
     c                   seton                                        66  90
     c                   endif
     c*
     c                   endif
     c*
     c* ========================== *
      * Validate From and TO Group
     c* ========================== *
     c*
     C  n90              if        scFrGrp <> *zeros
     c*
     C  n90              select
     C*
     c                   when      ValidGrp(scFrGrp) <> *on
     c                   seton                                        52  90
     c*
     c                   when      scToGrp < scFrGrp
     c                   seton                                        53  90
     c*
     c                   when      ValidGrp(scToGrp) <> *on
     c                   seton                                        54  90
     c*
     c                   endsl
     c*
     c                   endif
     c*
     C  n90              if        scFrGrp <> *zeros and
     c                             ValidGrp(scToGrp) <> *on
     c                   seton                                        54  90
     c                   endif
     c*
     c* ======================== *
      * Validate RATING State(s)
     c* ======================== *
     c  n90              do        #ofRatSt      x
     c                   if        ratSt(x) <> *blanks
     C                   eval      xxFnd = scFrGrp
     C                   eval      xxRtSt = ratSt(x)
     c     keyWDRRL1     chain     wdrrl1
     c                   if        not %found
     C                   EVAL      *IN90 =  *ON
     C                   eval      *in(70+x) = *on
     C                   leave
     c                   endif
     C  n90              if        scToProd <> *zeros
     C                   select
     C                   when      ratSt(x) = toAPRST and
     C                              toAPLFLG <> 'Y'
     c                   eval      *in90 = *on
     C                   when      ratSt(x) <> toAPRST
     c     keyWMPSL      chain     pps00                                        CO#,FND,FFEIN,FMOD
     C                   if        not %found or
     C                             PSSTAT = 'I' or
     C                             PSLFLG <> 'Y'
     c                   eval      *in90 = *on
     C                   endif
     C                   endsl
     c                   if        *in90
     C                   eval      *in(20+x) = *on
     C                   leave
     C                   endif
     C                   endif
     c                   endif
     c                   enddo
     c*
     c* ==================================== *
      * Validate Policy Effective date Range.
     c* ==================================== *
     c*
     c  n90              if        scPfDt <> *zeros
     c*
     C  n90              select
     c*
     c                   when      DtCheck(scPfDt) = *zeros
     c                   seton                                        81  90
     c*
     c                   when      DtCheck(scPtDt) = *zeros
     c                   seton                                        82  90
     c*
     c                   when      CvtToDate(scPtDt) < CvtToDate(scPfDt)
     c                   seton                                        83  90
     C*
     C                   endsl
     C*
     C                   endif
     C*
     C  n90              if        scPtDt <> *zeros and
     c                             DtCheck(scPtDt) = *zeros
     c                   seton                                        82  90
     C                   endif
     c*
     C                   select
     C                   when      scPrtOnly <> 'Y'
     c*
      * Display window for No From and To Group.
     c*
     C  n90              select
     c                   when      scFrGrp = *zeros
     c                   exfmt     msg3
     c                   eval      *in90 = *inkl
     c                   eval      *inkl = *off
     c                   eval      *in52 = *in90
     c                   eval      *in70 = *in52
     c                   endsl
     c*
      * Display window for No Policy From and To Dates.
     c*
     C  n90              select
     c                   when      scPtDt = *zeros
     c                   exfmt     msg1
     c                   eval      *in90 = *inkl
     c                   eval      *inkl = *off
     c                   eval      *in81 = *in90
     c                   eval      *in70 = *in81
     c                   endsl
      *
      *  Display window before for final OK before calling CL
      *
     C                   if        not *in90
     c                   exfmt     msg2
     c                   eval      *in90 = scyorn <> 'Y' or *inkl
     c                   eval      *inkl = *off
     c                   endif                                                  scyorn <> "Y"
     c*
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
     C*
     c                   if        DtCheck(scEfDt) <> *zeros
     c                   eval      scEfDt = DtCheck(scEfDt)
     c                   endif
     C*
     c                   if        DtCheck(scPfDt) <> *zeros
     c                   eval      scPfDt = DtCheck(scPfDt)
     c                   endif
     C*
     c                   if        DtCheck(scPtDt) <> *zeros
     c                   eval      scPtDt = DtCheck(scPtDt)
     c                   endif
     C*
     C                   eval      *in70 = *on
    C*
     c                   select
      *
      * ======================== *
      *  Select Agent Number
      * ======================== *
      *
     c                   when      hfld = 'SCFRFEIN' or
     C                             hfld = 'SCFRMOD' or
     c                             hfld = 'SCTOFEIN' or
     C                             hfld = 'SCTOMOD'
     c*
     C                   select
     C                   when      scFrGrp = *zero or
     C                             scFrGrp <> scToGrp
     C                   eval      ##rrn = AgtSrchAG(*inkc:*inkl)
     C                   clear                   AGFEIN
     C                   clear                   AGMOD
     c     ##rrn         chain     pag0p
     C                   other
     C                   eval      ##rrn = AgtSrchAA(*inkc:*inkl:err:
     C                                               scFrGrp)
     C                   clear                   AAFEIN
     C                   clear                   AAMOD
     c     ##rrn         chain     paa0p
     c                   eval      AGFEIN = AAFEIN
     c                   eval      AGMOD  = AAMOD
     C                   endsl
     c*
     C                   select
     c                   when      ##rrn <> *zeros
     C                   select
     c                   when      hfld = 'SCFRFEIN' or
     C                             hfld = 'SCFRMOD'
     C                   eval      scFrFEIN = AGFEIN
     C                   eval      scFrMod  = AGMOD
     C                   eval      *in33 = *on
     c                   when      hfld = 'SCTOFEIN' or
     C                             hfld = 'SCTOMOD'
     C                   eval      scToFEIN = AGFEIN
     C                   eval      scToMod  = AGMOD
     C                   eval      *in43 = *on
     C                   endsl
     C                   other
     C                   select
     c                   when      hfld = 'SCFRFEIN' or
     C                             hfld = 'SCFRMOD'
     C                   eval      *in84 = *on
     c                   when      hfld = 'SCTOFEIN' or
     C                             hfld = 'SCTOMOD'
     C                   eval      *in33 = *on
     C                   endsl
     C                   endsl
      *
      * ============================ *
      *  Select Group Numbers
      * ============================ *
      *
     C                   when      hfld = 'SCFRGRP' or
     C                             hfld = 'SCTOGRP'
     C                   eval      ##rrn2 = GrpSrch(*inkc:*inkl)
     c*
     C                   select
     c                   when      ##rrn2 <> *zeros
     C                   clear                   FNFND
     c     ##rrn2        chain     pfn0p
     C                   select
     C                   when      hfld = 'SCFRGRP'
     C                   eval      scFrGrp = FNFND
     C                   eval      *in54 = *on
     C                   when      hfld = 'SCTOGRP'
     C                   eval      scToGrp = FNFND
     C                   eval      *in71 = *on
     C                   endsl
     C                   other
     C                   select
     C                   when      hfld = 'SCFRGRP'
     C                   eval      *in52 = *on
     C                   when      hfld = 'SCTOGRP'
     C                   eval      *in54 = *on
     C                   endsl
     C                   endsl
      *
      * =============================== *
      *  Select Agent Rates
      * =============================== *
      *
     C                   when      hfld = 'SCFRRT#' or
     C                             hfld = 'SCTORT#'
     c*
     C                   select
     c                   when      scFrGrp <> scToGrp or
     C                             scFrGrp = *zero
     C                   select
     C                   when      hfld = 'SCFRRT#'
     C                   eval      *in55 = *on
     C                   when      hfld = 'SCTORT#'
     C                   eval      *in56 = *on
     C                   endsl
     C*
     C                   other
     c*
     c                   eval      ##rrn2 = GetGrpRRN(##Co#:scFrGrp)
     c*
     c                   call      'W0640R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm      '2'           ##call
     c                   parm      *zeros        ##rrn
     c                   parm                    ##rrn2
     c*
     C                   select
     c                   when      ##rrn <> *zeros
     C                   clear                   ARRT#
     c     ##rrn         chain     par0p
     C                   select
     C                   when      hfld = 'SCFRRT#'
     C                   eval      scFrRt# = ARRT#
     C                   eval      *in44 = *on
     C                   when      hfld = 'SCTORT#'
     C                   eval      scToRt# = ARRT#
     C                   eval      *in49 = *on
     C                   endsl
     c                   other
     C                   select
     C                   when      hfld = 'SCFRRT#'
     C                   eval      *in43 = *on
     C                   when      hfld = 'SCTORT#'
     C                   eval      *in44 = *on
     C                   endsl
     C                   endsl
     C                   endsl
      *
      * ============================ *
      *  Select Producer Number
      * ============================ *
      *
     C                   when      hfld = 'SCFRPROD' or
     C                             hfld = 'SCTOPROD'
     c*
     C                   select
     c*
     c*
     C                   when      hfld = 'SCFRPROD' and
     C                              not ValidFein(scFrFEIN:scFrMod:##Co#)
     C                   seton                                        31  90
     C                   when      hfld = 'SCTOPROD' and
     C                              not ValidFein(scToFEIN:scToMod:##Co#)
     C                   seton                                        34  90
     C*
     C                   other
     c*
     C                   select
     C                   when      hfld = 'SCFRPROD'
     C                   eval      ##rrn = GetMstrRRN(scFrFEIN:scFrMod)
     C                   when      hfld = 'SCTOPROD'
     C                   eval      ##rrn = GetMstrRRN(scToFEIN:scToMod)
     C                   endsl
     c*
     c                   call      'W0042R'
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm      '2'           ##call
     c                   parm                    ##rrn
     c*
     C                   select
     c                   when      ##rrn <> *zeros
     C                   clear                   APPROD
     c     ##rrn         chain     pap0p
     C                   select
     C                   when      hfld = 'SCFRPROD'
     C                   eval      scFrProd = APPROD
     C                   eval      *in50 = *on
     C                   when      hfld = 'SCTOPROD'
     C                   eval      scToProd = APPROD
     C                   eval      *in51 = *on
     C                   endsl
     C                   other
     C                   select
     C                   when      hfld = 'SCFRPROD'
     C                   eval      *in49 = *on
     C                   when      hfld = 'SCTOPROD'
     C                   eval      *in50 = *on
     C                   endsl
     C                   endsl
     C                   endsl
     c*
     c                   ENDSL
     C*
     C                   eval      *inkl = *off
     c*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
     C                   if        %parms >= 1 and
     C                              @@prds <> *blanks
     C                   eval      scPrDs = @@PrDs
     C                   else
     C                   eval      scPrDs = optNm
     C                   endif
     C                   eval      scPrDs = CenterFld(scPrDs)
    C*
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
    C*
      /Free
       If scPrtOnly = ' ';
          scPrtOnly = 'N';
       EndIf;
      /End-Free
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
      *
     c                   sorta     ratSt
      *
     c                   MOVE(P)   ##Co#         #@Co#
     c                   MOVE(P)   scFrFEIN      ##FrFEIN
     c                   MOVE(P)   scFrMod       ##FrMod
     c                   MOVE(P)   scToFEIN      ##ToFEIN
     c                   MOVE(P)   scToMod       ##ToMod
     c                   MOVE(P)   scEfDt        ##AeDt
     c                   MOVE(P)   scFrRt#       ##FrRr#
     c                   MOVE(P)   scToRt#       ##ToRt#
     c                   MOVE(P)   scFrProd      ##FrProd
     c                   MOVE(P)   scToProd      ##ToProd
     c                   MOVE(P)   scFrGrp       ##FrGrp
     c                   MOVE(P)   scToGrp       ##ToGrp
     c                   eval      ##RatSt = %trim(scRatSt)
     c                   MOVE(P)   scPfDt        ##PfDt
     c                   MOVE(P)   scPtDt        ##PtDt
     c                   eval      ##OutQ = GetUsrOutq(q1User)
      *
     C* If testPrompt is set *ON, the process will not continue w/next step
     C                   if        not testPrompt
     c*
     c                   call      'W8550C'
     c                   parm                    #@Co#
     c                   parm                    ##FrFEIN
     c                   parm                    ##FrMod
     c                   parm                    ##ToFEIN
     c                   parm                    ##ToMod
     c                   parm                    ##AeDt
     c                   parm                    ##FrRr#
     c                   parm                    ##ToRt#
     c                   parm                    ##FrProd
     c                   parm                    ##ToProd
     c                   parm                    ##FrGrp
     c                   parm                    ##ToGrp
     c                   parm                    ##RatSt
     c                   parm                    ##PfDt
     c                   parm                    ##PtDt
     c                   parm                    ##OutQ
     c                   parm      scPrtOnly     ##PrtOnly
     c                   parm      scpagt        ##scpagt
     c                   parm      scsagt        ##scsagt
     c                   parm      sctagt        ##sctagt
     C*
     C                   endif
     c
     C                   endsr
     c*
