     Hcopyright('(c) Copyright Fiserv, Inc., 2007')
     H*****************************************************************
     H/TITLE TESTEMPDEL - Test files included in deletions of employers
     H*****************************************************************
     H*  Created: 02/28/07    C:33    Seq#: WA045/54881               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
      * Employer Master - Live Only
     FWMNBL     if   e           k disk    usropn rename(pnb01:pnb00)
     FWMDPL     if   e           k disk    usropn rename(pdp01:pdp00)
     FWMMPL     if   e           k disk    usropn rename(pmp01:pmp00)
     FWZEXL     if   e           k disk    usropn rename(pex01:pex00)
      * Employer Master - Live & Quote
     FSDCMLB    if   e           k disk    usropn rename(pcm01:pcm0b)
     FSMARL1    if   e           k disk    usropn
     FSMH1L     if   e           k disk    usropn rename(ph101:ph100)
     FSMHDL     if   e           k disk    usropn rename(phd01:phd00)
     FWAQTL7    if   e           k disk    usropn rename(pqt01:pqt07)
     FWMA3L     if   e           k disk    usropn rename(pa301:pa300)
     FWMALL     if   e           k disk    usropn rename(pal01:pal00)
     FWMANL     if   e           k disk    usropn rename(pan01:pan00)
     FWMDBL     if   e           k disk    usropn rename(pdb01:pdb00)
     FWMECL     if   e           k disk    usropn rename(pec01:pec00)
     FWMEML     if   e           k disk    usropn rename(pem01:pem00)
     FWMOOL     if   e           k disk    usropn rename(poo01:poo00)
      * Employer Detail - Live Only
     FWDCXL10   if   e           k disk    usropn rename(pcx01:pcx10)
     FWDEHL     if   e           k disk    usropn rename(peh01:peh00)
     FWDERL     if   e           k disk    usropn rename(per01:per00)
     FWDERL3    if   e           k disk    usropn rename(per01:per03)
     FWDRTL     if   e           k disk    usropn rename(prt01:prt00)
     FWDXHL2    if   e           k disk    usropn rename(pxh01:pxh02)
     FWMAUL6    if   e           k disk    usropn rename(pau01:pau06)
     FWMCCL     if   e           k disk    usropn rename(pcc01:pcc00)
     FWMCCL1    if   e           k disk    usropn
     FWMENL4    if   e           k disk    usropn rename(pen01:pen04)
     FWMMBL5    if   e           k disk    usropn rename(pmb01:pmb05)
     FWMMBL6    if   e           k disk    usropn rename(pmb01:pmb06)
     FWMSHL13   if   e           k disk    usropn rename(psh01:psh13)
     FWXNAL     if   e           k disk    usropn rename(pna01:pna00)
     FWXNAL11   if   e           k disk    usropn rename(pna01:pna11)
     FWXNEL     if   e           k disk    usropn rename(pne01:pne00)
     FWXNHL     if   e           k disk    usropn rename(pnh01:pnh00)
     FWXNXL     if   e           k disk    usropn rename(pnx01:pnx00)
      * Employer Detail - Live & Quote
     FWABLL1    if   e           k disk    usropn
     FWASRL5    if   e           k disk    usropn rename(psr01:psr05)
     FWDDSL1    if   e           k disk    usropn
     FWDELL     if   e           k disk    usropn rename(pel01:pel00)
     FWDLDL1    if   e           k disk    usropn rename(pld01:pld00)
     FWDVHL     if   e           k disk    usropn rename(pvh01:pvh00)
     FWDXSL     if   e           k disk    usropn rename(pxs01:pxs00)
     FWMAHL     if   e           k disk    usropn rename(pah01:pah00)
     FWMCEL1    if   e           k disk    usropn
     FWMEAL     if   e           k disk    usropn rename(pea01:pea00)
     FWMEEL     if   e           k disk    usropn rename(pee01:pee00)
     FWMEPL     if   e           k disk    usropn rename(pep01:pep00)
     FWMEWL1    if   e           k disk    usropn
     FWMMUL     if   e           k disk    usropn rename(pmu01:pmu00)
     FWMRFL     if   e           k disk    usropn rename(rfp01:prf00)
     FWMRPL     if   e           k disk    usropn rename(prp01:prp00)
     FWMWVL1    if   e           k disk    usropn
     FWXFML     if   e           k disk    usropn rename(pfm01:pfm00)
      *
     Fqsysprt   o    f  132        printer oflind(*inoa)
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCo#                  1      3  0
     D  QCFnd                  4      6  0
     D  QCEmp#                 7     15  0
     D  QCDiv                 16     20  0
     D  QCQTE                 40     40

     D count           s              3  0
     D countSDCMP      s                   like(count)
     D countSMARP      s                   like(count)
     D countSMH1P      s                   like(count)
     D countSMHDP      s                   like(count)
     D countWABLP      s                   like(count)
     D countWAQTP      s                   like(count)
     D countWASRP      s                   like(count)
     D countWDCXP      s                   like(count)
     D countWDDSP      s                   like(count)
     D countWDEHP      s                   like(count)
     D countWDELP      s                   like(count)
     D countWDERP      s                   like(count)
     D countWDLDP      s                   like(count)
     D countWDRTP      s                   like(count)
     D countWDVHP      s                   like(count)
     D countWDXHP      s                   like(count)
     D countWDXSP      s                   like(count)
     D countWMAHP      s                   like(count)
     D countWMALP      s                   like(count)
     D countWMANP      s                   like(count)
     D countWMAUP      s                   like(count)
     D countWMA3P      s                   like(count)
     D countWMCCP      s                   like(count)
     D countWMCEP      s                   like(count)
     D countWMDBP      s                   like(count)
     D countWMDPP      s                   like(count)
     D countWMEAP      s                   like(count)
     D countWMECP      s                   like(count)
     D countWMEEP      s                   like(count)
     D countWMEMP      s                   like(count)
     D countWMENP      s                   like(count)
     D countWMEPP      s                   like(count)
     D countWMEWP      s                   like(count)
     D countWMMBP      s                   like(count)
     D countWMMPP      s                   like(count)
     D countWMMUP      s                   like(count)
     D countWMNBP      s                   like(count)
     D countWMOOP      s                   like(count)
     D countWMRFP      s                   like(count)
     D countWMRPP      s                   like(count)
     D countWMSHP      s                   like(count)
     D countWMWVP      s                   like(count)
     D countWXNAP      s                   like(count)
     D countWXNAP1     s                   like(count)
     D countWXFMP      s                   like(count)
     D countWXNEP      s                   like(count)
     D countWXNHP      s                   like(count)
     D countWXNXP      s                   like(count)
     D countWZEXP      s                   like(count)
     D env             s             10
     D detailOnly      s              1n
     D prtFyr          s              4    inz('*ALL')
     D xxQte           s              1    inz
     D @@LiveLib       s             10
     D @@QteLib        s             10

     DDltOvrs          pr
     D inFile                        10    const
     D inEnv                         10    const
     DDltOvrCmd        pr           255
     D inFile                        10    const
     D inEnv                         10    const
     DOvrCmd           pr           255
     D inFile                        10    const
     D inEnv                         10    const
     DOverrides        pr
     D inFile                        10    const
     D inEnv                         10    const

     D/copy sPrRunCmd
     C*
     C     masterKey     klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div
     C*
     C     masterKey2    klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Emp#
     C*
     C     detailKey     klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div
     C                   kfld                    @@Fyr
     C*
     C     detailKey2    klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Emp#
     C                   kfld                    @@Fyr
     C*
     C     detailKey3    klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Fyr
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div
     C*
     C     keyN          klist
     C                   kfld                    SHCo#
     C                   kfld                    SHFnd
     C                   kfld                    SHEmp#
     C                   kfld                    SHFyr
     C                   kfld                    SHUNID
     C*
     C     DLTKY2        klist
     C                   kfld                    SHCo#
     C                   kfld                    SHFnd
     C                   kfld                    SHEmp#
     C*
     C     keyWAQTL7     klist
     C                   kfld                    @@Co#
     C                   kfld                    @@Fnd
     C                   kfld                    @@Emp#
     C                   kfld                    @@Div
     C                   kfld                    xxQte
     C*
     C     *ENTRY        plist
     C                   parm                    @@Co#
     C                   parm                    @@Fnd
     C                   parm                    @@Emp#
     C                   parm                    @@Div
     C                   parm                    @@Fyr
     C                   parm                    @@LiveLib
     C                   parm                    @@QteLib
     C*
     C     *LIKE         DEFINE    OOCo#         @@Co#
     C     *LIKE         DEFINE    OOFnd         @@Fnd
     C     *LIKE         DEFINE    OOEmp#        @@Emp#
     C     *LIKE         DEFINE    OODiv         @@Div
     C     *LIKE         DEFINE    ELFYR         @@Fyr
     C*
     C                   eval      detailOnly = @@Fyr <> *zero
     C                   if        detailOnly
     C                   eval      prtFyr = %editc(@@Fyr:'X')
     C                   endif
     C*
     C                   except    head
     C*
     C                   except    headLive
     C                   eval      env = 'LIVE'
     C                   exsr      LiveOnlyOvr
     C                   exsr      LiveQteOvr
     C                   if        not detailOnly
     C                   exsr      MstrLiveOnly
     C                   exsr      MstrLiveQte
     C                   except    headMaster
     C                   except    mstrLCount
     C                   except    mstrLQCnt
     C                   endif
     C                   exsr      DtlLiveOnly
     C                   exsr      DtlLiveQte
     C                   except    headDetail
     C                   except    dtlLCount
     C                   except    dtlLQCnt
     C                   exsr      LiveOnlyCls
     C                   exsr      LiveQteCls
     C*
     C                   except    headQuote
     C                   eval      env = 'QTE'
     C                   eval      xxQte = 'Q'
     C                   exsr      LiveQteOvr
     C                   if        not detailOnly
     C                   exsr      MstrLiveQte
     C                   except    headMaster
     C                   except    mstrLQCnt
     C                   endif
     C                   exsr      DtlLiveQte
     C                   except    headDetail
     C                   except    dtlLQCnt
     C                   exsr      LiveQteCls
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*====================================================================
     C* Live Only Count - Employer Detail
     C*====================================================================
     C     DtlLiveOnly   begsr
     C*
     C                   clear                   countWDCXP
     C     masterKey     setll     WDCXL10
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WDCXL10
     C                   other
     C     masterKey     reade     WDCXL10
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDCXP = countWDCXP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDEHP
     C     masterKey     setll     WDEHL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WDEHL
     C                   other
     C     masterKey     reade     WDEHL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDEHP = countWDEHP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDERP
     C                   select
     C                   when      detailOnly
     C     detailKey3    setll     WDERL
     C                   other
     C     masterKey     setll     WDERL3
     C                   endsl
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey3    reade     WDERL
     C                   other
     C     masterKey     reade     WDERL3
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDERP = countWDERP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDXHP
     C     masterKey     setll     WDXHL2
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WDXHL2
     C                   other
     C     masterKey     reade     WDXHL2
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDXHP = countWDXHP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMAUP
     C     masterKey     setll     WMAUL6
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMAUL6
     C                   other
     C     masterKey     reade     WMAUL6
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMAUP = countWMAUP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMCCP
     C                   select
     C                   when      detailOnly
     C     detailKey3    setll     WMCCL
     C                   other
     C     masterKey     setll     WMCCL1
     C                   endsl
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey3    reade     WMCCL
     C                   other
     C     masterKey     reade     WMCCL1
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMCCP = countWMCCP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMENP
     C     masterKey     setll     WMENL4
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMENL4
     C                   other
     C     masterKey     reade     WMENL4
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMENP = countWMENP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMMBP
     C                   select
     C                   when      detailOnly
     C     detailKey3    setll     WMMBL5
     C                   other
     C     masterKey     setll     WMMBL6
     C                   endsl
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey3    reade     WMMBL5
     C                   other
     C     masterKey     reade     WMMBL6
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMMBP = countWMMBP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMSHP
     C                   clear                   countWXNAP
     C                   clear                   countWXNAP1
     C                   clear                   countWXNEP
     C                   clear                   countWXNHP
     C                   clear                   countWXNXP
     C     masterKey     setll     WMSHL13
     C                   dou       %eof(WMSHL13)
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMSHL13
     C                   other
     C     masterKey     reade     WMSHL13
     C                   endsl
     C                   if        not %eof
     C                   exsr      SubHistory
     C                   eval      countWMSHP = countWMSHP + 1
     C                   endif
     C                   enddo

     C                   endsr
     C*
     C*====================================================================
     C* Live & Quote Count - Employer Detail
     C*====================================================================
     C     DtlLiveQte    begsr
     C*
     C                   clear                   countWABLP
     C     masterKey     setll     WABLL1
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WABLL1
     C                   other
     C     masterKey     reade     WABLL1
     C                   endsl
     C                   if        not %eof
     C                   eval      countWABLP = countWABLP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWASRP
     C     masterKey     setll     WASRL5
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WASRL5
     C                   other
     C     masterKey     reade     WASRL5
     C                   endsl
     C                   if        not %eof
     C                   eval      countWASRP = countWASRP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDDSP
     C     masterKey     setll     WDDSL1
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WDDSL1
     C                   other
     C     masterKey     reade     WDDSL1
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDDSP = countWDDSP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDELP
     C     masterKey     setll     WDELL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WDELL
     C                   other
     C     masterKey     reade     WDELL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDELP = countWDELP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDLDP
     C     masterKey     setll     WDLDL1
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WDLDL1
     C                   other
     C     masterKey     reade     WDLDL1
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDLDP = countWDLDP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDVHP
     C     masterKey     setll     WDVHL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WDVHL
     C                   other
     C     masterKey     reade     WDVHL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDVHP = countWDVHP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWDXSP
     C     masterKey2    setll     WDXSL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey2    reade     WDXSL
     C                   other
     C     masterKey2    reade     WDXSL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWDXSP = countWDXSP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMAHP
     C     masterKey     setll     WMAHL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMAHL
     C                   other
     C     masterKey     reade     WMAHL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMAHP = countWMAHP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMCEP
     C     masterKey     setll     WMCEL1
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMCEL1
     C                   other
     C     masterKey     reade     WMCEL1
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMCEP = countWMCEP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMEAP
     C     masterKey     setll     WMEAL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMEAL
     C                   other
     C     masterKey     reade     WMEAL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMEAP = countWMEAP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMEEP
     C     masterKey     setll     WMEEL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMEEL
     C                   other
     C     masterKey     reade     WMEEL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMEEP = countWMEEP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMEPP
     C     masterKey     setll     WMEPL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMEPL
     C                   other
     C     masterKey     reade     WMEPL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMEPP = countWMEPP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMEWP
     C     masterKey     setll     WMEWL1
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMEWL1
     C                   other
     C     masterKey     reade     WMEWL1
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMEWP = countWMEWP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMMUP
     C     masterKey     setll     WMMUL
     C                   dou       %eof
     C     masterKey     reade     WMMUL
     C                   if        not %eof
     C                   eval      countWMMUP = countWMMUP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMRFP
     C     masterKey     setll     WMRFL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMRFL
     C                   other
     C     masterKey     reade     WMRFL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMRFP = countWMRFP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMRPP
     C     masterKey     setll     WMRPL
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMRPL
     C                   other
     C     masterKey     reade     WMRPL
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMRPP = countWMRPP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMWVP
     C     masterKey     setll     WMWVL1
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WMWVL1
     C                   other
     C     masterKey     reade     WMWVL1
     C                   endsl
     C                   if        not %eof
     C                   eval      countWMWVP = countWMWVP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWXFMP
     C     masterKey     setll     WXFML
     C                   dou       %eof
     C                   select
     C                   when      detailOnly
     C     detailKey     reade     WXFML
     C                   other
     C     masterKey     reade     WXFML
     C                   endsl
     C                   if        not %eof
     C                   eval      countWXFMP = countWXFMP + 1
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Live Only Count - Employer Master
     C*====================================================================
     C     MstrLiveOnly  begsr
     C*
     C                   clear                   countWMNBP
     C     masterKey2    setll     WMNBL
     C                   dou       %eof
     C     masterKey2    reade     WMNBL
     C                   if        not %eof
     C                   eval      countWMNBP = countWMNBP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMDPP
     C     masterKey     setll     WMDPL
     C                   dou       %eof
     C     masterKey     reade     WMDPL
     C                   if        not %eof
     C                   eval      countWMDPP = countWMDPP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMMPP
     C     masterKey     setll     WMMPL
     C                   dou       %eof
     C     masterKey     reade     WMMPL
     C                   if        not %eof
     C                   eval      countWMMPP = countWMMPP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWZEXP
     C     masterKey     setll     WZEXL
     C                   dou       %eof
     C     masterKey     reade     WZEXL
     C                   if        not %eof
     C                   eval      countWZEXP = countWZEXP + 1
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Live & Quote Count - Employer Master
     C*====================================================================
     C     MstrLiveQte   begsr
     C*
     C                   clear                   countWAQTP
     C     keyWAQTL7     setll     WAQTL7
     C                   dou       %eof
     C     keyWAQTL7     reade     WAQTL7
     C                   if        not %eof
     C                   eval      countWAQTP = countWAQTP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMA3P
     C     masterKey     setll     WMA3L
     C                   dou       %eof
     C     masterKey     reade     WMA3L
     C                   if        not %eof
     C                   eval      countWMA3P = countWMA3P + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMALP
     C     masterKey     setll     WMALL
     C                   dou       %eof
     C     masterKey     reade     WMALL
     C                   if        not %eof
     C                   eval      countWMALP = countWMALP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMANP
     C     masterKey     setll     WMANL
     C                   dou       %eof
     C     masterKey     reade     WMANL
     C                   if        not %eof
     C                   eval      countWMANP = countWMANP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMDBP
     C     masterKey     setll     WMDBL
     C                   dou       %eof
     C     masterKey     reade     WMDBL
     C                   if        not %eof
     C                   eval      countWMDBP = countWMDBP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMECP
     C     masterKey     setll     WMECL
     C                   dou       %eof
     C     masterKey     reade     WMECL
     C                   if        not %eof
     C                   eval      countWMECP = countWMECP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMEMP
     C     masterKey     setll     WMEML
     C                   dou       %eof
     C     masterKey     reade     WMEML
     C                   if        not %eof
     C                   eval      countWMEMP = countWMEMP + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countWMOOP
     C     masterKey     setll     WMOOL
     C                   dou       %eof
     C     masterKey     reade     WMOOL
     C                   if        not %eof
     C                   eval      countWMOOP = countWMOOP + 1
     C                   endif
     C                   enddo
     C*
     C                   exsr      Darts
     C*
     C                   endsr
     C*
     C*====================================================================
     C* DARTS deletions
     C*====================================================================
     C     DARTS         BEGSR
     C*
     C     dartsKey      klist
     C                   kfld                    QCKEY
     C*
     C     addlDartsKey  klist
     C                   kfld                    CMKEY
     C                   kfld                    CMSEQ#
     C*
     C                   CLEAR                   QCKEY
      *
     C                   eval      qcCo#  = @@Co#
     C                   eval      qcFnd  = @@Fnd
     C                   eval      qcEmp# = @@Emp#
     C                   eval      qcDiv  = @@Div
     C                   eval      qcQte  = *blanks
     C*
     C                   select
     C                   when      xxQte = 'Q'
     C                   eval      qcQte  = 'Q'
     C                   endsl
     C*
     C                   clear                   countSDCMP
     C                   clear                   countSMARP
     C     dartsKey      setll     SDCMLB
     C                   dou       %eof(SDCMLB)
     C     dartsKey      reade     SDCMLB
     C                   if        not %eof
     C                   eval      countSDCMP = countSDCMP + 1
     C*
     C     addlDartsKey  setll     SMARL1
     C                   dou       %eof
     C     addlDartsKey  reade     SMARL1
     C                   if        not %eof
     C                   eval      countSMARP = countSMARP + 1
     C                   endif
     C                   enddo
     C*
     C                   endif
     C                   enddo
     C*
     C                   clear                   countSMH1P
     C     dartsKey      setll     SMH1L
     C                   dou       %eof
     C     dartsKey      reade     SMH1L
     C                   if        not %eof
     C                   eval      countSMH1P = countSMH1P + 1
     C                   endif
     C                   enddo
     C*
     C                   clear                   countSMHDP
     C     dartsKey      setll     SMHDL
     C                   dou       %eof
     C     dartsKey      reade     SMHDL
     C                   if        not %eof
     C                   eval      countSMHDP = countSMHDP + 1
     C                   endif
     C                   enddo
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Sumbission History - Employer Detail
     C*====================================================================
     C     SubHistory    begsr
     C*
     C     subHistKey    klist
     C                   kfld                    SHCO#
     C                   kfld                    SHFND
     C                   kfld                    SHEMP#
     C                   kfld                    SHFYR
     C                   kfld                    SHUNID
     C*
     C     subHistKey2   klist
     C                   kfld                    SHCO#
     C                   kfld                    SHFND
     C                   kfld                    SHFYR
     C                   kfld                    SHEMP#
     C                   kfld                    SHDIV
     C*
     C     subHistKey    setll     WXNAL
     C                   dou       %eof
     C     subHistKey    reade     WXNAL
     C                   if        not %eof
     C                   eval      countWXNAP = countWXNAP + 1
     C                   endif
     C                   enddo
     C*
     C     subHistKey2   setll     WXNAL11
     C                   dou       %eof
     C     subHistKey2   reade     WXNAL11
     C                   if        not %eof
     C                   eval      countWXNAP1 = countWXNAP1 + 1
     C                   endif
     C                   enddo
     C*
     C     subHistKey    setll     WXNEL
     C                   dou       %eof
     C     subHistKey    reade     WXNEL
     C                   if        not %eof
     C                   eval      countWXNEP = countWXNEP + 1
     C                   endif
     C                   enddo
     C*
     C     subHistKey    setll     WXNHL
     C                   dou       %eof
     C     subHistKey    reade     WXNHL
     C                   if        not %eof
     C                   eval      countWXNHP = countWXNHP + 1
     C                   endif
     C                   enddo
     C*
     C     subHistKey    setll     WXNXL
     C                   dou       %eof
     C     subHistKey    reade     WXNXL
     C                   if        not %eof
     C                   eval      countWXNXP = countWXNXP + 1
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Close Live Only Files
     C*====================================================================
     C     LiveOnlyCls   begsr
     C*
     C* Master Files
     C                   close     WMNBL
     C                   callp     DltOvrs('WMNBL':'LIVE')
     C*
     C                   close     WMDPL
     C                   callp     DltOvrs('WMDPL':'LIVE')
     C*
     C                   close     WMMPL
     C                   callp     DltOvrs('WMMPL':'LIVE')
     C*
     C                   close     WZEXL
     C                   callp     DltOvrs('WZEXL':'LIVE')
     C*
     C* Detail Files
     C                   close     WDCXL10
     C                   callp     DltOvrs('WDCXL10':'LIVE')
     C*
     C                   close     WDEHL
     C                   callp     DltOvrs('WDEHL':'LIVE')
     C*
     C                   close     WDERL
     C                   callp     DltOvrs('WDERL':'LIVE')
     C*
     C                   close     WDERL3
     C                   callp     DltOvrs('WDERL3':'LIVE')
     C*
     C                   close     WDRTL
     C                   callp     DltOvrs('WDRTL':'LIVE')
     C*
     C                   close     WDXHL2
     C                   callp     DltOvrs('WDXHL2':'LIVE')
     C*
     C                   close     WMAUL6
     C                   callp     DltOvrs('WMAUL6':'LIVE')
     C*
     C                   close     WMCCL
     C                   callp     DltOvrs('WMCCL':'LIVE')
     C*
     C                   close     WMCCL1
     C                   callp     DltOvrs('WMCCL1':'LIVE')
     C*
     C                   close     WMENL4
     C                   callp     DltOvrs('WMENL4':'LIVE')
     C*
     C                   close     WMMBL5
     C                   callp     DltOvrs('WMMBL5':'LIVE')
     C*
     C                   close     WMMBL6
     C                   callp     DltOvrs('WMMBL6':'LIVE')
     C*
     C                   close     WMSHL13
     C                   callp     DltOvrs('WMSHL13':'LIVE')
     C*
     C                   close     WXNAL
     C                   callp     DltOvrs('WXNAL':'LIVE')
     C*
     C                   close     WXNAL11
     C                   callp     DltOvrs('WXNAL11':'LIVE')
     C*
     C                   close     WXNEL
     C                   callp     DltOvrs('WXNEL':'LIVE')
     C*
     C                   close     WXNHL
     C                   callp     DltOvrs('WXNHL':'LIVE')
     C*
     C                   close     WXNXL
     C                   callp     DltOvrs('WXNXL':'LIVE')
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Close Live/Quote Files
     C*====================================================================
     C     LiveQteCls    begsr
     C*
     C* Master Files
     C                   close     SDCMLB
     C                   callp     DltOvrs('SDCMLB':'LIVE')
     C*
     C                   close     SMARL1
     C                   callp     DltOvrs('SMARL1':'LIVE')
     C*
     C                   close     SMH1L
     C                   callp     DltOvrs('SMH1L':'LIVE')
     C*
     C                   close     SMHDL
     C                   callp     DltOvrs('SMHDL':'LIVE')
     C*
     C                   close     WAQTL7
     C                   callp     DltOvrs('WAQTL7':'LIVE')
     C*
     C                   close     WMA3L
     C                   callp     DltOvrs('WMA3L':env)
     C*
     C                   close     WMALL
     C                   callp     DltOvrs('WMALL':env)
     C*
     C                   close     WMANL
     C                   callp     DltOvrs('WMANL':env)
     C*
     C                   close     WMDBL
     C                   callp     DltOvrs('WMDBL':env)
     C*
     C                   close     WMECL
     C                   callp     DltOvrs('WMECL':env)
     C*
     C                   close     WMEML
     C                   callp     DltOvrs('WMEML':env)
     C*
     C                   close     WMOOL
     C                   callp     DltOvrs('WMOOL':env)
     C*
     C* Detail Files
     C                   close     WABLL1
     C                   callp     DltOvrs('WABLL1':env)
     C*
     C                   close     WASRL5
     C                   callp     DltOvrs('WASRL5':env)
     C*
     C                   close     WDDSL1
     C                   callp     DltOvrs('WDDSL1':env)
     C*
     C                   close     WDELL
     C                   callp     DltOvrs('WDELL':env)
     C*
     C                   close     WDLDL1
     C                   callp     DltOvrs('WDLDL1':env)
     C*
     C                   close     WDVHL
     C                   callp     DltOvrs('WDVHL':env)
     C*
     C                   close     WDXSL
     C                   callp     DltOvrs('WDXSL':'LIVE')
     C*
     C                   close     WMAHL
     C                   callp     DltOvrs('WMAHL':env)
     C*
     C                   close     WMCEL1
     C                   callp     DltOvrs('WMCEL1':env)
     C*
     C                   close     WMEAL
     C                   callp     DltOvrs('WMEAL':env)
     C*
     C                   close     WMEEL
     C                   callp     DltOvrs('WMEEL':env)
     C*
     C                   close     WMEPL
     C                   callp     DltOvrs('WMEPL':env)
     C*
     C                   close     WMEWL1
     C                   callp     DltOvrs('WMEWL1':env)
     C*
     C                   close     WMMUL
     C                   callp     DltOvrs('WMMUL':env)
     C*
     C                   close     WMRFL
     C                   callp     DltOvrs('WMRFL':env)
     C*
     C                   close     WMRPL
     C                   callp     DltOvrs('WMRPL':env)
     C*
     C                   close     WMWVL1
     C                   callp     DltOvrs('WMWVL1':env)
     C*
     C                   close     WXFML
     C                   callp     DltOvrs('WXFML':env)
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Perform Live Only Overrides
     C*====================================================================
     C     LiveOnlyOvr   begsr
     C*
     C* Master Files
     C                   callp     Overrides('WMNBL':'LIVE')
     C                   open      WMNBL
     C*
     C                   callp     Overrides('WMDPL':'LIVE')
     C                   open      WMDPL
     C*
     C                   callp     Overrides('WMMPL':'LIVE')
     C                   open      WMMPL
     C*
     C                   callp     Overrides('WZEXL':'LIVE')
     C                   open      WZEXL
     C*
     C* Detail Files
     C                   callp     Overrides('WDCXL10':'LIVE')
     C                   open      WDCXL10
     C*
     C                   callp     Overrides('WDEHL':'LIVE')
     C                   open      WDEHL
     C*
     C                   callp     Overrides('WDERL':'LIVE')
     C                   open      WDERL
     C*
     C                   callp     Overrides('WDERL3':'LIVE')
     C                   open      WDERL3
     C*
     C                   callp     Overrides('WDRTL':'LIVE')
     C                   open      WDRTL
     C*
     C                   callp     Overrides('WDXHL2':'LIVE')
     C                   open      WDXHL2
     C*
     C                   callp     Overrides('WMAUL6':'LIVE')
     C                   open      WMAUL6
     C*
     C                   callp     Overrides('WMCCL':'LIVE')
     C                   open      WMCCL
     C*
     C                   callp     Overrides('WMCCL1':'LIVE')
     C                   open      WMCCL1
     C*
     C                   callp     Overrides('WMENL4':'LIVE')
     C                   open      WMENL4
     C*
     C                   callp     Overrides('WMMBL5':'LIVE')
     C                   open      WMMBL5
     C*
     C                   callp     Overrides('WMMBL6':'LIVE')
     C                   open      WMMBL6
     C*
     C                   callp     Overrides('WMSHL13':'LIVE')
     C                   open      WMSHL13
     C*
     C                   callp     Overrides('WXNAL':'LIVE')
     C                   open      WXNAL
     C*
     C                   callp     Overrides('WXNAL11':'LIVE')
     C                   open      WXNAL11
     C*
     C                   callp     Overrides('WXNEL':'LIVE')
     C                   open      WXNEL
     C*
     C                   callp     Overrides('WXNHL':'LIVE')
     C                   open      WXNHL
     C*
     C                   callp     Overrides('WXNXL':'LIVE')
     C                   open      WXNXL
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Perform Live/Quote Overrides
     C*====================================================================
     C     LiveQteOvr    begsr
     C*
     C* Master Files
     C                   callp     Overrides('SDCMLB':'LIVE')
     C                   open      SDCMLB
     C*
     C                   callp     Overrides('SMARL1':'LIVE')
     C                   open      SMARL1
     C*
     C                   callp     Overrides('SMH1L':'LIVE')
     C                   open      SMH1L
     C*
     C                   callp     Overrides('SMHDL':'LIVE')
     C                   open      SMHDL
     C*
     C                   callp     Overrides('WAQTL7':'LIVE')
     C                   open      WAQTL7
     C*
     C                   callp     Overrides('WMA3L':env)
     C                   open      WMA3L
     C*
     C                   callp     Overrides('WMALL':env)
     C                   open      WMALL
     C*
     C                   callp     Overrides('WMANL':env)
     C                   open      WMANL
     C*
     C                   callp     Overrides('WMDBL':env)
     C                   open      WMDBL
     C*
     C                   callp     Overrides('WMECL':env)
     C                   open      WMECL
     C*
     C                   callp     Overrides('WMEML':env)
     C                   open      WMEML
     C*
     C                   callp     Overrides('WMOOL':env)
     C                   open      WMOOL
     C*
     C* Detail Files
     C                   callp     Overrides('WABLL1':env)
     C                   open      WABLL1
     C*
     C                   callp     Overrides('WASRL5':env)
     C                   open      WASRL5
     C*
     C                   callp     Overrides('WDDSL1':env)
     C                   open      WDDSL1
     C*
     C                   callp     Overrides('WDELL':env)
     C                   open      WDELL
     C*
     C                   callp     Overrides('WDLDL1':env)
     C                   open      WDLDL1
     C*
     C                   callp     Overrides('WDVHL':env)
     C                   open      WDVHL
     C*
     C                   callp     Overrides('WDXSL':env)
     C                   open      WDXSL
     C*
     C                   callp     Overrides('WMAHL':env)
     C                   open      WMAHL
     C*
     C                   callp     Overrides('WMCEL1':env)
     C                   open      WMCEL1
     C*
     C                   callp     Overrides('WMEAL':env)
     C                   open      WMEAL
     C*
     C                   callp     Overrides('WMEEL':env)
     C                   open      WMEEL
     C*
     C                   callp     Overrides('WMEPL':env)
     C                   open      WMEPL
     C*
     C                   callp     Overrides('WMEWL1':env)
     C                   open      WMEWL1
     C*
     C                   callp     Overrides('WMMUL':env)
     C                   open      WMMUL
     C*
     C                   callp     Overrides('WMRFL':env)
     C                   open      WMRFL
     C*
     C                   callp     Overrides('WMRPL':env)
     C                   open      WMRPL
     C*
     C                   callp     Overrides('WMWVL1':env)
     C                   open      WMWVL1
     C*
     C                   callp     Overrides('WXFML':env)
     C                   open      WXFML
     C*
     C                   endsr
    C*====================================================================
     Oqsysprt   e            head             04
     O                                          +40 '# of Records in Files'
     O          e            head        2  0
     O                                          + 0 'Co#:'
     O                       @@Co#         x    + 1
     O                                          + 3 'Grp:'
     O                       @@Fnd         x    + 1
     O                                          + 3 'Emp#:'
     O                       @@Emp#        x    + 1
     O                                          + 3 'Div:'
     O                       @@Div         x    + 1
     O                                          + 3 'Fyr:'
     O                       prtFyr             + 1
     O                                          + 5 'Live Library:'
     O                       @@LiveLib          + 1
     O                                          + 3 'Quote Library:'
     O                       @@QteLib           + 1
     O          e            headLive    3  0
     O                                          + 0 'Live Files'
     O          e            headLive       1
     O                                          + 0 '__________'
     O          e            headQuote   3  0
     O                                          + 0 'Quote Files'
     O          e            headQuote      1
     O                                          + 0 '___________'
     O          e            headMaster  1  0
     O                                          + 2 'Employer Master'
     O          e            headMaster     1
     O                                          + 2 '_______________'
     O          e            headDetail  1  0
     O                                          + 2 'Employer Detail'
     O          e            headDetail     1
     O                                          + 2 '_______________'
     O          e            mstrLCount  1  1
     O                                          + 2 'WMNBP'
     O                                          + 2 'WMDPP'
     O                                          + 2 'WMMPP'
     O                                          + 2 'WZEXP'
     O          e            mstrLCount     1
     O                       countWMNBP    x    + 3
     O                       countWMDPP    x    + 4
     O                       countWMMPP    x    + 4
     O                       countWZEXP    x    + 4
     O          e            mstrLQCnt   1  1
     O                                          + 2 'SDCMP'
     O                                          + 2 'SMARP'
     O                                          + 2 'SMH1P'
     O                                          + 2 'SMHDP'
     O                                          + 2 'WAQTP'
     O                                          + 2 'WMA3P'
     O                                          + 2 'WMALP'
     O                                          + 2 'WMANP'
     O                                          + 2 'WMDBP'
     O                                          + 2 'WMECP'
     O                                          + 2 'WMEMP'
     O                                          + 2 'WMOOP'
     O          e            mstrLQCnt      1
     O                       countSDCMP    x    + 3
     O                       countSMARP    x    + 4
     O                       countSMH1P    x    + 4
     O                       countSMHDP    x    + 4
     O                       countWAQTP    x    + 4
     O                       countWMA3P    x    + 4
     O                       countWMALP    x    + 4
     O                       countWMANP    x    + 4
     O                       countWMDBP    x    + 4
     O                       countWMECP    x    + 4
     O                       countWMEMP    x    + 4
     O                       countWMOOP    x    + 4
     O          e            dtlLCount   1  1
     O                                          + 2 'WDCXP'
     O                                          + 2 'WDEHP'
     O                                          + 2 'WDRTP'
     O                                          + 2 'WDXHP'
     O                                          + 2 'WMAUP'
     O                                          + 2 'WMCCP'
     O                                          + 2 'WMENP'
     O                                          + 2 'WMMBP'
     O                                          + 2 'WMSHP'
     O                                          + 2 'WXNAP'
     O                                          + 2 'WXNAP1'
     O                                          + 1 'WXNEP'
     O                                          + 2 'WXNHP'
     O                                          + 2 'WXNXP'
     O          e            dtlLCount      1
     O                       countWDCXP    x    + 3
     O                       countWDEHP    x    + 4
     O                       countWDRTP    x    + 4
     O                       countWDXHP    x    + 4
     O                       countWMAUP    x    + 4
     O                       countWMCCP    x    + 4
     O                       countWMENP    x    + 4
     O                       countWMMBP    x    + 4
     O                       countWMSHP    x    + 4
     O                       countWXNAP    x    + 4
     O                       countWXNAP1   x    + 4
     O                       countWXNEP    x    + 4
     O                       countWXNHP    x    + 4
     O                       countWXNXP    x    + 4
     O          e            dtlLQCnt    1  1
     O                                          + 2 'WABLP'
     O                                          + 2 'WASRP'
     O                                          + 2 'WDDSP'
     O                                          + 2 'WDELP'
     O                                          + 2 'WDLDP'
     O                                          + 2 'WDVHP'
     O                                          + 2 'WDXSP'
     O                                          + 2 'WMAHP'
     O                                          + 2 'WMCEP'
     O                                          + 2 'WMEAP'
     O                                          + 2 'WMEEP'
     O                                          + 2 'WMEPP'
     O                                          + 2 'WMEWP'
     O                                          + 2 'WMMUP'
     O                                          + 2 'WMRFP'
     O                                          + 2 'WMRPP'
     O                                          + 2 'WMWVP'
     O                                          + 2 'WXFMP'
     O          e            dtlLQCnt       1
     O                       countWABLP    x    + 3
     O                       countWASRP    x    + 4
     O                       countWDDSP    x    + 4
     O                       countWDELP    x    + 4
     O                       countWDLDP    x    + 4
     O                       countWDVHP    x    + 4
     O                       countWDXSP    x    + 4
     O                       countWMAHP    x    + 4
     O                       countWMCEP    x    + 4
     O                       countWMEAP    x    + 4
     O                       countWMEEP    x    + 4
     O                       countWMEPP    x    + 4
     O                       countWMEWP    x    + 4
     O                       countWMMUP    x    + 4
     O                       countWMRFP    x    + 4
     O                       countWMRPP    x    + 4
     O                       countWMWVP    x    + 4
     O                       countWXFMP    x    + 4





     P*====================================================================
     P* Create Delete Override command
     P*====================================================================
     PDltOvrCmd        b
     DDltOvrCmd        pi           255
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s                   like(OvrCmd)

     C                   eval      cmd = 'DLTOVR FILE(' + %trim(inFile)
     C                                 + ') '
     C                                 + 'LVL(*JOB)'

     C                   return    cmd

     PDltOvrCmd        e





     P*====================================================================
     P* Perform Override Deletions
     P*====================================================================
     PDltOvrs          b
     DDltOvrs          pi
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s            255

     C                   eval      cmd = DltOvrCmd(inFile:inEnv)
     C                   callp     RunCmd(cmd:%len(%trim(cmd)))

     PDltOvrs          e





     P*====================================================================
     P* Perform Overrides
     P*====================================================================
     POverrides        b
     DOverrides        pi
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s            255

     C                   eval      cmd = OvrCmd(inFile:inEnv)
     C                   callp     RunCmd(cmd:%len(%trim(cmd)))

     POverrides        e





     P*====================================================================
     P* Create Override command
     P*====================================================================
     POvrCmd           b
     DOvrCmd           pi           255
     D inFile                        10    const
     D inEnv                         10    const

     D cmd             s                   like(OvrCmd)

     C                   eval      cmd = 'OVRDBF FILE(' + %trim(inFile)
     C                                 + ') TOFILE('
     C                   select
     C                   when      inEnv = 'LIVE'
     C                   eval      cmd = %trim(cmd)
     C                                 + %trim(@@LiveLib)
     C                   when      inEnv = 'QTE'
     C                   eval      cmd = %trim(cmd)
     C                                 + %trim(@@QteLib)
     C                   endsl
     C                   eval      cmd = %trim(cmd)
     C                                 + '/' + %trim(inFile) + ') '
     C                                 + 'OVRSCOPE(*JOB)'

     C                   return    cmd

     POvrCmd           e





