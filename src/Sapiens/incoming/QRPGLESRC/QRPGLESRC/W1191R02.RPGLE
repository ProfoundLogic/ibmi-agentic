     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H*********************************************************************
    h/TITLE W1191R02 - HCN Program Information
    H*********************************************************************
     H* Created: 11/10/10   C:33   Project: 139WPL
    H*********************************************************************
     H* Revisions:
     H* 01/14/21   C:180  Project WC2786
     H*                   Rename field WMENP/ENDSC2 as it was giving a error
     h*                   on compile.
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw1191f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdhcp     uf a e             disk    rename(phc01:dbrcdf)
     F                                     infds(dbFds)
     Fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwdell1    if   e           k disk
     Fwmshl     if   e           k disk    rename(psh01:psh00)
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     Fwdhcl3    if   e           k disk    rename(phc01:phc03)
     Fwdhcl4    if   e           k disk    rename(phc01:phc04)
     F                                     infds(dHcFds4)
     Fwdhcl5    if   e           k disk    rename(phc01:phc05)
     Fwmhcl     if   e           k disk    rename(phc01:phc00)
     Fwdf2l     if   e           k disk    rename(pf201:pf200)
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     Fwmmul5    uf   e           k disk    rename(pmu01:pmu05)
     *** Begin Change *** 01/14/21 *****************************************
     F*wmenl3    if a e           k disk    rename(pen01:pen03) usrOpn
     Fwmenl3    if a e           k disk    rename(pen01:pen03) usrOpn prefix(x)
     *** End   Change *** 01/14/21 *****************************************
     Fwtenl     if   e           k disk    rename(pen01:pen00)
     Fwtmyp     if   e             disk    rename(pmy01:pmy0p)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'HCN Program Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     D optInRmCd       c                   'PSHI'
     D optOutRmCd      c                   'PSHO'
     D updRmCd         c                   'PSHU'
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D #@Co#           s              3
     D #@Fnd           s              3
     D #@Emp#          s              9
     D #@Div           s              5
     D #@Fyr           s              3
     D #@Mode          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D addMode         s              1n
     D chgMode         s              1n
     D cpyMode         s              1n
     D dspMode         s              1n
     D allowIn         s              1n   inz('0')
     D elRRN           s              9  0
     D fnRRN           s              9  0
     D emRRN           s              9  0
     D xxRRN           s              9  0
     D xxEfDt          s                   like(HCEFDT)
     D svIn            s                   like(HCIN)
     D svOut           s                   like(HCOUT)
     D holdDiv         s                   like(HCDIV)
     D holdIn          s                   like(HCIN)
     D holdOut         s                   like(HCOUT)
     D holdSt          s                   like(HCST)
     D holdHCIN        s                   like(HCIN)
     D holdHCOUT       s                   like(HCOUT)
     D quoteFlag       s              1
     D xxForm          s                   like(MUFORM) inz('WC420408')
     D xxEnd#          s                   like(MUEND#) inz(0)
     D endorseAdded    s              1n   inz
     D endorseInUpd    s              1n   inz
     D midTerm         s              1n
     D needEndorse     s              1n
     D*
     DsvLDA            ds          1024    inz
     D ldaEnd#                 1      5  0
     D ldaEndA                 1      5
     D ldaMYRR                 6     14  0
     D ldaEEDt                15     22  0
     D ldaFyr                 23     25  0
     D ldaPyRl                26     26
     D*
     D dbFds           ds
     D  dbRRN                397    400b 0
     D dHcFds4         ds
     D  dHcRRN4              397    400b 0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DAlphaOut         pr            10
     D inDate                              like(HCOUT)
    D*
     DChkQuoteEnv      pr             1
    D*
     DGetEmpSt         pr                  like(RSJRST)
     D inCo#                               like(RSCO#)
     D inRtCd                              like(RSRTCD)
    D*
     DMaxOptReached    pr             1n
     D inCo#                               like(HCCO#)
     D inFnd                               like(HCFND)
     D inEmp#                              like(HCEMP#)
     D inDiv                               like(HCDIV)
     D inFyr                               like(HCFYR)
     D*
     DMbrExists        pr             1n
     D outObj                        10    const
     D outType                        7    const
     D outMbr                        10    const
    D*
     DPgmExistsInStk   pr                  extPgm('SRCHKPGM')
     D program                       10                 const
     D exists                         1n
    D*
     DPolicyIncept     pr             1
     D inCo#                               like(HCCO#)
     D inFnd                               like(HCFND)
     D inEmp#                              like(HCEMP#)
     D inDiv                               like(HCDIV)
     D inFyr                               like(HCFYR)
    D*
     DProcessDivs      pr             1n
    D*
     DStateExists      pr             1n
     D inSt                                like(HCST)   const
     D inIn                                like(HCIN)   const
     D inOut                               like(HCOUT)  const
    D*
     DWriteDARTS       pr
     D inOrOut                        4    const
     D inRmCd                         5    const
    D*
     DWrtMidTermInfo   pr
     D inOrOut                        4    const
     D inRmCd                         5    const
    C*
     C/copy wPrGroup
     C/copy wPrEmplr
     C/copy wPrAgent
    C*
     C/COPY SCOPYMT42C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     keyGroup      klist
     C                   kfld                    FNCO#
     C                   kfld                    FNFND
     C*
     C     keyWDELL1     klist
     C                   kfld                    HCCO#
     C                   kfld                    HCFND
     C                   kfld                    HCFYR
     C                   kfld                    HCEMP#
     C*
     C     keyWMHCL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    scSt
     C*
     C     key1WMHCL     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    scSt
     C                   kfld                    xxEfDt
     C*
     C     keyWDHCL4     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    holdSt
     C                   kfld                    holdIn
     C*
     C     keyWDHCL5     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELFYR
     C*
     C     keyWMMUL5     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELFYR
     C                   kfld                    ELEMP#
     C                   kfld                    xxForm
     C                   kfld                    xxEnd#
     C*
     C     keyWTENL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    xxForm
    C*
    C*====================================================================
    C* ChkEndorse - Check to determine if endorsement should be attached
    C*====================================================================
     C     ChkEndorse    begsr
    C*
     C                   clear                   holdHCIN
     C                   clear                   holdHCOUT
    C*
     C     keyWDHCL5     setll     phc05
     C                   dou       %eof
     C     keyWDHCL5     reade     phc05
     C                   if        not %eof
     C                   select
     C                   when      holdHCIN > HCIN
     C                   eval      holdHCIN = HCIN
     C                   when      holdHCIN = *zero
     C                   eval      holdHCIN = HCIN
     C                   endsl
     C                   select
     C                   when      HCOUT = *zero
     C                   eval      holdHCOUT = *hival
     C                   when      holdHCOUT < HCOUT
     C                   eval      holdHCOUT = HCOUT
     C                   when      holdHCOUT = *zero
     C                   eval      holdHCOUT = HCOUT
     C                   endsl
     C                   endif
     C                   enddo
     C*
     C                   select
     C                   when      holdHCIN <> *zero
     C                              and holdHCOUT = *hival
     C                   eval      needEndorse = *on
     C                   when      holdHCIN <> *zero
     C                              and (holdHCOUT = *hival
     C                                 or holdHCOUT >= DateTo7(SystemDate))
     C                   eval      needEndorse = *on
     C                   other
     C                   eval      needEndorse = *off
     C                   endsl
     C*
     C* Check for the existence of a Policy Page endorsement
     C                   eval      xxEnd# = *zero
     C     keyWMMUL5     chain     pmu05
     C                   select
     C                   when      %found and not needEndorse
     C                              and MUEXFL <> 'E'
     C                   eval      MUEXFL = 'E'
     C                   eval      MUEEDT = holdHCIN
     C                   exsr      MaintFldsMU
     C                   update    pmu05
     C                   when      %found and needEndorse
     C                              and MUEXFL <> 'I'
     C                   eval      MUEXFL = 'I'
     C                   eval      MUEEDT = holdHCIN
     C                   exsr      MaintFldsMU
     C                   update    pmu05
     C                   eval      endorseAdded = *on
     C                   when      not %found and needEndorse
     C                   exsr      WriteWMMUP
     C                   eval      endorseAdded = *on
     C                   when      %found and MUEEDT > holdHCIN
     C                   eval      MUEEDT = holdHCIN
     C                   exsr      MaintFldsMU
     C                   update    pmu05
     C                   eval      endorseInUpd = *on
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* DltPre - Pre-processing before delete occurs
    C*====================================================================
     C     DltPre        begsr
     C                   endsr
    C*
    C*====================================================================
    C* DsplyR - Processing after displaying information
    C*====================================================================
     C     DsplyR        begsr
     C                   endsr
    C*
    C*====================================================================
    C* Errck1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     Errck1        begsr
     C*
     C                   if        addMode or allowIn
     C*
     C                   if        DtCheck(scIn) = *zero
     C                   seton                                        72  90
     C                   leavesr
     C                   endif
     C*
     C                   if        CvtToDate(scIn) < CvtToDate(ELPFDT)
     C                              or CvtToDate(scIn) > CvtToDate(ELPTDT)
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C*
     C                   if        scOut <> *zero and
     C                              DtCheck(scOut) = *zero
     C                   seton                                        73  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scOut <> *zero
     C                              and CvtToDate(scIn) > CvtToDate(scOut)
     C                   seton                                        80  90
     C                   leavesr
     C                   endif
     C*
     C                   if        StateExists(scSt
     C                                        :DtCYMD(scIn)
     C                                        :DtCYMD(scOut)
     C                                        )
     C                   seton                                        74  90
     C                   leavesr
     C                   endif
     C*
     C*
     C                   endsr
    C*
    C*====================================================================
    C* Errck2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
     C     Errck2        begsr
     C                   endsr
    C*
    C*====================================================================
    C* Errck3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
     C     Errck3        begsr
     C                   endsr
    C*
    C*====================================================================
    C* LoadScrHead - Load screen header information
    C*====================================================================
     C     LoadScrHead   begsr
    C*
     C                   eval      scFnd   = ELFND
     C                   eval      scFndNm = GetGrpName(ELFND:fnRRN:ELCO#)
     C                   eval      scEDN = EmpDivName(ELEMP#:ELDIV:emRRN
     C                                               :ELFND:ELCO#)
     C                   eval      *in63 = GetPrvEmp#(ELCO#:ELFND:ELEMP#
     C                                               :ELDIV
     C                                               :scOFnd:scOEmp:scODiv)
     C                                     = *blanks
     C                   eval      sc#Div = DivCount(ELCO#:ELFND:ELEMP#)
     C                   eval      scFyr = ELFYR
     C                   eval      scSt = GetEmpSt(ELCO#:ELRTCD)
     C                   eval      scCxDt = DtMDYY(GetCxDate(ELCO#:ELFND
     C                                                      :ELEMP#:ELDIV)
     C                                            )
     C                   eval      *in41 = scCxDt <> *zero
     C                   if        PolicyIncept(ELCO#:ELFND:ELEMP#
     C                                         :ELDIV:ELFYR) = 'Y'
     C                              and chgMode
     C                   eval      *in40 = *on
     C                   eval      allowIn = *on
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MaintFlds - Load maintenance fields
    C*====================================================================
     C     MaintFlds     begsr
     C*
     C                   eval      HCUSER = q1User
     C                   eval      HCPGM  = q1Pgm
     C                   eval      HCDT   = DateTo7(SystemDate)
     C                   eval      HCTIME = TimeTo6(SystemTime)
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MaintFldsInit - Load initial maintenance fields
    C*====================================================================
     C     MaintFldsInit begsr
     C*
     C                   exsr      MaintFlds
     C*
     C                   eval      HCIUSR = HCUSER
     C                   eval      HCIPGM = HCPGM
     C                   eval      HCIDT  = HCDT
     C                   eval      HCITM  = HCTIME
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MaintFldsMU - Load maintenance fields for WMMUP
    C*====================================================================
     C     MaintFldsMU   begsr
     C*
     C                   eval      MUUSER = q1User
     C                   eval      MUPGM  = q1Pgm
     C                   eval      MUDT   = DateTo7(SystemDate)
     C                   eval      MUTIME = TimeTo6(SystemTime)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
    C*
     C                   if        addMode
     C                   select
     C                   when      PolicyIncept(ELCO#:ELFND:ELEMP#
     C                                              :ELDIV:ELFYR)
     C                                 = 'Y'
     C                   eval      HCIN = ELPFDT
     C                   other
     C                   eval      HCIN = DtCYMD(ldaEEDt)
     C                   endsl
     C                   endif
    C*
     C                   eval      scIn   = DtMDYY(HCIN)
     C                   eval      scOut  = DtMDYY(HCOUT)
     C                   eval      scFact = HCFACT
     C                   eval      scRenw = HCRENW
     C*
     C* Save fields
     C                   eval      svIn   = HCIN
     C                   eval      svOut  = HCOUT
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MvScDb - Move screen fields to data base fields
    C*====================================================================
     C     MvScDb        begsr
     C*
     C                   if        addMode
     C                   eval      HCCO#  = ELCO#
     C                   eval      HCFND  = ELFND
     C                   eval      HCEMP# = ELEMP#
     C                   eval      HCDIV  = ELDIV
     C                   eval      HCFYR  = ELFYR
     C                   eval      HCST   = scSt
     C                   eval      HCINCP = PolicyIncept(HCCO#:HCFND:HCEMP#
     C                                                  :HCDIV:HCFYR)
     C                   endif
     C*
     C                   eval      HCIN   = DtCYMD(scIn)
     C                   eval      HCOUT  = DtCYMD(scOut)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OthErr - Processing for other modes
    C*====================================================================
     C     OthErr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OthFks - Processing for other functions keys
    C*====================================================================
     C     OthFks        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
     C*
     C                   if        not dspMode
     C                              and not *inkc and not *inkl
     C                   exsr      UpdateDivs
     C                   exsr      ChkEndorse
     C                   endif
     C*
     C                   eval      *inlr = *on
     C                   endsr
    C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C                   callp     PgmExistsInStk('W0051R1':midTerm)
     C*
     C                   if        midTerm
     C     *dtaAra       define    *lda          svLDA
     C     *lock         in        *dtaAra
     C                   out       *dtaAra
     C                   endif
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      chgMode = @@Mode = 'C'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      cpyMode = @@Mode = 'P'
     C* Screen processing indicators
     C                   eval      *in09 = dspMode
     C                   eval      *in40 = addMode
     C*
     C                   eval      QuoteFlag = ChkQuoteEnv
     C*
     C                   eval      xxRRN = @@rrn
     C                   eval      elRRN = @@rrn2
     C     @@rrn         chain     dbrcdf
     C                   if        %found
     C                              and not midTerm
     C                   eval      elRRN = GetEmpYrRR(HCFYR:HCEMP#:HCDIV
     C                                               :HCFND:HCCO#)
     C                   endif
     C     elRRN         chain     WDELP
     C                   eval      xxEfDt = HCIN
     C                   if        addMode
     C                   eval      xxEfDt = ELPFDT
     C                   endif
     C*
     C                   exsr      LoadScrHead
     C*
     C                   clear                   HCIN
     C                   clear                   HCOUT
     C                   clear                   svIn
     C                   clear                   svOut
     C*
     C                   clear                   FNPEOF
     C     fnRRN         chain     wmfnp
     C                   clear                   F2CAR#
     C     keyGroup      chain     wdf2l
     C                   clear                   EXUPDA
     C     keyGroup      chain     wmexl
     C*
     C* Get group record
     C     key1WMHCL     setgt     wmhcl
     C     keyWMHCL      readpe    wmhcl
     C                   if        addMode
     C                              and (%eof
     C                                 or (HCEXDT <> *zero
     C                                  and HCEXDT < xxEfDt)
     C                                  )
     C                   eval      scEfDt = DtMDYY(xxEfDt)
     C                   exfmt     SCRGRP
     C                   exsr      PgmEnd
     C                   return
     C                   endif
     C*
     C                   if        HCFORM <> *blanks
     C                   eval      xxForm = HCFORM
     C                   endif
     C*
     C* Check # of opt in/out times for add mode
     C                   if        addMode
     C                              and MaxOptReached(ELCO#:ELFND
     C                                               :ELEMP#:ELDIV:ELFYR)
     C                   exfmt     SCRMAX
     C                   exsr      PgmEnd
     C                   return
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PreOth - Pre-processing for other modes
    C*====================================================================
     C     PreOth        begsr
     C                   endsr
    C*
    C*====================================================================
    C* UpdateDivs - Update all divisions
    C*====================================================================
     C     UpdateDivs    begsr
     C*
     C                   if        not ProcessDivs
     C                   leavesr
     C                   endif
     C*
     C                   eval      holdDiv = HCDIV
     C                   eval      holdIn  = HCIN
     C                   eval      holdOut = HCOUT
     C                   eval      holdSt  = HCST
     C*
     C     keyWDELL1     setll     wdell1
     C                   dou       %eof
     C     keyWDELL1     reade     wdell1
     C                   if        not %eof and ELDIV <> holdDiv
     C                   select
     C                   when      chgMode
     C     keyWDHCL4     chain     wdhcl4
     C                   if        %found
     C                              and HCOUT = svOut
     C                   eval      xxRRN = dHcRRN4
     C                   exsr      UpdatR
     C                   endif
     C                   when      addMode
     C     keyWDHCL4     chain     wdhcl4
     C                   if        not %found
     C                              and GetEmpSt(ELCO#:ELRTCD) = holdSt
     C                              and not MaxOptReached(ELCO#:ELFND:ELEMP#
     C                                                   :ELDIV:ELFYR)
     C                              and not StateExists(scSt
     C                                                 :DtCYMD(scIn)
     C                                                 :DtCYMD(scOut)
     C                                                 )
     C                   exsr      WriteR
     C                   endif
     C                   endsl
     C                   endif
     C                   enddo
    C*
     C                   endsr
    C*
    C*====================================================================
    C* UpdatR - Update data base record and after processing
    C*====================================================================
     C     UpdatR        begsr
    C*
     C     xxRRN         chain     dbrcdf                             99
    C*
B01  C                   if        *in99 = *off
    C*
     C                   exsr      MvScDb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
    C*
     C                   exsr      ChkEndorse
    C*
     C                   select
     C                   when      svOut <> HCOUT
     C                              and svOut = *zero
     C                   callp     WriteDARTS('*OUT':optOutRmCd)
     C                   callp     WrtMidTermInfo('*OUT':optOutRmCd)
     C                   when      svOut <> HCOUT
     C                              or svIn <> HCIN
     C                   callp     WriteDARTS('*UPD':updRmCd)
     C                   callp     WrtMidTermInfo('*UPD':updRmCd)
     C                   endsl
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WriteR - Write data base record and after processing
    C*====================================================================
     C     WriteR        begsr
    C*
     C                   clear                   dbrcdf
     C                   exsr      MvScDb
     C                   exsr      MaintFldsInit
     C                   write     dbrcdf
     C                   feod      wdhcp
    C*
     C                   exsr      ChkEndorse
    C*
     C                   callp     WriteDARTS('*IN':optInRmCd)
     C                   callp     WrtMidTermInfo('*IN':optInRmCd)
    C*
     C                   eval      outFlg = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WriteWMMUP - Write record to WMMUP
    C*====================================================================
     C     WriteWMMUP    begsr
     C*
     C                   clear                   ENPGM1
     C     keyWTENL      chain     pen00
     C*
     C                   move      ELCO#         #@Co#
     C                   move      ELFND         #@Fnd
     C                   move      ELEMP#        #@Emp#
     C                   move      ELDIV         #@Div
     C                   move      ELFYR         #@Fyr
     C*
     C                   select
     C                   when      midTerm
     C                   eval      #@Mode = '4'
     C                   other
     C                   eval      #@Mode = '1'
     C                   endsl
     C*
     C                   call      ENPGM1                               99
     C                   parm                    #@Co#
     C                   parm                    #@Fnd
     C                   parm                    #@Emp#
     C                   parm                    #@Div
     C                   parm                    #@Fyr
     C                   parm                    #@Mode
     C*
     C                   select
     C                   when      midTerm
     C                   eval      xxEnd# = ldaEnd#
     C                   other
     C                   eval      xxEnd# = 0
     C                   endsl
     C*
     C     keyWMMUL5     chain     pmu05
     C                   if        %found
     C                   eval      MUEND# = *zero
     C                   eval      MUEEDT = holdHCIN
     C                   exsr      MaintFldsMU
     C                   update    pmu05
     C                   eval      xxEnd# = *zero
     C                   endif
     C*
     C                   endsr





    P*====================================================================
    P* Return a value for the opt out date
    P*====================================================================
     PAlphaOut         b
     DAlphaOut         pi            10
     D inDate                              like(HCOUT)

     C                   select
     C                   when      inDate = *zero
     C                   return    '*NONE'
     C                   other
     C                   return    AlphaUSA(CvtToDate(inDate))
     C                   endsl

     PAlphaOut         e





    P*====================================================================
    P* Return a 'Q' for Quote envirionment
    P*====================================================================
     PChkQuoteEnv      b
     DChkQuoteEnv      pi             1

     D##Env            s              1    inz

     C                   call      'SRTEST'
     C                   parm                    ##Env
     C*
     C                   select
     C                   when      ##Env = 'Q'
     C                   return    'Q'
     C                   other
     C                   return    *blank
     C                   endsl

     PChkQuoteEnv      e





    P*====================================================================
    P* Get the current state the employer exists in
    P*====================================================================
     PGetEmpSt         b
     DGetEmpSt         pi                  like(RSJRST)
     D inCo#                               like(RSCO#)
     D inRtCd                              like(RSRTCD)

     C     keyWMRSL      klist
     C                   kfld                    inCo#
     C                   kfld                    inRtCd

     C                   clear                   RSJRST
     C     keyWMRSL      chain     WMRSL
     C                   return    RSJRST

     PGetEmpSt         e





    P*====================================================================
    P* Determine if the maximim opt in/out has been reached
    P*====================================================================
     PMaxOptReached    b
     DMaxOptReached    pi             1n
     D inCo#                               like(HCCO#)
     D inFnd                               like(HCFND)
     D inEmp#                              like(HCEMP#)
     D inDiv                               like(HCDIV)
     D inFyr                               like(HCFYR)

     Dcount            s              3  0 inz

     C     keyWDHCL3     klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr

     C                   if        PolicyIncept(inCo#:inFnd
     C                                         :inEmp#:inDiv:inFyr)
     C                             = 'Y'
     C                   return    *off
     C                   endif
     C*
     C     keyWDHCL3     setll     phc03
     C                   dou       %eof
     C     keyWDHCL3     reade     phc03
     C                   if        not %eof and HCINCP <> 'Y'
     C                   eval      count = count + 1
     C                   endif
     C                   enddo
     C*
     C                   return    count >= HCOPT#

     PMaxOptReached    e





    P*====================================================================
    P* Determine if the file member exists
    P*====================================================================
     PMbrExists        b
     DMbrExists        pi             1n
     D outObj                        10    const
     D outType                        7    const
     D outMbr                        10    const

     D##Obj            s             10
     D##Type           s              7
     D##Mbr            s             10
     D##Err            s              1n   inz

     C                   call      'SRCHKMBR'
     C                   parm      outObj        ##Obj
     C                   parm      outType       ##Type
     C                   parm      outMbr        ##Mbr
     C                   parm                    ##Err
     C*
     C                   return    not ##Err

     PMbrExists        e





    P*====================================================================
    P* Determine if the program is added at policy inception
    P*====================================================================
     PPolicyIncept     b
     DPolicyIncept     pi             1
     D inCo#                               like(HCCO#)
     D inFnd                               like(HCFND)
     D inEmp#                              like(HCEMP#)
     D inDiv                               like(HCDIV)
     D inFyr                               like(HCFYR)

     C     keyWMSHL      klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inFyr

     C                   clear                   SHSTAT
     C*
     C     keyWMSHL      setgt     psh00
     C     keyWMSHL      readpe    psh00
     C*
     C                   select
     C                   when      SHSTAT = '3' or SHSTAT = '4'
     C                   return    'N'
     C                   other
     C                   return    'Y'
     C                   endsl

     PPolicyIncept     e





    P*====================================================================
    P* Determine if multiple divisions should be processed
    P*====================================================================
     PProcessDivs      b
     DProcessDivs      pi             1n

     C                   select
    C*
    C* Only process if changes are made
     C                   when      svIn = HCIN and svOut = HCOUT
     C                   return    *off
     C*
    C* Do not process PEO type 'S' or 'G'
     C                   when      FNPEOF = 'S' or FNPEOF = 'G'
     C                   return    *off
     C*
     C* Only process if flag is set at group level
     C                   when      EXUPDA <> 'Y'
     C                   return    *off
     C*
     C* Only process if more than one division
     C                   when      sc#Div <= 1
     C                   return    *off
     C*
     C                   endsl
     C*
     C* Prompt user to determine if all divisions should be updated
     C                   dou       not *in90
     C                   exfmt     SCRDIV
     C                   select
     C                   when      scUpdate <> 'Y' and scUpdate <> 'N'
     C                   seton                                        71  90
     C                   when      scCxFl <> 'Y' and scCxFL <> 'N'
     C                   seton                                        72  90
     C                   endsl
     C                   enddo
     C*
     C                   return    scUpdate = 'Y'

     PProcessDivs      e





    P*====================================================================
    P* Check for an active state record
    P*====================================================================
     PStateExists      b
     DStateExists      pi             1n
     D inSt                                like(HCST)   const
     D inIn                                like(HCIN)   const
     D inOut                               like(HCOUT)  const

     DkkDt             s                   like(HCEFDT)

     C     keyWDHCL4     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    inSt
     C*
     C     key1WDHCL4    klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    inSt
     C                   kfld                    kkDt

     C                   eval      kkDt = inIn
     C*
     C     key1WDHCL4    setgt     phc04
     C     keyWDHCL4     readpe    phc04
     C                   if        not %eof and dHcRRN4 <> @@rrn
     C                   select
     C                   when      HCOUT = *zero
     C                   return    *on
     C                   when      HCIN > inIn
     C                   return    *on
     C                   endsl
     C                   endif
     C*
     C     key1WDHCL4    setll     phc04
     C     keyWDHCL4     reade     phc04
     C                   if        not %eof and dHcRRN4 <> @@rrn
     C                   select
     C                   when      inOut = *zero
     C                   return    *on
     C                   when      inOut > HCIN
     C                   return    *on
     C                   endsl
     C                   endif

     C                   return    *off

     PStateExists      e





    P*====================================================================
    P* Write DARTS record
    P*====================================================================
     PWriteDARTS       b
     DWriteDARTS       pi
     D inOrOut                        4    const
     D inRmCd                         5    const
     D*
     D ##Key           s             40
     D ##Comm          s            256
     D ##CDat          s              8
     D ##DcId          s             12
     D ##PNam          s             10
     D ##SecL          s              3
     D ##User          s             10
     D ##Pgm           s             10
     D ##Fold          s             80
     D ##DiFl          s              1
     D ##IPgm          s             10
     D ##Fl01          s              1
     D ##Fl02          s              1
     D ##RmCd          s              5
     D ##Seq#          s              5

     C                   if        holdDiv = HCDIV
     C                   return
     C                   endif
     C*
     C                   select
     C                   when      inOrOut = '*IN'
     C                   eval      ##Comm = 'HCN program has been added:   -'
     C                   when      inOrOut = '*OUT'
     C                   eval      ##Comm = 'HCN program has been removed: -'
     C                   when      inOrOut = '*UPD'
     C                   eval      ##Comm = 'HCN program has been updated: -'
     C                   endsl
     C*
     C                   eval      ##Key  = %editc(HCCO# :'X')
     C                                    + %editc(HCFND :'X')
     C                                    + %editc(HCEMP#:'X')
     C                                    + %editc(HCDIV :'X')
     C                   eval      %subst(##Key:40) = QuoteFlag
     C                   eval      ##Comm = %trim(##Comm)
     C                                    + ' Opt in date: '
     C                                    +  AlphaUSA(CvtToDate(HCIN))
     C                                    + ' -'
     C                                    + ' Opt out date: '
     C                                    +  AlphaOut(HCOUT)
     C                   eval      ##CDat = %editc(DateTo8(SystemDate):'X')
     C                   eval      ##DcId = *blanks
     C                   eval      ##PNam = 'W0050R'
     C                   eval      ##SecL = *blanks
     C                   eval      ##User = q1User
     C                   eval      ##Pgm  = q1Pgm
     C                   eval      ##Fold = *blanks
     C                   eval      ##DiFl = *blanks
     C                   eval      ##IPgm = q1Pgm
     C                   eval      ##Fl01 = *blanks
     C                   eval      ##Fl02 = *blanks
     C                   eval      ##RmCd = inRmCd
     C                   eval      ##Seq# = *zeros
     C*
     C                   call      'DA700R'
     C                   parm                    ##Key
     C                   parm                    ##Comm
     C                   parm                    ##CDat
     C                   parm                    ##DcId
     C                   parm                    ##PNam
     C                   parm                    ##SecL
     C                   parm                    ##User
     C                   parm                    ##Pgm
     C                   parm                    ##Fold
     C                   parm                    ##DiFl
     C                   parm                    ##IPgm
     C                   parm                    ##Fl01
     C                   parm                    ##Fl02
     C                   parm                    ##RmCd
     C                   parm                    ##Seq#
     C*
     C                   return

     PWriteDARTS       e





    P*====================================================================
    P* Write Mid-Term Endorsement information
    P*====================================================================
     PWrtMidTermInfo   b
     DWrtMidTermInfo   pi
     D inOrOut                        4    const
     D inRmCd                         5    const
     D*
     D##ANam           s             40
     D##Cmd            s            255
     D##Len            s             15  5

     C     keyWMENL3     klist
     C                   kfld                    HCCO#
     C                   kfld                    HCFND
     C                   kfld                    HCEMP#
     C                   kfld                    HCDIV
     C                   kfld                    ldaFyr
     C                   kfld                    q1JobN
     C                   kfld                    ldaEnd#
     C*
     C     keyWMEML      klist
     C                   kfld                    HCCO#
     C                   kfld                    HCFND
     C                   kfld                    HCEMP#
     C                   kfld                    HCDIV
     C*
     C                   if        not midTerm
     C                   return
     C                   endif
     C*
     C* * * *            exsr      OvrFiles
     C*
     C                   open      WMENL3
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     ldaMYRR       chain     WTMYP
     C     keyWMEML      chain     WMEML
     C*
     C* Setup for next request #
     C*
      *** Begin Change *** 01/14/21 ************************************
     C***                clear                   ENREQ#
     C                   clear                   xENREQ#
      *** End   Change *** 01/14/21 ************************************
     C     keyWMENL3     setgt     pen03
     C     keyWMENL3     readpe    pen03
     C*
     C                   clear                   pen03
     C*
      *** Begin Change *** 01/14/21 ************************************
     C**                 eval      ENCO#  = HCCO#
     C**                 eval      ENFND  = HCFND
     C**                 eval      ENEMP# = HCEMP#
     C**                 eval      ENDIV  = HCDIV
     C**                 eval      ENFYR  = ldaFyr
     C**                 eval      ENJOB  = q1Jobn
     C**                 eval      ENEND# = ldaEnd#
     C**                 eval      ENEEDT = DtCYMD(ldaEeDt)
     C**                 eval      ENNAM1 = EMNAME
     C**                 eval      ENFDN1 = EMFDNM
     C**                 eval      ENENM1 = EMNAME + EMNAM2
     C**                 eval      ENENM2 = %subst(EMNAM2:11)
     C**                 eval      ENEFN1 = EMFDNM + EMFDN2
     C**                 eval      ENEFN2 = %subst(EMFDN2:11)
     C**                 eval      ENCON1 = EMCONT
     C**                 eval      ENATT1 = EMATTN
     C**                 eval      ENCN31 = EMCNT3
     C**                 eval      ENFEN1 = EMFEIN
     C**                 eval      ENTOB1 = EMTPOB
     C**                 eval      ENMA11 = EMMAD1
     C**                 eval      ENMA21 = EMMAD2
     C**                 eval      ENMCT1 = EMMCTY
     C**                 eval      ENMST1 = EMMST
     C**                 eval      ENMZP1 = EMMZIP
     C**                 eval      ENPA11 = EMPAD1
     C**                 eval      ENPA21 = EMPAD2
     C**                 eval      ENPCT1 = EMPCTY
     C**                 eval      ENPST1 = EMPST
     C**                 eval      ENPZP1 = EMPZIP
     C**                 eval      ENCNT1 = EMCNTY
     C**                 eval      ENFNNM = FNNAME
     C**                 eval      ENAGT# = GetEmpAgt(HCCO#:HCFND:HCEMP#
     C**                                             :HCDIV:HCFYR:*zero
     C**                                             :##ANam)
     C**                 eval      ENAGNM = ##ANam
     C**                 eval      ENRUSR = q1User
     C**
     C**                 select
     C**                 when      inOrOut = '*IN'
     C**                 eval      ENDSC1 = 'HCN program has been added.'
     C**                 when      inOrOut = '*OUT'
     C**                 eval      ENDSC1 = 'HCN program has been removed.'
     C**                 when      inOrOut = '*UPD'
     C**                 eval      ENDSC1 = 'HCN program has been updated.'
     C**                 endsl
     C**
     C**                 eval      ENDSC2 = 'Opt in date: '
     C**                                  +  AlphaUSA(CvtToDate(HCIN))
     C**                                  + ' - '
     C**                                  + 'Opt out date: '
     C**                                  +  AlphaOut(HCOUT)
     C**                 eval      ENDSC3 = *blanks
     C**                 eval      ENDT   = DateTo7(SystemDate)
     C**                 eval      ENTIME = TimeTo6(SystemTime)
     C**                 eval      ENUSER = q1User
     C**                 eval      ENPGM  = q1Pgm
     C**                 eval      ENSTAT = 'A'
     C**                 eval      ENRUSR = q1User
     C**                 eval      ENCAR1 = F2CAR#
     C**                 eval      ENPFD1 = ELPFDT
     C*** * *            eval      ENIAPF = 'Y'
     C**                 select
     C**                 when      endorseAdded
     C**                 eval      ENMSFL = 'H'
     C**                 other
     C**                 eval      ENMSFL = 'Y'
     C**                 endsl
     C**                 eval      ENRMCD = inRmCd
     C**
     C** Build Before/After lines & write WMENP records
     C**                 select
     C**                 when      inOrOut = '*IN'
     C**                 eval      ENBLIN = *blanks
     C**                 eval      ENALIN = 'HCN program has been added:'
     C**                 when      inOrOut = '*OUT'
     C**                 eval      ENBLIN = *blanks
     C**                 eval      ENALIN = 'HCN program has been removed.'
     C**                 when      inOrOut = '*UPD'
     C**                 eval      ENBLIN = 'HCN program has been updated.'
     C**                 eval      ENALIN = 'HCN program has been updated.'
     C**                 endsl
     C**
     C**                 eval      ENREQ# = ENREQ# + 1
     C**                 eval      ENSEQ# = *zero
     C**
     C**                 write     pen03
     C**
     C**                 select
     C**                 when      inOrOut = '*IN'
     C**                 eval      ENBLIN = *blanks
     C**                 when      inOrOut = '*OUT'
     C**                            or inOrOut = '*UPD'
     C**                 eval      ENBLIN = 'Opt in date: '
     C**                                  +  AlphaUSA(CvtToDate(svIn))
     C**                                  + ' - '
     C**                                  + 'Opt out date: '
     C**                                  +  AlphaOut(svOut)
     C**                 endsl
     C**
     C**                 eval      ENALIN = 'Opt in date: '
     C**                                  +  AlphaUSA(CvtToDate(HCIN))
     C**                                  + ' - '
     C**                                  + 'Opt out date: '
     C**                                  +  AlphaOut(HCOUT)
     C**
     C**                 eval      ENSEQ# = ENSEQ# + 1
     C**
     C**                 write     pen03
     C**
     C**                 select
     C**                 when      inOrOut = '*IN'
     C**                 eval      ENBLIN = *blanks
     C**                 eval      ENALIN = '(WC 42 04 08)'
     C**                 eval      ENSEQ# = ENSEQ# + 1
     C**                 write     pen03
     C**                 endsl
     C*
     C                   eval      xENCO#  = HCCO#
     C                   eval      xENFND  = HCFND
     C                   eval      xENEMP# = HCEMP#
     C                   eval      xENDIV  = HCDIV
     C                   eval      xENFYR  = ldaFyr
     C                   eval      xENJOB  = q1Jobn
     C                   eval      xENEND# = ldaEnd#
     C                   eval      xENEEDT = DtCYMD(ldaEeDt)
     C                   eval      xENNAM1 = EMNAME
     C                   eval      xENFDN1 = EMFDNM
     C                   eval      xENENM1 = EMNAME + EMNAM2
     C                   eval      xENENM2 = %subst(EMNAM2:11)
     C                   eval      xENEFN1 = EMFDNM + EMFDN2
     C                   eval      xENEFN2 = %subst(EMFDN2:11)
     C                   eval      xENCON1 = EMCONT
     C                   eval      xENATT1 = EMATTN
     C                   eval      xENCN31 = EMCNT3
     C                   eval      xENFEN1 = EMFEIN
     C                   eval      xENTOB1 = EMTPOB
     C                   eval      xENMA11 = EMMAD1
     C                   eval      xENMA21 = EMMAD2
     C                   eval      xENMCT1 = EMMCTY
     C                   eval      xENMST1 = EMMST
     C                   eval      xENMZP1 = EMMZIP
     C                   eval      xENPA11 = EMPAD1
     C                   eval      xENPA21 = EMPAD2
     C                   eval      xENPCT1 = EMPCTY
     C                   eval      xENPST1 = EMPST
     C                   eval      xENPZP1 = EMPZIP
     C                   eval      xENCNT1 = EMCNTY
     C                   eval      xENFNNM = FNNAME
     C                   eval      xENAGT# = GetEmpAgt(HCCO#:HCFND:HCEMP#
     C                                               :HCDIV:HCFYR:*zero
     C                                               :##ANam)
     C                   eval      xENAGNM = ##ANam
     C                   eval      xENRUSR = q1User
     C*
     C                   select
     C                   when      inOrOut = '*IN'
     C                   eval      xENDSC1 = 'HCN program has been added.'
     C                   when      inOrOut = '*OUT'
     C                   eval      xENDSC1 = 'HCN program has been removed.'
     C                   when      inOrOut = '*UPD'
     C                   eval      xENDSC1 = 'HCN program has been updated.'
     C                   endsl
     C*
     C                   eval      xENDSC2 = 'Opt in date: '
     C                                    +  AlphaUSA(CvtToDate(HCIN))
     C                                    + ' - '
     C                                    + 'Opt out date: '
     C                                    +  AlphaOut(HCOUT)
     C                   eval      xENDSC3 = *blanks
     C                   eval      xENDT   = DateTo7(SystemDate)
     C                   eval      xENTIME = TimeTo6(SystemTime)
     C                   eval      xENUSER = q1User
     C                   eval      xENPGM  = q1Pgm
     C                   eval      xENSTAT = 'A'
     C                   eval      xENRUSR = q1User
     C                   eval      xENCAR1 = F2CAR#
     C                   eval      xENPFD1 = ELPFDT
     C* * * *            eval      xENIAPF = 'Y'
     C                   select
     C                   when      endorseAdded
     C                   eval      xENMSFL = 'H'
     C                   other
     C                   eval      xENMSFL = 'Y'
     C                   endsl
     C                   eval      xENRMCD = inRmCd
     C*
     C*  Build Before/After lines & write WMENP records
     C                   select
     C                   when      inOrOut = '*IN'
     C                   eval      xENBLIN = *blanks
     C                   eval      xENALIN = 'HCN program has been added:'
     C                   when      inOrOut = '*OUT'
     C                   eval      xENBLIN = *blanks
     C                   eval      xENALIN = 'HCN program has been removed.'
     C                   when      inOrOut = '*UPD'
     C                   eval      xENBLIN = 'HCN program has been updated.'
     C                   eval      xENALIN = 'HCN program has been updated.'
     C                   endsl
     C*
     C                   eval      xENREQ# = xENREQ# + 1
     C                   eval      xENSEQ# = *zero
     C*
     C                   write     pen03
     C*
     C                   select
     C                   when      inOrOut = '*IN'
     C                   eval      xENBLIN = *blanks
     C                   when      inOrOut = '*OUT'
     C                              or inOrOut = '*UPD'
     C                   eval      xENBLIN = 'Opt in date: '
     C                                    +  AlphaUSA(CvtToDate(svIn))
     C                                    + ' - '
     C                                    + 'Opt out date: '
     C                                    +  AlphaOut(svOut)
     C                   endsl
     C*
     C                   eval      xENALIN = 'Opt in date: '
     C                                    +  AlphaUSA(CvtToDate(HCIN))
     C                                    + ' - '
     C                                    + 'Opt out date: '
     C                                    +  AlphaOut(HCOUT)
     C*
     C                   eval      xENSEQ# = xENSEQ# + 1
     C*
     C                   write     pen03
     C*
     C                   select
     C                   when      inOrOut = '*IN'
     C                   eval      xENBLIN = *blanks
     C                   eval      xENALIN = '(WC 42 04 08)'
     C                   eval      xENSEQ# = xENSEQ# + 1
     C                   write     pen03
     C                   endsl
      *** End   Change *** 01/14/21 ************************************
     C                   close     WMENL3
     C*
     C* * * *            exsr      DltOvrs
     C*
     C                   return
     C*
     C*====================================================================
     C* Override files
     C*====================================================================
     C     OvrFiles      begsr
     C*
     C                   if        not MbrExists('WMENP':'*FILE':'REVISED')
     C                   callp     RunCmd('ADDPFM FILE(WDHCP) MBR(REVISED)':35)
     C                   callp     RunCmd('ADDLFMBRS PHYFILE(WDHCP)':35)
     C                   endif
     C*
     C* WMENL3 to REVISED member
     C                   eval      ##Cmd = 'OVRDBF FILE(WMENL3) MBR(REVISED)'
     C                   eval      ##Len = %len(##Cmd)
     C                   call      'QCMDEXC'
     C                   parm                    ##Cmd
     C                   parm                    ##Len
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Delete overrides  DLTOVR
     C*====================================================================
     C     DltOvrs       begsr
     C*
     C* WMENL3
     C                   eval      ##Cmd = 'DLTOVR FILE(WMENL3)'
     C                   eval      ##Len = %len(##Cmd)
     C                   call      'QCMDEXC'
     C                   parm                    ##Cmd
     C                   parm                    ##Len
     C*
     C                   endsr

     PWrtMidTermInfo   e
