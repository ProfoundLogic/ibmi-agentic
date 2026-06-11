    Hcopyright('(C) Copyright StoneRiver, Inc., 2009')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
    H*********************************************************************
     H/TITLE W0093R2 - Third Party Billing Update Print Bill Prompt
    H*********************************************************************
     H* CREATED: 02/03/09  C:33  PROJECT 146WBW                           *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*********************************************************************
     Fw0093f2   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fwtabl     if   e           k disk    include(pbc01:pbs01)
     fstctl1    if   e           k disk
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Third Party Billing -
     D                                     Print Bill Prompt')
     D comapi          c                   const('WA001R')
     D @@kc            s              1
     D @@kl            s              1
     D @@ELrrn         s              9  0
     D @@Mode          s              1
     D @@Qte           s              1
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D cltID           s              3
     D EM_rrn          s              9  0
     D FN_rrn          s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     Dparms            ds          2000
     D postDt                         8
     D printBal                       1
     D interimDays                    8
     D commentCode1                   2
     D commentCode2                   2
     D commentCode3                   2
     D freeComment1                  25
     D freeComment2                  25
     D freeComment3                  25
     D resolve                        1
     D remarkCode                     5
     D hardCopy                       1
     D userOutQ                      10
    D*
     DSrchComCode      pr
     D field                          2
     D notSelInd                      2  0 const
     D selInd                         2  0 const
    D*
     DSrchRemCode      pr
     D field                          5
     D notSelInd                      2  0 const
     D selInd                         2  0 const
    D*
     DValidCommCode    pr             1n
     D inValue                             const like(BCKEY)
     D required                       1n   const
    D*
     DValidRmCd        pr             1n
     D inValue                             const like(CTRMCD)
     D required                       1n   const
    D*
     D/copy wPrAR
     D/copy wPrEmplr
     D/copy wPrGroup
     D/copy wPrUsrPrms
    D*
     C/copy sCopyPR41c
    C*
     C     *dtaara       Define    billParms     parms
     C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@Mode
     C                   parm                    @@ELrrn
     C                   parm                    @@Qte
    C*
    C*  Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     keyWMEXL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
    C*
     C     bsky          klist
     C                   kfld                    ELCO#
     C                   kfld                    xxBSCD           10
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Move employer information data base fields to screen fields
    C*====================================================================
     C     EmplrInfo     begsr
     C*
     C                   eval      scEmp# = ELEMP#
     C                   eval      scDiv  = ELDIV
     C                   eval      scFyr  = ELFYR
     C*
     C                   eval      scEDN = EmpDivName(ELEMP#:ELDIV:EM_rrn:
     C                                                ELFND:ELCO#)
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     ERRCK1        begsr
    C*
     C                   select
     C                   when      DtCheck(scDate) = *zero
     C                   seton                                        71  90
     C                   when      CvtToDate(scDate) < CvtToDate(ELBSDT)
     C                   seton                                        72  90
     C                   when      scDep <> 'Y' and scDep <> 'N'
     C                   seton                                        73  90
     C                   when      scDue < EXRVID and BSSEFL = 'S'
     C                   seton                                        74  90
     C                   when      not ValidCommCode(scCom1:*off)
     C                   seton                                        753190
     C                   when      not ValidCommCode(scCom2:*off)
     C                   seton                                        753290
     C                   when      not ValidCommCode(scCom3:*off)
     C                   seton                                        753390
     C                   when      (scCom1 <> *blank or
     C                              scCom2 <> *blank or
     C                              scCom3 <> *blank)
     C                             and
     C                             (scFCm1 <> *blank or
     C                              scFCm2 <> *blank or
     C                              scFCm3 <> *blank)
     C                   seton                                        76  90
     C                   when      scDart <> 'Y' and scDart <> 'N'
     C                   seton                                        77  90
     C                   when      not ValidRmCd(scRmCd:*on) and scDart = 'Y'
     C                   seton                                        78  90
     C                   when      scHard <> 'Y' and scHard <> 'N'
     C                   seton                                        79  90
     C                   when      scHard <> 'Y' and scDart <> 'Y'
     C                   seton                                        80  90
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Get Admin/DARTS control file information
    C*====================================================================
     C     GetAdminDARTS begsr
     C*
     C                   clear                   EXRVID
     C                   clear                   EXQTE
     C                   clear                   EXORG
     C                   clear                   EXAUD
     C                   clear                   EXREV
     C                   clear                   EXRFAF
     C     keyWMEXL      chain     pex00
     C*
     C                   endsr
    C*
    C*====================================================================
    C* Move group information data base fields to screen fields
    C*====================================================================
     C     GroupInfo     begsr
     C*
     C                   eval      scfnd  = ELFND
     C                   eval      scName = GetGrpName(ELFND:FN_rrn:ELCO#)
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     KJDO          begsr
    C*
     C                   select
     C                   when      hfld = 'SCRMCD'
     C                   callp     SrchRemCode(scRmCd:86:87)
     C                   when      hfld = 'SCCOM1'
     C                   callp     SrchComCode(scCom1:31:32)
     C                   when      hfld = 'SCCOM2'
     C                   callp     SrchComCode(scCom2:32:33)
     C                   when      hfld = 'SCCOM3'
     C                   callp     SrchComCode(scCom3:33:34)
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Load information to the header fields
    C*====================================================================
     C     LoadHeader    begsr
     C                   movel(p)  optnm         ##data
     C                   eval      scPrDs = CenterFld(##data:40)
     C                   exsr      GroupInfo
     C                   exsr      EmplrInfo
     C                   endsr
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     OTHFKS        begsr
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
     C                   endsr
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     PGMEND        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
     C     PGMSTR        begsr
     C                   call      'RTVCLTID'
     C                   parm                    cltID
     C*
     C     @@ELrrn       chain     pel0p
     C                   exsr      GetAdminDARTS
     C                   exsr      LoadHeader
     C                   endsr
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
     C     PREDSP        begsr
     C*
     C                   if        scDate = *zero
     C                   eval      scDate = DateTo8(SystemDate)
     C                   endif
     C*
     C                   if        scDep = *blank
     C                   eval      scDep = 'Y'
     C                   endif
     C*
     c                   movel(p)  ELBSCD        xxBSCD
     c     bsky          chain     pbs01
     C                   if        BSSEFL = 'S'
     C                   eval      scDue = EXRVID
     c                   eval      *in70 = *on                                  condition display
     c                   else
     C                   eval      scDue = 0
     c                   eval      *in70 = *off                                 condition display
     C                   endif
     C*
     C                   if        scDart = *blank
     C                   eval      scDart = 'Y'
     C                   endif
     C*
     C                   if        scRmCd = *blank
     C                   select
     C                   when      @@Qte  = 'Q'
     C                   eval      scRmCd = EXQTE
     C                   when      ELFAF  = 'E'
     C                   eval      scRmCd = EXORG
     C                   when      ELFAF  = 'A'
     C                   eval      scRmCd = EXAUD
     C                   when      ELFAF  = 'R'
     C                   eval      scRmCd = EXREV
     C                   other
     C                   eval      scRmCd = EXORG
     C                   endsl
     C                   endif
     C*
     C                   if        scHard = *blank
     C                   eval      scHard = 'Y'
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
     C     PRSCTL        begsr
     C*
     C     *lock         in        parms
     C                   move(p)   scDate        postDt
     C                   eval      printBal     = scDep
     C                   move(p)   scDue         interimDays
     C                   eval      commentCode1 = scCom1
     C                   eval      commentCode2 = scCom2
     C                   eval      commentCode3 = scCom3
     C                   eval      freeComment1 = scFCm1
     C                   eval      freeComment2 = scFCm2
     C                   eval      freeComment3 = scFCm3
     C                   eval      resolve      = scDart
     C                   eval      remarkCode   = scRmCd
     C                   eval      hardCopy     = scHard
     C                   eval      userOutQ     = GetUsrOutq(Q1USER)
     C                   out       parms
     C*
     C                   endsr
    C*





     C*====================================================================
     C* Search for Comment Codes
     C*====================================================================
     PSrchComCode      b
     DSrchComCode      pi
     D field                          2
     D notSelInd                      2  0 const
     D selInd                         2  0 const
     D*
     D##Call           s              1
     D##kc             s              1
     D##kl             s              1
     D##Key            s             10
     C*
     C                   call      'W2191R'
     C                   parm      '2'           ##Call
     C     *inkc         parm                    ##kc
     C                   parm                    ##kl
     C                   parm      *blanks       ##Key
     C*
     C                   eval      *in85 = *on
     C*
     C                   select
     C                   when      ##Key <> *blank
     C                   eval      field = ##Key
     C                   eval      *in(selInd) = *on
     C                   other
     C                   eval      *in(notSelInd) = *on
     C                   endsl
     C*
     C                   return
     C*
     PSrchComCode      e





     C*====================================================================
     C* Search for Remark Codes
     C*====================================================================
     PSrchRemCode      b
     DSrchRemCode      pi
     D field                          5
     D notSelInd                      2  0 const
     D selInd                         2  0 const
     D*
     D##RmCd           s              5
     D##Rtc            s              1
     C*
     C                   call      'SC580R'
     C                   parm      *blanks       ##RmCd
     C                   parm      *blanks       ##Rtc
     C*
     C                   select
     C                   when      ##Rtc = 'A'
     C                   eval      *inkc = *on
     C                   when      ##RmCd <> *blank
     C                   eval      field = ##RmCd
     C                   eval      *in(selInd) = *on
     C                   other
     C                   eval      *in(notSelInd) = *on
     C                   endsl
     C*
     C                   return
     C*
     PSrchRemCode      e





     C*====================================================================
     C* Validate Comment Codes
     C*====================================================================
     PValidCommCode    b
     DValidCommCode    pi             1n
     D inValue                             const like(BCKEY)
     D required                       1n   const
     C*
     C     keyTable      klist
     C                   kfld                    ELCO#
     C                   kfld                    inValue
     C*
     C     keyTable      chain     pbc01
     C                   select
     C                   when      inValue = *blanks and not required
     C                   return    *on
     C                   when      not %found or BCSTAT = 'I'
     C                   return    *off
     C                   other
     C                   return    *on
     C                   endsl
     C*
     PValidCommCode    e





     C*====================================================================
     C* Validate Remark Codes
     C*====================================================================
     PValidRmCd        b
     DValidRmCd        pi             1n
     D inValue                             const like(CTRMCD)
     D required                       1n   const
     C*
     C     inValue       chain     pct01
     C                   select
     C                   when      inValue = *blanks and not required
     C                   return    *on
     C                   when      not %found or CTSTAT = 'I'
     C                   return    *off
     C                   other
     C                   return    *on
     C                   endsl
     C*
     PValidRmCd        e
