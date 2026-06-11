     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H debug(*yes)
    H*********************************************************************
    h/TITLE W0191R - HCN Program Information
    H*********************************************************************
     H* Created: 11/10/10   C:33   Project: 139WPL
    H*********************************************************************
     H* Revisions:
     H*
    H*********************************************************************
     Fw0191f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmhcp     uf a e             disk    rename(phc01:dbrcdf)
     Fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwmhcl     if   e           k disk    rename(phc01:phc00)
     F                                     infds(mHcFds)
     Fststl3    if   e           k disk    rename(pst01:pst03)
     Fwdhcl2    if   e           k disk    rename(phc01:dhc02)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'HCN Program Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D addMode         s              1n
     D chgMode         s              1n
     D cpyMode         s              1n
     D cpyModeMass     s              1n
     D dspMode         s              1n
     D xxCo#           s                   like(FNCO#)
     D xxFnd           s                   like(FNFND)
     D mHcFds          ds
     D  mHcRRN               397    400b 0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D/copy wPrGroup
     D*
     DCpyRcds          pr
     D co#                                 like(HCCO#) const
     D fromGrp                             like(HCFND) const
     D fromSt                              like(HCST)  const
     D toGrp                               like(HCFND) const
     D*
     DEmplrExists      pr             1n
     D inSt                                like(HCST)   const
     D inEfDt                              like(HCEFDT) const
     D inExDt                              like(HCEXDT) const
     DStateExists      pr             1n
     D inSt                                like(HCST)   const
     D inEfDt                              like(HCEFDT) const
     D inExDt                              like(HCEXDT) const
     DValidGroup       pr             1n
     D inGrp                               like(HCFND) const
     DValidPremLvl     pr             1n
     D inLvl                               like(HCAPPL) const
     DValidState       pr             1n
     D inState                             like(HCST) const
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
     C*
    C*====================================================================
    C* CpyRcd - Copy records
    C*====================================================================
     C     CpyRcd        begsr
     C*
     C                   eval      scFnd = HCFND
     C                   eval      scCpySt = 'From state:' + '  ' + HCST
     C*
     C                   exsr      CpyScr
     C*
     C                   if        not *inkl
     C                   callp     CpyRcds(HCCO#:HCFND:HCST:scToGrp)
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* CpyRcdMass - Mass copy records
    C*====================================================================
     C     CpyRcdMass    begsr
     C*
     C                   eval      scFnd = FNFND
     C                   eval      scCpySt = 'All states'
     C*
     C                   exsr      CpyScr
     C*
     C                   if        not *inkl
     C                   callp     CpyRcds(xxCo#:xxFnd:*blanks:scToGrp)
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* CpyScr - Process copy screen
    C*====================================================================
     C     CpyScr        begsr
     C*
     C                   dou       not *in90 or *inkl
     C* Turn off *in71-90
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
     C                   exfmt     scrCpy
     C                   if        not *inkl
     C                   exsr      ErrCkCpy
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
    C*====================================================================
    C* DltPre - Pre-processing before delete occurs
    C*====================================================================
     C     DltPre        begsr
     C                   endsr
     C*
    C*====================================================================
    C* DsplyR - Processing after displaying information
    C*====================================================================
     C     DsplyR        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     ErrCk1        begsr
     C*
     C                   if        addMode
     C*
     C                   if        not ValidState(scSt)
     C                   seton                                        71  90
     C                   leavesr
     C                   endif
     C*
     C                   if        DtCheck(scEfDt) = *zero
     C                   seton                                        72  90
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C*
     C                   if        scExDt <> *zero and
     C                              DtCheck(scExDt) = *zero
     C                   seton                                        73  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scExDt <> *zero
     C                              and CvtToDate(scEfDt)>CvtToDate(scExDt)
     C                   seton                                        80  90
     C                   leavesr
     C                   endif
     C*
     C                   if        StateExists(scSt
     C                                        :DtCYMD(scEfDt)
     C                                        :DtCYMD(scExDt)
     C                                        )
     C                   seton                                        74  90
     C                   leavesr
     C                   endif
     C*
     C                   if        EmplrExists(scSt
     C                                        :DtCYMD(scEfDt)
     C                                        :DtCYMD(scExDt)
     C                                        )
     C                   seton                                        81  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scFact <= *zero
     C                   seton                                        75  90
     C                   leavesr
     C                   endif
     C*
     C                   if        not ValidPremLvl(scAppl)
     C                   seton                                        76  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scOpt# < *zero
     C                   seton                                        77  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scStCd <= *zero
     C                   seton                                        78  90
     C                   leavesr
     C                   endif
     C*
     C                   if        scRenw <> 'Y' and scRenw <> 'N'
     C                   seton                                        79  90
     C                   leavesr
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
     C     ErrCk2        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ErrCk3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
     C     ErrCk3        begsr
     C                   endsr
     C*
    C*====================================================================
    C* ErrCkCpy- Error checking for copy srceen (SCRCPY)
    C*====================================================================
     C     ErrCkCpy      begsr
     C*
     C                   if        scToGrp <> *zero
     C                              and not ValidGroup(scToGrp)
     C                   seton                                        71  90
     C                   leavesr
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
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
     C*
     C                   if        addMode
     C                   eval      HCCO#  = FNCO#
     C                   eval      HCFND  = FNFND
     C                   eval      HCOPT# = 1
     C                   eval      HCFACT = .95
     C                   eval      HCSTCD = 9874
     C                   eval      HCAPPL = 'S'
     C                   endif
     C*
     C                   eval      scSt   = HCST
     C                   eval      scEfDt = DtMDYY(HCEFDT)
     C                   eval      scExDt = DtMDYY(HCEXDT)
     C                   eval      scFact = HCFACT
     C                   eval      scAppl = HCAPPL
     C                   eval      scOpt# = HCOPT#
     C                   eval      scStCD = HCSTCD
     C                   eval      scRenw = HCRENW
     C                   eval      scForm = HCFORM
     C*
     C                   endsr
     C*
    C*====================================================================
    C* MvScDb - Move screen fields to data base fields
    C*====================================================================
     C     MvScDb        begsr
     C*
     C                   eval      HCCO#  = FNCO#
     C                   eval      HCFND  = FNFND
     C*
     C                   eval      HCST   = scSt
     C                   eval      HCEFDT = DtCYMD(scEfDt)
     C                   eval      HCEXDT = DtCYMD(scExDt)
     C                   eval      HCFACT = scFact
     C                   eval      HCAPPL = scAppl
     C                   eval      HCOPT# = scOpt#
     C                   eval      HCSTCD = scStCD
     C                   eval      HCRENW = scRenw
     C                   eval      HCFORM = scForm
     C*
     C                   endsr
     C*
    C*====================================================================
    C* OthErr - Processing for other modes
    C*====================================================================
     C     OthErr        begsr
     C                   endsr
     C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     othfks        begsr
     C                   endsr
     C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C     @@rrn         chain     dbrcdf
     C     @@rrn2        chain     WMFNP
     C                   eval      scFnd   = FNFND
     C                   eval      scFndNm = FNNAME
     C                   eval      xxCo#   = FNCO#
     C                   eval      xxFnd   = FNFND
     C*
     C                   eval      addMode = @@Mode = 'A'
     C                   eval      chgMode = @@Mode = 'C'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      cpyMode = @@Mode = 'P'
     C                   eval      cpyModeMass = @@Mode = 'M'
     C* Screen processing indicators
     C                   eval      *in09 = dspMode
     C                   eval      *in40 = addMode
     C*
     C* Process copy modes
     C                   select
     C                   when      cpyMode
     C                   exsr      CpyRcd
     C                   return
     C                   when      cpyModeMass
     C                   exsr      CpyRcdMass
     C                   return
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* PreOth - Pre-processing for other modes
    C*====================================================================
     C     PreOth        begsr
     C                   endsr
     C*
    C*====================================================================
    C* UpdatR - Update data base record and after processing
    C*====================================================================
     C     UpdatR        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      MvScDb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
     C*
    C*====================================================================
    C* WriteR - Write data base record and after processing
    C*====================================================================
     C     WriteR        begsr
     C                   clear                   dbrcdf
     C                   exsr      MvScDb
     C                   exsr      MaintFldsInit
     C                   write     dbrcdf
     C                   feod      wmhcp
     C                   endsr





    P*====================================================================
    P* Copy recrods for WMHCP
    P*====================================================================
     PCpyRcds          b
     DCpyRcds          pi
     D co#                                 like(HCCO#) const
     D fromGrp                             like(HCFND) const
     D fromSt                              like(HCST)  const
     D toGrp                               like(HCFND) const

     DsaveHCCO#        s                   like(HCCO#)
     DsaveHCFND        s                   like(HCFND)
     DsaveHCST         s                   like(HCST)
     DsaveHCEFDT       s                   like(HCEFDT)

     C     keyWMHCL      klist
     C                   kfld                    co#
     C                   kfld                    fromGrp

     C     key1WMHCL     klist
     C                   kfld                    co#
     C                   kfld                    fromGrp
     C                   kfld                    fromSt

     C     key2WMHCL     klist
     C                   kfld                    co#
     C                   kfld                    toGrp
     C                   kfld                    fromSt

     C     key3WMHCL     klist
     C                   kfld                    saveHCCO#
     C                   kfld                    saveHCFND
     C                   kfld                    saveHCST
     C                   kfld                    saveHCEFDT

     C     keyWMFNL      klist
     C                   kfld                    co#

     C                   select
     C                   when      fromSt = *blanks
     C                   exsr      AllStates
     C                   when      toGrp = *zeros
     C                   exsr      AllGroups
     C                   when      fromGrp = toGrp
     C                   return
     C                   other
     C                   exsr      Copy
     C                   endsl
     C*
     C                   return
     C*
     C*====================================================================
     C* Process all groups
     C*====================================================================
     C     AllGroups     begsr
     C*
     C     keyWMFNL      setll     pfn00
     C                   dou       %eof(WMFNL)
     C     keyWMFNL      reade     pFN00
     C                   if        not %eof and FNFND <> fromGrp
     C                   callp     CpyRcds(co#:fromGrp:HCST:FNFND)
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process all states
     C*====================================================================
     C     AllStates     begsr
     C*
     C                   eval      saveHCCO# = co#
     C                   eval      saveHCFND = fromGrp
     C                   clear                   saveHCST
     C                   clear                   saveHCEFDT
     C*
     C     keyWMHCL      setll     phc00
     C                   dou       %eof(WMHCL)
     C     keyWMHCL      reade     phc00
     C                   if        not %eof
     C                   eval      saveHCCO#  = HCCO#
     C                   eval      saveHCFND  = HCFND
     C                   eval      saveHCST   = HCST
     C                   eval      saveHCEFDT = HCEFDT
     C                   callp     CpyRcds(co#:fromGrp:HCST:toGrp)
     C     key3WMHCL     setgt     phc00
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Copy records
     C*====================================================================
     C     Copy          begsr
     C*
     C* Get current record
     C     key1WMHCL     chain     phc00
     C                   if        %found
     C* Check for record existence for new group
     C     key2WMHCL     chain     phc00
     C                   if        not %found
     C                   eval      HCFND  = toGrp
     C                   eval      HCUSER = q1User
     C                   eval      HCPGM  = q1Pgm
     C                   eval      HCDT   = DateTo7(SystemDate)
     C                   eval      HCTIME = TimeTo6(SystemTime)
     C                   eval      HCIUSR = HCUSER
     C                   eval      HCIPGM = HCPGM
     C                   eval      HCIDT  = HCDT
     C                   eval      HCITM  = HCTIME
     C                   write     dbrcdf
     C                   feod      wmhcp
     C                   endif
     C                   endif
     C*
     C                   endsr

     PCpyRcds          e





    P*====================================================================
    P* Check for an active state record at the employer level
    P*====================================================================
     PEmplrExists      b
     DEmplrExists      pi             1n
     D inSt                                like(HCST)   const
     D inEfDt                              like(HCEFDT) const
     D inExDt                              like(HCEXDT) const

     DkkDt             s                   like(HCEFDT)

     C     keyWDHCL      klist
     C                   kfld                    FNCO#
     C                   kfld                    FNFND
     C                   kfld                    inSt
     C*
     C     key1WDHCL     klist
     C                   kfld                    FNCO#
     C                   kfld                    FNFND
     C                   kfld                    inSt
     C                   kfld                    kkDt

     C                   if        inExDt = *zero
     C                   return    *off
     C                   endif
     C*
     C                   eval      kkDt = inEfDt
     C*
     C     key1WDHCL     setll     dhc02
     C                   dou       %eof
     C     keyWDHCL      reade     dhc02
     C                   if        not %eof
     C                   select
     C                   when      HCIN > inExDt
     C                   return    *on
     C                   endsl
     C                   endif
     C                   enddo

     C                   return    *off

     PEmplrExists      e





    P*====================================================================
    P* Check for an active state record
    P*====================================================================
     PStateExists      b
     DStateExists      pi             1n
     D inSt                                like(HCST)   const
     D inEfDt                              like(HCEFDT) const
     D inExDt                              like(HCEXDT) const

     DkkDt             s                   like(HCEFDT)

     C     keyWMHCL      klist
     C                   kfld                    FNCO#
     C                   kfld                    FNFND
     C                   kfld                    inSt
     C*
     C     key1WMHCL     klist
     C                   kfld                    FNCO#
     C                   kfld                    FNFND
     C                   kfld                    inSt
     C                   kfld                    kkDt

     C                   eval      kkDt = inEfDt
     C*
     C     key1WMHCL     setgt     phc00
     C     keyWMHCL      readpe    phc00
     C                   if        not %eof and @@rrn <> mHcRRN
     C                   select
     C                   when      HCEXDT = *zero
     C                   return    *on
     C                   when      HCEXDT > inEfDt
     C                   return    *on
     C                   endsl
     C                   endif
     C*
     C     key1WMHCL     setll     phc00
     C     keyWMHCL      reade     phc00
     C                   if        not %eof and @@rrn <> mHcRRN
     C                   select
     C                   when      inExDt = *zero
     C                   return    *on
     C                   when      inExDt > HCEFDT
     C                   return    *on
     C                   endsl
     C                   endif

     C                   return    *off

     PStateExists      e





    P*====================================================================
    P* Validate group
    P*====================================================================
     PValidGroup       b
     DValidGroup       pi             1n
     D inGrp                               like(HCFND) const

     C                   return    ValidGrp(inGrp)

     PValidGroup       e





    P*====================================================================
    P* Validate premium level
    P*====================================================================
     PValidPremLvl     b
     DValidPremLvl     pi             1n
     D inLvl                               like(HCAPPL) const

     Dvalid            s              1n

     C                   eval      valid = inLvl = 'M'
     C                                  or inLvl = 'J'
     C                                  or inLvl = 'E'
     C                                  or inLvl = 'S'
     C                                  or inLvl = 'D'
     C                   return    valid

     PValidPremLvl     e





    P*====================================================================
    P* Validate state
    P*====================================================================
     PValidState       b
     DValidState       pi             1n
     D inState                             like(HCST) const

     DkkKey            s                   like(STKEY)

     C     keySTSTL3     klist
     C                   kfld                    kkKey

     C                   eval      kkKey = inState
     C     keySTSTL3     chain     pst03
     C                   return    %found

     PValidState       e
