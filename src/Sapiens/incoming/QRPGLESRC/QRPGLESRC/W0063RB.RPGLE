     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*NEW)
     H*********************************************************************
     H/TITLE W0063RB - Work with Group Fee Types
     H*********************************************************************
     H*  Created: 02/11/11   C:33   Project: WC1851
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 09/21/11  C:65  Project: WC1851                                   *
     H*                 Change activation group above to *NEW.            *
     H*                                                                   *
     H* 05/26/10  C:61  Project: WC1851                                   *
     H*********************************************************************
     Fw0063fb   cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwtfip     if   e             disk    rename(pfi01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwtfil4    if   e           k disk    rename(pfi01:dbrcdf)
     F                                     infds(dbfds)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     Fwdefl1    if   e           k disk
     Fwmeml     if   e           k disk
    D*
     D#ofHd            c                   1
     D hd              s             78    dim(#ofHd) ctdata perrcd(1)
     D#ofOFD           c                   20
     D of              s              1    dim(#ofOFD) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFD) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Group Fee Types'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0063R02B'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   12
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9  0
     D @@Date          s              7  0
     D @@St            s              1
     D kkCo#           s                   like(FICO#)
     D kkFnd           s                   like(FIFND)
     D kkSt            s                   like(FIST)
     D kkEfDt          s                   like(FIEFDT)
     D kkType          s                   like(FITYPE)
     D kkDesc          s                   like(FIDESC)
     D xxSt            s                   like(RSJRST)
     D xxDate          s                   like(FIEFDT)
     D sysDt7          s                   like(FIEXDT)
     D emRRN           s              9  0
     D dspMode         s              1n
     D updMode         s              1n
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DEmpSelected      pr             1
     D*
     DGetEmpSt         pr             2
     D billSch                             like(RSRTCD) const
     D*
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
     D/copy sPrWrkWth
    D*
     D/copy wPrGroup
     D/copy wPrEmplr
    C*
     C/copy sCopyWW41C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     C                   parm                    @@call
     c                   parm                    @@rrn
     c                   parm                    @@Date
     c                   parm                    @@St
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
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
     C     key           klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkSt
     C*
     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkSt
     C* * * *            kfld                    kkEfDt
     C                   kfld                    kkType
     C                   kfld                    kkDesc
     C*
     C     emky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
    C*
    C*====================================================================
    C* ChkSsM - Set indicator 11 for subset message
    C*====================================================================
     C     ChkSsM        begsr
     C*
     c                   eval      *in11 = ssIn   <> *blanks
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ErrCk1 - Additional errors on subfile selection
    C*====================================================================
     C     ErrCk1        begsr
     C*
     C                   select
     C                   when      scSel = '1'
     C                   if        dspMode and scEmp <> 'Y'
     C                   seton                                        71  90
     C                   endif
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ErrCk2 - Error checking for the subset screen
    C*====================================================================
     C     ErrCk2        begsr
     C*
     C*  Include/Exclude Expired.
     c                   if        ssIn <> *blanks
     c                              and ssIn <> 'E'
     c                              and ssIn <> 'I'
     c                   seton                                        7390
     c                   leavesr
     c                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* InzSst - Initialize the subset fields
    C*====================================================================
     C     InzSst        begsr
    C*
     c                   eval      ssIn   = 'E'
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KeyInz - Initialize key values for program start
    C*====================================================================
     C     KeyInz        begsr
    C*
     C                   eval      kkCo#  = ELCO#
     C                   eval      kkFnd  = ELFND
     C                   eval      kkSt   = xxSt
     C                   eval      kkEfDt = xxDate
     C                   eval      kkType = *loval
     C                   eval      kkDesc = *loval
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KeySet - Set key values when doing "position to"
    C*====================================================================
     C     KeySet        begsr
    C*
     c                   select
    C*
     c                   when      *in21 = *on
     c                   eval      kkType = scPst1
     c                   eval      kkDesc = *blanks
    C*
     c                   endsl                                                  select
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KeySfl - Set key values from subfile records for paging
    C*====================================================================
     C     KeySfl        begsr
    C*
     c                   eval      kkSt   = scSt
     c                   eval      kkEfDt = scEfDt
    C*
     C                   endsr
    C*
    C*====================================================================
    C* LoadHdr - Load header information
    C*====================================================================
     C     LoadHdr       begsr
    C*
     c                   eval      scFnd  = ELFND
     c                   eval      scFNam = GetGrpName(ELFND)
     c                   eval      scEmp# = ELEMP#
     c                   eval      scDiv  = ELDIV
     c                   eval      scEDN  = EmpDivName(ELEMP#:ELDIV:emRRN:ELFND)
     c                   eval      scSt   = xxSt
     c                   eval      scFyr  = ELFYR
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MvDbSc - Move data base fields to screen fields
    C*====================================================================
     C     MvDbSc        begsr
    C*
     C                   eval      scType = FITYPE
     C                   eval      scDesc = FIDESC
     C                   eval      scEfDt = FIEFDT
     C                   eval      scExDt = FIEXDT
     C                   eval      scEmp  = EmpSelected
    C*
     c                   select
     c                   when      view# = 1 and info# = 1
     C                   eval      scdata = '' + FITYPE
     C                              + '  ' + FIDESC
     C                              + '  ' + AlphaUSA(CvtToDate(FIEFDT))
     C                              + '  ' + AlphaUSA(CvtToDate(FIEXDT))
     C                              + '   ' + scEmp
     c                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OthFks - Process other function keys
    C*====================================================================
     C     OthFks        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OthOpt - Process other options
    C*====================================================================
     C     OthOpt        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C                   eval      dspMode = @@Call = 'D'
     C                   eval      updMode = @@Call = 'C'
    C*
     C* This program should only be a selection screen
     C                   eval      @@Call = '2'
    C*
     c     @@rrn         chain(n)  wdelp
     C                   eval      xxDate = ELPFDT
     C                   if        q1Prms >= 5 and @@Date <> *zero
     C                   eval      xxDate = @@Date
     C                   endif
     C                   eval      xxSt = GetEmpSt(ELRTCD)
     C                   if        q1Prms >= 6 and @@St <> *blanks
     C                   eval      xxSt = @@St
     C                   endif
     C                   exsr      KeyInz
     C                   exsr      LoadHdr
     C*
     C                   clear                   @@rrn
     C*
     C                   eval      sysDt7 = DateTo7(SystemDate)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* Read - Read next record from file
    C*====================================================================
     C     Read          begsr
    C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ReadP - Read previous record from file
    C*====================================================================
     C     ReadP         begsr
    C*
     C                   select
     C                   when      *in21
     C     key           readpe    dbrcdf                                 95
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SetGt - Set greater than on file
    C*====================================================================
     C     setgt         begsr
    C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SetLl - Set lower limits on file
    C*====================================================================
     C     SetLl         begsr
    C*
     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Sf1Upd - Update subfile record when item is deleted
    C*====================================================================
     C     Sf1Upd        begsr
     C                   endsr
    C*
    C*====================================================================
    C* SlctRd - Move fields for selected record to parameters
    C*====================================================================
     C     SlctRd        begsr
    C*
     C                   eval      @@rrn = scDbRr
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Subset - Determine if record should be put in subfile
    C*====================================================================
     C     Subset        begsr
     c*
     c                   select
     c* Effective date
     c                   when      FIEFDT > xxDate
     c                   eval      meetss = *off
     c* Expired
     c                   when      ssIn = 'E'
     c                              and FIEXDT <> 0
     c                              and (FIEXDT < sysDt7
     c                                 or FIEXDT < xxDate)
     c                   eval      meetss = *off
     c*
     c                   endsl
     c*
     C                   endsr





    P*================================================================
    P* Determine if employer selected for Type/Desc
    P*================================================================
     PEmpSelected      b
     DEmpSelected      pi             1

     C     keyWDEFL1     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    FITYPE
     C                   kfld                    FIDESC

     C     keyWDEFL1     chain     wdefl1
     C                   if        %found
     C                   return    'Y'
     C                   endif
     C                   return    *blank

     PEmpSelected      e





    P*================================================================
    P* Get employer state
    P*================================================================
     PGetEmpSt         b
     DGetEmpSt         pi             2
     D billSch                             like(RSRTCD) const

     C     keyWMRSL      klist
     C                   kfld                    ELCO#
     C                   kfld                    billSch

     C                   clear                   RSJRST
     C     keyWMRSL      chain     wmrsl
     C                   return    RSJRST

     PGetEmpSt         e

** Column headings
Opt  Fee Type    Fee Description                  Eff Date    Exp Date   Emp
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
