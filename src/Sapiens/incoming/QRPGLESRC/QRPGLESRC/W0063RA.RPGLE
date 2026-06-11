     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*NEW)
     H*********************************************************************
     H/TITLE W0063R - Work with fee information
     H*********************************************************************
     H*  CREATED: 01/27/09  PROGRAMMER: C:190 PROJECT: WC1851             *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 09/21/11  C:65  Project: WC1851                                   *
     H*                 Change activation group above to *NEW.            *
     H*                                                                   *
     H* 02/11/11  C:33  Project: WC1851                                   *
     H*                 Re-write.  No comments.                           *
     H*                                                                   *
     H* 05/26/10  C:61  Project: WC1851                                   *
     H*                 Fix. No comments.                                 *
     H*                                                                   *
     H* 02/24/09  C:33  Project: WC1851   25WQL Priss# 8                  *
     H*                 Re-write.  No comments.                           *
     H*********************************************************************
    H*
     FW0063Fa   cf   e             workstn infds(fids01)
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
     F* View #1
     Fwtfil     if   e           k disk    rename(pfi01:dbrcdf)
     F                                     infds(dbfds)
     F* View #2
     Fwtfil1    if   e           k disk    rename(pfi01:dbrcdf2)
     F                                     infds(dbfds2)

     Fwtftp     if   e             disk    rename(pft01:pft0p)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     Fststl3    if   e           k disk    rename(pst01:pst03)
     Fwdfcl2    if   e           k disk    rename(pfc01:pfc02)

     D#ofHD            c                   2
     D hd              s             78    dim(#ofHD) ctdata perrcd(1)
     D#ofOFD           c                   20
     D of              s              1    dim(#ofOFD) ctdata perrcd(1)
     D ofd             s             35    dim(#ofOFD) alt(of)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0063R02A'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   11
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             60
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@call          s              1
     D kkCo#           s                   like(FICO#)
     D kkFnd           s                   like(FIFND)
     D kkType          s                   like(FITYPE)
     D kkDesc          s                   like(FIDESC)
     D kkSt            s                   like(FIST)
     D kkEfDt          s                   like(FIEFDT)
     D kkTbId          s                   like(GTTBID) inz('FEES')
     D optNm           s             40
     D sysDt           s               d
     D dwnDt           s               d

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     DClmFeeColtDtl    pr             1n
    D*
     D/copy sPrDate
     D/copy sPrMessage
     D/copy sPrRunCmd
     D/copy sPrString
     D/copy sPrWrkWth
    C*
     D/copy wPrGroup
     D/copy wPrEmplr
     D/copy wPrRateSet
    C*
     C/copy sCopyWW41C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     c                   parm      *inkc         @@kc              1
     c                   parm      *inkl         @@kl              1
     C                   parm                    @@call
     c                   parm                    @@rrn             9 0
    C*
    C*  Add/change/display program parameter list
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scDbRR        ##rrn
     C                   parm      @@rrn         ##rrn2
    C*
    C*  Parameter list for detail program(s)
     C     dtlPl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##Mode
     C                   parm      scDbRR        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam

    C*================================================================
    C*  KLIST definitions
    C*================================================================

     C     key           klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkType
     C                   kfld                    kkDesc

     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkType
     C                   kfld                    kkDesc
     C                   kfld                    kkSt
     C                   kfld                    kkEfDt

     C     key2          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkType
     C                   kfld                    kkDesc
     C                   kfld                    kkEfDt
     C                   kfld                    kkSt

     C     kkey3         klist
     C                   kfld                    xxKey            10
     C*
     C     keyWTGTL      klist
     C                   kfld                    kkCo#
     C                   kfld                    kkTbId
     C                   kfld                    kkType

    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr

     c                   eval      *in11 = ssSt   <> *blanks
     c                                  or ssEfDt <> *zeros
     c                                  or ssIn   <> *blanks

     C                   endsr

    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr
     C*
     C                   select
     C* Opt 4
     C                   when      scSel = '4'
     C                   if        ClmFeeColtDtl
     C                   seton                                        72  90
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr

    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr

     C*  state  must be valid
     c  n90              if        (ssSt <> *blanks)
     c                   movel     ssSt          xxKey
     c     kkey3         chain     pst03                              60
     c   60              seton                                        7190
     c                   endif

     C*  Effective date must be valid
     c  n90              if        DtCheck(ssEfDt) = *zeros
     c                              and ssEfDt <> *zeros
     C                   seton                                        7290
     c                   endif

     C*  Include/Exclude Expired.
     c  n90              if        ssIn <> *blanks
     c                              and ssIn <> 'E'
     c                              and ssIn <> 'I'
     c                   seton                                        7390
     c                   endif

     C                   endsr

    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr

     c                   eval      ssSt   = *blanks
     c                   eval      ssEfDt = *zeros
     c                   eval      ssIn   = *blanks

     C                   endsr

    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr

     C                   eval      kkCo#  = FTCO#
     C                   eval      kkFnd  = FTFND
     C                   eval      kkType = FTTYPE
     C                   eval      kkDesc = FTDESC
     C                   eval      kkEfDt = *loval
     C                   eval      kkSt   = *loval

     C                   endsr

    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr

     c                   select

     c                   when      *in21 = *on
     c                   eval      kkSt = scPst1
     c                   eval      kkEfDt = DtCYMD(scPst1A)

     c                   when      *in22 = *on
     c                   eval      kkEfDt = DtCYMD(numDate(scPst2))
     c                   eval      kkSt   = scPst2A

     c                   endsl                                                  select

     C                   endsr                                                  keyset

    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr

     c                   eval      kkSt   = scSt
     c                   eval      kkEfDt = scEfDt

     C                   endsr

      *===============================================================*
      * KPDO - Subroutine for F15=Mass copy
      *===============================================================*
     c     kpdo          begsr

     c                   move      'M'           ##mode                         Disp Mass cpy scr
     c                   call      acdpgm        acdpl                  99
     c                   exsr      kedo

     c                   endsr

    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   eval      scSt   = FIST
     C                   eval      scEfDt = FIEFDT
     C                   eval      scExDt = FIEXDT
     C                   eval      scCFee = FICFEE
     C                   eval      scColt = FICOLT

     c                   select
     c                   when      view# = 1 and info# = 1
     C                   eval      scdata = ' '+ FIST
     C                              + '    ' + AlphaUSA(CvtToDate(FIEFDT))
     C                              + '  ' + AlphaUSA(CvtToDate(FIEXDT))
     C*
     c                   when      view# = 2 and info# = 1
     C                   eval      scdata = AlphaUSA(CvtToDate(FIEFDT))
     c                              + '  ' + AlphaUSA(CvtToDate(FIEXDT))
     c                              + '   ' + FIST

     c                   endsl

     C                   endsr

    C*================================================================
    C* Processing for opt 7
    C*================================================================
     C     Opt7          begsr
     C*
     C                   eval      kkType = FTTYPE
     C                   clear                   GTCF01
     C     keyWTGTL      chain     pgt00
     C*
     C                   select
     C                   when      @@Call = *blank
     C                   eval      ##Mode = 'C'
     C                   other
     C                   eval      ##Mode = 'D'
     C                   endsl
     C*
     C                   call(e)   GTCF01        dtlPl
     C*
     C                   endsr

    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C*
     C     *inkp         caseq     *on           kpdo                     90
     c                   endcs
     C*
     C                   endsr

    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr
     C*
     C     scSel         caseq     '7'           Opt7                     90
     C                   endcs
     C*
     C                   endsr

    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr

    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     c     pgmstr        begsr

     c     @@rrn         chain(n)  pft0p
     C                   exsr      KeyInz
     c                   eval      scFnd  = FTFND
     c                   eval      scFNam = GetGrpName(FTFND)
     C*
     C                   eval      optNm = %trim(FTDESC) + ' Detail'
     C                   exsr      GetPgT

     C                   eval      sysDt = SystemDate

     C                   endsr

    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr

     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 95
     C                   when      *in22
     C     key           reade     dbrcdf2                                95
     C                   endsl

     C                   endsr

    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr

     C                   select
     C                   when      *in21
     C     key           readpe    dbrcdf                                 95
     C                   when      *in22
     C     key           readpe    dbrcdf2                                95
     C                   endsl

     C                   endsr

    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr

     C                   select
     C                   when      *in21
     C     key1          setgt     dbrcdf                             96
     C                   when      *in22
     C     key2          setgt     dbrcdf2                            96
     C                   endsl

     C                   endsr

    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr

     C                   select
     C                   when      *in21
     C     key1          setll     dbrcdf                             96
     C                   when      *in22
     C     key2          setll     dbrcdf2                            96
     C                   endsl

     C                   endsr

    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr

     C                   eval      scData = '* * * Deleted'

     C                   endsr

    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr
     C                   endsr

    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr

     c                   select
     c* State
     c                   when      FIST <> ssSt
     c                              and ssSt <> *blanks
     c                   eval      meetss = *off
     c* Effective Date
     c                   when      CvtToDate(FIEFDT) <> CvtToDate(ssEfDt)
     c                              and ssEfDt <> *zeros
     c                   eval      meetss = *off
     c* Expired
     c                   when      (ssIn = 'E')
     c                              and (FIEXDT <> 0)
     c                              and CvtToDate(FIEXDT) < sysDt
     c                   eval      meetss = *off
     c*
     c                   endsl

     c                   endsr

    P*================================================================
    P* Check for Claim Fee or Collateral Detail
    P*================================================================
     PClmFeeColtDtl    b
     DClmFeeColtDtl    pi             1n

     C     WDFCL2_key1   klist
     C                   kfld                    ##Co#
     C                   kfld                    scFnd
     C                   kfld                    scSt
     C                   kfld                    scEfDt

     C     WDFCL2_key2   klist
     C                   kfld                    ##Co#
     C                   kfld                    scFnd
     C                   kfld                    scSt

     C     WDFCL2_key1   setll     pfc02
     C     WDFCL2_key2   reade     pfc02

     C                   select
     C                   when      %eof
     C                   return    *off
     C                   other
     C                   return    *on
     C                   endsl

     PClmFeeColtDtl    e

      **==============================================================*
** Column headings
Opt  State   Eff Date    Exp Date
Opt   Eff Date    Exp Date   State
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Detail                 YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
215F15=Mass copy            YNN
217F17=Subset               YYY
3
