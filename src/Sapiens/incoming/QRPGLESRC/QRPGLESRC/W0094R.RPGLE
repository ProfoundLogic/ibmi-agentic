     Hcopyright('(c) Copyright StoneRiver, Inc., 2009')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H BNDDIR('W0094R')
     H*********************************************************************
     H/TITLE W0094R - W/W 3rd Party Billing A/R Update
     H*********************************************************************
     H*  Created: 02/03/09   C:33   Project: 146WBW                       *
     H*********************************************************************
     H* REVISIONS:                                                        *
     H*                                                                   *
     H* 08/01/16     C:079      Remove keyword qualified                  *
     H*                                                                   *
     H* 02/07/13     C:279      Changes to display function keys desc.    *
    H*                                                                   *
     H* 10/05/09     C:180      SUPPORT 700127 fix edit for               *
     H*                         change/delete options.
    H*                                                                   *
     H*********************************************************************
     Fw0094f    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    Fwdelp     if   e             disk    rename(pel01:pel0p)
    F*
    F*  Physical file definition (for refreshing subfile record)
    Fwae4p     if   e             disk    rename(pea01:pfrcdf) usropn
    F*
    F*  Primary logical file definition (for displaying items)
     Fwae4l     if   e           k disk    rename(pea01:dbrcdf) usropn
     F                                     infds(dbfds)
      *  Employer Detail Workfile
     Fwae3l     if   e           k disk    rename(pel01:pe30l) prefix(e3)
    D*
     D hd              s             78    dim(1) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)
     ****** Beg Add **** 02/07/13 **************************************
      * Add by Profound Logic for Dynamic Function Keys
     D*** Beg Del *** 08/01/16 ***********************************
     D***** AV_KEYS_DS      ds                  qualified
     D***** ARRAY                         81    dim(14)
     D***** KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D*** End Del *** 08/01/16 ***********************************
     ****** End Add **** 02/07/13 **************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   '3rd Party Billing A/R Update'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0094R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   1
     D sf1pag          c                   10
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
     D ##ELrrn         s              9  0
     D @@Call          s              1
     D @@rrn           s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D kkCo#           s                   like(EACO#)
     D kkFnd           s                   like(EAFND)
     D kkEmp#          s                   like(EAEMP#)
     D kkDiv           s                   like(EADIV)
     D kkFyr           s                   like(EAFYR)
     D kkPoDt          s                   like(EAPODT)
     D kkdt            s                   like(EADT)
     D kktime          s                   like(EATIME)
     D cltID           s              3
     D EM_rrn          s              9  0
     D FN_rrn          s              9  0
     d NonDisplay      c                   const(x'A7')
     C***** BEGIN ADD ***************** 10/05/09 ***************************
     D PostDate        s               d
     d SysDate         s               d
     D DateIso         s             10
     C***** END ADD ******************* 10/05/09 ***************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*
     DCheckAR          pr             1n
    D*
     DPriorMonth       pr             1n
     D inDate                         7  0
    D*
      *=====================================================================
    d* Procedure Prototypes
      *=====================================================================
     d resetData       pr                  extpgm('W0093C02')
     D qqfrFile                      10    const
     D qqtoFile                      10    const
     D qqco                           3    const
     D qqfnd                          3    const
     D qqemp#                         9    const
     D qqdiv                          5    const
     D qqfyr                          3    const

     D/copy wPrAR
     D/copy wPrEmplr
     D/copy wPrGroup
    C*
     C/copy sCopyWW42c
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@Call
     C                   parm                    @@rrn
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C                   parm      @@rrn         ##ELrrn
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
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C                   kfld                    kkFyr
     C*
     C     key1          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkFnd
     C                   kfld                    kkEmp#
     C                   kfld                    kkDiv
     C                   kfld                    kkFyr
     C                   kfld                    kkPoDt
     C                   kfld                    kkdt
     C                   kfld                    kktime
     C*
    C*====================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*====================================================================
     C     chkssm        begsr
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
     C                   eval      scEDN  = EmpDivName(ELEMP#:ELDIV:EM_rrn:
     C                                                 ELFND:ELCO#)
    C*
     C                   endsr
     C*
    C*====================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*====================================================================
     C     errck1        begsr
     C*
     C***** BEGIN CHANGE ************** 10/05/09 ***************************
     ***
     C****               if        (scSel = '2' or scSel = '4')
     C****                         and PriorMonth(scPoDt)
     C****                         and PriorMonth(scTrDt)
     c****               eval      *in71 = *on
     c****               eval      *in90 = *on
     c****               endif
     C*
     C                   if        (scSel = '2' or scSel = '4')
     c     *CYMD         MOVE      scPoDt        PostDate
     C                   MOVE      PostDate      DateIso
     C                   EVAL      %SUBST(DateIso:9) = '01'
     c                   move      Dateiso       PostDate


     C                   eval      sysdate = Systemdate()
     C                   MOVE      SysDate       DateIso
     C                   EVAL      %SUBST(DateIso:9) = '01'
     c                   move      DateIso       SysDate

     c                   if        (PostDate < SysDate)
     c                   eval      *in71 = *on
     c                   eval      *in90 = *on
     c                   endif
     c                   endif
     C***** END CHANGE   ************** 10/05/09 ***************************
     C                   endsr
    C*====================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*====================================================================
     C     errck2        begsr
     C                   endsr
    C*====================================================================
    C* Move group information data base fields to screen fields
    C*====================================================================
     C     GroupInfo     begsr
     C                   eval      scFnd  = ELFND
     C                   eval      scFNam = GetGrpName(ELFND:FN_rrn:ELCO#)
     C                   endsr
    C*====================================================================
    C* INZSST - Initialize the subset fields
    C*====================================================================
     C     inzsst        begsr

     ****** Beg Add **** 02/07/13 **************************************
      * Add by Profound Logic for Dyamic Function Keys
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl
     C                   Eval      AV_KEYS =  AV_KEYS_DS.KEYS
      /Endif
     ****** End Add **** 02/07/13 **************************************

     C                   endsr
    C*====================================================================
    C* KEYINZ - Initialize key values for program start
    C*====================================================================
     C     keyinz        begsr
     C     @@rrn         chain     WDELP
     C                   eval      kkCo#  = ELCO#
     C                   eval      kkFnd  = ELFND
     C                   eval      kkEmp# = ELEMP#
     C                   eval      kkDiv  = ELDIV
     C                   eval      kkFyr  = ELFYR
     C                   eval      kkPoDt = *loval
     C                   clear                   kkpodt
     C                   clear                   kkdt
     C                   clear                   kktime
     c     key           chain     pe30l                              60
     C                   endsr
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
     C     keyset        begsr
     C                   eval      kkPoDt = dtCYMD(numDate(scPst1))
     C                   endsr
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
     C     keysfl        begsr
     C                   eval      kkPoDt = scPoDt
     C                   eval      kkdt   = scdt
     C                   eval      kktime = sctime
     C                   endsr
    C*====================================================================
    C* loadHeader - Load information to the header fields
    C*====================================================================
     C     LoadHeader    begsr
     C                   exsr      GroupInfo
     C                   exsr      EmplrInfo
     C                   endsr
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
     C     mvdbsc        begsr
     C                   eval      scPoDt = EAPODT
     C                   eval      scTrDt = EATRDT
     C                   eval      scArCd = EAARCD
     C                   eval      scArTy = EAARTY
     C                   eval      scDesc = EADESC
     C                   eval      scAmt  = EAAMT
     C                   eval      scdt   = EADT
     C                   eval      sctime = EATIME
     C                   select
     C                   when      info# = 1 and view# = 1
     C                   eval      scData = AlphaUSA(CvtToDate(EAPODT)) + '  '
     C                                    + AlphaUSA(CvtToDate(EATRDT)) + '  '
     C                                    + EAARCD + '  '
     C                                    + EAARTY + '  '
     C                                    + %subst(EADESC:1:25) + ' '
     C                                    + %editc(ARValue(EAAMT:EAARTY):'J')
     C                   endsl
     C                   endsr
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
     C     othfks        begsr

     C                   select
     c                   when      *inkg = *on
     c                   close     wae4p
     c                   close     wae4l
     c                   move(p)   elco#         qqco#             3
     c                   move(p)   elfnd         qqfnd             3
     c                   move(p)   elemp#        qqemp#            9
     c                   move(p)   eldiv         qqdiv             5
     c                   move(p)   elfyr         qqfyr             3
     c                   callp     resetData     ('WMEAP' :
     c                                            'WAE4P' :
     c                                            qqco#   :
     c                                            qqfnd   :
     c                                            qqemp#  :
     c                                            qqdiv   :
     c                                            qqfyr
     c                                           )
     c                   open      wae4p
     c                   open      wae4l
     c                   exsr      kedo
     c                   endsl
     C                   endsr
    C*====================================================================
    C* OTHOPT - Process other options
    C*====================================================================
     C     othopt        begsr
     C                   endsr
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     pgmend        begsr
     c                   if        not(*inkc) and not(*inkl)
     C                   eval      rstPgm = CheckAR
     c                   endif
     c                   if        rstPgm = *off
     c                   close     wae4p
     c                   close     wae4l
     c                   eval      *inlr = *on
     c                   endif
     C                   endsr
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
     C     pgmstr        begsr
     c                   open      WAE4P
     c                   open      WAE4L
     C                   call      'RTVCLTID'
     C                   parm                    cltID
     C                   exsr      LoadHeader
     C                   endsr
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
     C     read          begsr
     C*
     C                   select
     C                   when      view# = 1
     C     key           reade     dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
     C     readp         begsr
     C*
     C                   select
     C                   when      view# = 1
     C     key           readpe    dbrcdf                                 95
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETGT  - Set greater than on file
    C*====================================================================
     C     setgt         begsr
     C*
     C                   select
     C                   when      view# = 1
     C     key1          setgt     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SETLL  - Set lower limits on file
    C*====================================================================
     C     setll         begsr
     C*
     C                   select
     C                   when      view# = 1
     C     key1          setll     dbrcdf                             96
     C                   endsl
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*====================================================================
     C     sf1upd        begsr
     C*
     C                   eval      scData = '* * * Deleted'
     C*
     C                   endsr
     C*
    C*====================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*====================================================================
     C     slctrd        begsr
     C                   endsr
     C*
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
     C     subset        begsr
     c                   if        EAARTY = 'P' or
     c                             EAARTY = 'Q' or
     c                             EAARTY = 'R' or
     c                             EAARTY = 'N' or
     c                             EAARTY = 'T' or
     c                             EAARTY = 'V'
     c                   eval      meetss = *off
     c                   endif
     C                   endsr
    P*====================================================================
    P* Check A/R records against premium, tax and assessments from WAE3P
    P*====================================================================
     PCheckAR          b
     DCheckAR          pi             1n
    D*
     Dissue            s              1n
     D premBillAmt     s                   like(ELANPR)
     D assBillAmt      s                   like(ELANPR)
     D taxBillAmt      s                   like(ELANPR)
     D ARCat           s                   like(EACTYP)
     *
      *
      * Total premium, assessments, and taxes and compare to coresponding WDELP amounts. Return
      * *ON if they don't match. There's a blurb in Issues and Concerns about this. (Premium
      * and taxes will match only on estimating or audited policies, not self-reporting.)
      *
     c                   eval      ar04 = nonDisplay
     c                   eval      ar05 = nonDisplay
     c                   eval      ds04 = nonDisplay
     c                   eval      ds05 = nonDisplay
     c                   eval      am04 = nonDisplay
     c                   eval      am05 = nonDisplay
     c     key           setll     dbrcdf
     c     key           reade     dbrcdf
     c                   dow       not(%eof)
     c                   eval      ARCat   = getARCat(EAARCD)
     c                   if        (EAARTY = 'B'  or
     c                              EAARTY = 'A'  or
     c                              EAARTY = 'D') and
     c                              (arCat  = 'P' or arCat = 'E')
     c                   eval      premBillAmt = premBillAmt + EAAMT
     c                   endif
     c                   if        (ARCat  = '1'  or
     c                              ARCat  = '2'  or
     c                              ARCat  = '3'  or
     c                              ARCat  = '4'  or
     c                              ARCat  = '5'  or
     c                              ARCat  = '6'  or
     c                              ARCat  = '7') and
     c                              EAARTY = 'F'
     c                   eval      assBillAmt = assBillAmt + EAAMT
     c                   endif
     c                   if        (ARCat  = 'T') and
     c                              EAARTY = 'F'
     c                   eval      taxBillAmt = taxBillAmt + EAAMT
     c                   endif
     c     key           reade     dbrcdf
     c                   enddo
     c                   eval      scar01 = premBillAmt
     c                   eval      scds01 = 'Premium'
     c                   if        e3ELFAF = 'A'
     c                   eval      scam01 = e3ELAAPR
     c                   else
     c                   eval      scam01 = e3ELANPR
     c                   endif
     c                   eval      scar02 = taxBillAmt
     c                   eval      scds02 = 'State Tax'
     c                   eval      scam02 = e3ELTXDU
     c                   eval      scar03 = assBillAmt
     c                   eval      scds03 = 'Assessments'
     c                   eval      scam03 =
     c                             (e3EL1AAM + e3EL2AAM + e3EL3AAM + e3EL4AAM +
     C                              e3EL5AAM + e3EL6AAM + e3EL7AAM)
     c                   if        scam01 <> premBillAmt
     c                   eval      issue = *on
     c                   endif
     c                   if        scam02 <> taxBillAmt
     c                   eval      issue = *on
     c                   endif
     c                   if        scam03 <> assBillAmt
     c                   eval      issue = *on
     c                   endif
     C                   if        issue
     C                   exfmt     wdw01
     C                   return    *on
     C                   endif
     C                   return    *off
     PCheckAR          e
    P*====================================================================
    P* Check a date for prior month
    P*====================================================================
     PPriorMonth       b
     DPriorMonth       pi             1n
     D inDate                         7  0
     D yrFlg           s              1n
     D mthFlg          s              1n
    C*
     C                   eval      yrFlg =  %subdt(CvtToDate(inDate):*Y) >=
     C                                      %subdt(SystemDate:*Y)
     C                   eval      mthFlg = %subdt(CvtToDate(inDate):*M) >=
     C                                      %subdt(SystemDate:*M)
     C                   return    not(yrFlg and mthFlg)
    C*
     PPriorMonth       e





** Column headings
Opt    Post Dt    Trans Dt   Cd Tp  Description                     Amount
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=Reset Data            YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
