     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H*********************************************************************
     H/TITLE W0094R02 - W/W 3rd Party Billing A/R Update Maintenance
     H*********************************************************************
     H*  Created: 02/03/09   C:33   Project: 146WBW                       *
     H*********************************************************************
     H* REVISIONS:                                                        *
     H*                                                                   *
     H* 06/20/11 C:136 Support: WA146 77093                               *
     H*          C:33 -Prevent cash transactions from being written to    *
     H*                Live                                               *
     H*               -Allow for entry of 6 digits.                       *
    H*                                                                   *
     H* 07/12/10     C:180      SUPPORT 73523 Change date fmt to USA.     *
     H* 12/31/09     C:180      SUPPORT 71180 fix edit for                *
     H*                         add option.
     H*                                                                   *
     H*********************************************************************
     Fw0094f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwae4p     uf a e             disk    rename(pea01:dbrcdf)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   '3rd Party Billing A/R Update'
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
     D @@ELrrn         s              9  0
     D addMode         s              1n
     D dspMode         s              1n
     D updMode         s              1n
     D cltID           s              3
     D EM_rrn          s              9  0
     D FN_rrn          s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    D*
     DPriorMonth       pr             1n
     D inDate                         8  0
    D*
     DSrchARCode       pr
     D field                          2
     D notSelInd                      2  0 const
     D selInd                         2  0 const
     D description                   30    options(*nopass)
    D*
     D/copy wPrAR
     D/copy wPrEmplr
     D/copy wPrGroup
     C***** BEGIN ADD ***************** 12/31/09 ***************************
     D PostDate        s               d
     D TranDate        s               d
     d SysDate         s               d
     D***  Begin Add  ***  06/20/11  **************************************
     D sysDate1stDay   s               d
     D***  End   Add  ***  06/20/11  **************************************
     D DateIso         s             10
     C***** END ADD ******************* 12/31/09 ***************************
    C*
     C/copy sCopyMT42c
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@ELrrn
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
    C*====================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*====================================================================
     C     dltpre        begsr
     C                   endsr
    C*
    C*====================================================================
    C* DSPLYR - Processing after displaying information
    C*====================================================================
     C     dsplyr        begsr
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
     C     errck1        begsr
    C*
     C                   select
     C                   when      DtCheck(scPoDt) = *zero and addMode
     C                   seton                                        713190
     C***** BEGIN DELETE ************** 12/31/09 ***************************
     C*****              when      PriorMonth(scPoDt) and addMode
     C*****              seton                                        723190
     C***** END DELETE **************** 12/31/09 ***************************
     C                   when      DtCheck(scTrDt) = *zero and addMode
     C                   seton                                        713290
     C***** BEGIN DELETE ************** 12/31/09 ***************************
     C*****              when      PriorMonth(scTrDt) and addMode
     C*****              seton                                        723290
     C***** END DELETE **************** 12/31/09 ***************************
     C                   when      ValidCode(scArCd) = *off and addMode
     C                   seton                                        73  90
     C                   when      scDesc = *blank
     C                   eval      scDesc = GetARDesc(scArCD)
     C                   eval      scPoDt = DtCheck(scPoDt)
     C                   eval      scTrDt = DtCheck(scTrDt)
     C                   seton                                        82  90
     C***** BEGIN ADD ***************** 12/31/09 ***************************
     C                   when      addMode
      *
      * Posting date cannot be in prior month
      *
     C***  Begin Chg  ***  06/20/11  **************************************
     C****BEGIN*CHANGE ***** 07/12/10 ***********************************
     c******CYMD         MOVE      scPoDt        PostDate
     C**********         MOVE      PostDate      DateIso
     C**********         EVAL      %SUBST(DateIso:9) = '01'
     c**********         move      Dateiso       PostDate
     c******USA*         MOVE      scPoDt        PostDate
     C****END***CHANGE ***** 07/12/10 ***********************************
     C**********
     C**********
     C**********         eval      sysdate = Systemdate()
     C**********         MOVE      SysDate       DateIso
     C**********         EVAL      %SUBST(DateIso:9) = '01'
     c**********         move      DateIso       SysDate
     C**********
     c**********         if        (PostDate < SysDate)
     C                   eval      scPoDt = DtCheck(scPoDt)
     C                   eval      postDate = CvtToDate(scPoDt)
     C                   eval      sysDate1stDay = GetFirstDay(SystemDate)
     C*
     C                   if        postDate < sysDate1stDay
     C***  End   Chg  ***  06/20/11  **************************************
     c                   eval      *in31 = *on
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   endif
      *
      * Transaction date cannot be in prior month
      *
      *** BEGIN CHANGE ***** 07/12/10 ***********************************
     c***  *CYMD         MOVE      scTrDt        TranDate
     C***                MOVE      TranDate      DateIso
     C***                EVAL      %SUBST(DateIso:9) = '01'
     c***                move      Dateiso       TranDate
     C***  Begin Chg  ***  06/20/11  **************************************
     c******USA*         MOVE      scTrDt        TranDate
     C                   eval      scPoDt = DtCheck(scPoDt)
     C                   eval      tranDate = CvtToDate(scPoDt)
     C***  End   Chg  ***  06/20/11  **************************************
      *** END   CHANGE ***** 07/12/10 ***********************************


     C                   eval      sysdate = Systemdate()
     C                   MOVE      SysDate       DateIso
     C                   EVAL      %SUBST(DateIso:9) = '01'
     c                   move      DateIso       SysDate

     c                   if        (TranDate < SysDate)
     c                   eval      *in32 = *on
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   endif
     C***** END ADD ******************* 12/31/09 ***************************
     C                   endsl
    C*
      *** beg add ***** 06/20/11 ***********************************************
      *
      * Prevent cash transactions from being written in 3rd Party. The A/R code is
      * protected in update mode.
      *
     C                   if        not(*in90) and addMode
     C                              and PayType(GetArType(scArCd))
     C                   seton                                        83  90
     C                   endif
      *** end add ***** 06/20/11 ***********************************************

     C                   endsr
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
     C     errck2        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
     C     errck3        begsr
     C                   endsr
    C*
    C*====================================================================
    C* Move group information data base fields to screen fields
    C*====================================================================
     C     GroupInfo     begsr
     C*
     C                   eval      scfnd  = ELFND
     C                   eval      scfnam = GetGrpName(ELFND:FN_rrn:ELCO#)
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     KJDO          begsr
    C*
     C                   select
     C                   when      hfld = 'SCARCD'
     C                   callp     SrchARCode(scArCd:81:82:scDesc)
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Load information to the header fields
    C*====================================================================
     C     LoadHeader    begsr
     C*
     C                   exsr      GroupInfo
     C                   exsr      EmplrInfo
     *
     C                   endsr
    C*
    C*====================================================================
    C* Update maintenance fields
    C*====================================================================
     C     MaintFlds     begsr
     C                   eval      EAPGM  = q1Pgm
     C                   eval      EAUSER = q1User
      *
      *  Do not update timestamp if in change mode so that the sequence of records doesn't get
      *  re-arranged when copying back to WMEAP
      *
     c                   if        addMode
     C                   eval      EADT   = DateTo7(SystemDate)
     C                   eval      EATIME = TimeTo6(SystemTime)
     c                   endif
     C                   endsr
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
     C     mvdbsc        begsr
     C                   eval      scPoDt = DtMDYY(EAPODT)
     C                   eval      scTrDt = DtMDYY(EATRDT)
     C                   eval      scArCd = EAARCD
     C                   eval      scDesc = EADESC
     C                   eval      scAmt  = EAAMT
     C                   endsr
    C*====================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*====================================================================
     C     mvscdb        begsr
    C*
     C                   if        addMode
     C                   eval      EACO#  = ELCO#
     C                   eval      EAFND  = scFnd
     C                   eval      EAFYR  = scFyr
     C                   eval      EAEMP# = scEmp#
     C                   eval      EADIV  = scDiv
     C                   eval      EARDIV  = scDiv
     c                   eval      EAARTY = getARType(scarcd)
     c                   eval      EACTYP = getARCat(scarcd)
     C                   endif
    C*
     C                   eval      EAPODT = DtCYMD(scPoDt)
     C                   eval      EATRDT = DtCYMD(scTrDt)
     C                   eval      EAARCD = scArCd
     C                   eval      EADESC = scDesc
     C                   eval      EAAMT  = scAmt
    C*
     C                   exsr      MaintFlds
     C                   endsr
    C*====================================================================
    C* OTHERR - Processing for other modes
    C*====================================================================
     C     otherr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     othfks        begsr
     C*
     C***  End   Add  ***  06/20/11  **************************************
     C                   eval      scPoDt = DtCheck(scPoDt)
     C                   eval      scTrDt = DtCheck(scTrDt)
     C***  End   Add  ***  06/20/11  **************************************
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     pgmend        begsr
     c                   eval      *inlr = *on
     C                   endsr
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
     C     pgmstr        begsr
     C*
     C                   call      'RTVCLTID'
     C                   parm                    cltID
     C*
     C                   eval      *in09 = *off
     C                   eval      *in40 = *off
     C*
     C                   select
     C                   when      @@Mode = 'A'
     C                   eval      addMode = *on
     C                   eval      *in40 = *on
     C                   when      @@Mode = 'C'
     C                   eval      updMode = *on
     C                   when      @@Mode = 'D'
     C                   eval      dspMode = *on
     C                   eval      *in09 = *on
     C                   endsl
     C*
     C     @@ELrrn       chain(n)  WDELP
     C     @@rrn         chain(n)  dbrcdf
     C*
     C                   exsr      LoadHeader
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PREOTH - Pre-processing for other modes
    C*====================================================================
     C     preoth        begsr
     C                   endsr
    C*
    C*====================================================================
    C* UPDATR - Update data base record and after processing
    C*====================================================================
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WRITER - Write data base record and after processing
    C*====================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wae4p
    C*
     C                   eval      outFlg = *on
    C*
     C                   endsr





    P*====================================================================
    P* Check a date for prior month
    P*====================================================================
     PPriorMonth       b
     DPriorMonth       pi             1n
     D inDate                         8  0
    C*
     C                   if        %subdt(CvtToDate(inDate):*M) >=
     C                             %subdt(SystemDate:*M) and
     C                             %subdt(CvtToDate(inDate):*Y) >=
     C                             %subdt(SystemDate:*Y)
     c                   return    *off
     c                   else
     c                   return    *on
     c                   endif
    C*
     PPriorMonth       e





     C*====================================================================
     C* Search for A/R Codes
     C*====================================================================
     PSrchARCode       b
     DSrchARCode       pi
     D field                          2
     D notSelInd                      2  0 const
     D selInd                         2  0 const
     D description                   30    options(*nopass)
     D*
     D##kc             s              1
     D##kl             s              1
     D##Key            s             10
     C*
     C                   eval      ##Key = ARCodeSrch(##kc:##kl)
     C*
     C                   select
     C                   when      ##kl = *on
     C                   eval      *in(notSelInd) = *on
     C                   when      ##Key <> *blank
     C                   eval      field = ##Key
     C                   if        %parms >= 4
     C                   eval      description = GetARDesc(field)
     C                   endif
     C                   eval      *in(selInd) = *on
     C                   other
     C                   eval      *in(notSelInd) = *on
     C                   endsl
     C*
     C                   return
     C*
     PSrchARCode       e
