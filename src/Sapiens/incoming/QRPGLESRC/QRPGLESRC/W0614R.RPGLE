     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
    H*********************************************************************
    H/TITLE W0614R - Account Current A/R by Policy
    H*********************************************************************
    H* Created:  01/26/00     C:33     Project: WC899   Seq#: WA107/21502*
    H* Rewrite:  06/09/00     C:33     Project: WC696                    *
    H*********************************************************************
    H* Revisions:                                                        *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
    H*                                                                   *
    H* 11/06/01 C:142 SEQ#: 29466 WA107                                  *
    H*          Correct ##rrn1 parm should have been from                *
    H*          the subfile line that was selected.                      *
    H*                                                                   *
    H* 10/31/01 C:136 Support: 29522 WA096                               *
    H*          Canx date displays  erroneously.                         *
    H*                                                                   *
    H* 10/04/01 C:136 Support: 29143 WA107                               *
    H*          Option 5 causes the balance to increment erroneously.    *
    H*                                                                   *
    H* 06/21/2001  Clt: 132  Project: WA107 Seq#: 27685                  *
    H* Chg for incorrect Balance, Not using Type N in totals             *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0614F    cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwdabp     if   e             disk    rename(pab01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fwdabl9    if   e           k disk    rename(pab01:dbrcdf)  infds(dbfds)
     Fwdabl8    if   e           k disk    rename(pab01:dbrcdf2) infds(dbfds2)
     Fwsasl     if   e           k disk    rename(pas01:pas00)   infds(asfds)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwtabl     if   e           k disk
     Fwtavl     if   e           k disk    rename(pav01:pav00)
    D*
     D hd              s             78    dim(4) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
    D*
     D asfds           ds
     D asrrn                 397    400b 0
    D*
    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
    D*
     D optnm           c                   'Account Current A/R by Policy'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W3103R02'
     D comapi          c                   'WA001R'
     D agtapi          c                   'WA002R'
     D uspapi          c                   'W0006R'
     D chgnum          c                   'SA020R'
     D infomx          c                   2
     D viewmx          c                   2
     D sf1pag          c                   10
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@fnd           s                   like(EMFND)
     D @@emp#          s                   like(EMEMP#)
     D @@div           s                   like(EMDIV)
     D @@fyr           s                   like(ELFYR)
     D @#call          s                   like(@@call)
     D @#fnd           s                   like(@@fnd)
     D @#emp#          s                   like(@@emp#)
     D @#div           s                   like(@@div)
     D @#fyr           s                   like(@@fyr)
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnd           s              3  0
     D ##fnd_name      s             40
     D ##emp#          s                   like(scemp#)
     D ##div           s                   like(scdiv)
     D ##fyr           s                   like(scyr)
     D** **##agt#          s                   like(scagt#)
     D ##agt_name      s             60
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##rrn1          s              9  0
     D kkpodt          s                   like(ABPODT)
     D kkstdt          s                   like(ABSTDT)
     D kkidt           s                   like(ABIDT)
     D kkitim          s                   like(ABITIM)
     D xxkey           s                   like(TTKEY)
     D firstRead       s              1
     D readBack        s              1
     D balance         s             11  2
     D priorAmt        s                   like(scamt)
     D prv_emp#        s             40
     D ABCHK#_a        s              7
     D numOfZeros      s             15  0
     D error           s              1
     D rrn             s              9  0
     D db_rrn          s              9  0
     D FN_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     D***** beg add ***** 07/07/05 ****************************************
     D fgfyr           s                   like(*IN90)
     D***** end add ***** 07/07/05 ****************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     DAR_value         PR            11  2
     D AR_amount                     11  2  const
     D AR_type                        1     const

     DbalValue         PR            11  2
     D AR_Amount                     11  2  const
     D netAmount                     11  2  const
     D AR_type                        1     const

     DmeetSubset       PR             1

     DsflBalance       PR            11  2
     D rrn                            9  0 const

     D/copy SPRDATE
     D/copy SPRMESSAGE
     D/copy SPRRUNCMD
     D/copy SPRSTRING
     D/copy SPRWRKWTH

     D/copy WPREMPLR
     D/copy WPRGROUP
     D*
     C/COPY SCOPYWW41C
    C*
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
     C     *entry        plist
     C                   parm                    @#call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     C**** BEGIN CHG **** 11/06/01 *********************************************************
     C*                  parm      asrrn         ##rrn1
     C                   parm      scasrrn       ##rrn1
     C**** END   CHG **** 11/06/01 *********************************************************
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnd_name
    C*
     C     agtpl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm      scemp#        ##emp#
     C                   parm      scdiv         ##div
     C                   parm      scyr          ##fyr
     C** **scagt#        parm                    ##agt#
     C                   parm                    ##agt_name
    C*
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
    C*
     C     key           klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scyr
    C*
     C     key1          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scyr
     C                   kfld                    kkstdt
     C                   kfld                    kkpodt
     C                   kfld                    kkidt
     C                   kfld                    kkitim
    C*
     C     key2          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scyr
     C                   kfld                    kkpodt
     C                   kfld                    kkidt
     C                   kfld                    kkitim
    C*
     C     asky          klist
     C                   kfld                    ABCO#
     C                   kfld                    ABSTDT
     C                   kfld                    ABAGT#
    C*
     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    xxkey
    C*
    C*====================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*====================================================================
    C*
     C     chkssm        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*====================================================================
    C*
     C     errck1        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1A - Error checking for SCR01A
    C*====================================================================
    C*
     C     errck1a       begsr
     C*
     C  n90              if        ValidEmp(scemp#:scdiv:EM_rrn:##fnd)
     C                              = *off
     C                   seton                                        71  90
     C                   endif
     C*
     C  n90              if        ValidEmpYr(scyr:scemp#:scdiv:EL_rrn:##fnd)
     C                              = *off
     C                   seton                                        72  90
     C                   endif
    C*
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*====================================================================
    C*
     C     errck2        begsr
     C                   endsr
    C*
    C*====================================================================
    C* GET_WSASL - Get the summary record for record passed in
    C*====================================================================
    C*
     C     get_WSASL     begsr
     C*
     C                   clear                   asrrn
     C     asky          chain     pas00                              60
     C                   eval      scasrrn = asrrn
     C*
     C                   endsr
    C*
    C*====================================================================
    C* INZSST - Initialize the subset fields
    C*====================================================================
    C*
     C     inzsst        begsr
     C*
     C                   if        @#emp# = *zero
     C                   clear                   scemp#
     C                   clear                   scdiv
     C                   clear                   scyr
     C                   exsr      scr01a_sr
     C                   else
     C                   eval      scemp# = @#emp#
     C                   eval      scdiv  = @#div
     C                   eval      scyr   = @#fyr
     C                   exsr      load_hdr
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KCKL_DO - Load *INKC and INKL to parameters, if needed
    C*====================================================================
    C*
     C     kckl_do       begsr
     C*
     C                   if        q1prms >= 2
     C                   eval      @@KC = *inkc
     C                   endif
     C*
     C                   if        q1prms >= 3
     C                   eval      @@KL = *inkl
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYINZ - Initialize key values for program start
    C*====================================================================
    C*
     C     keyinz        begsr
    C*
     C                   clear                   kkpodt
     C                   clear                   kkstdt
     C                   clear                   kkidt
     C                   clear                   kkitim
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYSET - Set key values when doing "position to"
    C*====================================================================
    C*
     C     keyset        begsr
     C*
     C                   clear                   kkpodt
     C                   clear                   kkstdt
     C                   clear                   kkidt
     C                   clear                   kkitim
    C*
     C                   select
     C                   when      *in21
     C                   eval      kkstdt = dt5CYMD(numDate6(scpst1))
     C                   when      *in22
     C                   eval      kkpodt = dtCYMD(numDate(scpst2))
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*====================================================================
    C*
     C     keysfl        begsr
     C*
     C                   eval      kkpodt = scpodt
     C                   eval      kkstdt = scstdt
     C                   eval      kkidt  = scidt
     C                   eval      kkitim = scitim
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO - Process F10
    C*====================================================================
    C*
     C     kjdo          begsr
    C*
     C                   select
     C*____________________________________________________________________
     C*
     C                   when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     C*
     C                   eval      EM_rrn = empSrch(*inkc:*inkl:error:##fnd)
    C*
     C     EM_rrn        chain     wmemp                              60
    C*
     C                   select
     C                   when      *inkl = *on or *in60
     C                   eval      *in81 = *on
     C                   other
     C                   eval      *in82 = *on
     C                   eval      scemp# = EMEMP#
     C                   eval      scdiv  = EMDIV
     C                   endsl
     C*____________________________________________________________________
     C*
     C                   when      hfld = 'SCYR'
     C*
     C                   eval      EL_rrn = empYrSrch(*inkc:*inkl:error:scemp#:
     C                                              scdiv:##fnd)
    C*
     C     EL_rrn        chain     wdelp                              60
    C*
     C                   select
     C                   when      error = '2'
     C                   eval      *in71 = *on                                  invalid emp#
      *** beg add **** 10/04/01 ************************************************************
     C                   eval      *in81 = *on                                  position cursor
      *** end add **** 10/04/01 ************************************************************
     C                   when      *inkl = *on or *in60
      *** beg chg **** 10/04/01 ************************************************************
     C*****************  eval      *in81 = *on
     C                   eval      *in82 = *on                                  position cursor
      *** end chg **** 10/04/01 ************************************************************
     C                   other
      *** beg chg **** 10/04/01 ************************************************************
     C***********        eval      *in82 = *on
     C***********        eval      scemp# = EMEMP#
     C***********        eval      scdiv  = EMDIV
     C                   eval      scyr = ELFYR
     C                   if        scemp# = 0
     C                   eval      scemp# = ELEMP#
     C                   eval      scdiv = ELDIV
     C                   endif
      *** end chg **** 10/04/01 ************************************************************
     C                   endsl
     C*____________________________________________________________________
     C*
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* LOAD_bal - Load balance when adding a new subfile screen
    C*====================================================================
    C*
     C     load_bal      begsr
    C*
     C                   if        firstRead = *on
     C                   eval      balance   = sflBalance(db_rrn)
     C                   eval      firstRead = *off
     C                   eval      readBack  = *off
     C***** beg add ***** 07/07/05 ****************************************
     C                   if        fgfyr = *OFF
     C                   eval      EL_rrn = GetEmpYrRR(scyr:scemp#:scdiv)
     C     EL_rrn        chain     WDELP                              60
     C*
     C                   clear                   AVDESC
     C                   eval      xxkey = ELASCD
     C     tbky          chain     pav00                              60
     C                   eval      scact_sts = ELASCD + ' ' + AVDESC
     C                   eval      *in55 = scact_sts <> *blanks
     C                   eval      fgfyr = *ON
     C                   endif
     C***** end add ***** 07/07/05 ****************************************
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* LOAD_HDR - Load the header for subfile display
    C*====================================================================
    C*
     C     load_hdr      begsr
    C*
     C                   eval      scedn = EmpDivName(scemp#:scdiv:EM_rrn:##fnd)
    C*
     C     EM_rrn        chain     wmemp                              60
    C*
     C                   eval      prv_emp# = GetPrvEmp#(##co#:##fnd:scemp#:
     C                                          scdiv:scofnd:scoemp:scodiv)
     C                   eval      *in51 = prv_emp# <> *blanks
    C*
      *** beg chg **** 10/31/01 *********************************************************
     C****************** eval      sccndt = GetCxDate(##co#:##fnd:scemp#:scdiv)
     C                   eval      sccndt = dtMDYY(GetCxDate
     C                                      (##co#:##fnd:scemp#:scdiv))
      *** end chg **** 10/31/01 *********************************************************
     C                   eval      *in52  = sccndt <> *zero
    C*
     C                   eval      scdivc = DivCount(##co#:##fnd:scemp#)
    C*
      *** end del ****  7/07/05 *********************************************************
     C************       clear                   AVDESC
     C************       eval      xxkey = EMASTS
     C*****tbky***       chain     pav00                              60
     C************       eval      scact_sts = EMASTS + ' ' + AVDESC
      *** end del ****  7/07/05 *********************************************************
    C** **
    C** GET AGENT # & NAME
    C** **
    C** **              call      agtapi        agtpl
    C** **
    C** **              eval      scanam = ##agt_name
    C** **
    C** **              move      *date         usa_date
    C** ***cymd         move      usa_date      x7date
    C** **
    C** CALC BILLED, PAID & BALANCE
    C** **
    C** **              eval      scbill = *zero
    C** **              eval      scpaid = *zero
    C** **              eval      scbal  = *zero
    C** **              eval      xxfyr = scyr
    C** **
    C** **key           chain     dbrcdf                             60
    C** **
    C** **              dow       *in60 = *off and ABPODT <= x7date
    C** **
    C** **              select
    C** **              when      ABARTY = 'A'
    C** **                        or ABARTY = 'B'
    C** **                        or ABARTY = 'D'
    C** **                        or ABARTY = 'F'
    C** **                        or ABARTY = 'G'
    C** **                        or ABARTY = 'R'
    C** **                        or ABARTY = 'V'
    C** **                        or ABARTY = 'N'
    C** **                        or ABARTY = 'I'
    C** **              eval      scbill = scbill + ABNET
    C** **              when      ABARTY = 'P'
    C** **                        or ABARTY = 'Q'
    C** **              eval      scpaid = scpaid - ABAMT
    C** **              endsl
    C** **
    C** **key           reade     dbrcdf                                 60
    C** **
    C** **              enddo
    C** **
    C** **              eval      scbal = scbill + scpaid
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
    C*
      *** beg add **** 10/04/01 *********************************************************
     C                   if        ##mode <> 'D'
      *** end add **** 10/04/01 *********************************************************
     C                   eval      scpodt = ABPODT
     C                   eval      scstdt = ABSTDT
     C                   eval      scidt  = ABIDT
     C                   eval      scitim = ABITIM
     C*
     C                   exsr      get_WSASL
     C*
     C                   eval      scamt = AR_value(ABAMT:ABARTY)
     C                   eval      scnet = ABNET
     C*
     C                   if        readBack = *off
     C                   eval      balance = balance +
     C                               balValue(ABAMT:ABNET:ABARTY)
     C                   else
     C                   eval      balance = balance - priorAmt
     C                   endif
     C*
     C                   eval      scbal    = balance
     C                   eval      priorAmt = balValue(ABAMT:ABNET:ABARTY)
     C*
     C                   clear                   scmadj
     C*
     C                   if        ABMADJ = 'Y'
     C                   eval      scmadj = 'M'
     C                   endif
     C*
     C                   eval      *in59 = ABOPEN = 'Y'
     C*
     C                   move      ABCHK#        ABCHK#_a
     C     '0'           check     ABCHK#_a      numOfZeros
     C                   if        numOfZeros = 0
     C                   eval      numOfZeros = 8
     C                   endif
     C                   eval      %subst(ABCHK#_a:1:numOfZeros - 1) = '       '
     C*
     C                   clear                   scdata
     C*
     C                   select
     C*____________________________________________________________________
     C*
     C                   when      view# = 1 and info# = 1
     C                   eval      scdata = alpha6USA(dt6MDYY(ABSTDT)) + '  '
     C                                    + alphaUSA(cvtToDate(ABPODT)) + '   '
     C                                    + ABARCD + '  ' + ABARTY
     C*____________________________________________________________________
     C*
     C                   when      view# = 1 and info# = 2
     C                   eval      scdata = alpha6USA(dt6MDYY(ABSTDT)) + '  '
     C                                    + alphaUSA(cvtToDate(ABPODT)) + '  '
     C                                    + ABCHK#_a
     C*____________________________________________________________________
     C*
     C                   when      view# = 2 and info# = 1
     C                   eval      scdata = alphaUSA(cvtToDate(ABPODT)) + '  '
     C                                    + alpha6USA(dt6MDYY(ABSTDT)) + '   '
     C                                    + ABARCD + '  ' + ABARTY
     C*____________________________________________________________________
     C*
     C                   when      view# = 2 and info# = 2
     C                   eval      scdata = alphaUSA(cvtToDate(ABPODT)) + '  '
     C                                    + alpha6USA(dt6MDYY(ABSTDT)) + '  '
     C                                    + ABCHK#_a
     C*____________________________________________________________________
     C*
     C                   endsl
    C*
      *** beg add **** 10/04/01 *********************************************************
     C                   else
     C                   eval      ##mode = *blank
     C                   if        ABOPEN = 'Y'
     C                   eval      *in59 = *on
     C                   else
     C                   eval      *in59 = *off
     C                   endif                                                  ABOPEN = 'Y'
     C                   endif                                                  ##mode = 'D'
      *** end add **** 10/04/01 *********************************************************
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Process other function keys
    C*====================================================================
    C*
     C     othfks        begsr
     C*
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
     C                   endcs
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHOPT - Process other options
    C*====================================================================
    C*
     C     othopt        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     pgmend        begsr
     C*
     C                   if        *inkl = *off
     C                   clear                   scyr
     C                   clear                   scemp#
     C                   clear                   scdiv
     C                   endif
     C*
     C                   if        *inkc = *off and @#emp# = *zero
     C                   exsr      scr01a_sr
     C                   exsr      keyinz
     C                   exsr      kedo
     C                   eval      *in06  = *off
     C                   eval      rstpgm = *on
E05  C                   endif
     C*
     C                   exsr      kckl_do
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
    C*
     C     pgmstr        begsr
     C*
     C                   eval      *in12 = *on
     C*
     C                   if        q1prms >= 1
     C                   eval      @@call = @#call
     C                   endif
     C*
     C                   if        q1prms >= 4 and @@fnd <> *zero
     C                   eval      @#fnd = @@fnd
     C                   eval      ##fnd = @@fnd
     C                   eval      *in12 = *off
     C                   endif
     C*
     C                   if        q1prms >= 5
     C                   eval      @#emp# = @@emp#
     C                   eval      @#div  = @@div
     C                   eval      @#fyr  = @@fyr
     C                   endif
     C*
     C                   eval      scfnd      = ##fnd
     C                   eval      scname     = GetGrpName(##fnd:FN_rrn)
     C                   eval      scname_fnd = scname
     C*
     C                   endsr
    C*
    C*====================================================================
    C* READ   - Read next record from file
    C*====================================================================
    C*
     C     read          begsr
    C*
     C                   select
     C                   when      *in21 = *on
     C     key           reade     dbrcdf                                 95
     C                   eval      db_rrn = dbrrn
     C                   when      *in22 = *on
     C     key           reade     dbrcdf2                                95
     C                   eval      db_rrn = dbrrn2
     C                   endsl
    C*
     C                   exsr      load_bal
    C*
     C                   endsr
    C*
    C*====================================================================
    C* READP  - Read previous record from file
    C*====================================================================
    C*
     C     readp         begsr
    C*
     C                   select
     C                   when      *in21 = *on
     C     key           readpe    dbrcdf                                 95
     C                   eval      db_rrn = dbrrn
     C                   when      *in22 = *on
     C     key           readpe    dbrcdf2                                95
     C                   eval      db_rrn = dbrrn2
     C                   endsl
    C*
     C                   eval      readBack = *on
    C*
     C                   exsr      load_bal
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SCR01A_SR - Subroutine for executing SCR01A
    C*====================================================================
    C*
     C     scr01a_sr     begsr
     C*
     C                   eval      scprda = optnm
     C                   eval      scprda = centerFld(scprda)
     C*
     C                   dou       *in90 = *off
     C*
     C                   exfmt     scr01a
     C*
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
     C*
     C                   if        *inkc = *off
     C*
     C                   exsr      othfks
     C*
     C                   if        *inkc = *off
     C*
B04  C                   if        *in90 = *off
     C                   exsr      errck1a
E04  C                   endif
     C*
E04  C                   endif
     C*
E03  C                   endif
     C*
E02  C                   enddo
     C*
     C                   if        *inkc = *on
     C                   exsr      kckl_do
     C                   return
     C                   endif
     C*
     C                   exsr      load_hdr
     C*
     C                   endsr
    C*
    C*====================================================================
    C* SETGT  - Set greater than on file
    C*====================================================================
    C*
     C     setgt         begsr
    C*
     C                   select
     C                   when      *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   when      *in22 = *on
     C     key2          setgt     dbrcdf2                            96
     C                   endsl
     C*
     C                   eval      firstRead = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SETLL  - Set less than on file
    C*====================================================================
    C*
     C     setll         begsr
    C*
     C                   select
     C                   when      *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   when      *in22 = *on
     C     key2          setll     dbrcdf2                            96
     C                   endsl
     C*
     C                   eval      firstRead = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*====================================================================
    C*
     C     sf1upd        begsr
     C                   endsr
    C*
    C*====================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*====================================================================
    C*
     C     slctrd        begsr
     C                   endsr
    C*
    C*====================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*====================================================================
    C*
     C     subset        begsr
     C*
     C                   eval      meetss = meetSubset
     C*
     C                   endsr
    C*





    P*====================================================================
    P* AR_value - Determine if A/R amount is positive or negative
    P*====================================================================
    P*
     PAR_value         b
    P*
     DAR_value         PI            11  2
     D AR_amount                     11  2  const
     D AR_type                        1     const

     D amount          s             11  2

     C                   select
     C                   when      AR_type = 'P' or AR_type = 'Q' or            Prem, Fee Pmts
     C                             AR_type = 'I'                                Deposit Pymt
     C                   eval      amount = AR_amount * -1
     C                   other
     C                   eval      amount = AR_amount
     C                   endsl
     C*
     C                   return    amount

     PAR_value         e





    P*====================================================================
    P* balValue - Determine the amount used for calculating balance
    P*====================================================================
    P*
     PbalValue         b
    P*
     DbalValue         PI            11  2
     D AR_Amount                     11  2  const
     D netAmount                     11  2  const
     D AR_type                        1     const

     D amount          s             11  2

     C                   select
     C                   when      AR_type = 'P' or AR_type = 'Q' or            Prem, Fee Payments
     C**** BEGIN CHG **** 06/21/01 *********************************************************
     C***********                  AR_type = 'I'                                Deposit, Return Prem
     C                             AR_type = 'I' or AR_type = 'R'               Deposit, Return Prem
     C**** END   CHG **** 06/21/01 *********************************************************
     C                   eval      amount = AR_value(AR_Amount:AR_type)
     C                   other
     C                   eval      amount = AR_value(netAmount:AR_type)
     C                   endsl
     C*
     C                   return    amount

     PbalValue         e





    P*====================================================================
    P* meetSubset - Determine if the records meet the subset criteria
    P*====================================================================
    P*
     PmeetSubset       b
    P*
     DmeetSubset       PI             1

     D meetSS          s              1

     C                   eval      meetSS = *on
     C*
     C*
     C                   return    meetSS

     PmeetSubset       e





    P*====================================================================
    P* sflBalance - Calculate the balance prior to the first 'displayed'
    P*====================================================================
    P*
     PsflBalance       b
    P*
     DsflBalance       PI            11  2
     D saveRRN                        9  0  const

     D balance         s             11  2
     D DB_rrn          s              9  0

     C                   clear                   balance
     C                   clear                   ABAMT
     C*
     C                   select
     C                   when      *in21
     C     key           chain     dbrcdf                             60
     C                   eval      DB_rrn = dbrrn
     C                   when      *in22
     C     key           chain     dbrcdf2                            60
     C                   eval      DB_rrn = dbrrn2
     c                   endsl
     C*
     C                   dow       not *in60 and DB_rrn <> saveRRN
     C*
     C                   if        meetSubset <> *off
     C                   eval      balance = balance +
     C                               balValue(ABAMT:ABNET:ABARTY)
     C                   endif
     C*
     C                   select
     C                   when      *in21
     C     key           reade     dbrcdf                                 60
     C                   eval      DB_rrn = dbrrn
     C                   when      *in22
     C     key           reade     dbrcdf2                                60
     C                   eval      DB_rrn = dbrrn2
     c                   endsl
     C*
     C                   enddo
     C*
     C                   return    balance

     PsflBalance       e





** Column headings
Opt Stmt Dt   Post Date   Cd Tp    A/R Amount       Net Due         Balance
Opt  Post Date  Stmt Dt   Cd Tp    A/R Amount       Net Due         Balance
Opt Stmt Dt   Post Date   Check#   A/R Amount       Net Due         Balance
Opt  Post Date  Stmt Dt   Check#   A/R Amount       Net Due         Balance
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
