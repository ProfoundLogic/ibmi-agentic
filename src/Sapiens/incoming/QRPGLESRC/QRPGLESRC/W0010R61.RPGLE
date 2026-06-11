     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
      *=============================================================================================
      /title W0010R61 - Auto Cash Dissolve Info                                                    *
     H option(*nodebugio:*srcstmt)
      *=============================================================================================
      *  Program description: Maintenance program for Cash Dissolve Info                           *
                                                                                                   *
      *  Called from: W0010R (W/W Groups - Option 61)                                              *
                                                                                                   *
      *  Calls to:    W0010R62                                                                     *
                                                                                                   *
      *  Created 10/07/05 C:136  Project: WC1179                                                   *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 03/15/21 c:062 Project: WC2663                                                             *
      *                Add deductible and retrospective payment codes                              *
      *                                                                                            *
      * 11/19/09 C:136 Support: WA136 70672                                                        *
      *                Change edits affected by SCCPMT being disabled by WC1854.                   *
      *                                                                                            *
      * 10/27/09 C:136 Project: WC1854                                                             *
      *                Add control grid flags for how cash, revised, audit, and lockbox            *
      *                transactions are to be dissolved across policy years.                       *
      *                                                                                            *
      * 05/03/09 C:136 Project: WC1663                                                             *
      *                Add 'Dissolve cash payments to renewal year' flag                           *
      *                                                                                            *
      * 01/10/08 C:136 Project: WC1536                                                             *
      *                Add 'Restrict dissolve if contested audit' flag                             *
      *                                                                                            *
      * 12/07/05 C:136 Support: xxxxx                                                              *
      *                Correct conditioning on call to W0010R62                                    *
      *=============================================================================================
     fw0010f61  cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     *  Physical file definition (for maintaining record)
     fwdf2p     uf a e             disk
     f                                     rename(pf201:dbrcdf)
     fwdf2l     if   e           k disk    infds(f2rrnds)
     fwmfnp     if   e             disk
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     d optnm           c                   const('Auto Cash Dissolve')
     d comapi          c                   'WA001R'
     d #ofscr          c                   1
     d ##co#           s              3  0
     d ##conm          s             40
     d ##fnam          s             40
     d ##fnd           s              3  0
     d key             s             10

     d f2rrnds         ds
     d  f2rrn                397    400b 0
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     d*entry
     d w0010r61        pr
     d  @@kc                          1
     d  @@kl                          1
     d  @@mode                        1
     d  @@rrn                         9  0
     d w0010r61        pi
     d  @@kc                          1
     d  @@kl                          1
     d  @@mode                        1
     d  @@rrn                         9  0

     d Hierarchy       pr                  extpgm('W0010R62')
     d  @@kc                          1
     d  @@kl                          1
     d  @@call                        1
     d  @@rrn                         9  0

     d/copy sprstring
     d/copy sprdate
     d/copy sprcompany
     d/copy wprusrprms
     d/copy wprar
     d/copy wprgroup

     c/copy scopymt41c
      *=============================================================================================
     *  PLIST definitions
      *=============================================================================================
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
      *=============================================================================================
     *  KLIST definitions
      *=============================================================================================
     c     f2ky          klist
     c                   kfld                    fnco#
     c                   kfld                    fnfnd
     *=============================================================================================
     * DLTPRE - Pre-processing before delete occurs
     *=============================================================================================
     c     dltpre        begsr
     c                   endsr
     *=============================================================================================
     * DSPLYR - Processing after displaying information
     *=============================================================================================
     c     dsplyr        begsr
     c                   endsr
      *=============================================================================================
    c* ERRCK1 - Error checking for srceen 1 (SCR01)
      *=============================================================================================
     c     errck1        begsr

      *** beg add ***** 10/27/09 *******************************************
     c                   if        sccpyf <> *blank and
     c                             sccpyf <> 'Y'
     c                   seton                                        318490
     c                   leavesr
     c                   endif
     c                   if        scccyf <> *blank and
     c                             scccyf <> 'Y'
     c                   seton                                        318590
     c                   leavesr
     c                   endif
     c                   if        sccryf <> *blank and
     c                             sccryf <> 'Y'
     c                   seton                                        318690
     c                   leavesr
     c                   endif
     c                   if        scccyf =  *blank and
     c                             (sccpyf <> *blank or
     c                              sccryf <> *blank)
     c                   seton                                        318790
     c                   leavesr
     c                   endif
     c                   if        scccyf <> *blank and
     c                             sccpdt <> 'B' and
     c                             sccpdt <> 'C'
     c                   seton                                        318890
     c                   leavesr
     c                   endif
     c                   if        scccyf =  *blank and
     c                             sccpdt <> *blank
     c                   seton                                        318990
     c                   leavesr
     c                   endif
     c                   if        scrpyf <> *blank and
     c                             scrpyf <> 'Y'
     c                   seton                                        328490
     c                   leavesr
     c                   endif
     c                   if        scrcyf <> *blank and
     c                             scrcyf <> 'Y'
     c                   seton                                        328590
     c                   leavesr
     c                   endif
     c                   if        scrryf <> *blank and
     c                             scrryf <> 'Y'
     c                   seton                                        328690
     c                   leavesr
     c                   endif
     c                   if        scrcyf =  *blank and
     c                             (scrpyf <> *blank or
     c                              scrryf <> *blank)
     c                   seton                                        328790
     c                   leavesr
     c                   endif
     c                   if        scrcyf <> *blank and
     c                             scrpdt <> 'B' and
     c                             scrpdt <> 'C'
     c                   seton                                        328890
     c                   leavesr
     c                   endif
     c                   if        scrcyf =  *blank and
     c                             scrpdt <> *blank
     c                   seton                                        328990
     c                   leavesr
     c                   endif
     c                   if        scapyf <> *blank and
     c                             scapyf <> 'Y'
     c                   seton                                        338490
     c                   leavesr
     c                   endif
     c                   if        scacyf <> *blank and
     c                             scacyf <> 'Y'
     c                   seton                                        338590
     c                   leavesr
     c                   endif
     c                   if        scaryf <> *blank and
     c                             scaryf <> 'Y'
     c                   seton                                        338690
     c                   leavesr
     c                   endif
     c                   if        scacyf =  *blank and
     c                             (scapyf <> *blank or
     c                              scaryf <> *blank)
     c                   seton                                        338790
     c                   leavesr
     c                   endif
     c                   if        scacyf <> *blank and
     c                             scapdt <> 'B' and
     c                             scapdt <> 'C'
     c                   seton                                        338890
     c                   leavesr
     c                   endif
     c                   if        scacyf =  *blank and
     c                             scapdt <> *blank
     c                   seton                                        338990
     c                   leavesr
     c                   endif
     c                   if        sclpyf <> *blank and
     c                             sclpyf <> 'Y'
     c                   seton                                        348490
     c                   leavesr
     c                   endif
     c                   if        sclcyf <> *blank and
     c                             sclcyf <> 'Y'
     c                   seton                                        348590
     c                   leavesr
     c                   endif
     c                   if        sclryf <> *blank and
     c                             sclryf <> 'Y'
     c                   seton                                        348690
     c                   leavesr
     c                   endif
     c                   if        sclcyf =  *blank and
     c                             (sclpyf <> *blank or
     c                              sclryf <> *blank)
     c                   seton                                        348790
     c                   leavesr
     c                   endif
     c                   if        sclcyf <> *blank and
     c                             sclpdt <> 'B' and
     c                             sclpdt <> 'C'
     c                   seton                                        348890
     c                   leavesr
     c                   endif
     c                   if        sclcyf =  *blank and
     c                             sclpdt <> *blank
     c                   seton                                        348990
     c                   leavesr
     c                   endif
      *** end add ***** 10/27/09 *******************************************
     c                   if             sccpmt <> *blank
     c                             and  sccpmt <> '1'
     c                             and  sccpmt <> '2'
     c                   seton                                        71  90
     c                   endif

      *** beg chg ***** 11/13/09 *******************************************
     c**n90******        if                 sccpmt <> *blank
      *
      * Make sure New Business payment code is supplied if estimated bills are to
      * dissolve cash
      *
     c  n90              if             scrcyf <> *blank
      *** end chg ***** 11/13/09 *******************************************
     c                                  and scnpcd =  *blank
     c                   seton                                        72  90
     c                   endif

     c  n90              if                ValidCode(scnpcd) = *off
     c                                  or GetARType(scnpcd) <> 'P'
     c                                  or GetARCat(scnpcd) <> 'P'
     c                   seton                                        73  90
     c                   endif

      *** beg chg ***** 11/13/09 *******************************************
     c**n90******        if                 sccpmt <> *blank
      *
      * Make sure New Business payment code is supplied if estimated bills are to
      * dissolve cash
      *
     c  n90              if             scrcyf <> *blank
      *** end chg ***** 11/13/09 *******************************************
     c                                  and scnxcd =  *blank
     c                   seton                                        74  90
     c                   endif

     c  n90              if                ValidCode(scnxcd) = *off
     c                                  or GetARType(scnxcd) <> 'P'
     c                                  or GetARCat(scnxcd) <> 'P'
     c                   seton                                        75  90
     c                   endif

     c  n90              if                 sccpmt <> *blank
     c                                  and sccdty <> 'B'
     c                                  and sccdty <> 'C'
     c                   seton                                        76  90
     c                   endif

      *** beg add ***** 05/03/09 **********************************************
     c  n90              if             scrnew <> *blank and
     c                                  scrnew <> 'Y'
     c                   seton                                        79  90
     c                   endif
      *** end add ***** 05/03/09 **********************************************
      *** beg add ***** 01/10/08 **********************************************
      *** beg del ***** 11/19/09 **********************************************
     c**n90******        if             sccaud <> *blank and
     c***********                       sccpmt <> '1'
     c***********        seton                                        77  90
     c***********        endif
      *** end del ***** 11/19/09 **********************************************

     c  n90              if            sccaud <> *blank and
     c                                 sccaud <> 'Y'
     c                   seton                                        78  90
     c                   endif
      *** end add ***** 01/10/08 **********************************************
      *** Begin Add *** 03/15/21 *******************************************
     c* Deductible Payment Code must be a payment code
     c  n90              if             scDpCd <> ' ' And
     c                                  (ValidCode(scDpCd) = *Off Or
     c                                   GetARType(scDpCd) <> 'Q' Or
     c                                   GetARCat(scDpCd) <> 'R')
     c                   seton                                        76  90
     c                   endif
     c* Retrospective Payment Code must be a payment code
     c  n90              if             scRpCd <> ' ' And
     c                                  (ValidCode(scRpCd) = *Off Or
     c                                   GetARType(scRpCd) <> 'P' Or
     c                                   GetARCat(scRpCd) <> 'C')
     c                   seton                                        79  90
     c                   endif
      *** End   Add *** 03/15/21 *******************************************

     c                   endsr
     *=============================================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *=============================================================================================
     c     errck2        begsr
     c                   endsr
     *=============================================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *=============================================================================================
     c     errck3        begsr
     c                   endsr
    C*=============================================================================================
    C* KJDO   - Processing for F10
    C*=============================================================================================
     C     kjdo          begsr

      *** beg del ***** 10/27/09 ***********************************************
     c***********        eval      key = ARCodeSrch(*inkc:*inkl)
      *** end del ***** 10/27/09 ***********************************************

     c                   select
     c                   when      hfld = 'SCNPCD'
     c                   eval      key = ARCodeSrch(*inkc:*inkl)
     c                   if        key  <> *blank
     c                   eval      scnpcd = key
     c                   eval      *in82 = *on
     c                   else
     c                   eval      *in81 = *on
     c                   endif
     c                   when      hfld = 'SCNXCD'
     c                   eval      key = ARCodeSrch(*inkc:*inkl)
     c                   if        key  <> *blank
     c                   eval      scnxcd = key
     c                   eval      *in83 = *on
     c                   else
     c                   eval      *in82 = *on
     c                   endif
      *** Begin Add *** 03/15/21 *******************************************
     c                   when      hfld = 'SCDPCD'
     c                   eval      key = ARCodeSrch(*inkc:*inkl)
     c                   if        key  <> *blank
     c                   eval      scDpCd = key
     c                   eval      *in85 = *on
     c                   else
     c                   eval      *in84 = *on
     c                   endif
     c                   when      hfld = 'SCRPCD'
     c                   eval      key = ARCodeSrch(*inkc:*inkl)
     c                   if        key  <> *blank
     c                   eval      scRpCd = key
     c                   eval      *in85 = *on
     c                   else
     c                   eval      *in85 = *on
     c                   endif
      *** End   Add *** 03/15/21 *******************************************
     c                   endsl

     C                   endsr
     *=============================================================================================
     * MVDBSC - Move data base fields to screen fields
     *=============================================================================================
     c     mvdbsc        begsr
     c                   eval      sccpmt = F2CPMT
     c                   eval      scnpcd = F2NPCD
     c                   eval      scnxcd = F2NXCD
     c                   eval      sccdty = F2CDTY
      *** beg add ***** 01/10/08 *******************************************************
     c                   eval      sccaud = F2CAUD
      *** end add ***** 01/10/08 *******************************************************
      *** beg add ***** 05/03/09 *******************************************************
     c                   eval      scrnew = F2RNEW
      *** end add ***** 05/03/09 *******************************************************
      *** Begin Add *** 03/15/21 *******************************************
     c                   Eval      scDpCd = F2dpCd
     c                   Eval      scRpCd = F2rpCd
      *** End   Add *** 03/15/21 *******************************************
      *** beg add ***** 10/27/09 *******************************************************
      *
      * Default new flags based upon values of old flags IAW WC1854 Spec's and clear
      * old flags
      *
     c                   if        sccpmt <> *blank or
     c                             scrnew <> *blank
     c                   select
     c                   when      sccpmt = '1'
     c                   eval      sccpyf = 'Y'
     c                   eval      scccyf = 'Y'
     c                   eval      scrpyf = 'Y'
     c                   eval      scrcyf = 'Y'
     c                   eval      scapyf = 'Y'
     c                   eval      scacyf = 'Y'
     c                   eval      sclcyf = 'Y'
     c                   when      sccpmt = '2'
     c                   eval      scccyf = 'Y'
     c                   eval      scrcyf = 'Y'
     c                   eval      scacyf = 'Y'
     c                   eval      sclcyf = 'Y'
     c                   other
     c                   endsl
     c                   if        scrnew = 'Y'
     c                   eval      sccryf = 'Y'
     c                   eval      scrryf = 'Y'
     c                   eval      scaryf = 'Y'
     c                   endif
     c                   eval      sccpdt = sccdty
     c                   eval      scrpdt = sccdty
     c                   eval      scapdt = 'C'
     c                   eval      sclpdt = 'B'
     c                   eval      scrnew = *blank
     c                   eval      sccpmt = *blank
     c                   eval      sccdty = *blank
     c                   else
     c                   eval      sccpyf = F2CPYF
     c                   eval      scccyf = F2CCYF
     c                   eval      sccryf = F2CRYF
     c                   eval      sccpdt = F2CPDT
     c                   eval      scrpyf = F2RPYF
     c                   eval      scrcyf = F2RCYF
     c                   eval      scrryf = F2RRYF
     c                   eval      scrpdt = F2RPDT
     c                   eval      scapyf = F2APYF
     c                   eval      scacyf = F2ACYF
     c                   eval      scaryf = F2ARYF
     c                   eval      scapdt = F2APDT
     c                   eval      sclpyf = F2LPYF
     c                   eval      sclcyf = F2LCYF
     c                   eval      sclryf = F2LRYF
     c                   eval      sclpdt = F2LPDT
     c                   endif
      *** end add ***** 10/27/09 *******************************************************
     c                   endsr
     *=============================================================================================
     * MVSCDB - Move screen fields to data base fields
     *=============================================================================================
     c     mvscdb        begsr
     c                   eval      F2CPMT = sccpmt
     c                   eval      F2NPCD = scnpcd
     c                   eval      F2NXCD = scnxcd
     c                   eval      F2CDTY = sccdty
      *** beg add ***** 01/10/08 *******************************************************
     c                   eval      F2CAUD = sccaud
      *** end add ***** 01/10/08 *******************************************************
      *** beg add ***** 05/03/09 *******************************************************
     c                   eval      F2RNEW = scrnew
      *** end add ***** 05/03/09 *******************************************************
      *** beg add ***** 10/27/09 *******************************************************
     c                   eval      F2CPYF = sccpyf
     c                   eval      F2CCYF = scccyf
     c                   eval      F2CRYF = sccryf
     c                   eval      F2CPDT = sccpdt
     c                   eval      F2RPYF = scrpyf
     c                   eval      F2RCYF = scrcyf
     c                   eval      F2RRYF = scrryf
     c                   eval      F2RPDT = scrpdt
     c                   eval      F2APYF = scapyf
     c                   eval      F2ACYF = scacyf
     c                   eval      F2ARYF = scaryf
     c                   eval      F2APDT = scapdt
     c                   eval      F2LPYF = sclpyf
     c                   eval      F2LCYF = sclcyf
     c                   eval      F2LRYF = sclryf
     c                   eval      F2LPDT = sclpdt
      *** end add ***** 10/27/09 *******************************************************
      *** Begin Add *** 03/15/21 *******************************************
     c                   Eval      F2dpCd = scDpCd
     c                   Eval      F2rpCd = scRpCd
      *** End   Add *** 03/15/21 *******************************************
     c                   endsr
     *=============================================================================================
     * OTHERR - Processing for other modes
     *=============================================================================================
     c     otherr        begsr
     c                   endsr
      *=============================================================================================
    c* OTHFKS - Processing for other functions keys
      *=============================================================================================
     c     OTHFKS        begsr
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     c                   endsr
      *=============================================================================================
    c* PGMEND - End of program processing
      *=============================================================================================
     c     PGMEND        begsr
     c
      *** beg chg ***** 12/07/05 *********************************************************
     c*****************  if        (not *inkc or not *inkl) and
      *** beg chg ***** 10/27/09 *********************************************************
     c*****************  if        (not *inkc and not *inkl) and
      *** beg chg ***** 12/07/05 *********************************************************
     c*****************             F2CPMT <> *blanks
     c                   if        (not *inkc and not *inkl)
      *** end chg ***** 10/27/09 *********************************************************

     C                   call      'W0010R62'
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C                   parm      *blank        ##call            1
     C                   parm      @@mode        ##mode            1
     C                   parm                    fnfnd

     C                   endif

     c                   endsr
      *=============================================================================================
    c* PGMSTR - Used for any start of program processing (one-time)
      *=============================================================================================
     c     PGMSTR        begsr
    
     c     @@mode        comp      'D'                                    09
     c                   movel(p)  optnm         ##data
     c                   eval      scprds = CenterFld(##data:%len(scprds))
    
     c     @@rrn         chain     pfn01
     c                   eval      scfnd  = fnfnd
     c                   eval      scname = GetGrpName(fnfnd)
    
    c     f2ky          chain     pf201
    c                   eval      @@rrn = f2rrn
    
     c                   endsr
     *=============================================================================================
     * PREOTH - Pre-processing for other modes
     *=============================================================================================
     c     preoth        begsr
     c                   endsr
     *=============================================================================================
     * UPDATR - Update data base record and after processing
     *=============================================================================================
     c     updatr        begsr
     c     @@rrn         chain     dbrcdf                             99
B01  c                   if        *in99 = *off
     c                   exsr      mvscdb
     c                   update    dbrcdf
E01  c                   endif
     c                   endsr
     *=============================================================================================
     * WRITER - Write data base record and after processing
     *=============================================================================================
     c     writer        begsr
     c                   clear                   dbrcdf
     c                   exsr      mvscdb
     c                   write     dbrcdf
     c                   feod      wdf2p
     c                   endsr
