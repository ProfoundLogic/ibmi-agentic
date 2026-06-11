     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      /TITLE WW501R02 - Return Fund Batch Detail Info
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      *  DESCRIPTION:  Return Fund Batch Detail Info                  *
      *                                                               *
      * CALLED BY: W0501R                                             *
      *                                                               *
      * CALLS TO:  NONE                                               *
      *                                                               *
      * CREATED: 03/12/03  Matt Dawson   Project: WC1033              *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 01/09/24  C:300  Project: WC2676                              *
      *                  Some corrections in getBalAB()               *
      *                                                               *
      * 09/08/15  C:298  Support: Phaseware 36948                     *
      *                  Allow the Check request to be payable to
      *                  Premium Finance Co in edit mode.
      *                                                               *
      * 08/18/14  C:279  Support: WA129 86400                         *
      *                  Modified tp process 'Paid To' field edit     *
      *                  correctly.                                   *
      *                                                               *
      * 03/19/13  C:283  Support WA102 81877                          *
      *                  Agent number is getting cleared as the       *
      *                  Autofill flag is not getting the correct     *
      *                  value. Moving the block of code to end of    *
      *                  the subroutine ERRCK1 so that any error shoul*
      *                  be captured before populating AutoFill flag. *
      *                                                               *
      * 04/27/11  C:180  Support 76721
      *                  Fix error:
      *                  Job hangs up when pressing F10 on non-promptable
      *                  fields.
      *
      * 04/11/11  C:180  Support 76457
      *                  Fix error: Missing agent information
      *                   - Fix call to WA002R
      *
      *                   - Retrieve agent number when PAID-TO is
      *                     changed to Agent.
      *
      * 03/03/11 C:180 SUP 76075
      *                Do not allow payment detail entry when payee
      *                is agent and agent is zero/does not exist.
      *                                                               *
      * 12/07/10 C:189 WA129 74722                                    *
      *                The employer edit is occuring only for one time*
      *                after which Autofill flag becomes on and any   *
      *                invalid employer can be entered.               *
      *
      * 10/22/08 C:136 WA132 65711                                    *
      *                Fix Employer Search on new customer ID field.  *
      *                Goes into endless loop if searched on in add   *
      *                mode.                                          *
      *                                                               *
      * 06/27/08 C:136 WA132 61665                                    *
      *                Prevent direct bill employer from being        *
      *                added as an Account Current batch detail       *
      *                record                                         *
      *                                                               *
      * 06/26/08 C:136 Project: 81WZB                                 *
      *                Auto populate customer ID when manually        *
      *                adding a transaction                           *
      *                                                               *
      * 12/26/07 C:183 PROJECT: 81WZB                                 *
      *                Added Customer ID screen field and Surplus     *
      *                dividend field for client 81.                  *
      *                                                               *
      * 07/18/07 C:136 PROJECT: WC1491                                *
      *                IMPLEMENTATION OF IND CASH AND RETURN DEPOSIT  *
      *                                                               *
      * 07/14/05 WA081 48064                                          *
      *              - Fix getBalance procedure.                      *
      *                                                               *
      * 07/12/05 C:136 SUPPORT: WA081 48031                           *
      *                CORRECT CODE TO NOT ZERO OUT AGENT NUMBER      *
      *                IF NOT AGENCY BILL.                            *
      *                                                               *
      *  11/05/04  C:33  Project: WC1033                              *
      *                  Include calculations for Agency Billing.     *
      *                                                               *
      * 06/21/04 C:136 Support: WA093 42570                           *
      *              - Auto load premium finance code if paid to flag *
      *                is 'P'. Throw error if a premium finance code  *
      *                is not assigned to the policy.                 *
      *              - Fix getBalance procedure.                      *
      *****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     FW0501F02  CF   E             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwxrtp     uf a e             disk
     F                                     rename(prt01:dbrcdf)
      * Return premium batch control
     Fwxrnp     if   e             disk
     * Tables
     Fwtabl     if   e           k disk
     F                                     Ignore(Prt01)
     * Fund master
     Fwmfnl     if   e           k disk
     F                                     Infds(fninfds)
     * Employer detail
     Fwdell     if   e           k disk
     Fwdelp     if   e             disk
     F                                     rename(pel01:pel02)
     * Employer master
     Fwmeml     if   e           k disk
     F                                     Infds(eminfds)
     Fwmemp     if   e             disk
     F                                     rename(pem01:pem02)
     F***  Begin Add  ***  11/05/04  **************************************
     * Agent master
     Fwmagl     if   e           k disk    rename(pag01:pag00)
     F                                     infds(AGinfds)
     Fwmagp     if   e             disk    rename(pag01:pag0p)
     F***  End   Add  ***  11/05/04  **************************************
     * Premium Finance Company File
     Fwmpfl     if   e           k disk
      * A/R file
     Fwmeal     if   e           k disk
     F***  Begin Add  ***  11/05/04  **************************************
      * Agency Billing File
     Fwdabl11   if   e           k disk    rename(pab01:pab11)
     F***  End   Add  ***  11/05/04  **************************************
      * Return Premium Batch
     Fwxrtl5    if   e           k disk    prefix(l5_)
     F***  Begin Add  ***  11/05/04  **************************************
     Fwxrtl6    if   e           k disk    prefix(l6_) rename(prt01:prt06)
     F***  End   Add  ***  11/05/04  **************************************
      * Return Premium Check Request
     Fwar4l4    if   e           k disk
     F***  Begin Add  ***  11/05/04  **************************************
     Fwar4l10   if   e           k disk    rename(pr401:pr410)
     F***  End   Add  ***  11/05/04  **************************************
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'Work with Return Fund Batch-
     D                                      Detail'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D autoFill        s               n
     D firstErrCk      s               n   inz(*on)
     D***  Begin Add  ***  11/05/04  **************************************
     D agencyBill      s               n
     D addMode         s               n
     D chgMode         s               n
     D dspMode         s               n
     D***  End   Add  ***  11/05/04  **************************************
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D***  Begin Add  ***  11/05/04  **************************************
     D @@mode2         s              1
     D AG_rrn          s              9  0
     D***  End   Add  ***  11/05/04  **************************************
     D svcom1          s                   like(sccom1)
     D svcom2          s                   like(sccom1)
     D svcom3          s                   like(sccom1)
     D svamt           s                   like(scrtrn)
     D***  BEG ADD  *****  12/26/07  **************************************
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D @SCCUST         S                   LIKE(SCCUST)
     D @EMCUST         S                   LIKE(EMCUST)
     D***  END ADD  *****  12/26/07  **************************************
     D***  BEGIN ADD ****  04/11/11  **************************************
     D svpdto          S              1A   INZ
     D***  END   ADD ****  04/11/11  **************************************
     D fninfds         ds
     D  fnrrn                397    400b 0
     D eminfds         ds
     D  emrrn                397    400b 0
     D***  Begin Add  ***  11/05/04  **************************************
     D AGinfds         ds
     D  AGrrn                397    400b 0
     D***  End   Add  ***  11/05/04  **************************************

     D getBalance      pr                  like(*in90)
     D***  Begin Add  ***  11/05/04  **************************************
     D getBalAB        pr                  like(*in90)
     D*
     D/copy wprUsrPrms
     D/copy wprAR
     D/copy wprAgent
     D***  End   Add  ***  11/05/04  **************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     C/COPY SCOPYMT01C
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
     C***  Begin Add  ***  11/05/04  **************************************
     C                   parm                    @@mode2
     C***  End   Add  ***  11/05/04  **************************************
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      *Blanks       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     fnpl          plist
     C                   parm      *blank        ##kc              1
     C                   parm      *blank        ##kl              1
     C                   parm      '2'           ##call            1
     C                   parm                    #1co#             3
     C                   parm      *blank        #1fnd             3
     C                   parm      *blank        #1fyr             3
     C                   parm      1             ##view            1 0
     *================================================================
     *  KLIST definitions
     *================================================================
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
      *
     C     elky          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scfyr
     *
     C     elky1         klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     *
     C     emky          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C
     C***  Begin Add  ***  11/05/04  **************************************
     C     keyWMAGL      klist
     C                   kfld                    ##co#
     C                   kfld                    scFein
     C                   kfld                    scMod
     C***  End   Add  ***  11/05/04  **************************************
      *
     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    key              10
      *
     C     alky          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scagt#
    C*
     C     pfky          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     c                   kfld                    scpf
     *================================================================
     * GETAGT - Get agent information
     *================================================================
     C     getagt        begsr
      *
      **** Beg change ***** 04/11/11 ***************************************
     C***                call      'WA002R'
     C***                parm      ##co#         ##co#             3 0
     C***                parm      scfnd         ##fnd             3 0
     C***                parm      scemp#        ##emp             9 0
     C***                parm      scdiv         ##div             5 0
     C***                parm      scfyr         ##fyr             3 0
     C***  scagt#        parm                    ##agt             5 0
     C***  scagtn        parm                    ##anam           60
     C***                parm                    ##prod            5 0
     C***                parm                    ##pnam           60
     C***                parm                    ##area            3 0
     C***                parm                    ##phon            7 0
     C***                parm      '*'           ##plan            1
     C***                parm                    ##date            8 0
     C***                parm                    ##nwrt            5 0
     C***                parm                    ##rnrt            5 0
      *
     C                   call      'WA002R'
     C                   parm      ##co#         ##co#             3 0
     C                   parm      scfnd         ##fnd             3 0
     C                   parm      scemp#        ##emp             9 0
     C                   parm      scdiv         ##div             5 0
     C                   parm      scfyr         ##fyr             3 0
     C     scagt#        parm      *ZEROS        ##agt             5 0
     C     scagtn        parm      *BLANKS       ##anam           60
     C                   parm      *ZEROS        ##prod            5 0
     C                   parm      *BLANKS       ##pnam           60
     C                   parm      *ZEROS        ##area            3 0
     C                   parm      *ZEROS        ##phon            7 0
     C                   parm      '*'           ##plan            1
     C                   parm      *ZEROS        ##date            8 0
     C                   parm      *ZEROS        ##nwrt            5 0
     C                   parm      *ZEROS        ##rnrt            5 0
      **** End change ***** 04/11/11 ***************************************
      *
     C                   endsr
     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr
     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr
     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr
     C*
     C***  BEG ADD ******  12/26/07  **************************************
     C  N90              IF        SCCUST <> *BLANKS
     C     EMKY          CHAIN     WMEML
     C                   IF        %FOUND(WMEML)
     C                   EVAL      @SCCUST = %XLATE(LO:UP:SCCUST)
     C                   EVAL      @EMCUST = %XLATE(LO:UP:EMCUST)
     C                   IF        @EMCUST <> @SCCUST
     C                   SETON                                        33  90
     C                   ENDIF                                                  @EMCUST <> @SCCUST
     C                   ENDIF                                                  %FOUND(WMEML)
     C*** BEG ADD ***** 06/26/08 ******************************************
     c                   else
     c     emky          chain     wmeml
     c                   if        %found(WMEML) and EMCUST <> *blanks
     c                   eval      sccust = EMCUST
     c                   endif                                                  %FOUND(WMEML)
     C*** end ADD ***** 06/26/08 ******************************************
     C                   ENDIF                                                  SCCUST <> *BLANKS
     C***  END ADD ******  12/26/07  **************************************
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C*__________
     C*
     C                   when      agencyBill
     C*
     C* Edit Fein/Mod
     C*
     C  n90              if        addMode and
     C                             not ValidFein(scFein:scMod)
     C                   seton                                        76  90
     C                   else
     C                   clear                   AGNAME
     C     keyWMAGL      chain     pag00
     C                   eval      scAgtName = AGNAME
     C                   endif
     C*
     C* Return premium must be positive
     C*
     C  n90              if        scrtrn_a <= *zero
     C                   seton                                        74  90
     C                   endif
      *
      * Edit Return Amount for Debit Situation   - use 75
      *
     C  n90              if        scrtrn_a <> svamt
     C                   eval      svamt = scrtrn_a
     C                   eval      *in90 = getBalAB
     C                   endif
     C*
     C* Edit A/R code
     C*
      *** beg chg ***** 07/18/07 ************************************************************
     C**n90************  if        ValidCode(scArCd) = *off
     C  n90              if        ValidCode(scArCd) = *off  and not *in44
      *** end chg ***** 07/18/07 ************************************************************
     C                   seton                                        77  90
     C                   endif
     C*
     C* Edit Selected
     C*
     C  n90              if        scselc_a <> 'Y' and
     C                             scselc_a <> 'N'
     C                   seton                                        78  90
     C                   endif
     C***  BEG ADD ******  12/26/07  **************************************
     C  N90              IF         SCCUST <> *BLANKS
     C     EMKY          CHAIN     WMEML
     C                   IF        %FOUND(WMEML)
     C                   EVAL      @SCCUST = %XLATE(LO:UP:SCCUST)
     C                   EVAL      @EMCUST = %XLATE(LO:UP:EMCUST)
     C                   IF        @EMCUST <> @SCCUST
     C                   SETON                                        33  90
     C                   ENDIF                                                  @EMCUST <> @SCCUST
     C                   ENDIF                                                  %FOUND(WMEML)
     C*** BEG ADD ***** 06/26/08 ******************************************
     c                   else
     c     emky          chain     wmeml
     c                   if        %found(WMEML) and EMCUST <> *blanks
     c                   eval      sccust = EMCUST
     c                   endif                                                  %FOUND(WMEML)
     C*** end ADD ***** 06/26/08 ******************************************
     C                   ENDIF                                                  SCCUST <> *BLANKS
     C***  END ADD ******  12/26/07  **************************************
     C*
     C* Edit Group/Emp info
     C*
     C                   eval      scfnd  = scfnd_a
     C                   eval      scemp# = scemp#_a
     C                   eval      scdiv  = scdiv_a
     C                   eval      scfyr  = scfyr_a
     C*
      *** beg chg ***** 07/18/07 ************************************************************
     C**n90******        if        scfnd_a <> *zero
     C*****fnky**        chain     pfn01
     C***********        if        not %found
     C  n90fnky          chain     pfn01
     C  n90              if        not %found
      *** end chg ***** 07/18/07 ************************************************************
     C                   seton                                        71  90
     C                   else
     C                   eval      scfndn_a = FNNAME
     C                   endif
      *** beg del ***** 07/18/07 ************************************************************
     C***********        endif
      *** end del ***** 07/18/07 ************************************************************
     C*
     C  n90              if        scemp#_a <> *zero or scdiv_a <> *zero
     C     emky          chain     pem01
     C                   if        not %found
     C                   seton                                        72  90
     C                   else
     C                   eval      scempn_a = EMNAME
     C                   endif
     C                   endif
     C*
     C  n90              if        scfyr_a <> *zero or
     C                             scemp#_a <> *zero or scdiv_a <> *zero
     C     elky          chain     pel01
     C                   if        not %found
     C                   seton                                        73  90
     C                   endif
     C                   endif

      *** beg add ***** 06/27/08 ******************************************
     C  n90              if        scfyr_a <> *zero or
     C                             scemp#_a <> *zero or scdiv_a <> *zero
      **** Beg change ***** 04/11/11 ***************************************
     C***                call      'WA002R'
     C***                parm      ##co#         ##co#             3 0
     C***                parm      scfnd_a       ##fnd             3 0
     C***                parm      scemp#_a      ##emp             9 0
     C***                parm      scdiv_a       ##div             5 0
     C***                parm      scfyr_a       ##fyr             3 0
     C***                parm                    ##agt             5 0
     C***                parm                    ##anam           60
     C***                parm                    ##prod            5 0
     C***                parm                    ##pnam           60
     C***                parm                    ##area            3 0
     C***                parm                    ##phon            7 0
     C***                parm      '*'           ##plan            1
     C***                parm                    ##date            8 0
     C***                parm                    ##nwrt            5 0
     C***                parm                    ##rnrt            5 0
      *
     C                   call      'WA002R'
     C                   parm      ##co#         ##co#             3 0
     C                   parm      scfnd_a       ##fnd             3 0
     C                   parm      scemp#_a      ##emp             9 0
     C                   parm      scdiv_a       ##div             5 0
     C                   parm      scfyr_a       ##fyr             3 0
     C                   parm      *zeros        ##agt             5 0
     C                   parm      *blanks       ##anam           60
     C                   parm      *zeros        ##prod            5 0
     C                   parm      *blanks       ##pnam           60
     C                   parm      *zeros        ##area            3 0
     C                   parm      *zeros        ##phon            7 0
     C                   parm      '*'           ##plan            1
     C                   parm      *zeros        ##date            8 0
     C                   parm      *zeros        ##nwrt            5 0
     C                   parm      *zeros        ##rnrt            5 0
      **** End change ***** 04/11/11 ***************************************

     C                   if        ##plan <> 'A'
     C                   seton                                        79  90    *in79
     C                   endif

     C                   endif
      *** end add ***** 06/27/08 ******************************************
     C*__________
     C*
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
      *
      * Edit fund
      *
     C***  Begin Del  ***  12/07/10  **************************************
     C************       if        *in40 and autoFill = *off
     C***  End   Del  ***  12/07/10  **************************************

     C     fnky          chain     pfn01
     C                   if        not %found
     C                   seton                                        71  90
     C                   else
     C                   eval      scfndn = FNNAME
     C                   endif
      *
      * Edit employer number
      *
     C     emky          chain     pem01
     C  n90              if        not %found
     C                   seton                                        72  90
     C                   else
     C                   eval      scempn = EMNAME
     C                   endif
      *
      * Edit year
      *
     C     elky          chain     pel01
     C  n90              if        not %found
     C                   seton                                        73  90
     C                   endif
      *** beg add ***** 06/21/04 ***********************************************
     C***  Begin Add  ***  12/07/10  **************************************
     C***  Begin Del  ***  03/19/13  **************************************
     C**********         if        *in40 and autoFill = *off
     C***  End   Add  ***  12/07/10  **************************************
      *
      * Autofill premium finance code if one is assigned to the year.
      *
     C**********         if        %found
     C**********         eval      scpf   = elpfin

     C********** pfky          chain     ppf01
     C**********         if        %found
     C**********         eval      scpfnm = PFNAME
     C**********         else
     C**********         eval      scpfnm = *blanks
     C**********         endif

     C**********         endif
      *** end add ***** 06/21/04 ***********************************************
      *
      * Get Agent Information in Add Mode
      *
     C**********         exsr      getagt
     C********** n90              if        ##plan = 'A'
     C**********         seton                                        79  90
     C**********         endif
      ** Begin Add **************** 03/03/11 *******************************
     C********** n90              if        scpdto = 'A' and scagt# =  *zero
     c**********         seton                                          3590
     c**********         endif
      ** End   Add **************** 03/03/11 *******************************
      *
      * Autofill contested audit flag in Add mode
      *
     C**********         eval      sccafl = *blank
     C********** elky1         setll     pel01
     C**********         dou       %eof
     C**********  elky1         reade     pel01
     C**********         if        elctst = 'Y'
     C**********         eval      sccafl = 'Y'
     C**********         leave
     C**********         endif
     C**********         enddo

     C**********         eval      autoFill = not *in90
     C**********         endif                                                  *in40
      ** End   Del **************** 03/19/13 *******************************
      *
      * Return premium must be positive
      *
     C  n90              if        scrtrn <= *zero
     C                   seton                                        74  90
     C                   endif
      *
      * Edit Return Amount for Debit Situation   - use 75
      *
     C  n90              if        scrtrn <> svamt
      *** beg add ***** 07/18/07 ***********************************************
     C                             and scbtyp <> 'I'
      *** end add ***** 07/18/07 ***********************************************
     C                   eval      svamt = scrtrn
     C                   eval      *in90 = getBalance
     C                   endif
      *
      * Edit Paid To
      *
      ********** beg del ******** 08/18/14 *******************************
     C**n90*****         if        scpdto <> 'A' and
     C**********                   scpdto <> 'E' and
     C**********                   scpdto <> 'G' and
     C**********                   scpdto <> 'P' or
     C**********                   scpdto =  'P' and
     C**********                   scpf   =  *blank
     C**********         seton                                        77  90
     C**********         endif
      ********** end del ******** 08/18/14 *******************************
      ***** Begin Add ******** 04/11/11 *******************************
     C                   if        chgMode and scpdto <> svpdto and
     C                             scpdto = 'A'
      *
     C                   call      'WA002R'
     C                   parm      ##co#         ##co#             3 0
     C                   parm      scfnd         ##fnd             3 0
     C                   parm      scemp#        ##emp             9 0
     C                   parm      scdiv         ##div             5 0
     C                   parm      scfyr         ##fyr             3 0
     C                   parm      *zeros        ##agt             5 0
     C                   parm      *blanks       ##anam           60
     C                   parm      *zeros        ##prod            5 0
     C                   parm      *blanks       ##pnam           60
     C                   parm      *zeros        ##area            3 0
     C                   parm      *zeros        ##phon            7 0
     C                   parm      '*'           ##plan            1
     C                   parm      *zeros        ##date            8 0
     C                   parm      *zeros        ##nwrt            5 0
     C                   parm      *zeros        ##rnrt            5 0
      *
     c                   if        ##plan = 'A'
     C                   seton                                        79  90    *in79
     c                   endif
      *
     c                   if        ##agt = *zero
     c                   seton                                        35  90
     c                   else
     C                   eval      scAGT# = ##agt
     C                   eval      scAGTN = ##anam
     c                   move      scpdto        svpdto
     c                   endif
      *
     c                   ENDIF
      ***** End   Add ******** 04/11/11 *******************************
      *
      * Edit Selected
      *
     C  n90              if        scselc <> 'Y' and
     C                             scselc <> 'N'
     C                   seton                                        78  90
     C                   endif
     C***  Begin Add  ***  03/19/13  **************************************
      ***** Beg Chg ***** 09/08/15 ****************************************
     C**************     if        *in40 and autoFill = *off
      /Free
       If (*in40 = '1' and autoFill = *off)
        or (chgMode = '1' and autoFill = *off);
      /End-free
      ***** End Chg ***** 09/08/15 ****************************************
      *
      * Autofill premium finance code if one is assigned to the year.
      *
     C                   if        %found
     C                   eval      scpf   = elpfin

     C     pfky          chain     ppf01
     C                   if        %found
     C                   eval      scpfnm = PFNAME
     C                   else
     C                   eval      scpfnm = *blanks
     C                   endif

     C                   endif
      *
      * Get Agent Information in Add Mode
      *
     C                   exsr      getagt
     C  n90              if        ##plan = 'A'
     C                   seton                                        79  90
     C                   endif
     C  n90              if        scpdto = 'A' and scagt# =  *zero
     c                   seton                                          3590
     c                   endif
      *
      * Autofill contested audit flag in Add mode
      *
     C                   eval      sccafl = *blank
     C     elky1         setll     pel01
     C                   dou       %eof
     C     elky1         reade     pel01
     C                   if        elctst = 'Y'
     C                   eval      sccafl = 'Y'
     C                   leave
     C                   endif
     C                   enddo

     C                   eval      autoFill = not *in90
     C                   endif                                                  *in40
     C***  End   Add  ***  03/19/13  **************************************
      /free
       //********** beg del ******** 08/18/14 *******************************
       // Edit Paid To
       If *in90 = *off;
          If (scPdTo <> 'A' and scPdTo <> 'E' and scPdTo <> 'G' and
           scPdTo <> 'P') or (scPdTo = 'P' and scPf = *Blank);
             *in77 = *on;
             *in90 = *on;
          Endif;
       Endif;
       //********** end del ******** 08/18/14 *******************************
      /end-free

     C                   if        not *in90 and firstErrCk
     C                   eval      *in90 = autoFill
     C                   eval      firstErrCk = *off
     C                   endif
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************

     C                   endsr
     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr
     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr
     *================================================================
      * KJDO - Search                                                  *
     *================================================================
     C     kjdo          begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C                   when      hfld = 'SCARCD'
     C                   exsr      srchARCode
     C                   eval      *in90 = *on
     C                   when      hfld = 'SCFEIN' or
     C                             hfld = 'SCMOD'
     C                   exsr      srchFein
     C                   eval      *in90 = *on
     C*** beg add ***** 04/27/11 ******************************************
     C                   when      hfld = 'SCRTRN' or
     C                             hfld = 'SCRTRN_A'
     C                   eval      *in82 = *on
     C                   eval      *in90 = *on

     C                   when      hfld = 'SCPDTO'
     C                   eval      *in84 = *on
     C                   eval      *in90 = *on

     C                   when      hfld = 'SCCOM1'
     C                   eval      *in89 = *on
     C                   eval      *in90 = *on

     C                   when      hfld = 'SCSELC' or hfld = 'SCSELC_A'
     C                   eval      *in88 = *on
     C                   eval      *in90 = *on

     C                   when      hfld = *BLANK
     C                   eval      *in90 = *on
     C*** END add ***** 04/27/11 ******************************************
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
      *
      *  Group Search
      *
     C  n90              if        hfld = 'SCFND'
     C***  Begin Add  ***  11/05/04  **************************************
     C                             or hfld = 'SCFND_A'
     C***  End   Add  ***  11/05/04  **************************************
     C                   call      'W0010R'      fnpl

     C                   if        #1FND <> *blank
     C                   move(p)   #1FND         scfnd
     C                   seton                                        80  90
     C     fnky          chain     pfn01
     C                   eval      scfndn = FNNAME
     C                   else
     C                   seton                                        90
     C                   endif
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      scfnd_a  = scfnd
     C                   eval      scfndn_a = scfndn
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   endif                                                  hfld = scfnd
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      scfnd  = scfnd_a
     C                   eval      scemp# = scemp#_a
     C                   eval      scdiv  = scdiv_a
     C                   eval      scfyr  = scfyr_a
     C***  End   Add  ***  11/05/04  **************************************
      *
     C  n90              dou       scfyr <> 0
      *
     C                   if        hfld = 'SCEMP#'
     C***  Begin Add  ***  11/05/04  **************************************
     C                             or hfld = 'SCEMP#_A'
     C***  End   Add  ***  11/05/04  **************************************
     C*** beg add ***** 10/22/08 ******************************************
     C                             or hfld = 'SCCUST'
     C*** end add ***** 10/22/08 ******************************************
     C*** beg add ***** 04/27/11 ******************************************
     C                             or hfld = 'SCDIV'
     C                             or hfld = 'SCDIV_A'
     C*** end add ***** 04/27/11 ******************************************
      *
     C     fnky          chain     pfn01
     C                   call      'W0050R'
     C                   parm      *blank        ##kc              1
     C                   parm      *blank        ##kl              1
     C                   parm      '2'           ##call            1
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         parm                    ##rrn             9 0
     C                   parm      *zeros        ##rrn             9 0
     C***  End   Chg  ***  11/05/04  **************************************
     C                   parm      fnrrn         ##rrn2            9 0
     C                   parm                    ##qte             1
     C                   parm                    ##view            1 0
     C                   parm                    ##rrn3            9 0
      *
     C                   if        ##rrn <> *zero
     C     ##rrn         chain     pem02
     C                   eval      scfnd  = EMFND
     C                   eval      scemp# = EMEMP#
     C                   eval      scdiv  = EMDIV
     C*** beg add ***** 10/22/08 ******************************************
     C                   eval      sccust = EMCUST
     C*** end add ***** 10/22/08 ******************************************
     C     fnky          chain     pfn01
     C                   eval      scfndn = FNNAME
     C                   else
     C                   seton                                        80  90
     C                   leave
     C                   endif                                                  ##rrn <> 0
      *
      *
     C                   if        scfyr = *zero
     C                   call      'W0051R'
     C                   parm      *blank        ##kc
     C                   parm      *blank        ##kl
     C                   parm      '2'           ##call
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         parm                    #1rrn             9 0
     C                   parm      *zeros        #1rrn             9 0
     C***  End   Chg  ***  11/05/04  **************************************
     C                   parm      ##rrn         ##rrn2            9 0
     C                   parm                    ##qte
     C
     C                   if        #1rrn <> *zero
     C     #1rrn         chain     pel02
     C                   eval      scfyr = elfyr
     C                   else
     C                   seton                                        81  90
     C                   leave
     C                   endif                                                  #1rrn <> 0
      *
     C                   endif                                                  scfyr = 0
      *
     C                   exsr      getagt
     C***  Begin Add  ***  11/05/04  **************************************
     C                   if        agencyBill
     C                   seton                                        89  90
     C                   else
     C***  End   Add  ***  11/05/04  **************************************
     C                   seton                                        82  90
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endif
     C***  End   Add  ***  11/05/04  **************************************
     C                   endif                                                  hfld = scemp
      *
     C  n90              if        hfld = 'SCFYR'
     C***  Begin Add  ***  11/05/04  **************************************
     C                             or hfld = 'SCFYR_A'
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   if        scemp# = *zero
      *
     C     fnky          chain     pfn01
     C                   call      'W0050R'
     C                   parm      *blank        ##kc              1
     C                   parm      *blank        ##kl              1
     C                   parm      '2'           ##call            1
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         parm                    ##rrn             9 0
     C                   parm      *zeros        ##rrn             9 0
     C***  End   Chg  ***  11/05/04  **************************************
     C                   parm      fnrrn         ##rrn2            9 0
     C                   parm                    ##qte             1
     C                   parm                    ##view            1 0
     C                   parm                    ##rrn3            9 0
      *
     C                   if        ##rrn <> *zero
     C     ##rrn         chain     pem02
     C                   eval      scfnd  = EMFND
     C                   eval      scemp# = EMEMP#
     C                   eval      scdiv  = EMDIV
     C*** beg add ***** 10/22/08 ******************************************
     C                   eval      sccust = EMCUST
     C*** beg add ***** 10/22/08 ******************************************
     C     fnky          chain     pfn01
     C                   eval      scfndn = FNNAME
     C                   else
     C                   seton                                        80  90
     C                   leave
     C                   endif                                                  ##rrn <> 0
      *
     C                   endif                                                  scemp# = 0
      *
     C     emky          chain     pem01
     C  n90              call      'W0051R'
     C                   parm      *blank        ##kc
     C                   parm      *blank        ##kl
     C                   parm      '2'           ##call
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         parm                    #1rrn             9 0
     C                   parm      *zeros        #1rrn             9 0
     C***  End   Chg  ***  11/05/04  **************************************
     C                   parm      emrrn         ##rrn2            9 0
     C                   parm                    ##qte
     C
     C  n90              if        #1rrn <> *zero
     C     #1rrn         chain     pel02
     C                   eval      scfyr = elfyr
     C***  Begin Add  ***  11/05/04  **************************************
     C                   if        agencyBill
     C                   seton                                        89  90
     C                   else
     C***  End   Add  ***  11/05/04  **************************************
     C                   seton                                        82  90
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endif
     C***  End   Add  ***  11/05/04  **************************************
     C                   else
     C                   seton                                        81  90
     C                   leave
     C                   endif                                                  #1rrn <> 0
      *
     C                   exsr      getagt
     C                   endif                                                  hfld = scfyr
      *
     C                   enddo                                                  scfyr <> 0
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      scfnd_a  = scfnd
     C                   eval      scemp#_a = scemp#
     C                   eval      scdiv_a  = scdiv
     C                   eval      scfyr_a  = scfyr
     C                   eval      scfndn_a = scfndn
     C                   eval      scempn_a = scempn
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
      *
     C                   endsr
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

     C                   eval      scfnd  = RTFND
     C                   eval      scrtrn = RTAMT

     C     fnky          chain     pfn01
     C                   if        %found
     C                   eval      scfndn = FNNAME
     C                   else
     C                   eval      scfndn = *blanks
     C                   endif

     C                   eval      scemp# = RTEMP#
     C                   eval      scdiv  = RTDIV
     C                   eval      scempn = RTEMPN
     C                   eval      scfyr  = RTFYR
     C                   eval      scrtrn = RTAMT
     C                   eval      svamt  = RTAMT
     C                   eval      scagt# = RTAGT#
     C                   eval      scagtn = RTAGTN
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      scFein = RTFEIN
     C                   eval      scMod  = RTMOD
     C                   eval      scAgtName = RTAGTN
     C                   eval      scArCd = RTARCD
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      scpf   = RTPFIN
      *
     C     pfky          chain     ppf01
     C                   if        %found
     C                   eval      scpfnm = PFNAME
     C                   else
     C                   eval      scpfnm = *blanks
     C                   endif
      *
      *** beg chg ***** 07/18/07 ******************************************
     C***********        eval      scpdto = RTPTFL
     C                   if        *in44 = *off
     C                   eval      scpdto = RTPTFL
     C**** BEGIN ADD **** 04/11/11 ************************************
     C                   eval      svpdto = RTPTFL
     C**** END   ADD **** 04/11/11 ************************************
     C                   endif
      *** end chg ***** 07/18/07 ******************************************
     C***  BEG ADD ******  12/26/07  **************************************
     C                   EVAL      SCCUST = RTCUST
     C***  END ADD ******  12/26/07  **************************************
     C                   eval      scselc = RTSELC
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   if        addMode
     C                   eval      scselc = 'Y'
     C                   endif
     C*
     C                   eval      scfnd_a  = scfnd
     C                   eval      scemp#_a = scemp#
     C                   eval      scdiv_a  = scdiv
     C                   eval      scfyr_a  = scfyr
     C                   eval      scfndn_a = scfndn
     C                   eval      scempn_a = scempn
     C                   eval      scselc_a = scselc
     C                   eval      scrtrn_a = scrtrn
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   eval      sccafl = RTCAFL
      *
     C                   if        not *in40
     C                   eval      sccom1 = RTCOM1
     C                   eval      sccom2 = RTCOM2
     C                   eval      sccom3 = RTCOM3
     C                   else
     C                   eval      sccom1 = svcom1
     C                   eval      sccom2 = svcom2
     C                   eval      sccom3 = svcom3
     C                   endif
      *
     C                   endsr
     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr
      *
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   if        agencyBill
     C                   eval      scfnd  = scfnd_a
     C                   eval      scemp# = scemp#_a
     C                   eval      scdiv  = scdiv_a
     C                   eval      scfyr  = scfyr_a
     C                   eval      scfndn = scfndn_a
     C                   eval      scempn = scempn_a
     C                   eval      scselc = scselc_a
     C                   eval      scrtrn = scrtrn_a
     C                   endif
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   eval      RTCO#  = ##co#
     C                   eval      RTBAT# = scbat#
     C                   eval      RTFND  = scfnd
     C                   eval      RTEMP# = scemp#
     C                   eval      RTDIV  = scdiv
     C                   eval      RTEMPN = scempn
     C                   eval      RTAGT# = scagt#
     C                   eval      RTAGTN = scagtn
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********         eval      RTFEIN = 0
     C**********         eval      RTMOD  = 0
     C                   eval      RTFEIN = scFein
     C                   eval      RTMOD  = scMod
     C                   if        agencyBill
     C                   eval      RTAGTN = scAgtName
      *** BEG ADD ***** 07/12/05 ********************************************
     C                   eval      RTAGT# = GetAgent#(RTCO#:RTFEIN:RTMOD:RTFND)
      *** END ADD ***** 07/12/05 ********************************************
     C                   endif
      *** BEG DEL ***** 07/12/05 ********************************************
     C*****************  eval      RTAGT# = GetAgent#(RTCO#:RTFEIN:RTMOD:RTFND)
      *** END DEL ***** 07/12/05 ********************************************
     C                   eval      RTARCD = scarcd
     C***  End   Chg  ***  11/05/04  **************************************
     C                   eval      RTFYR  = scfyr
     C                   eval      RTCAFL = sccafl
     C                   eval      RTCOM1 = sccom1
     C                   eval      RTCOM2 = sccom2
     C                   eval      RTCOM3 = sccom3
     C                   eval      RTAMT  = scrtrn
     C                   eval      RTSELC = scselc
     C***  Begin Add  ***  11/05/04  **************************************
     C                   select
     C                   when      agencyBill
     C                   eval      RTACFL = 'Y'
     C                   other
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      RTACFL = 'N'
     C***  Begin Add  ***  11/05/04  **************************************
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      RTPTFL = scpdto
     C                   eval      RTPFIN = scpf
     C***  BEG ADD ******  12/26/07  **************************************
     C                   EVAL      RTCUST = SCCUST
     C***  END ADD ******  12/26/07  **************************************
      *
     C                   endsr
     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        Begsr
     C                   Endsr
     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C     *inkj         caseq     *on           kjdo
     C                   endcs
     C                   endsr
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      addMode = @@mode = 'A'
     C                   eval      *in40 = addMode
     C*
     C                   eval      chgMode = @@mode = 'C'
     C*
     C                   eval      dspMode = @@mode = 'D'
     C                   eval      *in09 = dspMode
     C*
      *** BEG DEL ***** 07/18/07 ******************************************
     C*****************  eval      agencyBill = @@mode2 = 'A'
     C*****************  eval      *in98 = agencyBill
      *** END DEL ***** 07/18/07 ******************************************
     C***  End   Add  ***  11/05/04  **************************************
      *
      *  Change mode
      *
     C                   if        @@mode = 'S'
     C                   exsr      chgsts
     C                   seton                                        lr
     C                   return
     C                   endif

     C     @@mode        comp      'A'                                    40
      *
      *  Load header
      *
     C     @@rrn2        chain     prn01
     C*** beg add ***** 07/18/07 ******************************************
     c                   if        RNMIN  <> 0
     c                   eval      *in41 = *on
     c                   endif
     c                   if        RNTGRP <> 0
     c                   eval      *in42 = *on
     c                   endif
     c                   if        RNTAUD <> 0
     c                   eval      *in43 = *on
     c                   endif

     c                   eval      scbtyp = RNBTYP
     c                   select
     c                   when      scbtyp = 'A'
     c                   eval      scbdsc = 'Account Current'
     c                   eval      agencyBill = *on
     c                   eval      *in98 = *on
     c                   when      scbtyp = 'D'
     c                   eval      scbdsc = 'Deposit'
     c                   when      scbtyp = 'I'
     c                   eval      scbdsc = 'Independent Cash Distribution'
     c                   eval      scpdto = RNPTYP
     c                   eval      *in44 = *on
     c                   if        RNPTYP = 'A'
     c                   eval      agencyBill = *on
     c                   eval      *in98 = *on
     c                   endif
     c                   when      scbtyp = 'P'
     c                   eval      scbdsc = 'Premium'
     c                   endsl
     C*** end add ***** 07/18/07 ******************************************
     C                   eval      scbat#  = RNBAT#
     C                   eval      scfgrp  = RNFGRP
     C                   eval      sctgrp  = RNTGRP
     C                   eval      scmin   = RNMIN
     C                   eval      srdti   = RNFAUD
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         scfaud
     C                   eval      srdti   = RNTAUD
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         sctaud
     C                   eval      sccom1  = RNCOM1
     C                   eval      sccom2  = RNCOM2
     C                   eval      sccom3  = RNCOM3
     C                   eval      svcom1  = RNCOM1
     C                   eval      svcom2  = RNCOM2
     C                   eval      svcom3  = RNCOM3
      *
     C*** BEG ADD ***** 12/26/07 ******************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     @@rrn         chain(n)  dbrcdf
     C                   eval      scsdfl = rtsdfl
      *
      * Reflect the new changes only if the Batch type <> 'A' and it is payable to Employer
      *
     C                   IF        CLTID = '081' AND SCBTYP = 'I'
     C                             AND RTPTFL = 'E'
     C                   EVAL      *IN31 = *ON
     C                   ENDIF                                                  CLTID EQ '081'
      *
     C                   IF        SCBTYP <> 'A'
     C                   EVAL      *IN97 = *ON
     C                   ENDIF                                                  SCBTYP <> 'A'
      *
     C                   IF        SCSDFL = 'Y'
     C                   EVAL      *IN09 = *ON
     C                   ENDIF                                                  SCSDFL = 'Y'
      *
     C*** END ADD ***** 12/26/07 ******************************************
     C                   endsr
     *================================================================
     * CHGSTS - UPDATE WXRTP WITH NEW STATUS FLAG
     *================================================================
     C     chgsts        Begsr
      *
     C     @@rrn         chain     dbrcdf
      *
     C                   if        %found
      *
     C                   if        RTSELC = 'Y'
     C                   eval      RTSELC = 'N'
     C                   else
     C                   eval      RTSELC = 'Y'
     C                   endif
      *
     C                   eval      RTUSER = Q1user
     C                   eval      RTPGM  = Q1pgm
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         RTDATE
     C                   eval      RTTIME = dstime
     C                   update    dbrcdf
      *
E01  C                   endif
      *
     C                   endsr
     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr
     C***  Begin Add  ***  11/05/04  **************************************
     *================================================================
     * srchARCode - Search A/R codes
     *================================================================
     C     srchARCode    begsr
     C*
     C                   eval      key = ARCodeSrch(*inkc:*inkl)
     C*
     C                   select
     C                   when      key <> *blanks and addMode
     C                   eval      scArCd = key
     C                   eval      *in88  = *on
     C                   when      key <> *blanks and chgMode
     C                   eval      scArCd = key
     C                   eval      *in82  = *on
     C                   other
     C                   eval      *in87  = *on
     C                   endsl
     C*____________________________________________________________________
    C*
     C                   endsr
     *================================================================
     * srchFein - Search Agent Fein
     *================================================================
     C     srchFein      begsr
     C*
     C                   eval      AG_rrn = AgtSrchAG(*inkc:*inkl)
     C*
     C                   select
     C                   when      AG_rrn <> *zero
     C     AG_rrn        chain     WMAGP
     C                   eval      scFein = AGFEIN
     C                   eval      scMod  = AGMOD
     C                   eval      scAgtName = AGNAME
     C                   eval      *in82  = *on
     C                   other
     C                   eval      *in86  = *on
     C                   endsl
     C*
     C                   endsr
     C***  End   Add  ***  11/05/04  **************************************
     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     Updatr        Begsr

     C     @@rrn         chain     dbrcdf

B01  C                   if        %found
     C                   exsr      mvscdb
     C                   eval      RTUSER = Q1USER
     C                   eval      RTPGM  = Q1PGM
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         RTDATE
     C                   eval      RTTIME = dstime
     C                   update    dbrcdf
E01  C                   endif

     C                   endsr
     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
      *
      * reset flags if user wants to add another record
      *
     C                   eval      firstErrCk = *on
     C                   eval      autoFill   = *off
     C                   exsr      mvscdb
     C                   eval      RTIPGM = Q1PGM
     C                   eval      RTPGM  = Q1PGM
     C                   eval      RTIUSR = Q1USER
     C                   eval      RTUSER = Q1USER
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         RTIDT
     C                   movel     srdto         RTDATE
     C                   eval      RTITIM = dstime
     C                   eval      RTTIME = dstime
      *** beg add ***** 07/18/07 ******************************************************
     C                   eval      RTBTYP = scbtyp
      *** end add ***** 07/18/07 ******************************************************
     C                   write     dbrcdf
     C                   feod      WXRTP
      *
      * reset flags if user wants to add another record
      *
     C                   eval      firstErrCk = *on
     C                   eval      autoFill   = *off
      *
     C                   endsr
     *==================================================================================
      * getBalance - Calculate A/R balance (A/R balance -  pending return premium checks)
     *==================================================================================
     PgetBalance       b


     D getBalance      pi                  like(*in90)
     D balance         s             11  2

     C     eaky          klist
      *** beg chg ***** 06/21/04 **********************************************
     C*****************  kfld                    RTCO#
     C*****************  kfld                    RTFND
     C*****************  kfld                    RTEMP#
     C*****************  kfld                    RTDIV
     C*****************  kfld                    RTFYR
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scemp#
     C                   kfld                    scdiv
     C                   kfld                    scfyr
      *** end chg ***** 06/21/04 **********************************************
      *
      * Get balance from A/R
      *
     C                   eval      balance = 0

     C     eaky          setll     pea01
     C     eaky          reade     pea01
     C                   dow       not %eof
      *
     C                   select
     C                   when      EAARTY = 'A' or EAARTY = 'B' or
     C                             EAARTY = 'D' or EAARTY = 'N' or
     C                             EAARTY = 'R' or EAARTY = 'V'
     C                   add       eaamt         balance
     C                   when      EAARTY = 'P' or EAARTY = 'T'
     C                   sub       eaamt         balance
     C                   endsl

     C     eaky          reade     pea01
     C                   enddo

      *
      * Add other batch records to balance
      *
     C     eaky          setll     prt01
     C     eaky          reade     prt01
     C                   dow       not %eof

     C                   if        l5_rtbat# <> rtbat#
      *** beg chg ***** 07/14/05 ***********************************************
     C*****************  sub       l5_rtamt      balance
     C                   add       l5_rtamt      balance
      *** end chg ***** 07/14/05 ***********************************************
     C                   endif

     C     eaky          reade     prt01
     C                   enddo
      *
      * Add unissued check request records from balance.
      *
     C     eaky          setll     pr401
     C     eaky          reade     pr401
     C                   dow       not %eof

     C                   if        R4STAT <> 'Y'
      *** beg chg ***** 07/14/05 ***********************************************
     C*****************  sub       R4AMT         balance
     C                   add       R4AMT         balance
      *** end chg ***** 07/14/05 ***********************************************
     C                   endif

     C     eaky          reade     pr401
     C                   enddo

      *** beg chg ***** 06/21/04 ***********************************************
     C*****************  if        -(balance) - scrtrn  < 0
      *** beg chg ***** 07/14/05 ***********************************************
     C*****************  if        balance - scrtrn  < 0
     C                   if        balance + scrtrn  > 0
      *** end chg ***** 07/14/05 ***********************************************
      *** end chg ***** 06/21/04 ***********************************************
     C                   seton                                        75
     C                   return    *on
     C                   endif

     C                   return    *off

     PgetBalance       e





     D***  Begin Add  ***  11/05/04  **************************************
     *====================================================================
      * getBalAB - Calculate Agency Billing bal (bal - pend ret prem chks)
     *====================================================================
     PgetBalAB         b


     D getBalAB        pi                  like(*in90)

     D balance         s             11  2

     C     keyWDABL11    klist
     C                   kfld                    ##co#
     C                   kfld                    scFein
     C                   kfld                    scMod

     C                   eval      balance = *zero
      *
      * Get balance from Agency Billing
      *

     C     keyWDABL11    setll     pab11
     C     keyWDABL11    reade     pab11
     C*
     C                   dow       not %eof
      *
     C                   select
     C                   when      ABARTY = 'A' or ABARTY = 'B' or
     C                             ABARTY = 'D' or ABARTY = 'N' or
     C                             ABARTY = 'R' or ABARTY = 'V'
      *** beg chg ***** 01/09/24 **********************************************
     C*****              eval      balance = balance + eaamt
     C                   eval      balance = balance + ABamt
      *** end chg ***** 01/09/24 **********************************************
     C                   when      ABARTY = 'P' or ABARTY = 'T'
      *** beg chg ***** 01/09/24 **********************************************
     C*****              eval      balance = balance - eaamt
     C                   eval      balance = balance - ABamt
      *** end chg ***** 01/09/24 **********************************************
     C                   endsl

     C     keyWDABL11    reade     pab11
     C*
     C                   enddo
      *
      * Add  other batch records to balance
      *
     C     keyWDABL11    setll     prt06
     C     keyWDABL11    reade     prt06
     C*
     C                   dow       not %eof

     C                   if        l6_RTBAT# <> rtbat#
      *** beg chg ***** 07/14/05 **********************************************
     C*****************  eval      balance = balance - l6_RTAMT
     C                   eval      balance = balance + l6_RTAMT
      *** end chg ***** 07/14/05 **********************************************
     C                   endif

     C     keyWDABL11    reade     prt06

     C                   enddo
      *
      * Add unissued check request records to balance.
      *
     C     keyWDABL11    setll     pr410
     C     keyWDABL11    reade     pr410
     C*
     C                   dow       not %eof

     C                   if        R4STAT <> 'Y'
      *** beg chg ***** 07/14/05 **********************************************
     C*****************  eval      balance = balance - R4AMT
     C                   eval      balance = balance + R4AMT
      *** beg chg ***** 07/14/05 **********************************************
     C                   endif

     C     keyWDABL11    reade     pr410

     C                   enddo

      *** beg chg ***** 07/14/05 **********************************************
     C*****************  eval      *in75 = (balance - scrtrn  < 0)
      *** beg chg ***** 01/09/24 **********************************************
     C*****              eval      *in75 = (balance + scrtrn  > 0)
     C                   eval      *in75 = (balance + scrtrn_a > 0)
      *** end chg ***** 01/09/24 **********************************************
      *** end chg ***** 07/14/05 **********************************************
     C                   return    *in75

     PgetBalAB         e
     D***  End   Add  ***  11/05/04  **************************************
