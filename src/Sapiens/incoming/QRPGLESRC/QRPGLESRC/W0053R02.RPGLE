     Hcopyright('(c) Copyright Fiserv, Inc., 2004 - 2008')
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H optimize(*none)
      /title 'Vendor History update/display'
      *=========================================================================
      *  DESCRIPTION:  Update/display Vendor History
      *  CALLED FROM: W0053R
      *
      *  CREATED: 01/09/2004  Matt Dawson  Project: WC1115
      *=========================================================================
      *  REVISIONS:
     *
     * 04/23/08 C:136 SUPPORT: WA129 63086                                              *
     *                Edit the effective date in add mode only as it is not editable    *
     *                in change mode.                                                   *
     *                                                                                  *
     * 04/07/08 C:136 Support: WA139 62859
     *                Edit to ensure vendor master record exists
     *
      * 10/10/07  C:190 Project WC1454(Priss# 12)
      *                 While updating to the Vendor History Expiration
     *                 date across all divisions, there is a program error
      *                 in that the wrong Expiration Date is being updated.
      *
      * 07/27/07  C:190 Project WC1454
      *                 Add a new popup window to appear(conditionally)
      *                 after the final screen is passed.
      *
     * 09/27/06 C:136 Support: WA135 54884
     *                Allow addition of date prior to existing records if recor
     *                expired on the same day it was created.
     *
      * 06/24/06 C: 132 PROJECT: WA099 - 53249
      *                - Change *DATE to use The Current Date from DSDTTM
     *
      * 03/08/06 C: 132 PROJECT: WC071 - 51160
      *                - Change *DATE to use The Current Date from DSDTTM
      *                  Data Structure DSDATE.
      *
      *  06/17/04 C:136 Support: WC1115
      *                 Fill in vendor name if selection is made from search.
      *
      *  06/04/04 C:136 Support: WA129 42766
      *                 Add vendor name to subfile
      *
      *=========================================================================
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0053f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdvhp     uf a e             disk
     F                                     rename(pvh01:dbrcdf)
      *** beg add ****** 09/27/06 **********************************************
     F                                     infds(dbrfds)
      *** end add ****** 09/27/06 **********************************************
     Fwdelp     if   e             disk
     Fwmfnl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwzexl     if   e           k disk
     Fwmvpl     if   e           k disk
      ***** beg add ***** 07/27/07 ***********************************************
     Fwmexl     if   e           k disk    rename(pex01:pex02)
     Fwdell5    if   e           k disk    rename(pel01:pel05)
      ***** end add ***** 07/27/07 ***********************************************
      ***** beg add ***** 10/10/07 ***********************************************
     FWDVHLQ    uf   e           k disk    prefix(l2_)
     F                                     rename(pvh01:pvh02)
      ***** end add ***** 10/10/07 ***********************************************
     Fwmvpp     if   e             disk    rename(pvp01:pvp01p)
     Fwdvhl1    uf   e           k disk    prefix(l1_)
     F                                     infds(vhinfds)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Vendor History'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##emp#          s                   like(ememp#)
     D ##div           s                   like(emdiv)
     D ##name          s                   like(emname)
     D ##edn           s             80
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D kkefdt          s              7  0
      ***** beg add ***** 07/27/07 ***********************************************
     D svexdt          s              7  0
      ***** end add ***** 07/27/07 ***********************************************
      *** beg add ***** 09/27/06 ***********************************************
     D xxexdt          s              7  0
      *** end add ***** 09/27/06 ***********************************************
     D sys_date        s               d   datfmt(*usa)
     D policyStrDt     s               d
     D policyEndDt     s               d
     D vendorEffDt     s               d
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
     D vhinfds         ds
     D  vhrrn                397    400b 0
     D dbrfds          ds
     D  pfrrn                397    400b 0
    C*
     c/copy sprstring
     c/copy sprdate
     c/copy scopymt41C
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
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     c     emky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
      *
     c     emky5         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    @@fyr
     C                   kfld                    vhvdr#
     C                   kfld                    vhefdt

      ***** beg add ***** 07/27/07 ***********************************************
     c     emky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    @@div

     c     emky2         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
      ***** end add ***** 07/27/07 ***********************************************

     c     fnky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd

     c     vhky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr

     c     vhky1         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     C                   kfld                    kkefdt

      ***** beg add ***** 07/27/07 ***********************************************
     c     vhky2         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    @@emp#

     c     vhky3         klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     C                   kfld                    @@emp#
     C                   kfld                    @@div
     C                   kfld                    @@fyr
     C                   kfld                    vhvdr#
     C                   kfld                    vhefdt
      ***** end add ***** 07/27/07 ***********************************************

     c     vpky          klist
     C                   kfld                    elco#
     C                   kfld                    scvdr#

    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
      *
      *  Validate vendor number
      *
      *** beg add ***** 09/27/06 ***********************************************
      *** beg del ***** 04/07/08 ***********************************************
     c***********        if        not *in40
      *** end del ***** 04/07/08 ***********************************************
      *** end add ***** 09/27/06 ***********************************************
     c     vpky          chain     pvp01
     c                   if        not %found
     c                   seton                                        71  90
      *** beg add ***** 06/04/04 ***********************************************
     c                   eval      scvnam = *blanks
     c                   else
     c                   eval      scvnam = vpname
      *** end add ***** 06/04/04 ***********************************************
     c                   endif
      *** beg add ***** 09/27/06 ***********************************************
      *** beg del ***** 04/07/08 ***********************************************
     c***********        endif
      *** end del ***** 04/07/08 ***********************************************
      *** end add ***** 09/27/06 ***********************************************
      *
      *  Validate effective date
      *
      *** beg add ***** 04/23/08 *******************************************************
     c                   if        @@mode = 'A'
      *** end add ***** 04/23/08 *******************************************************
      *
     c                   if        not *in90
     c                   eval      scefdt = dtCheck(scefdt)
     c                   if        scefdt = *zero
     c                   seton                                        72  90
     c                   endif
     c                   endif
      *
      *  Ensure new effective date doesn't overlap other dates.
      *
      *** beg del ***** 09/27/06 ***********************************************
     c***********        if        not *in90
     c***********                  and *in40 = *on
      *** end del ***** 09/27/06 ***********************************************

     c                   eval      kkefdt = dtCYMD(scefdt)
      *
      *  Ensure effective date falls within policy range
      *
     c                   if        not *in90

     c                   if        kkefdt <  elpfdt or
     c                             kkefdt >= elptdt
     c                   seton                                        74  90
     c                   endif

     c                   endif
      *
      *** beg add ***** 04/23/08 *******************************************************
     c                   endif                                                  @@mode = 'A'
      *** end add ***** 04/23/08 *******************************************************
      *
      *  Validate expiration date
      *
     c                   if        not *in90 and scexdt <> 0
     c                   eval      scexdt = dtCheck(scexdt)
     c                   if        scexdt = *zero
     c                   seton                                        75  90
     c                   endif
     c                   endif

      *
      *  Ensure expiration date is greater or equal to effective date
      *
     c                   if        not *in90 and scexdt <> 0
     c                   eval      vhexdt = dtCymd(scexdt)
     c                   eval      vhefdt = dtCymd(scefdt)
     c                   if        vhexdt < vhefdt
     c                   seton                                        76  90
     c                   endif
     c                   endif
      *
      *  Ensure expiration date falls within policy range
      *
     c                   if        not *in90 and scexdt <> 0
      *** beg add ***** 04/23/08 *******************************************************
     c                             and vhefdt <> vhexdt
      *** end add ***** 04/23/08 *******************************************************

     c                   if        vhexdt <  elpfdt or
     c                             vhexdt >= elptdt
     c                   seton                                        77  90
     c                   endif

     c                   endif

     c                   eval      xxexdt = dtCYMD(scexdt)
      *** beg add ***** 09/27/06 ***********************************************
     c                   if        not *in90
      *** end add ***** 09/27/06 ***********************************************

      *** beg add ***** 04/23/08 *******************************************************
     c                   eval      kkefdt = dtCYMD(scefdt)
      *** end add ***** 04/23/08 *******************************************************
     c     vhky1         setgt     pvh01
     c     vhky          reade(N)  pvh01
      *** beg chg ***** 09/27/06 ***********************************************
     c***********        if        not %eof and @@mode = 'A' or
     c***********                  not %eof and @@mode = 'C' and
     c***********                  @@rrn <> vhrrn and kkefdt >= l1_vhefdt

     c                   dow       not(%eof)
     c                   if        l1_vhefdt <> l1_vhexdt
     c                   leave
     c                   endif
     c     vhky          reade(N)  pvh01
     c                   enddo

     c   40              if        not %eof and kkefdt < l1_vhexdt
      *** end chg ***** 09/27/06 ***********************************************
     c                   seton                                        73  90
     c                   endif

      *** beg add ***** 09/27/06 ***********************************************
     c  n90              if        not %eof and
     c                             (xxexdt > l1_vhefdt or xxexdt = 0)
     c                   seton                                        79  90
     c                   endif
      *** end add ***** 09/27/06 ***********************************************

     c                   if        not *in90

      *** beg chg ***** 09/27/06 ***********************************************
     c*****vhky1*        setll     pvh01
     c*****vhky**        readpe(N) pvh01
     c***********        if        not %eof
     c***********        if        kkefdt < l1_vhefdt or
     c***********                  kkefdt < l1_vhexdt
     c***********        seton                                        73  90
     c***********        endif
     c***********        endif

     c     vhky1         setgt     pvh01
     c     vhky          readpe(N) pvh01
     c                   dow       not(%eof)
     c                   select
     c                   when      @@mode = 'A'
     c                   if        l1_vhefdt <> l1_vhexdt
     c                   leave
     c                   endif
     c                   when      @@mode = 'C'
     c                   if        l1_vhefdt <> l1_vhexdt and
     c                             pfrrn <> vhrrn
     c                   leave
     c                   endif
     c                   endsl
     c     vhky          readpe(N) pvh01
     c                   enddo

     c                   if        not %eof and
     c                             kkefdt < l1_vhexdt and
     c                             @@mode = 'A'
     c                   seton                                        73  90
     c                   endif

     c  n90              if        not %eof         and
     c                             @@mode = 'C'     and
     c                             xxexdt <> vhexdt and
     c                             (l1_vhexdt > kkefdt or l1_vhexdt = 0)  and
     c                             l1_vhefdt <> l1_vhexdt
     c                   seton                                        79  90
     c                   endif
      *** end chg ***** 09/27/06 ***********************************************

     c                   endif

     c                   endif
      ***** beg add ***** 07/27/07 ***********************************************
     C                   if        #ofscr = 1 and *in90 = *off
     C                   exsr      srupda
     C                   endif
      ***** end add ***** 07/27/07 ***********************************************
     C                   endsr                                                  errck1
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
      ***** beg add ***** 07/27/07 ***********************************************
     C                   if        #ofscr = 2 and *in90 = *off
     C                   exsr      srupda
     C                   endif
      ***** end add ***** 07/27/07 ***********************************************
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
      ***** beg add ***** 07/27/07 ***********************************************
     C                   if        #ofscr = 3 and *in90 = *off
     C                   exsr      srupda
     C                   endif
      ***** end add ***** 07/27/07 ***********************************************
     C                   endsr
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
     C     kjdo          begsr

     C                   select
     C                   when      hfld = 'SCVDR#'
     c                   call      'W0003R'
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm      '2'           qqcall            1
     c                   parm      *zero         qqrrn             9 0

     c                   if        *inkl <> *on and qqrrn <> *zero
     C     qqrrn         chain     pvp01p
     c                   eval      scvdr# = vpvdr#
      *** beg add ***** 06/17/04 ***********************************************
     c                   eval      scvnam = vpname
      *** edg add ***** 06/17/04 ***********************************************
     c                   seton                                        81  90
     C                   else
     c                   seton                                        80  90
     c                   endif

     C                   endsl

     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     c                   eval      scvdr# = vhvdr#
      *
      *** beg add ***** 06/04/04 ***********************************************
     c     vpky          chain     pvp01
     c                   if        not %found
     c                   eval      scvnam = *blanks
     c                   else
     c                   eval      scvnam = vpname
     c                   endif
      *** end add ***** 06/04/04 ***********************************************
      *
     c                   eval      scefdt = dtMDYY(vhefdt)
     c                   eval      scexdt = dtMDYY(vhexdt)
      ***** beg add ***** 07/27/07 ***********************************************
     C                   movel     vhexdt        svexdt
      ***** end add ***** 07/27/07 ***********************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     c                   eval      vhco#  = elco#
     c                   eval      vhgrp  = scgrp
     c                   eval      vhemp# = elemp#
     c                   eval      vhdiv  = eldiv
     c                   eval      vhfyr  = scfyr
     c                   eval      vhvdr# = scvdr#
     c                   eval      vhefdt = dtCYMD(scefdt)
     c                   eval      vhexdt = dtCYMD(scexdt)
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C     *inkj         caseq     *on           kjdo
     C                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
      ***** beg add ***** 07/27/07 ***********************************************
     c                   if        (@@mode = 'A' and vhefdt < elpfdt)
     c                   eval      vhefdt = elpfdt
     c                   endif
      ***** end add ***** 07/27/07 ***********************************************
     c                   seton                                        lr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C     @@rrn2        chain     wdelp
     C                   if        %found
     C                   eval      scgrp  = elfnd
     C                   eval      scfyr  = elfyr

     C     fnky          chain     wmfnl
     c                   if        %found
     c                   eval      scname = fnname
     c                   else
     c                   eval      scname = *blanks
     C                   endif

     C     emky          chain     wmeml
     c                   if        %found
     c                   call      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c     scedn         parm                    ##edn
     C                   endif

     C     emky          chain     wzexl                              63
     c                   if        not *in63
     c                   eval      scofnd = exofnd
     c                   eval      scoemp = exoemp
     c                   eval      scodiv = exodiv
     C                   endif

      ***** BEG ADD ***** 06/24/06 *********************************************
     C                   If        @@mode = 'A'
     C                   eval      *in40  = *ON
     C                   Else
     C                   eval      *in40 = *OFF
     C                   endIF
      ***** END ADD ***** 06/24/06 *********************************************

     C                   else
     C                   exsr      pgmend
     C                   endif

      ***** beg add ***** 07/27/07 ***********************************************
     C     fnky          chain     pex02
      ***** end add ***** 07/27/07 ***********************************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     c*** 07/27/07 ******* BEG ADD ********************************************
     c                   if        (scupda = 'N' or scupda = ' ')
     c******                       (*inkl = *off and *inkc = *off)
     c*** 07/27/07 ******* END ADD ********************************************
     c     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C*
      ***** BEG CHG ***** 03/08/06 *********************************************
     C                   TIME                    DSDTTM
     C     *usa          move      DSDATE        sys_date
      ***** END CHG ***** 03/08/06 *********************************************
     C     *cymd         move      sys_date      vhdt
     C                   time                    vhtime
     c                   eval      vhuser = q1user
     c                   eval      vhpgm  = q1pgm
     C                   update    dbrcdf
E01  C                   endif                                                  *in99 = *off
      ***** beg add ***** 07/27/07 ***********************************************

     c                   else                                                   scupda='N'

     c                   if        (*in20 = *on) and
     c                             (*inkl = *off and *inkc = *off)
     C*** update vendor history reocrds for each division
     c     emky2         chain     wmeml                              40
     c                   dow       *IN40=*OFF
     c                   clear                   @@divact
     c                   z-add     emdiv         eldiv
      ***** beg add ***** 10/10/07 ***********************************************
     c     vhky2         chain     pel05
     c                   z-add     elfyr         @@fyr             3 0
      ***** end add ***** 10/10/07 ***********************************************
     c*** determine if division is active
     c                   if        (emcad1 = 0 and emcad2 = 0) and
     c                             (emcad3 = 0 and emcad4 = 0 and emcad5 = 0)
     c                   move      'Y'           @@divact          1
     c                   else
     c                   move      'N'           @@divact          1
     c                   endif                                                  emcad1 <> 0
     c*** update record if division is active or update canceled div flag is 'Y'
     c                   if        @@divact = 'Y' or sccxfl = 'Y'
     c     emky5         chain     pvh02                              99
     C                   if        (*in99 = *off)
     c                   eval      l2_vhexdt = dtCYMD(scexdt)
      ***** beg add ***** 10/10/07 ***********************************************
     c                   if        (@@mode = 'A' and vhefdt < elpfdt)
     c                   eval      vhefdt = elpfdt
     c                   endif
      ***** end add ***** 10/10/07 ***********************************************
     c                   update    pvh02
     C                   endif                                                  *in99 = *off
     C                   endif                                                  @@divact = 'Y'
     c     emky2         reade     wmeml                                  40
     c                   enddo
     c                   endif
     c                   if        *in21 = *on
     C*** update vendor history reocrds for each policy
     c     fnky          chain     wmeml                              40
     c                   dow       *IN40=*OFF
     c                   clear                   @@polact
     c                   z-add     ememp#        @@emp#            9 0
     c                   z-add     emdiv         @@div             5 0
      ***** beg add ***** 10/10/07 ***********************************************
     c     vhky2         chain     pel05
     c                   z-add     elfyr         @@fyr             3 0
      ***** end add ***** 10/10/07 ***********************************************
     c*** determine if policy is active
     c                   if        (emcad1 = 0 and emcad2 = 0) and
     c                             (emcad3 = 0 and emcad4 = 0 and emcad5 = 0)
     c                   move      'Y'           @@polact          1
     c                   else
     c                   move      'N'           @@polact          1
     c                   endif                                                  emcad1 <> 0
     c*** update record if policy is active or update canceled div flag is 'Y'
     c                   if        @@polact = 'Y' or sccxfl = 'Y'
     c***  vhky3         chain     pvh01                              99
     c     vhky3         chain     pvh02                              99
     C                   if        (*in99 = *off)
     c***                eval      l1_vhexdt = dtCYMD(scexdt)
     c                   eval      l2_vhexdt = dtCYMD(scexdt)
     c                   move      @@emp#        l2_VHemp#
     c                   move      @@div         l2_vhdiv
      ***** beg add ***** 10/10/07 ***********************************************
     c                   if        (@@mode = 'A' and vhefdt < elpfdt)
     c                   eval      vhefdt = elpfdt
     c                   endif
      ***** end add ***** 10/10/07 ***********************************************
     c**                 update    pvh01
     c                   update    pvh02
     C                   endif                                                  *in99 = *off
     C                   endif                                                  @@divact = 'Y'
     c     fnky          reade     wmeml                                  40
     c                   enddo
     c                   endif
      ***** end add ***** 07/27/07 ***********************************************
     C                   endif                                                  scupda = 'N'
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
      ***** beg add ***** 07/27/07 ***********************************************
     c                   if        (scupda = 'N' or scupda = ' ') and
     c                             (*inkl = *off and *inkc = *off)
      ***** end add ***** 07/27/07 ***********************************************
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     c                   eval      vhuser = q1user
     c                   eval      vhiusr = q1user
     c                   eval      vhpgm  = q1pgm
     c                   eval      vhipgm = q1pgm
     C*
      ***** BEG CHG ***** 03/08/06 *********************************************
     C                   TIME                    DSDTTM
     C     *usa          move      DSDATE        sys_date
      ***** END CHG ***** 03/08/06 *********************************************
     C     *cymd         move      sys_date      vhdt
     c                   eval      vhidt  = vhdt
     C                   time                    vhtime
     c                   eval      vhitim = vhtime
     C                   write     dbrcdf
      ***** beg add ***** 07/27/07 ***********************************************
     c                   else                                                   scupda='N'
     c                   if        (scupda <> 'N' and *in20 = *on) and
     c                             (*inkl = *off and *inkc = *off)
     C*** write vendor history reocrds for each division
     c     emky2         chain     wmeml                              40
     c                   dow       *IN40=*OFF
     c                   clear                   @@divact
     c                   z-add     emdiv         @@div             5 0
     c**   vhky2         chain     pel05
     c**                 z-add     elfyr         @@fyr             3 0
     c*** determine if division is active
     c                   if        (emcad1 = 0 and emcad2 = 0) and
     c                             (emcad3 = 0 and emcad4 = 0 and emcad5 = 0)
     c                   move      'Y'           @@divact          1
     c                   else
     c                   move      'N'           @@divact          1
     c                   endif                                                  emcad1 <> 0
     c*** write record if division is active or update canceled div flag us 'Y'
     c                   if        @@divact = 'Y' or sccxfl = 'Y'
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     c                   move      @@div         VHDIV
     c****               move      @@fyr         vhfyr
     c                   eval      vhuser = q1user
     c                   eval      vhiusr = q1user
     c                   eval      vhpgm  = q1pgm
     c                   eval      vhipgm = q1pgm
     C***
     C                   TIME                    DSDTTM
     C     *usa          move      DSDATE        sys_date
     C     *cymd         move      sys_date      vhdt
     c                   eval      vhidt  = vhdt
     C                   time                    vhtime
     c                   eval      vhitim = vhtime
      ***** beg add ***** 10/10/07 ***********************************************
     c                   if        (@@mode = 'A' and vhefdt < elpfdt)
     c                   eval      vhefdt = elpfdt
     c                   endif
      ***** end add ***** 10/10/07 ***********************************************
     C                   write     dbrcdf
     c                   endif                                                  @@divact='Y'
     c     emky2         reade     wmeml                                  40
     c                   enddo
     c                   endif                                                  scupda='N'
     c***   Loop when changes to all policies and divisions
     c                   if        (scupda <> 'N' and *in21 = *on) and
     c                             (*inkl = *off and *inkc = *off)
     C***   write vendor history reocrds for each division
     c     fnky          chain     wmeml                              40
     c                   dow       *IN40=*OFF
     c                   clear                   @@polact
     c                   z-add     ememp#        @@emp#            9 0
     c     vhky2         chain     pel05
     c                   z-add     elfyr         @@fyr             3 0
     c*** determine if division is active
     c                   if        (emcad1 = 0 and emcad2 = 0) and
     c                             (emcad3 = 0 and emcad4 = 0 and emcad5 = 0)
     c                   move      'Y'           @@polact          1
     c                   else
     c                   move      'N'           @@polact          1
     c                   endif                                                  emcad1 <> 0
     c*** write record if division is active or update canceled div flag us 'Y'
     c                   if        @@polact = 'Y' or sccxfl = 'Y'
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     c                   move      @@emp#        VHemp#
     c                   move      emdiv         vhdiv
     c                   move      @@fyr         vhfyr
     c                   eval      vhuser = q1user
     c                   eval      vhiusr = q1user
     c                   eval      vhpgm  = q1pgm
     c                   eval      vhipgm = q1pgm
     C*
     C                   TIME                    DSDTTM
     C     *usa          move      DSDATE        sys_date
     C     *cymd         move      sys_date      vhdt
     c                   eval      vhidt  = vhdt
     C                   time                    vhtime
     c                   eval      vhitim = vhtime
      ***** beg add ***** 10/10/07 ***********************************************
     c                   if        (@@mode = 'A' and vhefdt < elpfdt)
     c                   eval      vhefdt = elpfdt
     c                   endif
      ***** end add ***** 10/10/07 ***********************************************
     C                   write     dbrcdf
     c                   endif                                                  @@divact='Y'
     c     fnky          reade     wmeml                                  40
     c                   enddo
     c******End of loop when changes to all policies and divisions
     c                   endif                                                  scupda<>'N'
     c                   endif                                                  scupda = 'N'
      ***** end add ***** 07/27/07 ***********************************************
      *
      * If adding an additional record to an employer update the expiration date
      * record if the expiration date is zero. Two readpe in case previous records'
      * effective date is equal which would make a setll and readpe miss the record.
      *
     C                   eval      kkefdt = vhefdt
      *** beg chg ***** 09/27/06 *******************************************************
     C*****vhky1         setll     pvh01
     C*****vhky          readpe    pvh01
     C*****vhky          readpe    pvh01
     C     vhky1         setgt     pvh01
     C     vhky          readpe    pvh01
     c                   dow       not(%eof)
     c                   if        l1_vhefdt <> l1_vhexdt and
     c                             pfrrn <> vhrrn
     c                   leave
     c                   endif
     c     vhky          readpe    pvh01
     c                   enddo
      *** end chg ***** 09/27/06 *******************************************************
     C                   if        not %eof and l1_vhexdt = 0
     C                   eval      l1_vhexdt = vhefdt
     C                   update    pvh01
     C                   endif

     C                   feod      wdvhp
     C                   endsr
      ***** beg add ***** 07/27/07 ***********************************************
    C*================================================================
    C* SRUPDA -
    C*================================================================
     c     srupda        begsr
     c*** display a window screen to accept input flags
     c                   if        (exupda = 'Y') and
     c                             (@@mode = 'A' or vhexdt <> svexdt)
      *
     c***set indicators to display correct prompt
     c                   if        fnpeof = 'M'
     c                   seton                                        21
     c                   else                                                   fnpeof='M'
     c                   seton                                        20
     c                   endif                                                  fnpeof='M'
      *
     C                   DOU       *IN90 = *off
     c                   exfmt     updaw
     C                   eval      *in90 =  *off
     C                   eval      *in52 =  *off
     C                   eval      *in56 =  *off
     c                   if        (scupda <> 'Y' AND scupda <> ' ')
     c                   seton                                        52  90
     c                   endif                                                  SCUPDA <> 'Y'
     c                   if        (sccxfl <> 'Y' AND sccxfl <> ' ')
     c                   seton                                        56  90
     c                   endif                                                  SCUPDA <> 'Y'
     C                   ENDDO
     c                   endif                                                  exupda='Y'
     c                   endsr
      ***** end add ***** 07/27/07 ***********************************************
