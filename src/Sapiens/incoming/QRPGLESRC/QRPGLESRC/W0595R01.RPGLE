     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H*
     H* PROGRAM DESCRIPTION: Wage Calculation maintenance
     H*
     H* CREATED: 06/02/11  C:53 PROJECT 25WPW
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 01/28/14 C:190 RedMine:3839
     H*                Fringe Benefits calculation not not calculating
     H*                Main wage page.
     H*
     H* 02/24/12 C:53  Seq# 79165
     H*                Issue with multiple records being written.
     H*
     H* 02/02/12 C:190 Seq#79079
     H*                Added Multiple Employer AWW with the TIBS AWW
     H*                in CALC1 & CALC2 for TIB 75% calculation.
     H*
     H* 01/18/12 C:062 Project: 25WPW  Priss 135
     H*                Retrieve the min/max based upon where the accident
     H*                date falls within date range.
     H*
     H* 09/29/11 C:190 Project: 25WPW  Priss 135
     H*                Corrected Rounding off issue.
     H*
     H* 09/26/11 C:190 Project: 25WPW  Priss 118, 133, 136
     H*                Contact school district employee was not saving the calculator.
     H*
     H* 09/21/11 C:53  Project: 25WPW  priss xxx
     H*                Update database when function key is pressed.
     H*
     H* 09/08/11 C:53  Project: 25WPW  priss 118
     H*                Only allow F4 if Fringe = Y
     H*
     H* 08/15/11 C:53  Project: 25WPW  priss 111
     H*                Only execute calc4 if Type = NS
     H*                Change hardcoded weeks of 50 to screen weeks value
     H*                  in calc4
     H*
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0595f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmwcp     uf a e             disk
     F                                     rename(pwc01:dbrcdf)
     F                                     infds(dbfds)
     Fwmwcl     if   e           k disk
     F                                     rename(pwc01:lwc01) prefix(l)
     Fwmcmp     if   e             disk
     Fwtgtl     if   e           k disk
     Fwmcxl     if   e           k disk
     F*** Begin Chg *** 01/18/12 *******************************************
     F**********wdwbl     if   e           k disk
     Fwdwbl1    if   e           k disk
     F*** End   Chg *** 01/18/12 *******************************************
     Fwatil     uf   e           k disk
     Fwaf1l     uf   e           k disk
     Fwaf2l     uf   e           k disk
     Fwamel     uf   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Wage Calculation'
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
     D @@mode1         s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D ##mode          s              1
     D ##mode1         s              1
     D ##rrn           s              9  0
     D ##rrn1          s              9  0
     D nxtpgm          s              1
     D kkco#           s                   like(wcco#)
     D kkfnd           s                   like(wcfnd)
     D kkfyr           s                   like(wcfyr)
     D kkcase          s                   like(wccase)
     D kkseq#          s                   like(wcseq#)
     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
     D kkyear          s                   like(wbyear)
     D*** Begin Add *** 01/18/12 *******************************************
     D kkstdt          s                   like(wbstdt)
     D*** End   Add *** 01/18/12 *******************************************
     D nseq#           s                   like(wcseq#)
     D sqseq#          s                   like(wcseq#)
     D error           s                   like(*in01)
     D xxcalc          s                   like(wccalc)
     D xxdt7           s                   like(cmacdt)
     D xxtr70          s                   like(sctr70)
     D xxtr75          s                   like(sctr75)
     D xxirat          s                   like(scirat)
     D xxiaww          s                   like(sciaww)
     D dbfds           ds
     D  dbrrn                397    400b 0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn                          wage rrn
     C                   parm                    @@rrn1                         claim rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
     C     acdpl3        plist
     C                   parm                    ##prds           40
     C                   parm      '2'           ##call            1
     C                   parm      'WCBT'        ##id              4
     C                   parm                    ##key            10
     C                   parm                    ##rtc             1
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
      *
     C*
     C     xxkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*
     C     xxkeyp        klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*
     C     cxkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
     C     sqkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    sqseq#
     C*
     C     wbkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkyear
     C*** Begin Add *** 01/18/12 *******************************************
     C*
     C     wbky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkstdt
     C*
     C     wbky2         klist
     C                   kfld                    kkco#
     C*** End   Add *** 01/18/12 *******************************************
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* APPROV - Processing for approve mode
    C*================================================================
     C     approv        begsr
     C                   exsr      getdtm
     C                   eval      wcsts = 'A'                                  Rsrv  wrksht status
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wcdedt
     C                   eval      wcduid=q1user                                Adjuster userid
     C                   endsr
    C*================================================================
    C* CALC1 - Processing for calculation 1
    C*================================================================
     C     calc1         begsr
     C* TIB AWW
     C     sctwag        div(h)    scwks         sctaww

     C* TIB 70%
     C*** beg chg *** 09/26/11 **************************************
     C*****sctaww        add       scawwm        sctr70
     C*****sctr70        mult      .70           sctr70
     C     sctaww        add       scawwm        sctr70
     C     sctr70        mult(h)   .70           sctr70
     C*** end chg *** 09/26/11 **************************************

     C* TIB 75%
     C                   if        *in10 = *on
     C*** beg chg *** 09/26/11 **************************************
     C*****sctaww        mult      .75           sctr75
     C*** beg chg *** 02/02/12 **************************************
     C     sctaww        add       scawwm        sctr75
     C     sctr75        mult(h)   .75           sctr75
     C**** sctaww        mult(h)   .75           sctr75
     C*** end chg *** 02/02/12 **************************************
     C*** end chg *** 09/26/11 **************************************
     C                   endif
     C                   endsr
    C*================================================================
    C* CALC2 - Processing for calculation 2
    C*================================================================
     C     calc2         begsr
     C*
     C* TIB AWW
     C                   if        sccday <> 0
     C*** beg chg *** 09/26/11 **************************************
     C*****sccamt        div(h)    sccday        sctaww
     C*****sctaww        mult      5             sctaww
     C     sccamt        div(h)    sccday        sctaww
     C     sctaww        mult(h)   5             sctaww
     C*** end chg *** 09/26/11 **************************************
     C                   endif
     C                   if        sccmth <> 0
     C     sccamt        div(h)    sccmth        sctaww
     C     sctaww        div(h)    4.34821       sctaww
     C                   endif
     C* TIB rate
     C*** beg chg *** 09/26/11 **************************************
     C*****sctaww        add       scawwm        sctr70
     C*****sctr70        mult      .70           sctr70
     C     sctaww        add       scawwm        sctr70
     C     sctr70        mult(h)   .70           sctr70
     C*** end chg *** 09/26/11 **************************************

     C* TIB 75%
     C                   if        *in10 = *on
     C*** beg chg *** 09/26/11 **************************************
     C*****sctaww        mult      .75           sctr75
     C*** beg chg *** 02/02/12 **************************************
     C     sctaww        add       scawwm        sctr75
     C     sctr75        mult(h)   .75           sctr75
     C*****sctaww        mult(h)   .75           sctr75
     C*** end chg *** 02/02/12 **************************************
     C*** end chg *** 09/26/11 **************************************
     C                   endif
     C                   endsr
    C*================================================================
    C* CALC3 - Processing for calculation 3
    C*================================================================
     C     calc3         begsr
     C* IIB AWW
     C     sctwag        div(h)    50            sciaww

     C* IIB 70%
     C*** beg chg *** 09/26/11 **************************************
     C*****sciaww        add       scawwm        xxiaww
     C*****xxiaww        mult      .70           scirat
     C     sciaww        add       scawwm        xxiaww
     C     xxiaww        mult(h)   .70           scirat
     C*** end chg *** 09/26/11 **************************************
     C*
     C                   endsr
    C*================================================================
    C* CALC4 - Processing for calculation 4
    C*================================================================
     C     calc4         begsr
     C*
     C                   if        sctype = 'TI' or
     C                             sctype = 'NS'
     C* TIB AWW
     C     sctwag        add       scnnpw        sctaww
     C     sctaww        div(h)    scwks         sctaww

     C* TIB 70%
     C*** beg chg *** 09/26/11 **************************************
     C*****sctaww        add       scawwm        sctr70
     C*****sctr70        mult      .70           sctr70
     C     sctaww        add       scawwm        sctr70
     C     sctr70        mult(h)   .70           sctr70
     C*** end chg *** 09/26/11 **************************************

     C* TIB 75%
     C                   if        *in10 = *on
     C*** beg chg *** 09/26/11 **************************************
     C*****sctaww        add       scawwm        sctr75
     C*****sctr75        mult      .75           sctr75
     C     sctaww        add       scawwm        sctr75
     C     sctr75        mult(h)   .75           sctr75
     C*** end chg *** 09/26/11 **************************************
     C                   endif
     C                   endif
     C*
     C                   if        sctype = 'II' or
     C                             sctype = 'NS'
     C* IIB AWW
     C     sctwag        add       scnnpw        sciaww
     C*** beg chg *** 08/15/11 **************************************
     C**** sciaww ****** div(h)    50            sciaww
     C     sciaww        div(h)    scwks         sciaww
     C*** end chg *** 08/15/11 **************************************

     C* IIB 70%
     C*** beg chg *** 09/26/11 **************************************
     C*****sciaww        add       scawwm        scirat
     C*****scirat        mult      .70           scirat
     C     sciaww        add       scawwm        scirat
     C     scirat        mult(h)   .70           scirat
     C*** end chg *** 09/26/11 **************************************
     C                   endif
      *
     C                   endsr
    C*================================================================
    C* CHKDTA - Check secondary data base on flags
    C*================================================================
     C     chkdta        begsr
     C*
     C                   if        scnpwf = 'N' and
     C                             scnnpw <> 0
     C                   eval      ##mgid = 'A'
     C                   exsr      msgapi
     C                   endif
     C*
     C                   if        scmlte = 'N' and
     C                             scawwm <> 0
     C                   eval      ##mgid = 'B'
     C                   exsr      msgapi
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
      *
      * del associated rcds
     C                   eval      kkco#  = wcco#
     C                   eval      kkfnd  = wcfnd
     C                   eval      kkfyr  = wcfyr
     C                   eval      kkcase = wccase
     C                   eval      kkseq# = wcseq#
      *
     C     *in99         doueq     *on
     C     xxkey         delete    pti01                              99
     C                   enddo
     C     *in99         doueq     *on
     C     xxkey         delete    pf101                              99
     C                   enddo
     C     *in99         doueq     *on
     C     xxkey         delete    pf201                              99
     C                   enddo
     C     *in99         doueq     *on
     C     xxkey         delete    pme01                              99
     C                   enddo
      *
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C*
     C                   exsr      tibiib
     C*
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*
     C*** beg dlt *** 09/26/11 **************************************
     C***********        if        *in01
     C***********        eval      xxcalc = *blank
     C***********        endif
     C*** end dlt *** 09/26/11 **************************************
     C*
     C* ovrd
     C                   if        scovrd <> *blanks and
     C                             scovrd <> 'Y'
     C                   move      *on           *in70
     C     *in70         cabeq     *on           ender1                   90
     C                   endif
     C* ovrd
     C                   if        sctype = 'II' and
     C                             scovrd = 'Y'
     C                   move      *on           *in66
     C     *in66         cabeq     *on           ender1                   90
     C                   endif
     C* type
     C                   if        sctype = *blanks
     C                   move      *on           *in71
     C     *in71         cabeq     *on           ender1                   90
     C                   else
     C                   move(p)   'WCBT'        kktbid
     C                   movel(p)  sctype        kkkey
     C     gtky          chain     pgt01                              72
     C  N72gtstat        comp      'I'                                    72
     C     *in72         cabeq     *on           ender1                   90
     C                   endif
     C* stdt
     C                   eval      srdti = scstdt
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   move      *on           *in73
     C     *in73         cabeq     *on           ender1                   90
     C                   else
     C                   movel     srdto         scstdt
     C                   endif
     C* stdt < cmacdt
     C                   z-add     scstdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt7
     C                   if        xxdt7 >= cmacdt
     C                   move      *on           *in68
     C     *in68         cabeq     *on           ender1                   90
     C                   endif
     C* lsdt
     C                   eval      srdti = sclsdt
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   move      *on           *in74
     C     *in74         cabeq     *on           ender1                   90
     C                   else
     C                   movel     srdto         sclsdt
     C                   endif
     C* lsdt < cmacdt
     C                   z-add     sclsdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt7
     C                   if        xxdt7 >= cmacdt
     C                   move      *on           *in69
     C     *in69         cabeq     *on           ender1                   90
     C                   endif
     C* sdem
     C                   if        scsdem <> *blanks and
     C                             scsdem <> 'Y' and
     C                             scsdem <> 'N'
     C                   move      *on           *in78
     C     *in78         cabeq     *on           ender1                   90
     C                   endif
     C* contract info if II
     C                   if        sctype = 'II'
     C                   if        sccnem = 'Y' or
     C                             sccamt <> *zeros or
     C                             sccmth <> *zeros or
     C                             sccday <> *zeros
     C                   move      *on           *in67
     C     *in67         cabeq     *on           ender1                   90
     C                   endif
     C                   endif
     C* cnem
     C                   if        sccnem <> *blank and
     C                             sccnem <> 'Y' and
     C                             sccnem <> 'N'
     C                   move      *on           *in80
     C     *in80         cabeq     *on           ender1                   90
     C                   endif
     C* npwf
     C                   if        scnpwf <> 'N' and
     C                             scnpwf <> 'Y'
     C                   move      *on           *in79
     C     *in79         cabeq     *on           ender1                   90
     C                   endif
     C* npwf
     C                   if        scnpwf = 'Y' and
     C                             scsdem = 'Y'
     C                   move      *on           *in88
     C     *in88         cabeq     *on           ender1                   90
     C                   endif
     C* wks/sctype = II
     C                   if        sctype = 'II'
     C                   if        scwks <> 0
     C                   move      *on           *in77
     C     *in77         cabeq     *on           ender1                   90
     C                   endif
     C                   else
     C* wks
     C                   if        sccnem <> 'Y'
     C                   if        scwks < 1 or
     C                             scwks > 14
     C                   move      *on           *in75
     C     *in75         cabeq     *on           ender1                   90
     C                   endif
     C* wks/sccnem = Y
     C                   else
     C                   if        scwks <> 0
     C                   move      *on           *in76
     C     *in76         cabeq     *on           ender1                   90
     C                   endif
     C                   endif
     C                   endif
     C* camt/cnem = Y
     C                   if        sccnem = 'Y' and
     C                             sccamt <= 0
     C                   move      *on           *in81
     C     *in81         cabeq     *on           ender1                   90
     C                   endif
     C* camt/cnem = blank
     C                   if        sccnem <> 'Y' and
     C                             sccamt <> 0
     C                   move      *on           *in82
     C     *in82         cabeq     *on           ender1                   90
     C                   endif
     C*
     C* cday/cmth check for both if cnem = Y
     C                   if        sccnem = 'Y'
     C                   if        sccmth <> 0  and
     C                             sccday <> 0
     C                   move      *on           *in83
     C     *in83         cabeq     *on           ender1                   90
     C                   endif
     C*
     C* cday/cnem = Y
     C                   if        sccmth = 0  and
     C                             sccday = 0
     C                   move      *on           *in84
     C     *in84         cabeq     *on           ender1                   90
     C                   endif
     C* cday/cnem = blank
     C                   else
     C                   if        sccday <> 0
     C                   move      *on           *in85
     C     *in85         cabeq     *on           ender1                   90
     C                   endif
     C                   if        sccmth <> 0
     C                   move      *on           *in86
     C     *in86         cabeq     *on           ender1                   90
     C                   endif
     C                   endif
     C*
     C* mlte
     C                   if        scmlte <> 'N' and
     C                             scmlte <> 'Y'
     C                   move      *on           *in87
     C     *in87         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   exsr      chkdta
     C*
     C     ender1        endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* GETWB  - Get WDWBP inofo
    C*================================================================
     C     getwb         begsr
     C*
     C* verify the  control record has been entered.
     C*** Begin Chg *** 01/18/12 *******************************************
     C*************      z-add     cmacdt        srdti
     C*************      call      'SRCVTDAT'    dtmdyy
     C*************      movel     srdto         xxkydt            8 0
     C*************      move      xxkydt        kkyear
     C*************
     C*****wbkey***      chain     pwb01                              99
     C*************      if        *in99 = *on or
     C*************                wbtthr = *zeros
     C                   eval      kkstdt=cmacdt
     C     wbky1         setgt     pwb01
     C     wbky2         readpe    pwb01                                  99
     C                   if        *in99 = *on or
     C                             wbtthr = *zeros or wbendt<cmacdt
     C*** End   Chg *** 01/18/12 *******************************************
     C                   move      *on           error
     C                   endif
      * no control info
     C                   if        error = *on
     C                   eval      ##mgid = '7'
     C                   exsr      msgapi
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* HOURLY - Processing for hourly wage
    C*================================================================
     C     hourly        begsr
     C*
     C                   if        sccnem <> 'Y'
     C                   if        scthrs <> *zeros
     C     sctwag        div(h)    scthrs        schryr
     C                   endif
     C* cnem = Y
     C                   else
     C* Use months
     C                   if        sccmth > *zeros
     C     sccamt        div       sccmth        schryr
     C     schryr        div       4.34821       schryr
     C     schryr        div(h)    40            schryr
     C* Use days
     C                   else
     C                   if        sccday > *zeros
     C     sccamt        div       sccday        schryr
     C                   endif
     C     schryr        mult      5             schryr
     C     schryr        div(h)    40            schryr
     C                   endif
     C                   endif
     C*
     C* get and compare the rate to the control threshold
     C                   if        *in99 = *off
     C                   if        schryr <= wbtthr
     C                   move      *on           *in10
     C                   else
     C                   move      *off          *in10
     C                   endif
     C                   endif
     C*
     C     enhour        endsr
    C*================================================================
    C* KDDO - F4=Fringe processing
    C*================================================================
     C     kddo          begsr
      *
      *** BEG DEL *** 09/08/11 *********************************************
     C*******            if        scsdem = 'Y'
      *** END DEL *** 09/08/11 *********************************************
      *** BEG ADD *** 09/08/11 *********************************************
     C                   select
     C                   when      scsdem = 'Y'
      *** END ADD *** 09/08/11 *********************************************
     C                   eval      ##mgid = '1'
     C                   exsr      msgapi
      *
      *** BEG ADD *** 09/08/11 *********************************************
     C                   when      scnpwf <> 'Y'
     C                   eval      ##mgid = '0'
     C                   exsr      msgapi
      *** END ADD *** 09/08/11 *********************************************
      *** BEG CHG *** 09/08/11 *********************************************
     C                   other
     C*******            else
      *** END CHG *** 09/08/11 *********************************************
     C                   call      'W0595R03'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      @@mode        ##mode
     C                   parm      @@rrn         ##rrn                          wmwcp
     C                   parm      @@rrn1        ##rrn1                         wmcmp
     C                   parm      ' '           ##beg             1
      *** BEG CHG *** 09/08/11 *********************************************
     C                   endsl
     C*******            endif
      *** END CHG *** 09/08/11 *********************************************
     C*
     C*** beg dlt *** 09/26/11 **************************************
B01  C********           if        @@mode = 'A'
     C********           eval      @@mode = 'C'
E01  C********           endif
     C*** end dlt *** 09/26/11 **************************************
     C*************      exsr      refrsh
     C*
     c                   if        ##beg = 'Y'
     c                   eval      ##kc  = '1'
     c                   eval      *in90 = '0'
     c                   leavesr
     c                   endif

     C                   if        *inkc = *on
     C                   eval      *in90 = *off
E01  C                   else
     C                   eval      *in90 = *on
E01  C                   endif
      *
     C                   endsr
    C*================================================================
    C* KFDO - F6=Mult Emp processing
    C*================================================================
     C     kfdo          begsr
      *
     C****               if        wcwks = 0
     C****               eval      ##mgid = 'C'
     C****               exsr      msgapi
     C**** ##mgid        cabeq     'C'           enkfdo
E01  C****               endif
      *
     C                   if        scmlte <> 'Y'
     C                   eval      ##mgid = '4'
     C                   exsr      msgapi
     C                   else
      *
     C                   call      'W0595R05'
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      @@mode        ##mode
     C                   parm      @@rrn         ##rrn                          wmwcp
     C                   parm      @@rrn1        ##rrn1                         wmcmp
      *
     C*
B01  C                   if        @@mode = 'A'
     C                   eval      @@mode = 'C'
E01  C                   endif
     C                   exsr      refrsh
     C*
     C                   if        *inkc = *on
     C                   eval      *in90 = *off
E01  C                   else
     C                   eval      *in90 = *on
E01  C                   endif
      *
     C                   endif
     C     enkfdo        endsr
    C*================================================================
    C* KGDO - F7=Calculate processing
    C*================================================================
     C     kgdo          begsr
     C*** beg add *** 09/26/11 **************************************
     c                   exsr      errck1
     c                   if        *in90 = *off
     C*** end add *** 09/26/11 **************************************
     C*
     C* check for TIB/IIB data
B01  C                   if        @@mode <> 'V'
B01  C                             and sccnem <> 'Y'
     C                   eval      kkseq# = wcseq#
     C     xxkey         chain(n)  pti01                              99
     C                   if        *in99 = *on
     C                   eval      ##mgid = '6'
     C                   exsr      msgapi
     C                   goto      enkgdo
     C                   endif
     C*
     C* Cannot have TIBS/IIBS data for cnem, del as user may have changed.
     C                   else
     C                   eval      kkseq# = wcseq#
     C     *in99         doueq     *on
     C     xxkey         delete    pti01                              99
     C                   enddo
     C* update screen fields
     C                   eval      scthrs = *zeros
     C                   eval      sctwag = *zeros
     C                   endif
     C* check for control
     C                   exsr      getwb
     C     error         cabeq     *on           enkgdo
      *
     C                   eval      sctibx = wbtibx
     C                   eval      sctibn = wbtibn
     C                   eval      sciibx = wbiibx
     C                   eval      sciibn = wbiibn
     C*
     C                   eval      schryr = *zeros
     C*
     C**** scsdem        caseq     'Y'           hourly
     C                   exsr      hourly
     C***                endcs
     C*
     C                   eval      sctaww = *zeros
     C                   eval      sctr70 = *zeros
     C                   eval      sctr75 = *zeros
     C                   eval      sciaww = *zeros
     C                   eval      scirat = *zeros
     C*
     C*******            if        sccnem = 'Y'
     C*******            eval      sctwag = sccamt
     C*******            endif
     C*
     C                   select
     C* sdem & cnem = blank and type=NS
     C                   when      scsdem <> 'Y' and
     C*** beg chg *** 08/15/11 **************************************
     C*************                sccnem <> 'Y'
     C                             sccnem <> 'Y' and
     C*** end chg *** 08/15/11 **************************************
     C*** beg add *** 08/15/11 **************************************
     C                             sctype = 'NS'
     C*** end add *** 08/15/11 **************************************
     C                   exsr      calc4
     C* sdem=Y & type=II
     C                   when      scsdem = 'Y' and
     C                             sccnem <> 'Y' and
     C                             sctype = 'II'
     C                   exsr      calc3
     C* sdem=Y/cnem=Y/type=TI
     C                   when      scsdem = 'Y' and
     C                             sccnem = 'Y' and
     C                             sctype = 'TI'
     C                   exsr      calc2
     C* default calc
     C* sdem = Y cnem = blank
     C                   when      scsdem = 'Y' and
     C*** beg chg *** 08/15/11 **************************************
     C***********                  sccnem <> 'Y'
     C                             sccnem <> 'Y' and
     C*** end chg *** 08/15/11 **************************************
     C*** beg add *** 08/15/11 **************************************
     C                             sctype <> 'NS'
     C*** end add *** 08/15/11 **************************************
     C                   exsr      calc1
     C* no valid criteria has been met.  display error.
     C                   other
     C                   eval      ##mgid = '8'
     C                   exsr      msgapi
     C                   goto      enkgdo
     C                   endsl
     C*
     C                   exsr      minmax
     C*
     C                   if        scovrd = 'Y'
     C                   exsr      mnmxov
     C                   endif
     C*
     C                   eval      xxcalc = 'Y'
     C*
     C                   eval      nxtpgm = 'N'
     C                   exsr      updatr
     C                   eval      nxtpgm = ' '
     C*
     C                   exsr      chkdta
     C*
     C*** beg chg *** 09/26/11 **************************************
     c                   endif
     C*** end chg *** 09/26/11 **************************************
     C     enkgdo        endsr
    C*================================================================
    C* KIDO - F9=Submit processing
    C*================================================================
     C     kido          begsr
      *
     C                   if        xxcalc <> 'Y'
     C                   eval      ##mgid = '2'
     C                   exsr      msgapi
      *
     C                   else
      *
     C** Fill qqrmcd
     C     cxkey         chain     pcx01                              89
     C*
     C                   if        *in89 = *off and
     C                             cxwcr = *blanks
     C                   eval      *in89 = *on
     C                   endif
     C     *in89         cabeq     *on           endf9                    90
     C*
     C                   exsr      writedarts
      *
     C                   eval      *inlr = *on
     C                   return
     C*
     C                   endif
     C     endF9         endsr
    C*================================================================
    C* KJDO - F10=Search
    C*================================================================
     C     kjdo          begsr
     C*** beg add *** 09/26/11 **************************************
     c                   if        *in30 = '0'
     C*** end add *** 09/26/11 **************************************
     C                   if        hfld = 'SCTYPE'
     C                   call      'W2493R'      acdpl3
      *
     C                   move(p)   'WCBT'        kktbid
     C                   movel(p)  ##key         kkkey
     C     gtky          chain     pgt01                              99
     C     *in99         ifeq      *off
     C                   movel(p)  gtkey         sctype
     C                   eval      *in41 = *on
     C                   endif
     C                   endif
     C*** beg add *** 09/26/11 **************************************
     C                   endif
     C*** beg add *** 09/26/11 **************************************
     C                   endsr
    C*================================================================
    C* MINMAX - Minimum/Maximum
    C*================================================================
     C     minmax        begsr
     C* save
     C                   eval      xxtr70 = sctr70
     C                   eval      xxtr75 = sctr75
     C                   eval      xxirat = scirat
     C*
     C                   if        sctype = 'II' or sctype = 'NS'
     C*
     C                   if        scirat < wbiibn
     C                   eval      scirat = wbiibn
     C                   endif
     C                   if        scirat > wbiibx
     C                   eval      scirat = wbiibx
     C                   endif
     C                   endif
     C*
     C                   if        sctype = 'TI' or sctype = 'NS'
     C*
     C                   if        sctr70 < wbtibn
     C                   eval      sctr70 = wbtibn
     C                   endif
     C*
     C                   if        sctr70 > wbtibx
     C                   eval      sctr70 = wbtibx
     C                   endif
     C*
     C                   if        *in10 = *on
     C                   if        sctr75 < wbtibn
     C                   eval      sctr75 = wbtibn
     C                   endif
     C*
     C                   if        sctr75 > wbtibx
     C                   eval      sctr75 = wbtibx
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MNMXOV - Minimum override
    C*================================================================
     C     mnmxov        begsr
     C*
     C                   if        xxtr70 < wbtibn
     C                   eval      sctr70 = xxtr70
     C                   endif
     C*
     C                   if        *in10 = *on
     C                   if        xxtr75 < wbtibn
     C                   eval      sctr75 = xxtr75
     C                   endif
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MSGAPI - Call the message display API
    C*================================================================
     C     msgapi        begsr
     C*
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     C*
     C*** beg add *** 09/26/11 **************************************
     c                   eval      *in90 = *on
     C*** end add *** 09/26/11 **************************************
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   if        wcauid = *blanks
     C                   movel(p)  q1user        scauid
     C                   else
     C                   eval      scauid = wcauid
     C                   endif
     C*
     C                   if        wcmlte = *blanks
     C                   eval      scmlte = 'N'
     C                   else
     C                   eval      scmlte = wcmlte
     C                   endif
     C*
     C                   if        wcnpwf = *blanks
     C                   eval      scnpwf = 'N'
     C                   else
     C                   eval      scnpwf = wcnpwf
     C                   endif
     C*
     C                   if        wcsts = *blanks
     C                   eval      scsts = 'P'
     C                   else
     C                   eval      scsts  = wcsts
     C                   endif
     C*
     C                   if        scsts = 'P'
     C                   eval      scstsd = 'Pended    '
     C                   else
     C                   eval      scstsd = 'Approved  '
     C                   endif
     C*****              eval      scrvsd = wcrvsd
     C                   eval      xxcalc = wccalc
     C                   eval      sctype = wctype
     C                   eval      scwks  = wcwks
     C*******            eval      scdedt = wcdedt
     C                   eval      sctibn = wctmin
     C                   eval      sctibx = wctmax
     C                   eval      sciibn = wcimin
     C                   eval      sciibx = wcimax
     C                   eval      scovrd = wcovrd
     C                   eval      scsdem = wcsdem
     C                   eval      sccnem = wccnem
     C                   eval      sccamt = wccamt
     C                   eval      sccday = wccday
     C                   eval      sccmth = wccmth
     C                   eval      sctaww = wctaww
     C                   eval      sctr70 = wctr70
     C                   eval      sctr75 = wctr75
     C                   eval      sciaww = wciaww
     C                   eval      scirat = wcirat
     C                   eval      sctwag = wctwag
     C                   eval      scthrs = wcthrs
     C                   eval      schryr = wchryr
     C                   eval      scnnpw = wcnnpw
     C                   eval      scawwm = wcawwm
     C* Dates
     C*
     C                   z-add     wcstdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scstdt
     C                   z-add     wclsdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sclsdt
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C**
     C                   eval      wcco# = cmco#                                Company
     C                   eval      wcfnd = cmfnd                                Fund
     C                   eval      wcfyr = cmfyr                                Fund year
     C                   eval      wccase= cmcase                               Case
     C**
     C                   eval      wcsts  = scsts
     C*****              eval      wcrvsd = scrvsd
     C                   eval      wccalc = xxcalc
     C                   eval      wctype = sctype
     C                   eval      wcauid = scauid
     C*******************eval      wcstdt = scstdt
     C*******************eval      wclsdt = sclsdt
     C                   eval      wcwks  = scwks
     C*******            eval      wcdedt = scdedt
     C                   eval      wctmin = sctibn
     C                   eval      wctmax = sctibx
     C                   eval      wcimin = sciibn
     C                   eval      wcimax = sciibx
     C                   eval      wcovrd = scovrd
     C                   eval      wcsdem = scsdem
     C                   eval      wccnem = sccnem
     C                   eval      wccamt = sccamt
     C                   eval      wccday = sccday
     C                   eval      wccmth = sccmth
     C                   eval      wcnpwf = scnpwf
     C                   eval      wcmlte = scmlte
     C                   eval      wctaww = sctaww
     C                   eval      wctr70 = sctr70
     C                   eval      wctr75 = sctr75
     C                   eval      wciaww = sciaww
     C                   eval      wcirat = scirat
     C                   eval      wctwag = sctwag
     C                   eval      wcthrs = scthrs
     C                   eval      wchryr = schryr
     C**************     eval      wcnnpw = scnnpw
     C                   eval      wcawwm = scawwm
     C*
     C* Dates
     C*
     C                   z-add     scstdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wcstdt
     C                   z-add     sclsdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wclsdt
     C*
     C                   movel     q1user        wcuser
     C                   movel     q1pgm         wcpgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wcdt
     C*
     C                   movel     dstime        wctm
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C*
     C     othfks        begsr
     C*
      *** BEG ADD *** 09/21/11 *********************************************
     C****               select
     C****               when      @@mode = 'A'
     C****               exsr      writer
     C****               when      @@mode = 'C'
     C****               exsr      updatr
     C****               endsl
      *** END ADD *** 09/21/11 *********************************************
     C*
     C     *inkd         caseq     *on           kddo                     90
     C     *inkf         caseq     *on           kfdo                     90
     C     *inkg         caseq     *on           kgdo                     90
     C     *inki         caseq     *on           kido                     90
     C     *inkj         caseq     *on           kjdo                     90
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
     C     pgmstr        begsr
     C*
     C     @@rrn1        chain     pcm01                              99
     C                   move      cmco#         kkco#
     C                   move      cmfnd         kkfnd
     C                   move      cmfyr         kkfyr
     C                   move      cmcase        kkcase
      *
     C                   call      'WA006R1'
     C                   parm                    kkco#
     C                   parm                    kkfnd
     C                   parm                    kkfyr
     C                   parm                    kkcase
     C                   parm                    sccs01
     C                   parm                    sccs02
     C                   parm                    sccs03
      *
     c                   move(p)   cmco#         xaco#             3
     c                   move(p)   cmfnd         xafnd             3
     c                   move(p)   cmfyr         xafyr             3
     c                   move(p)   cmcase        xacase            7
     c     xaco#         cat       xafnd:0       qqkey
     c     qqkey         cat       xafyr:0       qqkey
     c     qqkey         cat       xacase:0      qqkey
      *
      * get control info
     C                   exsr      getwb
      *
B01  C                   if        @@mode = 'C'
     C                   eval      *in30 = *on
E01  C                   endif
      *
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
      *
     C* Approve mode
     C     @@mode        ifeq      'V'
     C                   exsr      updatr
     C                   eval      *inlr = *on
     C                   return
     C                   endif
      *
     C                   endsr
    C*================================================================
    C* TIBIIB - TIB's IIB's processinger modes
    C*================================================================
     C     tibiib        begsr
      *
     C*** beg chg *** 09/26/11 **************************************
B01  C**********         if        *inkd  =  '0'
B01  C                   if        *inkd  =  '0' and *inkg = '0'
     C*** end chg *** 09/26/11 **************************************

B01  C                   if        @@mode <> 'V'
B01  C                   if        sccnem <> 'Y'
     C*
     C                   if        sctype = 'TI' or
     C                             sctype = 'NS'
     C                   eval      ##mode1 = 'T'
     C                   endif
     C*
     C                   if        sctype = 'II'
     C                   eval      ##mode1 = 'I'
     C                   endif
     C                   call      'W0595R02'
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl
     C                   parm      @@mode        ##mode
     C                   parm                    ##mode1                        IIBS/TIBS
     C                   parm      @@rrn         ##rrn                          wmwcp
     C                   parm      @@rrn1        ##rrn1                         wmcmp
     C                   parm      ' '           ##beg             1
      *
     C*
B01  C                   if        @@mode = 'A'
     C                   eval      @@mode = 'C'
E01  C                   endif
     C*
     C                   exsr      refrsh
      *
     C                   else
     C*** beg chg *** 09/26/11 **************************************
B01  C*******            if        xxcalc <> 'Y'
     C*******            eval      ##mgid = '3'
     C*******            exsr      msgapi
E01  C*******            else
     C*******            eval      *inlr = *on
     C*******            return
E01  C*******            endif
     c                   select
     c                   when      xxcalc <> 'Y'
     C                   eval      ##mgid = '3'
     C                   exsr      msgapi
     c                   when      xxcalc =  'Y'  and *inkg = '1'
     c                   eval      *in90 = *on
     c                   other
     C                   eval      *inlr = *on
     C                   return
     c                   endsl
     C*** end chg *** 09/26/11 **************************************
E01  C                   endif
E01  C                   endif
     C*
B01  C                   if        @@mode = 'A'
     C                   eval      @@mode = 'C'
E01  C                   endif
     C*
     c                   if        ##beg = 'Y'
     c                   eval      @@kc  = '1'
     c                   eval      *in90 = '0'
     c                   leavesr
     c                   endif

     C                   if        *inkc = *on
     C                   eval      *in90 = *off
E01  C                   else
     C                   eval      *in90 = *on
E01  C                   endif
     C*** beg add *** 09/26/11 **************************************
     c                   else
     C                   eval      *in90 = *on
     C*** end add *** 09/26/11 **************************************

E01  C                   endif                                                  *inkd = 0
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C*** beg chg *** 01/28/14 **************************************
     C                   eval      scnnpw = wcnnpw
     C*** end chg *** 01/28/14 **************************************
     C*
B01  C                   if        @@mode = 'V'
     C                   exsr      approv
E01  C                   else
     C                   exsr      mvscdb
E01  C                   endif
     C*
     C                   update    dbrcdf
E01  C                   endif
     C* Call next pgm
     C                   if        nxtpgm <> 'N'
     C                   exsr      tibiib
     C                   endif
     C*
     C                   endsr
     c*===============================================================
     c* WriteDarts - Sent a Darts dairy
     c*===============================================================
     c     Writedarts    begsr
     C*
     C                   call      'SC001R'
     C                   parm      optnm         qqprds           40
     C                   parm      '2'           qqcall            1
     C                   parm                    qqkey            40
     C                   parm      'W0070R'      qqpnam           10
     C                   parm      qqkey         qqfold           80
     C                   parm      *BLANKS       qqdcid           12
     C                   parm                    qqdate            8
     C                   parm      'CLWCD'       qqrmcd            5
     C                   parm                    qqcom            60
     C                   parm      *blanks       qqdusr           10
     C                   parm      'Y'           qqrfld            1
     C                   parm      'Y'           qqrdoc            1
     C                   parm      *blanks       qqrdt             1
     C                   parm      'Y'           qqrrmc            1
     C                   parm      *blanks       qqrcom            1
     C                   parm      *blanks       qqradd            1
     C                   parm      *blanks       qqrdty            1
     C                   parm      *blanks       qqrdus            1

     c                   endsr                                                  Writedarts
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C*
     C* Set init status to Pend
     C                   eval      wcsts = 'P'                                  Rsrv  wrksht status
     C*
     C* Get next seq#
     C                   move      *hival        sqseq#
     C     sqkey         setgt     lwc01
     C     xxkeyp        readpe    lwc01                                9999
     C                   if        *in99 = *off
     C                   eval      nseq# = lwcseq# + 1
     C                   else
     C                   eval      nseq# = nseq# + 1
     C                   endif
     C*
     C                   eval      wcseq# = nseq#
     C*
     C* Init user
     C*
     C                   movel     q1user        wciusr
     C                   movel     q1pgm         wcipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wcidt
     C                   movel     dstime        wcitm
     C* Entry date
     C                   movel     srdto         wcendt
     C*
     C                   write     dbrcdf
     C                   eval      @@rrn = dbrrn
     C*
     C                   feod      wmwcp
     C*
     C* Call next pgm
     C                   exsr      tibiib
     C*
     C*** beg add *** 02/24/12 **************************************
B01  C                   if        @@mode = 'A'
     C                   eval      @@mode = 'C'
E01  C                   endif
     C*** end add *** 02/24/12 **************************************
     C*
     C                   endsr
