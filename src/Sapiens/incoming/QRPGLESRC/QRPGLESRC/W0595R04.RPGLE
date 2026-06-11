     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*********************************************************************
     H/TITLE W0595R04 - NPW Fringe Benefits Continuance                   *
     H*********************************************************************
     H*  CREATED: 06/01/11  PROGRAMMER: C:190 PROJECT: 25WPW              *
     H*********************************************************************
     H* Revisions:                                                        *
     H*
     H* 09/09/11 C:53  Project: 25WPW  priss 117
     H*                Remove F8 processing.
     H*                                                                   *
     H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0595f04  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwaf2p     uf a e             disk
     F                                     rename(pf201:dbrcdf)
     Fwaf2l     uf   e           k disk
     Fwmwcp     uf   e             disk
     Fwmcmp     if   e             disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'NPW Fringe Benefits Continuance'
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
     D ##kc            s              1
     D @@kl            s              1
     D ##kl            s              1
     D @@mode          s              1
     D @@mode1         s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D @@rrn2          s              9  0
     D @@beg           s              1
     D xxbeg           s              1
     D kkco#           s                   like(f2co#)
     D kkfnd           s                   like(f2fnd)
     D kkfyr           s                   like(f2fyr)
     D kkcase          s                   like(f2case)
     D kkseq#          s                   like(f2seq#)
     D kksseq#         s                   like(f2seq#)
     D sqseq#          s                   like(f2seq#)
     D nseq#           s                   like(f2seq#)  inz(0)
     D xxfyr           s                   like(f2fyr)

     d i               s              2  0
     D d1              s              2  0
     D d2              s              2  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     d* Will Emp continue array.
     d wm2ds           ds
     d schinc
     d scldrc
     d sccloc
     d screnc
     d scfodc
     d scfulc
     d scothc
     d wm2                                 overlay(wm2ds)
     d                                     like(schinc) dim(7) inz(' ')
      *
     d* Date Benifit Suspended.
     d To2ds           ds
     d schinf
     d sccdrf
     d scclof
     d screnf
     d scfodf
     d scfulf
     d scothf
     d to2                                 overlay(to2ds)
     d                                     like(schinf) dim(7) inz(0)
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
     C                   parm                    @@rrn                          wmwcp
     C                   parm                    @@rrn1                         wmcmp
     C                   parm                    @@beg
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
      *
     C     xxkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
      *
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
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

     c                   exsr      valcalc
     c                   if        *in90 =  *off
     c                   if        f2calc <> 'Y' or *in95 = *on
     C                   eval      ##mgid = '2'
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     c                   eval      *in90 = *on
     c                   eval      f2calc = ' '
     c                   leavesr                                                f2calc <> 'Y'
     c                   endif
     c                   endif                                                  *in90 = 0

     c                   endsr                                                  errck1
     C*
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
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr
     c*
     c     xxkey         chain     pf201                              60

     c                   eval      schinc = f2hinc
     C                   z-add     f2hins        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         schins
      *
     c                   eval      scldrc = f2ldrc
     C                   z-add     f2ldrs        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scldrs
      *
     c                   eval      sccloc = f2cloc
     C                   z-add     f2clos        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scclos
      *
     c                   eval      screnc = f2renc
     C                   z-add     f2rens        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         screns

      *
     c                   eval      scfodc = f2fodc
     C                   z-add     f2fods        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scfods
      *
      *
     c                   eval      scfulc = f2fulc
     C                   z-add     f2fuls        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scfuls
      *
     c                   eval      scothc = f2othc
     C                   z-add     f2oths        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scoths
      *
     c                   eval      schinf = f2hinf                              Health Tot
     c                   eval      sccdrf = f2ldrf                              Laundry Tot
     c                   eval      scclof = f2clof                              Clothing Tot
     c                   eval      screnf = f2renf                              Lodging Tot
     c                   eval      scfodf = f2fodf                              Food Tot
     c                   eval      scfulf = f2fulf                              Vehical Tot
     c                   eval      scothf = f2othf                              Other Tot

     c                   eval      scfbco = f2fbco                              Continued
     c                   eval      scfbno = f2fbno                              Non Continued
     c*
     c                   endsr
    c*================================================================
    c* MVSCDB - Move screen fields to data base fields
    c*================================================================
     c     mvscdb        begsr
     c*
     c                   eval      f2co#  = cmco#
     c                   eval      f2fnd  = cmfnd
     c                   eval      f2fyr  = cmfyr
     c                   eval      f2case = cmcase
      *
     c                   eval      f2hinc = schinc
     C                   z-add     schins        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f2hins
      *
     c                   eval      f2ldrc = scldrc
     C                   z-add     scldrs        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f2ldrs
      *
     c                   eval      f2cloc = sccloc
     c                   z-add     scclos        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         f2clos
      *
     c                   eval      f2renc = screnc
     c                   z-add     screns        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         f2rens

      *
     c                   eval      f2fodc = scfodc
     c                   z-add     scfods        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         f2fods
      *
     c                   eval      f2fulc = scfulc
     c                   z-add     scfuls        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         f2fuls
      *
     c                   eval      f2othc = scothc
     c                   z-add     scoths        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         f2oths
      *
     c                   eval      f2hinf = schinf                              Health Tot
     c                   eval      f2ldrf = sccdrf                              Laundry Tot
     c                   eval      f2clof = scclof                              Clothing Tot
     c                   eval      f2renf = screnf                              Lodging Tot
     c                   eval      f2fodf = scfodf                              Food Tot
     c                   eval      f2fulf = scfulf                              Vehical Tot
     c                   eval      f2othf = scothf                              Other Tot

     c                   eval      f2fbco = scfbco                              Continued
     c                   eval      f2fbno = scfbno                              Non Continued

     C                   endsr                                                  mvscdb
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C     *inkg         caseq     *on           kgdo                     90
     C*** beg del *** 09/09/11 *********************************************
     C**** *inkh         caseq     *on           khdo                     90
     C*** end del *** 09/09/11 *********************************************
     C     *inki         caseq     *on           kido                     90
     C                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     c                   return
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C     @@rrn1        chain(n)  pcm01                              99
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
     c     'SS#:'        scan      sccs01        d1
     C                   eval      sccs01 = %subst(sccs01:d1)
     c     'SCL#:'       scan      sccs03        d2
     c                   eval      sccs03 = %subst(sccs03:1:d2-1)
     c                   movel     sccs01        scss#
     c                   movel     sccs03        scyrcl
     c*
      *
     C* Get original master rcd info
     C     @@rrn         chain(n)  pwc01                              99
B01  C                   if        *in99 = *off
     C                   move      wcco#         kkco#
     C                   move      wcfnd         kkfnd
     C                   move      wcfyr         kkfyr
     C                   move      wccase        kkcase
     C                   move      wcseq#        kkseq#
     C                   endif

     C                   endsr                                                  pgmstr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     c                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   endsr
    C*================================================================
    C* WRTUPD - Write & Update after processing
    C*================================================================
     c     wrtupd        begsr
     c*
     c                   eval      kkseq#  = wcseq#
     c     xxkey         chain     pf201                              60

     c                   if        *in60 = *off
     c*
     c** If record alreday exist, update the record.
     c                   eval      f2calc  = 'Y'                                Calc
     C                   movel     q1user        f2user
     C                   movel     q1pgm         f2pgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f2dt
     C                   movel     dstime        f2tm

     c                   exsr      mvscdb
     c                   update    pf201

     c                   else
     c*
     c** Write new record

     c                   clear                   dbrcdf
     c*
     c                   eval      f2seq# = wcseq#                              Seq
     c                   eval      f2calc  = 'Y'                                Calc
     c*
     C                   movel     q1user        f2iusr
     C                   movel     q1pgm         f2ipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f2idt
     C                   movel     dstime        f2itm

     c                   exsr      mvscdb

     c                   write     dbrcdf

     c                   endif

     c                   endsr                                                  wrtupd

     C*** beg del *** 09/09/11 *********************************************
    C*================================================================
    C**KHDO - F8=Fringe processing
    C*================================================================
     C***  khdo          begsr
      ***
     C***                if        f2calc = ' '
     C***                eval      ##mgid = '2'
     C***                call      'WA727R'
     C***                parm                    ##mgid            1
     C***                parm                    ##rtcd            1
     C***  *inkl         parm                    ##kl              1
     c***                eval      *in90 = *on
     c***                leavesr
     c***                else
     c***                exsr      wrtupd
     c***                eval      *in90 = *off
     c***                endif
      ***
     C***                endsr
     C*** beg del *** 09/09/11 *********************************************
    C*================================================================
    C* KiDO - F9=WW wage calc
    C*================================================================
     C     kido          begsr
      *
     c                   eval      xxbeg = ' '
     C                   if        f2calc = ' '
     C                   eval      ##mgid = '2'
     C                   call      'WA727R'
     C                   parm                    ##mgid            1
     C                   parm                    ##rtcd            1
     C     *inkl         parm                    ##kl              1
     c                   eval      *in90 = *on
     c                   leavesr
     c                   endif
     c                   eval      *in90 = *off
     c                   eval      xxbeg =  'Y'
     c                   eval      @@beg =  xxbeg
     c                   eval      @@kc  =  '1'
     C                   endsr
    C*================================================================
    C* KGDO - F7=Calculate processing
    C*================================================================
     C     kgdo          begsr
     c                   exsr      valcalc

     c                   eval      i = 1
     c                   move      *zeros        xxfbco            9 2
     c                   move      *zeros        xxfbno            9 2
     c                   do        7
     c                   if        wm2(i) = 'Y'
     c                   eval      xxfbco = to2(i) + xxfbco
     c                   else
     c                   eval      xxfbno = to2(i) + xxfbno
     c                   endif
     c                   eval      i = i + 1
     c                   enddo

     c                   eval      scfbco = xxfbco
     c                   eval      scfbno = xxfbno

     c                   exsr      wrtupd

     C     @@rrn         chain     pwc01                              99
     c                   eval      wcnnpw = f2fbno
     c                   eval      wccalc = ' '
     c                   update    pwc01

     C                   endsr
    C*================================================================
    C*================================================================
    C* VALCALC- Vakidate
    C*================================================================
     C     valcalc       begsr


     c* Health insure
     c***************    if        schinc <> 'Y' and schinc <> 'N'
     c                   if        schinf <> *zeros  and
     c                             schinc <> 'Y' and schinc <> 'N'
     C                   eval      *in28 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        schinc = 'N' and schins = 0
     C                   eval      *in29 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        schins <>  0
     c                   eval      srdti = schins
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      *in30 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   else
     c                   movel     srdto         schins
     c                   endif                                                  srerr = '1'
     c                   endif

     c* Laundry cleaning:
     c*************      if        scldrc <> 'Y' and scldrc <> 'N'
     c                   if        sccdrf <> *zeros and
     c                             scldrc <> 'Y' and scldrc <> 'N'
     C                   eval      *in31 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scldrc = 'N' and scldrs = 0
     C                   eval      *in32 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scldrs <>  0
     c                   eval      srdti = scldrs
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      *in33 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   else
     c                   movel     srdto         scldrs
     c                   endif                                                  srerr = '1'
     c                   endif

     c* Clothing/Uniform:
     c*************      if        sccloc <> 'Y' and sccloc <> 'N'
     c                   if        scclof <> *zeros and
     c                             sccloc <> 'Y' and sccloc <> 'N'
     C                   eval      *in34 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        sccloc = 'N' and scclos = 0
     C                   eval      *in35 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scclos <>  0
     c                   eval      srdti = scclos
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      *in36 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   else
     c                   movel     srdto         scclos
     c                   endif                                                  srerr = '1'
     c                   endif

     c* Lodging/Rent:
     c**************     if        screnc <> 'Y' and screnc <> 'N'
     c                   if        screnf <> *zeros and
     c                             screnc <> 'Y' and screnc <> 'N'
     C                   eval      *in37 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        screnc = 'N' and screns = 0
     C                   eval      *in38 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        screns <>  0
     c                   eval      srdti = screns
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      *in39 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   else
     c                   movel     srdto         screns
     c                   endif                                                  srerr = '1'
     c                   endif

     c* Food/Meals:
     c*************      if        scfodc <> 'Y' and scfodc <> 'N'
     c                   if        scfodf <> *zeros  and
     c                             scfodc <> 'Y' and scfodc <> 'N'
     C                   eval      *in41 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scfodc = 'N' and scfods = 0
     C                   eval      *in42 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scfods <>  0
     c                   eval      srdti = scfods
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      *in43 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   else
     c                   movel     srdto         scfods
     c                   endif                                                  srerr = '1'
     c                   endif

     c* Vehical/Fuel:
     c**************     if        scfulc <> 'Y' and scfulc <> 'N'
     c                   if        scfulf <> *zeros and
     c                             scfulc <> 'Y' and scfulc <> 'N'
     C                   eval      *in44 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scfulc = 'N' and scfuls = 0
     C                   eval      *in45 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scfuls <>  0
     c                   eval      srdti = scfuls
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      *in46 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   else
     c                   movel     srdto         scfuls
     c                   endif                                                  srerr = '1'
     c                   endif
     c* Others::
     c**************     if        scothc <> 'Y' and scothc <> 'N'
     c                   if        scothf <> *zeros and
     c                             scothc <> 'Y' and scothc <> 'N'
     C                   eval      *in47 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scothc = 'N' and scoths = 0
     C                   eval      *in48 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   endif

     c                   if        scoths <>  0
     c                   eval      srdti = scoths
     c                   call      'SRCVTDAT'    datchk
     c                   if        srerr = '1'
     c                   eval      *in49 = '1'
     C                   eval      *in90 = '1'
     c                   leavesr
     c                   else
     c                   movel     srdto         scoths
     c                   endif                                                  srerr = '1'
     c                   endif

     c                   endsr                                                  valcalc
