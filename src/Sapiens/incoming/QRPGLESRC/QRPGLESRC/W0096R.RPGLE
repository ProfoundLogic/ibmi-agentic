     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H*****************************************************************
     H/TITLE W0096R - W/W Premium Entry program.
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*                                                               *
     H* CALLED FROM: W0051R, W0051R02                                 *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: C:255                                             *
     H* CREATED: 12/17/10               PROJECT: 71WLD                *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/30/10 C:136 Project: 71WLD Priss 44                        *
     H*               -Eff Dt and Trans Dt are not being populated    *
     H*                in WDEHP                                       *
     H*               -Correct writing of WDEHP records, a new record *
     H*                should be written each time, no info should    *
     H*                carry over from the previous record.           *
     H*****************************************************************
     Fw0096f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      * Employer History
     Fwdehp     uf a e             disk
     F                                     rename(peh01:dbrcdf)
     F                                     infds(dbfds)
     Fwdehl12   if   e           k disk    rename(peh01 : peh02)
      * NCCI Submission History
     Fwmshl1    uf a e           k disk
      * Agent History Master File
     Fwmahl     if   e           k disk
      * Employer Detail file
     Fwdelp     if   e             disk
     F                                     rename(pel01:pel0p)
     Fwdell8    if   e           k disk
     F                                     rename(pel01:pel03)
      * Group Master
     Fwmfnl     if   e           k disk    rename(pfn01 : pfn0l)
      * Employer Master
     Fwmemp     if   e             disk
     Fwmeml     if   e           k disk    rename(pem01 : pem0l)
     Fwtgtl     if   e           k disk
     Fwmrsl     if   e           k disk
    D*=====================================================================
    D*  /COPY required constants
    D*=====================================================================
     D optnm           c                   'Employer Premium Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*=====================================================================
    D* Stand Alone Fields - TOP
    D*=====================================================================
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@elrrn         s              9  0
     D sccx            s              7  0
     D x               s              2  0
     D i1              s              3  0
     D                 DS                  INZ
     D EF                             7  0 DIM(5)
     D emefd1                  1      7  0
     D emefd2                  8     14  0
     D emefd3                 15     21  0
     D emefd4                 22     28  0
     D emefd5                 29     35  0
     D
     D                 DS                  INZ
     D cx                             7  0 DIM(5)
     D emcad1                  1      7  0
     D emcad2                  8     14  0
     D emcad3                 15     21  0
     D emcad4                 22     28  0
     D emcad5                 29     35  0
     D dbfds           ds
     D dbrrn                 397    400b 0
     D*
     D fids01        e ds                  extname(szq2p)
     D xxagt#          s                   like(ahagt#)
     D xxco#           s                   like(elco#)
     D xxdiv           s                   like(eldiv)
     D xxemp#          s                   like(elemp#)
     D xxfnd           s                   like(elfnd)
     D xxfyr           s                   like(elfyr)
     D xxefdt          s                   like(elpfdt)
     D xxname          s                   like(emname)
     D xxtbid          s                   like(gttbid)
     D xxkey           s                   like(gtkey)
     D agtapi          c                   const('WA002R')
     D*
     D                 ds
     D  sst                    1     30    dim(15)
     D  shst01                 1      2
     D  shst02                 3      4
     D  shst03                 5      6
     D  shst04                 7      8
     D  shst05                 9     10
     D  shst06                11     12
     D  shst07                13     14
     D  shst08                15     16
     D  shst09                17     18
     D  shst10                19     20
     D  shst11                21     22
     D  shst12                23     24
     D  shst13                25     26
     D  shst14                27     28
     D  shst15                29     30
     D                 ds
     D  sss                    1     15    dim(15)
     D  shss01                 1      1
     D  shss02                 2      2
     D  shss03                 3      3
     D  shss04                 4      4
     D  shss05                 5      5
     D  shss06                 6      6
     D  shss07                 7      7
     D  shss08                 8      8
     D  shss09                 9      9
     D  shss10                10     10
     D  shss11                11     11
     D  shss12                12     12
     D  shss13                13     13
     D  shss14                14     14
     D  shss15                15     15
     D                 ds
     D  ssc                    1     15    dim(15)
     D  shcd01                 1      1
     D  shcd02                 2      2
     D  shcd03                 3      3
     D  shcd04                 4      4
     D  shcd05                 5      5
     D  shcd06                 6      6
     D  shcd07                 7      7
     D  shcd08                 8      8
     D  shcd09                 9      9
     D  shcd10                10     10
     D  shcd11                11     11
     D  shcd12                12     12
     D  shcd13                13     13
     D  shcd14                14     14
     D  shcd15                15     15
     C/COPY SCOPYMT01C
    C* ====================================================================
    C*  PLIST definitions
    C*=====================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@mode            1
     C                   parm                    @@rrn2            9 0          wdelp
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     agtpl         plist
     C                   parm                    ##co#
     C                   parm      emfnd         ##fnd
     C                   parm      ememp#        ##emp#            9 0
     C                   parm      emdiv         ##div             5 0
     C                   parm                    ##fyr             3 0
     C     scagt#        parm                    ##agt#            5 0
     C                   parm                    ##anam           60
     C     scprod        parm                    ##prod            5 0
     C                   parm                    ##pnam           60
     C                   parm      *zeros        ##area            3 0
     C                   parm      *zeros        ##phon            7 0
     C                   parm      '*'           ##plan            1
     C                   parm      *zeros        ##date            8 0
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     elky          klist
     C                   kfld                    elco#
     C                   kfld                    elfnd
     c                   kfld                    elemp#
     C                   kfld                    eldiv
     C                   kfld                    elfyr
     C*
     C     epky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    scfyr
     C                   kfld                    ememp#
     C                   kfld                    emdiv
     C*
     C     ehky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
     C*
     C     elky2         KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    xxemp#
     C                   KFLD                    xxfyr
     C*
     C     rsky1         klist
     C                   kfld                    xxco#
     C                   kfld                    @@rtcd
    C*
     C     gtky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxtbid
     C                   kfld                    xxkey
    C*
     c     shky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxfyr
     c                   kfld                    xstype
     C*
     C     ahky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfyr
    C*
     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     C*
     c     emky2         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     C*
     c     fnky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
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
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     errck1        begsr
     C*Validate transaction date
     C                   z-add     sctrdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sctrdt
     C*
     C  n90srerr         ifeq      *on
     C                   seton                                        24  90
     C                   endif                                                  srerr=*on
     C*Validate effective date
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C  n90srerr         ifeq      *on
     C                   seton                                        25  90
     C                   endif                                                  srerr=*on
     C*  Effective date  must be within policy year range.
     C*
     C                   Z-ADD     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxefdt
     C*
     C  n90xxefdt        iflt      elpfdt
     C                   seton                                        27  90
     C                   endif                                                  xxefdt<elpfdt
     C*
     C  n90xxefdt        ifge      elptdt
     C                   seton                                        27  90
     C                   endif                                                  xxefdt>=elptdt
     C*
     C*Validate Final Audit Flag (E/A)
     C  n90              if        scfaf <> 'E' and scfaf <> 'A'
     C                   seton                                        78  90
     C                   endif
     C*Chain WDELP File to get Policy dates
     C*    @@rrn2        chain     pel0p                              60
      *  Retrieve current date
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt              7 0
      *  Check whether Employer is cancelled (Retrieve cancellation date)
     C                   z-add     0             sccx
     C                   z-add     1             i1
     C     *zeros        lookup    ef(i1)                                 61
     C     *in61         ifeq      *on
     C                   sub       1             i1
     C                   else
     C                   z-add     5             i1
     C                   endif                                                  *IN61 IFEQ *ON
     C*
     C     i1            ifne      0
     C                   z-add     cx(i1)        sccx
     C                   endif                                                  I1 IFNE 0
     C     sccx          ifgt      elptdt
     C                   z-add     0             sccx
     C                   endif
     C*
     C  n90              select
     C                   when      scfaf = 'A'
     C*
     C  n90sccx          ifeq      0
     C     xxdt          andlt     elptdt
     C     elpfdt        andne     elptdt
     C                   move      *on           *in37
     C                   move      *on           *in90
     C                   endif
     C*
     C* when flag set to 'E'
     C                   when      scfaf = 'E'
     C  n90sccx          ifne      0
     C     xxdt          orgt      elptdt
     C     elpfdt        andne     elptdt
     C                   move      *on           *in26
     C                   move      *on           *in90
     C                   endif
     C*
     C  n90              if         sccx <>0
     C                   move      *on           *in38
     C                   move      *on           *in90
     C                   endif
     C*
     C  n90              if        newrcd = 'Y'
     C                   eval      scfaf = 'E'
     C                   else
     C                   eval      scfaf = 'R'
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr                                                  errck1
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
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     mvdbsc        begsr
     C                   move      ehefdt        scefdt
     C                   move      ehtrdt        sctrdt
     C                   move      ehnprm        scnprm
     C                   move      ehprm         scprm
     C                   move      ehfaf         scfaf
     C                   endsr                                                  mvdbsc
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     mvscdb        begsr
      *** beg chg ***** 12/30/10 *************************************
     C***********        move      scefdt        ehefdt
     C***********        move      sctrdt        ehtrdt
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ehefdt
     C                   z-add     sctrdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ehtrdt
      *** end chg ***** 12/30/10 *************************************
     C                   move      scnprm        ehnprm
     C                   move      scprm         ehprm
     C                   move      scfaf         ehfaf
     C                   endsr                                                  mvscdb
     C*
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      @@mode = ' '
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@rrn2        chain     pel0p
     c                   eval      scfnd = elfnd
     c                   eval      scfyr = elfyr
     c*Chain Fund Master file
     c     fnky          chain     pfn0l
     c*Chain Employee Master file to get total no. of divisions for an employer.
     C     emky2         chain     pem0l                              60
     C                   z-add     0             sc#div
     C                   dow       *in60= *off
     C                   add       1             sc#div
     C     emky2         reade     pem0l                                  60
     C                   enddo
     c*Chain Employee Master file
     c     emky          chain     pem0l
     c                   eval      scname = fnname
     C*
     C                   move      elco#         xxco#
     C                   move      elfnd         xxfnd
     C                   move      elemp#        xxemp#
     C                   move      eldiv         xxdiv
     C                   move      elfyr         xxfyr
     C*
     C                   call      'WA005R'
     C                   parm                    xxemp#
     C                   parm                    xxdiv
     C                   parm      emname        xxname
     C                   parm                    xxedn            80
     C*
     C                   movel     xxedn         scedn
     C                   move      'P'           @@mode
     C                   eval      *in40 = *on
     C* Check record is already in file WDEHP (Check for Revised bill)
     C     ehky          setgt     peh02
     C     ehky          readpe    peh02                                  60
     C                   if        *in60 = *on
     C                   move      'Y'           newrcd            1
     C                   else
     C                   move      'N'           newrcd
     C                   endif
     C*
     C*  Get Agent/Producer Information
     C*
     C                   z-add     *zeros        ##fyr
     C                   call      'WA002R'      agtpl
     C                   movel     ##anam        scanam
     C                   movel     ##pnam        scpnam
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
     C                   endsr
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     writer        begsr
     C*
     C                   clear                   dbrcdf
      *** beg del ***** 12/31/10 ***********************************************
     C***********        if        newrcd = 'Y'
      *** end del ***** 12/31/10 ***********************************************
     C                   move(p)   elco#         ehco#
     C                   move(p)   elFND         ehfnd
     C                   move(p)   elfyr         ehfyr
     C                   move(p)   elemp#        ehemp#
     C                   move(p)   eldiv         ehdiv
     C                   move(p)   elbscd        ehbscd
     C                   move(p)   elpfdt        ehpfdt
     C                   move(p)   elptdt        ehptdt
     C                   move(p)   elandt        ehandt
     C                   move(p)   elrtcd        ehrtcd
     C                   move(p)   elasp2        ehasp2
     C                   move(p)   eldvef        ehdvef
     C                   move(p)   eldvex        ehdvex
     C                   move(p)   elpror        ehpror
     C                   move(p)   elprtd        ehprtd
     C                   move(p)   elprtf        ehprtf
     C                   move(p)   elwvfl        ehwvfl
     C                   move(p)   elcref        ehcref
     C                   move(p)   elcrex        ehcrex
     C                   move(p)   eledc1        ehwdc1
     C                   move(p)   eledc2        ehwdc2
     C                   move(p)   elasp3        ehasp3
     C                   move(p)   eldisc        ehdisc
     C                   move(p)   eledc3        ehwdc3
     C                   move(P)   elexm1        ehexm1
     C                   move(p)   elemd1        ehemd1
     C                   move(p)   elexp1        ehwxp1
     C                   move(p)   elexm2        ehexm2
     C                   move(p)   elemd2        ehemd2
     C                   move(p)   elexp2        ehwxp2
     C                   move(p)   elexm3        ehexm3
     C                   move(p)   elexm3        ehexm3
     C                   move(p)   elemd3        ehemd3
     C                   move(p)   elexp3        ehwxp3
     C                   move(p)   elilcd        ehilcd
     C                   move(p)   elilef        ehilef
     C                   move(p)   elilex        ehilex
     C                   move(p)   elein1        ehwin1
     C                   move(p)   elinl2        ehi2cd
     C                   move(p)   eli2ef        ehi2ef
     C                   move(p)   eli2ex        ehi2ex
     C                   move(p)   elein2        ehwin2
     C                   move(p)   eldcde        ehdcde
     C                   move(p)   elddct        ehddct
     C                   move(p)   elcxbl        ehcxbl
     C                   move(p)   eldcef        ehdcef
     C                   move(p)   eldcex        ehdcex
     C                   move(p)   eledct        ehwdct
     C                   move(p)   eldcd2        ehdcd2
     C                   move(p)   eldd2p        ehdd2p
     C                   move(p)   el2def        eh2def
     C                   move(p)   el2dex        eh2dex
     C                   move(p)   eledd2        ehwdd2
     C                   move(p)   elaspm        ehaspm
     C                   move(p)   elmcef        ehmcef
     C                   move(p)   elmcex        ehmcex
     C                   move(p)   eledcm        ehwdcm
     C                   move(p)   elasps        ehasps
     C                   move(p)   elscef        ehscef
     C                   move(p)   elscex        ehscex
     C                   move(p)   eledcs        ehwdcs
     C                   move(p)   elstat        ehstat
     C                   move(p)   q1user        ehuser
     C                   move(p)   q1user        ehiusr
     C                   move(p)   q1pgm         ehpgm
      *** beg chg ***** 12/30/10 ***********************************************
     C***********        eval      ehtime = dstime
     C***********        z-add     dsdate        srdti
     C***********        call      'SRCVTDAT'    dtcymd
     C***********        movel     srdto         xxefdt
     C***********        eval      ehdt   = xxefdt
     C***********        eval      ehidt  = ehdt
     C***********        eval      ehtrdt = sctrdt
     c                   eval      ehdt   = xxdt
     c                   eval      ehidt  = xxdt
     c                   eval      ehtime = dstime
      *** end chg ***** 12/30/10 ***********************************************
     C                   move(p)   eldev2        ehdev2
     C                   move(p)   eledv2        ehwdv2
     C                   move(p)   eld2ef        ehd2ef
     C                   move(p)   eld2ex        ehd2ex
     C                   move(p)   eldbcr        ehdbcr
     C                   move(p)   elcmul        ehcmul
     C                   move(p)   eledcr        ehwdcr
     C                   move(p)   eledc         ehwdc
     C                   move(p)   elesbj        ehwsbj
     C                   move(p)   elempm        ehwmpm
     C                   move(p)   elefrs        ehwfrs
     C                   move(p)   eltxpr        ehtxpr
     C                   move(p)   ellfef        ehlfef
     C                   move(p)   elavcc        ehavcc
     C                   move(p)   elein2        ehwin2
     C                   move(p)   elsdef        ehsdef
     C                   move(p)   elsdex        ehsdex
     C                   move(p)   eledcr        ehwdcr
     C                   z-add     elewav        ehwwav
     C                   move(p)   elnwrw        ehnwrw
     C                   z-add     elloss        ehwlos
     C                   z-add     eleter        ehwter
     C                   z-add     eledtr        ehwdtr
     C                   z-add     elrpe1        ehwrp1
     C                   z-add     elrpe2        ehwrp2
     C                   z-add     elrpe3        ehwrp3
     C                   z-add     elvlca        ehwvlc
     C                   z-add     elhxmn        ehwhxm
     C                   z-add     elarca        eharca
     C                   z-add     elarcp        eharcp
     C                   z-add     elaref        eharef
     C                   z-add     elarex        eharex
     C                   z-add     elearc        ehwarc
     C*
      *** beg del ***** 12/31/10 ***********************************************
     C***********        else
     C*****ehky**        setgt     peh02
     C*****ehky**        readpe    peh02
     C***********        endif
      *** end del ***** 12/31/10 ***********************************************
     C*
     C                   exsr      mvscdb
     C*
     C                   select
     C                   when      ehfaf = 'E'
     C                   eval      ehfaf2 ='E'
     C                   when      ehfaf = 'R'
     C                   eval      ehfaf2 ='E'
     C                   when      ehfaf = 'A'
     C                   eval      ehfaf2 =' '
     C*
     C                   endsl
     C                   write     dbrcdf
     C*Write records in WMSHP file if not present.
     C                   move      'P'           xstype            1
     C     shky          chain     psh01                              50
     C                   if        *in50 = *on
     C                   clear                   psh01
     C                   move      xxco#         shco#
     C                   move      xxfnd         shfnd
     C                   move      xxemp#        shemp#
     C                   move      xxdiv         shdiv
     C                   move      xxfyr         shfyr
     C                   eval      shagt# = *zero
     C     ahky          setgt     pah01
     C                   readp     pah01                                  60
     C                   dow       *in60 = *off
     C                             and ahstat = 'I'
     C                   readp     pah01                                  60
     C                   enddo
     C*
     C                   if        *in60 = *off
     C                             and xxco# = ahco#
     C                             and xxfnd = ahfnd
     C                             and xxemp# = ahemp#
     C                             and xxdiv = ahdiv
     C                             and xxfyr = ahfyr
     C                   movel     ahagt#        xxagt#
     C                   else
     C                   movel     ahagt#        xxagt#
     C                   endif
     C*
     C                   move      xxagt#        ahagt#
     C                   move      'P'           shtype
     C                   move      '4'           shstat
     C                   move      elpfdt        shpfdt
     C                   movel     q1user        shuser
     C                   movel     q1pgm         shpgm
     C                   movel     q1user        shiusr
     C                   TIME                    DSDTTM
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         emdt
     C                   move      dstime        shtime
     C                   move      emdt          shdt
     C                   z-add     shdt          shidt
     C                   eval      shunid = 0
     C                   eval      shsbdt = 0
     C*
     C     1             do        15            d                 2 0
     C                   clear                   sst(d)
     C                   enddo
     C*
     C                   movel(p)  'NCCI'        xxtbid
     C                   eval       x=1
     C*
     C     elky2         setll     pel03
     C     elky2         reade     pel03                                  60
     C     *IN60         doweq     *off
     C                   movel     elrtcd        @@RTCD            3
     C     rsky1         chain     prs01
     C                   move      rsjrst        xxst              2
     C                   move      xxst          xxkey
     C*
     C     gtky          chain     pgt01                              60
    C*
     C                   if        *in60 = *off
     C                   move      'XX'          xzst              2
     C                   z-add     1             i                 2 0
     C     xzst          lookup    sst(i)                                 60
     C                   if        *in60 = *off
     C                   move      xzst          sst(x)
     C                   eval       x= x+1
     C                   endif
     C*
     C                   else
     C                   move      xxst          xzst
     C                   eval      i = 1
     C     xzst          lookup    sst(i)                                 60
     C                   if        *in60 = *off
     C                   move      xzst          sst(x)
     C                   endif
     C                   endif                                                  1st *in60 file
    C*
     C                   enddo                                                  *in60 =wdelp
     C*
     C                   clear                   sss
     C                   clear                   ssc
     C*
     C                   if        ehfaf = 'E'
     C                   eval      shcode = 'O'
     C                   endif
     C                   if        ehfaf = 'R'
     C                   eval      shcode = 'R'
     C                   endif
     C*
     C     1             do        15            d                 2 0
     C                   if        sst(d) <> *blanks
     C
     C                   eval      sss(d) = '4'
     C                   eval      ssc(d) = shcode
     C                   endif
     C                   enddo
     C*
     C                   write     psh01
     C                   endif                                                  *in50 = *on
     C*
     C                   endsr
