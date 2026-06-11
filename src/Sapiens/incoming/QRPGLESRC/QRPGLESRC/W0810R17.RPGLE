     H*  (C) COPYRIGHT Fiserv Insurnace Solutions, Inc., 2007
     H*****************************************************************
     h/title W0810R17 - certificate holder Out of Country Addresses
     H*****************************************************************
     H*  DESCRIPTION:  This program will allow a user to change, view *
     H*                records in the Certificate Holder file and add *
     H*                Out of Country Codes and Province/Postal Codes *
     H*  CALLED FROM:                                                 *
     H*    W0810R - WORK WITH CERTIFICATE HOLDERS                     *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 02/28/06  PROGRAMMER: C:132 PROJECT: WC1304         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  21 - (WC32389) Country Code blank or invalid                 *
     H*  22 -                                                         *
     H*  23 -                                                         *
     H*  24 -                                                         *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0810f17  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     infds(fids01)
     fwmcep     uf a e             disk
     f                                     rename(pce01:dbrcdf)
     f                                     infds(dbfds)
     fwmcel4    if   e           k disk
     fwdelp     if   e             disk
     fwmeml     if   e           k disk
     fwmfnl     if   e           k disk
     f*******wmrsl     if   e           k disk
     f*******wtxcl     if   e           k disk
     fwtgtl2    if   e           k disk
     fwdf2l     if   e           k disk
     d*================================================================
     d*  Array & Table definitions
     d*================================================================
     d xxbl            s             50    dim(5)
     d xxal            s             50    dim(5)
     d*================================================================
     d*  Data Structures & constants definitions
     d*================================================================
     d*
     d*
     d dbfds           ds
     d  dbrrn                397    400b 0
     d*
     d svlda           ds          1024
     d  ldend#                 1      5  0
     d  ldenda                 1      5
     d  ldmyrr                 6     14  0
     d  ldeedt                15     22  0
     d  ldfyr                 23     25  0
     d  ldpyrl                26     26
     d*
     d optnm           c                   const('Certificate Holder -
     d                                     Information')
     d comapi          c                   const('WA001R')
     d agtapi          c                   const('WA002R')
     d audit           c                   const('Audited')
     d est             c                   const('Estimated')
     d #ofscr          c                   const(1)
     d*
     d fids01        e ds                  extname(szq2p)
      *
     d ##agt#          s              5  0
     d ##anam          s             60
     d ##co#           s              3  0
     d ##conm          s             40
     d ##div           s              5  0
     d ##emp#          s              9  0
     d ##fnam          s             40
     d ##fnd           s              3  0
     d ##fyr           s              3  0
     d ##pnam          s             60
     d ##prod          s              5  0
     d @@kc            s              1
     d @@kl            s              1
     d @@mode          s              1
     d @@rrn           s              9  0
     d @@rrn2          s              9  0
     d i               s              1  0
     d i1              s              3  0
     d xxedn           s             80
     d xxkey           s             10
     d xxtbid          s              4    inz('CTRY')
      *
      *================================================================
      *  *LIKE DEFN definitions
      *================================================================
      *
     d s#ctry          s                   like(cectry)  inz
     d s#prov          s                   like(ceprov)  inz
     d sysdt           s                   like(fnexdt)  inz
     d xxemp#          s                   like(ememp#)  inz
     d xxdiv           s                   like(emdiv)   inz
     d xxname          s                   like(emname)  inz
     d s#cer#          s                   like(cecer#)  inz
     d s#name          s                   like(cename)  inz
     d s#adr1          s                   like(ceadr1)  inz
     d s#adr2          s                   like(ceadr2)  inz
     d s#cty           s                   like(cecty)   inz
     d s#st            s                   like(cest)    inz
     d s#zip           s                   like(cezip)   inz
     d s#attn          s                   like(ceattn)  inz
      *
     d/copy scopymt01c
      *
      *================================================================
      *  PLIST Definitions
      *================================================================
     c     *entry        plist
     c                   parm      *inkc         @@kc
     c                   parm      *inkl         @@kl
     c                   parm                    @@mode
     c                   parm                    @@rrn
     c                   parm                    @@rrn2
      *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
      *
      *  Agent/Producer retrieve program parameter list
      *
     c     agtpl         plist
     c                   parm                    ##co#
     c                   parm      elfnd         ##fnd
     c                   parm      elemp#        ##emp#
     c                   parm      eldiv         ##div
     c                   parm      elfyr         ##fyr
     c     scagt#        parm                    ##agt#
     c                   parm                    ##anam
     c     scprod        parm                    ##prod
     c                   parm                    ##pnam
      *
      *  Agent/Producer retrieve program parameter list
      *
     c     agtpl2        plist
     c                   parm                    emco#
     c                   parm                    emfnd
     c                   parm                    ememp#
     c                   parm                    emdiv
     c                   parm      ldfyr         ##fyr
     c                   parm                    ##agt#
     c                   parm                    ##anam
     c                   parm                    ##prod
     c                   parm                    ##pnam
      *================================================================
      *  KLIST Definitions
      *================================================================
      *
      *  CERTIFICATE HOLDER FILE
      *
     c     ceky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    sccer#
     c                   kfld                    scfyr
      *
     c     ceky1         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
      *
      *  EMPLOYER MASTER FILE
      *
     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
      *
      *  GROUP MASTER FILE
      *
     c     fnky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     c     f2ky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
      *
      * KEY LIST for the Country Code Table file
      *
     c     gtky          klist
     c                   kfld                    elco#
     c                   kfld                    xxtbid
     c                   kfld                    xxkey
      *
      *
      *===============================================================
      * CHGSTS - SUBROUTINE FOR CHANGING STATUS
      *===============================================================
      *
     c     chgsts        begsr
      *
     c                   endsr                                                  CHGSTS
      *
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     c     dltpre        begsr
      *
     c                   endsr                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     c     dsplyr        begsr
     c                   endsr                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     c     errck1        begsr
      *
      *  Country Code cannot be blank or invalid
      *
     c  n90              if        (scctry = *blanks)
     c                   seton                                        21  90
     c                   endif                                                  SCEOLF
      *
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     c  n90@@rrn         chain(n)  dbrcdf                             99
      *
     c  n90              if        (cedt > savedt) or
     c                             (cedt = savedt) and
     c                             (cetime > savetm)
     c                   seton                                        93  90
     c                   endif                                                  CEDT>SAVEDT
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     c                   endsr                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     c     errck2        begsr
     c                   endsr                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     c     errck3        begsr
     c                   endsr                                                  ERRCK3
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     c     kjdo          begsr
     c                   endsr                                                  KJDO
      *
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     c     mvdbsc        begsr
      *
     c                   movel     ceadr1        scadr1
     c                   movel     ceadr2        scadr2
     c                   movel     cectry        scctry
     C                   move      ceprov        scprov
      *
     c                   endsr                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     c     mvscdb        begsr
      *
     c                   movel     scadr1        ceadr1
     c                   movel     scadr2        ceadr2
     c                   move      scctry        cectry
     c                   move      scprov        ceprov
      *
     c                   endsr                                                  MVSCDB
      *
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     c     otherr        begsr
     c                   endsr                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     c     othfks        begsr
      *
     c     *inkj         caseq     *on           kjdo                     90
     c                   endcs
      *
     c                   endsr                                                  OTHFKS
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     c     pgmend        begsr
      *
     c                   move      *on           *inlr
      *
     c                   endsr
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     c     pgmstr        begsr
      *
     c     @@mode        comp      '1'                                    09    INQUIRY
     c     @@mode        comp      '2'                                    09    INQUIRY
     c     @@mode        comp      '3'                                    09    INQUIRY
     c     @@mode        comp      'D'                                    09    INQUIRY
      *
     c                   movel(p)  optnm         ##data
      *
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      40            ##leng
     c                   movel     ##data        scprds
     c                   move      *on           dsptyp
      *
      *  GET EMPLOYER DETAIL RECORD
      *
     c     @@rrn2        chain     wdelp                              60
      *
     c                   eval      scfyr = elfyr
      *
     c                   if        (elfaf = 'A')
     c                   movel(p)  audit         scpyst
     c                   else
     c                   movel(p)  est           scpyst
     c                   endif                                                   ELFAF=A
      *
      *  GET EMPLOYER MASTER RECORD
      *
     c     emky          chain     pem01                              60
      *
     c                   eval      scfnd = emfnd
     c                   eval      scemp# = ememp#
     c                   eval      scdiv = emdiv
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     c                   call      'WA005R'
     c                   parm      ememp#        xxemp#
     c                   parm      emdiv         xxdiv
     c                   parm      emname        xxname
     c                   parm                    xxedn
      *
     c                   movel     xxedn         scedn
      *
      *
      *  GET GROUP INFORMATION
      *
     c     fnky          chain     pfn01                              60
     c     f2ky          chain     pf201                              60
      *
     c                   movel     fnname        scname
      *
     c                   time                    dsdttm
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysdt
      *
     c                   if        (fnexdt <> *zeros) and
     c                             (not *in09)
     c     sysdt         comp      fnexdt                             09  09
     c                   endif                                                  FNEXDT<>*ZEROS
      *
      *  GET AGENT/PRODUCER INFORMATION
      *
     c                   call      agtapi        agtpl
      *
     c                   movel     ##anam        scanam
     c                   movel     ##pnam        scpnam
      *
      *  SAVE THE OLD DATA BASE VALUES
      *
     c     @@rrn         chain(n)  dbrcdf                             99
      *
     c                   if        (*in99)
     c                   clear                   s#cer#
     c                   clear                   s#name
     c                   clear                   s#adr1
     c                   clear                   s#adr2
     c                   clear                   s#cty
     c                   clear                   s#st
     c                   clear                   s#zip
     c                   clear                   s#attn
     c                   clear                   s#ctry
     c                   clear                   s#prov
      *
     c                   else
      *
     c                   movel     cecer#        sccer#
      *
     c                   move      cecer#        s#cer#
     c                   move      cename        s#name
     c                   move      ceadr1        s#adr1
     c                   move      ceadr2        s#adr2
     c                   move      cecty         s#cty
     c                   move      cest          s#st
     c                   move      cezip         s#zip
     c                   move      ceattn        s#attn
     c                   move      cectry        s#ctry
     c                   move      ceprov        s#prov
      *
     c                   endif
      *
     c                   endsr                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     c     preoth        begsr
     c                   endsr
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     c     updatr        begsr
      *
     c     @@rrn         chain     dbrcdf                             99
      *
b02  c                   if        (not *in99)
      *
     c                   exsr      mvscdb
      *
     c                   movel     q1user        ceuser
     c                   movel     q1pgm         cepgm
     c                   time                    dsdttm
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cedt
     c                   movel     dstime        cetime
      *
     c                   update    dbrcdf
      *
e02  c                   endif                                                  *IN99=*OFF
      *
     c                   move      *on           outflg
      *
     c                   endsr                                                  UPDATR
      *
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
      *===============================================================
      *
     c     writer        begsr
      *
     c                   clear                   dbrcdf
      *
     c                   eval      ceco# = elco#
     c                   eval      cefnd = elfnd
     c                   eval      ceemp# = elemp#
     c                   eval      cediv = eldiv
     c                   eval      cefyr = elfyr
      *
     c                   exsr      mvscdb
      *
     c                   movel     q1user        ceiusr
     c                   movel     q1user        ceuser
     c                   movel     q1pgm         ceipgm
     c                   movel     q1pgm         cepgm
      *
     c                   time                    dsdttm
      *
     c                   movel     dstime        ceitm
     c                   movel     dstime        cetime
      *
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         ceidt
     c                   movel     srdto         cedt
      *
     c                   write     dbrcdf
      *
     c                   feod      wmcep
      *
     c                   endsr                                                  WRITER
      *===============================================================
