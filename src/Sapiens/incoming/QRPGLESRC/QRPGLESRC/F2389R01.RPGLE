     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE F2389R01                                                                      *
     H**************************************************************************************
     H*  Description:  Medicare Zip Codes + 4 Maintenance                                  *
     H*                                                                                    *
     H*  Created: 09/18/12   Programmer C:284   Project:TX079                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     Ff2389f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftzpp     uf a e             disk
     F                                     rename(pzp01:dbrcdf)
     Fststl     if   e           k disk
     Fftzpl1    if   e           k disk    prefix(tp)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare Zip Codes + 4'
     D comapi          c                   'FA007R'
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
     D kkkey           s                   like(stkey)
     D kkefdt          s                   like(zpefdt)
     D kkst            s                   like(zpst)
     D kkzip           s                   like(zpzip)
     D kkpls4          s                   like(zppls4)
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
     C                   parm                    @@rrn
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
     C     Zpky          klist
     C                   kfld                    kkst
     C                   kfld                    kkzip
     C                   kfld                    kkpls4
     C                   kfld                    kkefdt
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*
     C     Zpky1         klist
     C                   kfld                    tpzpst
     C                   kfld                    tpzpzip
     C                   kfld                    tpzppls4
     C                   kfld                    tpzpefdt
    C*================================================================
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*** Beg Add *** 10/11/11 ****************************************
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
    C*
     C     chgsts        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
     C                   if        *in99 = *off
    C*
     C                   if        zpstat = 'I'
     C                   eval      zpstat = 'A'
     C                   else
     C                   eval      zpstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        zpuser
     C                   movel     q1pgm         zppgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         zpdt
     C                   movel     dstime        zptime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*** End Add *** 10/11/11 ****************************************
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
     C*
     C* If in ADD mode
     C                   if        *in40 = *on
     C* State must not be blank
     C                   if        scst = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   else
     C                   eval      kkkey = scst
     C     stky          chain     pst01
     C                   if        not %found
     C                   seton                                        769089
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C                   endif
     C*
     C                   if        sczip  = *zeros
     C                   seton                                        729085
     C                   leavesr
     C                   endif
     C*
     C                   If        *in40 = *On
     C                   If        (Sc4flg <> '1') and (Sc4flg <> '0')
     C                   Seton                                        779087
     C                   Leavesr
     C                   Endif
     C                   Endif
     C* Plus 4 zip code must not be blank
     C                   if        scpls4 = *zeros
     C                   seton                                        749086
     C                   leavesr
     C                   endif
     C*
     C*
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        759087
     C                   leavesr
     C                   else
     C*
     C* Effective Date must be valid
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        799087
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Check for duplicate records
     C*
     C                   eval      tpzpst = scst
     C                   eval      tpzpzip = sczip
     C                   eval      tpzppls4 = scpls4
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tpzpefdt
     C*
     C     Zpky1         chain     pzp01
     C                   if        %found
     C                   seton                                        789089
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
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
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   clear                   scst
     C                   clear                   sczip
     C                   clear                   scpls4
     C                   clear                   scefdt
     C                   clear                   sccarr
     C                   clear                   scloc
     C                   clear                   scrur
     C                   clear                   sc4flg
     C*
     C                   if        *in40 = *on
     C                   eval      scst = *blanks
     C                   eval      sczip = *zeros
     C                   eval      scpls4 = *zeros
     C                   eval      scefdt = *zeros
     C                   eval      sccarr = *zeros
     C                   eval      scloc  = *zeros
     C                   eval      scrur  = *blanks
     C                   eval      sc4flg = *blanks
     C                   eval      scstat = 'A'
     C*
     C                   else
     C*
     C                   eval      scst = zpst
     C                   eval      sczip = zpzip
     C                   eval      scpls4 = zppls4
     C                   z-add     zpefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   eval      sccarr = zpcarr
     C                   eval      scrur  = zprur
     C                   eval      scloc  = zploc
     C                   eval      sc4flg = zp4flg
     C                   eval      scstat = zpstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      zpst = scst
     C                   eval      zpzip = sczip
     C                   eval      zppls4 = scpls4
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         zpefdt
     C                   eval      zpcarr = sccarr
     C                   eval      zploc  = scloc
     C                   eval      zprur  = scrur
     C                   eval      zp4flg = sc4flg
     C                   eval      zpstat = scstat
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
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C     @@mode        comp      'C'                                    10
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
    C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        zpuser
     C                   movel     q1pgm         zppgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         zpdt
     c                   movel     dstime        zptime
     C                   update    dbrcdf
     C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        zpuser
     C                   movel     q1pgm         zppgm
     C                   movel     q1pgm         zpipgm
     C                   movel     q1user        zpiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         zpdt
     C                   movel     srdto         zpidt
     c                   movel     dstime        zptime
     c                   movel     dstime        zpitim
     C                   write     dbrcdf
     C                   feod      ftzpp
     C                   endsr
