     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE F2373R01                                                                      *
     H**************************************************************************************
     H*  Description:  Medicare Anesthesia Conversion Factors  Maintenance                 *
     H*                                                                                    *
     H*  Created: 08/12/10   Programmer C:251   Project:114MWS                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/11/11   C:280    Project : SEQ78229                                            *
     H*                      Added option 20 for status change                             *
     H*                                                                                    *
     H*                                                                                    *
     H**************************************************************************************
     H actgrp(*caller)
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2373f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmacp     uf a e             disk
     F                                     rename(pac01:dbrcdf)
     Fststl     if   e           k disk
     Ffmacl1    if   e           k disk    prefix(tm)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare Anesthesia Conversion'
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
     D kkefdt          s                   like(acefdt)
     D kkst            s                   like(acst)
     D kkloc           s                   like(acloc)
     D*
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
    C*================================================================
     C     acky          klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    kkefdt
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*
     C     acky1         klist
     C                   kfld                    tmacst
     C                   kfld                    tmacloc
     C                   kfld                    tmacefdt
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
     C                   if        acstat = 'I'
     C                   eval      acstat = 'A'
     C                   else
     C                   eval      acstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        acuser
     C                   movel     q1pgm         acpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         acdt
     C                   movel     dstime        actime
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
     C* Description should not be blank
     C*
     C                   if        scdesc = *blanks
     C                   seton                                        759050
     C                   leavesr
     C                   endif
     C*
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        739087
     C                   leavesr
     C                   else
     C*
     C* Effective Date must be valid
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779087
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Check for duplicate records
     C*
     C                   eval      tmacst = scst
     C                   eval      tmacloc = scloc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tmacefdt
     C*
     C     acky1         chain     pac01
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
     C                   clear                   scloc
     C                   clear                   scefdt
     C                   clear                   scdesc
     C                   clear                   sccf
     C*
     C                   if        *in40 = *on
     C                   eval      scst = *blanks
     C                   eval      scloc = *zeros
     C                   eval      scefdt = *zeros
     C                   eval      scdesc = *blanks
     C                   eval      sccf   = *zeros
     C                   eval      scstat = 'A'
     C*
     C                   else
     C*
     C                   eval      scst = acst
     C                   eval      scloc = acloc
     C*
     C                   z-add     acefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   movel     acdesc        scdesc
     C                   eval      sccf   = accf
     C                   eval      scstat = acstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      acst = scst
     C                   eval      acloc = scloc
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         acefdt
     C*
     C                   eval      acdesc = scdesc
     C                   eval      accf   = sccf
     C                   eval      acstat = scstat
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
    C*** Beg Add *** 10/11/11 ****************************************
     C     @@mode        comp      'C'                                    10
    C*** End Add *** 10/11/11 ****************************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
    C*
    C*** Beg Add *** 10/11/11 ****************************************
    C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
    C*
    C*** End Add *** 10/11/11 ****************************************
    C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C*
     C                   movel     q1user        acuser
     C                   movel     q1pgm         acpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         acdt
     C                   movel     dstime        actime
     C*
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C*
     C                   movel     q1user        acuser
     C                   movel     q1pgm         acpgm
     C                   movel     q1pgm         acipgm
     C                   movel     q1user        aciusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         acdt
     C                   movel     srdto         acidt
     c                   movel     dstime        actime
     c                   movel     dstime        acitim
     C*
     C                   write     dbrcdf
     C                   feod      fmacp
     C                   endsr
