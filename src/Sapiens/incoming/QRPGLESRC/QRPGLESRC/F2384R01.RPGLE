     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H/TITLE F2384R01                                                                      *
     H**************************************************************************************
     H*  Description:  CMG Code Maintenance                                                *
     H*                                                                                    *
     H*  Created: 10/17/11   Programmer C:274   Project:114MAR                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2384f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmmgp     uf a e             disk
     F                                     rename(pmg01:dbrcdf)
     Ffmmgl1    if   e           k disk
     F                                     prefix(tm)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'CMG Codes'
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
     D kkefdt          s                   like(mgefdt)
     D kkcode          s                   like(mgcode)
     D kktier          s                   like(mgtier)
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
     C     mgky          klist
     C                   kfld                    kkcode
     C                   kfld                    kktier
     C                   kfld                    kkefdt
     C*
     C     mgky1         klist
     C                   kfld                    tmmgcode
     C                   kfld                    tmmgtier
     C                   kfld                    tmmgefdt
    C*================================================================
    C*  *LIKE DEFN statements
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
     C* CMG code must not be blank
     C                   if        *in40 = *on and
     C                             sccode = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   endif
     C*
     C* Effective date should not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        729087
     C                   leavesr
     C                   else
     C*
     C* Effective Date must be valid
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779087
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Description must not be blank
     C                   if        *in40 = *on and
     C                             scdesc = *blanks
     C                   seton                                        739088
     C                   leavesr
     C                   endif
     C*
     C* Tier must not be blank
     C                   if        sctier = *blanks
     C                   seton                                        749086
     C                   leavesr
     C                   endif
     C*
     C* Check for duplicate records
     C                   eval      tmmgcode = sccode
     C                   eval      tmmgtier = sctier
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tmmgefdt
     C*
     C     mgky1         chain     pmg01
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
     C                   clear                   sccode
     C                   clear                   scefdt
     C                   clear                   scdesc
     C                   clear                   sctier
     C                   clear                   screlw
     C                   clear                   scalos
     C                   clear                   scstat
     C*
     C                   if        *in40 = *on
     C                   eval      sccode = *blanks
     C                   eval      scefdt = *zeros
     C                   eval      scdesc = *blanks
     C                   eval      sctier = *blanks
     C                   eval      screlw = *zeros
     C                   eval      scalos = *zeros
     C                   eval      scstat = 'A'
     C*
     C                   else
     C*
     C                   eval      sccode = mgcode
     C                   z-add     mgefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   eval      sctier = mgtier
     C                   eval      scdesc = mgdesc
     C                   eval      scalos = mgalos
     C                   eval      screlw = mgrelw
     C                   eval      scstat = mgstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      mgcode = sccode
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgefdt
     C                   eval      mgtier = sctier
     C                   eval      mgdesc = scdesc
     C                   eval      mgrelw = screlw
     C                   eval      mgalos = scalos
     C                   eval      mgstat = scstat
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
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        mguser
     C                   movel     q1pgm         mgpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
     c                   movel     dstime        mgtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        mguser
     C                   movel     q1pgm         mgpgm
     C                   movel     q1pgm         mgipgm
     C                   movel     q1user        mgiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
     C                   movel     srdto         mgidt
     c                   movel     dstime        mgtime
     c                   movel     dstime        mgitim
     C                   write     dbrcdf
     C                   feod      fmmgp
     C                   endsr
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
     C                   if        mgstat = 'I'
     C                   eval      mgstat = 'A'
     C                   else
     C                   eval      mgstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        mguser
     C                   movel     q1pgm         mgpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         mgdt
     C                   movel     dstime        mgtime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
