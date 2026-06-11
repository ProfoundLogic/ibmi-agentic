     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H/TITLE F2377R01                                                                      *
     H**************************************************************************************
     H*  Description:  RUG IV Code Maintenance                                             *
     H*                                                                                    *
     H*  Created: 10/06/11   Programmer C:251   Project:114MZO                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2377f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmrgp     uf a e             disk
     F                                     rename(prg01:dbrcdf)
     Ffmrgl1    if   e           k disk
     F                                     prefix(tm)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'RUG IV Codes'
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
     D kkefdt          s                   like(rgefdt)
     D kkcat           s                   like(rgcat)
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
     C     rgky          klist
     C                   kfld                    kkcat
     C                   kfld                    kkefdt
     C*
     C     rgky1         klist
     C                   kfld                    tmrgcat
     C                   kfld                    tmrgefdt
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
     C* RUG code must not be blank
     C                   if        *in40 = *on and
     C                             sccat = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   endif
     C*
     C* Effective date should not be blank (If in ADD mode)
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
     C                   eval      tmrgcat = sccat
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tmrgefdt
     C*
     C     rgky1         chain     prg01
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
     C                   clear                   sccat
     C                   clear                   scefdt
     C                   clear                   scunin
     C                   clear                   scrnin
     C                   clear                   scutin
     C                   clear                   scrtin
     C                   clear                   scunc
     C                   clear                   scrnc
     C                   clear                   scutcp
     C                   clear                   scrtcp
     C                   clear                   scunct
     C                   clear                   scrnct
     C                   clear                   scuncc
     C                   clear                   scrncc
     C                   clear                   scutot
     C                   clear                   scrtot
     C                   clear                   scstat
     C*
     C                   if        *in40 = *on
     C                   eval      sccat = *blanks
     C                   eval      scefdt = *zeros
     C                   eval      scunin = *zeros
     C                   eval      scrnin = *zeros
     C                   eval      scutin = *zeros
     C                   eval      scrtin = *zeros
     C                   eval      scunc  = *zeros
     C                   eval      scrnc  = *zeros
     C                   eval      scutcp = *zeros
     C                   eval      scrtcp = *zeros
     C                   eval      scunct = *zeros
     C                   eval      scrnct = *zeros
     C                   eval      scuncc = *zeros
     C                   eval      scrncc = *zeros
     C                   eval      scutot = *zeros
     C                   eval      scrtot = *zeros
     C                   eval      scstat = 'A'
     C*
     C                   else
     C*
     C                   eval      sccat = rgcat
     C                   z-add     rgefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   eval      scunin = rgunin
     C                   eval      scrnin = rgrnin
     C                   eval      scutin = rgutin
     C                   eval      scrtin = rgrtin
     C                   eval      scunc  = rgunc
     C                   eval      scrnc  = rgrnc
     C                   eval      scutcp = rgutcp
     C                   eval      scrtcp = rgrtcp
     C                   eval      scunct = rgunct
     C                   eval      scrnct = rgrnct
     C                   eval      scuncc = rguncc
     C                   eval      scrncc = rgrncc
     C                   eval      scutot = rgutot
     C                   eval      scrtot = rgrtot
     C                   eval      scstat = rgstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      rgcat = sccat
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rgefdt
     C                   eval      rgunin = scunin
     C                   eval      rgrnin = scrnin
     C                   eval      rgutin = scutin
     C                   eval      rgrtin = scrtin
     C                   eval      rgunc  = scunc
     C                   eval      rgrnc  = scrnc
     C                   eval      rgutcp = scutcp
     C                   eval      rgrtcp = scrtcp
     C                   eval      rgunct = scunct
     C                   eval      rgrnct = scrnct
     C                   eval      rguncc = scuncc
     C                   eval      rgrncc = scrncc
     C                   eval      rgutot = scutot
     C                   eval      rgrtot = scrtot
     C                   eval      rgstat = scstat
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
     C                   movel     q1user        rguser
     C                   movel     q1pgm         rgpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rgdt
     c                   movel     dstime        rgtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        rguser
     C                   movel     q1pgm         rgpgm
     C                   movel     q1pgm         rgipgm
     C                   movel     q1user        rgiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rgdt
     C                   movel     srdto         rgidt
     c                   movel     dstime        rgtime
     c                   movel     dstime        rgitim
     C                   write     dbrcdf
     C                   feod      fmrgp
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
     C                   if        rgstat = 'I'
     C                   eval      rgstat = 'A'
     C                   else
     C                   eval      rgstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        rguser
     C                   movel     q1pgm         rgpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rgdt
     C                   movel     dstime        rgtime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
