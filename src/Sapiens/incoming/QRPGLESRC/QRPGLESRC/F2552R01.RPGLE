     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE F2552R01
     H**************************************************************************************
     H*  Description:  ME DRG Rates Maintenance                                            *
     H*                                                                                    *
     H*  Created: 06/14/13   Programmer C:277   PROJECT: ME005                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
    H*
     Ff2552f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftdgpme   uf a e             disk
     F                                     rename(pdg01:dbrcdf)
    F*  Physical file definition (for maintaining record)
     Fftdgl1me  if   e           k disk    rename(pdg01:pdg02)
    F*
    F*  Physical file definition (for maintaining record)
     Fstabl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Inpatient DRG Rates Info'
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
     D kkdrg           s                   like(dgdrg)
     D kkefdt          s                   like(dgefdt)
     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
     D kkst            s                   like(stkey)
     D @@st            s                   like(stkey)
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
     C                   parm                    @@st
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
    C*
     C     ftdgl1mekey   klist
     C                   kfld                    kkdrg
     C                   kfld                    kkefdt
    C*
     C     stablkey      klist
     C                   kfld                    kkst
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
     C                   if        dgstat = 'I'
     C                   eval      dgstat = 'A'
     C                   else
     C                   eval      dgstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        dguser
     C                   movel     q1pgm         dgpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgdt
     C                   movel     dstime        dgtime
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
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
     C* If add mode
     C*
     C                   If        *in40 = *on
     C*
     C* DRG  code must not be blank.
     C*
     C                   if        scDRG = *zeros
     C                   seton                                        719081
     C                   leavesr
     C                   endif
     C*
     C* Effective date not be blank.
     C*
     C                   if        scefdt = *zero
     C                   seton                                        739082
     C                   leavesr
     C                   else
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        749082
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* DRG TYPE must not be blank.
     C*
     C                   if        sctype = *blanks
     C                   seton                                        759083
     C                   leavesr
     C                   endif
     C*
     C* DRG desc must not be blank.
     C*
     C                   if        scdesc = *blanks
     C                   seton                                        769084
     C                   leavesr
     C                   endif
     C*
     C* Check for duplicate record in FTDGPME
     C*
     C                   eval      kkdrg  = scdrg
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C     ftdgl1mekey   chain     pdg02
     C                   if        %found(ftdgl1me)
     C                   seton                                        729081
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C*
     C* If change Mode
     C*
     C                   If        *in10 = *on
     C*
     C* DRG TYPE must not be blank.
     C*
     C                   if        sctype = *blanks
     C                   seton                                        759083
     C                   leavesr
     C                   endif
     C*
     C* DRG desc must not be blank.
     C*
     C                   if        scdesc = *blanks
     C                   seton                                        769084
     C                   leavesr
     C                   endif
     C                   Endif
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
     C*
     C                   if        *in40 = *on
     C*
     C                   move      *blanks       scdrg
     C                   move      *blanks       scdrgc
     C                   move      *zeros        scefdt
     C                   move      *blanks       sctype
     C                   move      *blanks       scdesc
     C                   move      *zeros        scglos
     C                   move      *zeros        screlw
     C                   move      *zeros        scafee
     C                   move      *zeros        sccfee
     C                   move      'A'           scstat
     C*
     C                   else
     C*
     C                   If        *in40 = *off
     C                   eval      scdrgc = %editc(dgdrg:'X')
     C                   eval      scdrg = dgdrg
     C                   Endif
     C                   z-add     dgefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   movel     dgtype        sctype
     C                   movel     dgdesc        scdesc
     C                   eval      scglos = dgglos
     C                   eval      screlw = dgrelw
     C                   eval      scafee = dgafee
     C                   eval      sccfee = dgcfee
     C                   eval      scstat = dgstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      dgdrg  = scdrg
     C                   eval      dgdesc = scdesc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgefdt
     C                   eval      dgtype = sctype
     C                   eval      dgglos = scglos
     C                   eval      dgrelw = screlw
     C                   eval      dgafee = scafee
     C                   eval      dgcfee= sccfee
     C                   eval      dgstat = scstat
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
    C* ERRWDW1   - Error check on window 1
    C*================================================================
     C     errwdw1       begsr
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
     C*
     C     @@mode        comp      'D'                                    09    Inquiry
     C     @@mode        comp      'A'                                    40    Add
     C     @@mode        comp      'C'                                    10    change
     C                   setoff                                           31
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   Movel     @@st          kkst
     C                   Movel     @@st          scst
     C*
     C     stablkey      chain     pst01
     C                   If        %found(stabl)
     C                   Movel     stname        scstnm
     C                   Endif
     C*
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
    C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        dguser
     C                   movel     q1pgm         dgpgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgdt
     c                   movel     dstime        dgtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        dguser
     C                   movel     q1pgm         dgpgm
     C                   movel     q1pgm         dgipgm
     C                   movel     q1user        dgiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgdt
     C                   movel     srdto         dgidt
     c                   movel     dstime        dgtime
     c                   movel     dstime        dgitim
     C                   write     dbrcdf
     C                   feod      ftdgpme
     C                   endsr
