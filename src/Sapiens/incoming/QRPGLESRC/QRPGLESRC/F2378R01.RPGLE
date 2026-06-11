     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F2378R01
    H* DESCRIPTION: IRF Wage Index Table Maintenance
    H* CALLED FROM: F2378R
    H* CALLS TO   :
    H**********************************************************************
    H*  CREATED: 10/20/11  C:280  PROJECT: 114MAR
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
    H*  INDICATORS
    H*
    H**********************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FF2378F01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  IRF Wage Index Table
    F*
     FFMIWP     uf a e             disk
     F                                     rename(PIW01:dbrcdf)
    F*
    F*  IRF Wage Index Table logical by cbsa/efdt
    F*
     FFMIWL2    if   e           k disk    prefix(tm)
    F*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
    D*
    C*
     D optnm           c                   'IRF Wage Index Table'
     D comapi          c                   'FA007R'
     D #ofscr          c                   1
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
    D*
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkcbsa          s                   like(iwcbsa)
     D kkefdt          s                   like(iwefdt)
     D kkarea          s                   like(iwarea)
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
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
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     iwky          klist
     C                   kfld                    kkcbsa
     C                   kfld                    kkefdt
     C                   kfld                    kkarea
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C* none used
    C*
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
     C                   if        iwstat = 'I'
     C                   eval      iwstat = 'A'
     C                   else
     C                   eval      iwstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        iwuser
     C                   movel     q1pgm         iwpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         iwdt
     C                   movel     dstime        iwtime
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
    C*
     C     dltpre        begsr
     C                   endsr
    C*
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
    C*
     C     dsplyr        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
    C*
     C     errck1        begsr
    C*
    C* If in ADD mode
    C*
b01  C                   if        *in10 = *on
    C*
    C* IRF code must not be blank
    C*
b02  C                   if        sccbsa = *blanks
     C                   eval      *in86 = *on
     C                   leavesr
e02  C                   endif
    C*
    C* Description should not be blank
    C*
b02  C                   if        scarea = *blanks
     C                   eval      *in87 = *on
     C                   leavesr
e02  C                   endif
    C*
    C* Effective date not be blank (If in ADD mode)
    C*
b02  C                   if        scefdt = *zero
    C*
     C                   eval      *in88 = *on
     C                   leavesr
    C*
x02  C                   else
    C*
    C* Effective Date must be valid
    C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
    C*
b03  C                   if        srerr = '1'
     C                   eval      *in88 = *on
     C                   leavesr
e03  C                   endif
    C*
e02  C                   endif
    C*
    C* Wage Index should not be zero
    C*
b02  C                   if        scwind = *zeros
     C                   eval      *in89 = *on
     C                   leavesr
e02  C                   endif
    C*
    C* Check for duplicate records
    C*
     C                   eval      kkcbsa = sccbsa
    C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
    C*
     C                   eval      kkarea = scarea
    C*
     C     iwky          chain     piw01
    C*
b02  C                   if        %found
     C                   Eval      *in85 = *on
     C                   leavesr
e02  C                   endif
    C*
e01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
    C*
     C     errck2        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
    C*
     C     errck3        begsr
     C                   endsr
    C*
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
    C*
     C     mvdbsc        begsr
    C*
     C                   clear                   sccbsa
     C                   clear                   scefdt
     C                   clear                   scarea
     C                   clear                   scwind
     C                   clear                   scstat
    C*
     C                   if        *in10 = *on
    C*
     C                   eval      sccbsa = *blanks
     C                   eval      scefdt = *zeros
     C                   eval      scarea = *blanks
     C                   eval      scstat = 'A'
    C*
     C                   else
    C*
     C                   eval      sccbsa = iwcbsa
    C*
     C                   z-add     iwefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
    C*
     C                   eval      scarea = iwarea
     C                   eval      scwind = iwwind
     C                   eval      scstat = iwstat
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
    C*
     C     mvscdb        begsr
    C*
     C                   eval      iwcbsa = sccbsa
    C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         iwefdt
    C*
     C                   eval      iwarea = scarea
     C                   eval      iwwind = scwind
     C                   eval      iwstat = scstat
    C*
     C                   endsr
    C*
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
    C*
     C     otherr        begsr
     C                   endsr
    C*
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
    C*
     C     othfks        begsr
     C                   endsr
    C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
    C*
     C     pgmend        begsr
     C                   endsr
    C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
    C*
     C     pgmstr        begsr
    C*
     C     @@mode        comp      'A'                                    10
     C     @@mode        comp      'C'                                    11
     C     @@mode        comp      'D'                                    12
    C*
     C                   endsr
    C*
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
    C*
     C     preoth        begsr
    C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
    C*
     C                   endsr
    C*
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
    C*
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
B01  C                   if        *in99 = *off
    C*
     C                   exsr      mvscdb
     C*
     C                   movel     q1user        iwuser
     C                   movel     q1pgm         iwpgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         iwdt
     C                   movel     dstime        iwtime
     C*
     C                   update    dbrcdf
     C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
    C*
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
    C*
     C                   If        Not *in85
    C*
     C                   exsr      mvscdb
    C*
     C                   movel     q1user        iwuser
     C                   movel     q1pgm         iwpgm
     C                   movel     q1pgm         iwipgm
     C                   movel     q1user        iwiusr
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
    C*
     C                   movel     srdto         iwdt
     C                   movel     srdto         iwidt
     c                   movel     dstime        iwtime
     c                   movel     dstime        iwitim
    C*
     C                   write     dbrcdf
    C*
     C                   feod      fmiwp
    C*
     C                   endif
    C*
     C                   endsr
    C*
