     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F2376R01
    H* DESCRIPTION: Medicare Hipps Codes Maintenance
    H* CALLED FROM: F2376R
    H* CALLS TO   : F7106C1
    H**********************************************************************
    H*  CREATED: 10/06/11  C:280  PROJECT: 114MZO
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
     FF2376F01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Medicare HIPPS Code Master File
    F*
     FFMHPP     uf a e             disk
     F                                     rename(PHP01:dbrcdf)
    F*
    F*  Medicare HIPPS code Logical by code/efdt
    F*
     FFMHPL1    if   e           k disk    prefix(tm)
    F*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
    D*
    C*
     D optnm           c                   'Medicare HIPPS Code'
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
     D kkcode          s                   like(hpcode)
     D kkefdt          s                   like(hpefdt)
     D kktodt          s                   like(hptodt)
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
     C     hpky          klist
     C                   kfld                    kkcode
     C                   kfld                    kkefdt
     C                   kfld                    kktodt
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
     C                   if        hpstat = 'I'
     C                   eval      hpstat = 'A'
     C                   else
     C                   eval      hpstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        hpuser
     C                   movel     q1pgm         hppgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hpdt
     C                   movel     dstime        hptime
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
    C* HIPPS code must not be blank
    C*
b02  C                   if        sccode = *blanks
     C                   seton                                        719086
     C                   leavesr
e02  C                   endif
    C*
    C* Description should not be blank
    C*
b02  C                   if        scdsc1 = *blanks
     C                   seton                                        749050
     C                   leavesr
e02  C                   endif
    C*
    C* Effective date not be blank (If in ADD mode)
    C*
b02  C                   if        scefdt = *zero
    C*
     C                   Eval      *in87 = *on
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
     C                   seton                                        779089
     C                   leavesr
e03  C                   endif
    C*
e02  C                   endif
    C*
b02  C                   if        sctodt <> *zero
    C*
    C* Effective Date must be valid
    C*
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sctodt
    C*
b03  C                   if        srerr = '1'
     C                   seton                                        779088
     C                   leavesr
e03  C                   endif
    C*
e02  C                   endif
    C*
    C* Check for duplicate records
    C*
     C                   eval      kkcode = sccode
    C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
    C*
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kktodt
    C*
     C     hpky          chain     php01
    C*
b02  C                   if        %found
     C                   Eval      *in86 = *on
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
     C                   clear                   sccode
     C                   clear                   scefdt
     C                   clear                   sctodt
     C                   clear                   scdsc1
     C                   clear                   scdsc2
     C                   clear                   scdsc3
     C                   clear                   scdsc4
     C                   clear                   scdsc5
     C                   clear                   scdsc6
     C                   clear                   scdsc7
     C                   clear                   scstat
    C*
     C                   if        *in10 = *on
    C*
     C                   eval      sccode = *blanks
     C                   eval      scefdt = *zeros
     C                   eval      sctodt = *zeros
     C                   eval      scdsc1 = *blanks
     C                   eval      scdsc2 = *blanks
     C                   eval      scdsc3 = *blanks
     C                   eval      scdsc4 = *blanks
     C                   eval      scdsc5 = *blanks
     C                   eval      scdsc6 = *blanks
     C                   eval      scdsc7 = *blanks
     C                   eval      scstat = 'A'
    C*
     C                   else
    C*
     C                   eval      sccode = hpcode
    C*
     C                   z-add     hpefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
    C*
     C                   z-add     hptodt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sctodt
    C*
     C                   eval      scdsc1 = hpdsc1
     C                   eval      scdsc2 = hpdsc2
     C                   eval      scdsc3 = hpdsc3
     C                   eval      scdsc4 = hpdsc4
     C                   eval      scdsc5 = hpdsc5
     C                   eval      scdsc6 = hpdsc6
     C                   eval      scdsc7 = hpdsc7
     C                   eval      scstat = hpstat
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
     C                   eval      hpcode = sccode
    C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hpefdt
    C*
     C                   z-add     sctodt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hptodt
    C*
     C                   eval      hpdsc1 = scdsc1
     C                   eval      hpdsc2 = scdsc2
     C                   eval      hpdsc3 = scdsc3
     C                   eval      hpdsc4 = scdsc4
     C                   eval      hpdsc5 = scdsc5
     C                   eval      hpdsc6 = scdsc6
     C                   eval      hpdsc7 = scdsc7
     C                   eval      hpstat = scstat
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
     C                   movel     q1user        hpuser
     C                   movel     q1pgm         hppgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hpdt
     C                   movel     dstime        hptime
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
     C                   exsr      mvscdb
    C*
     C                   movel     q1user        hpuser
     C                   movel     q1pgm         hppgm
     C                   movel     q1pgm         hpipgm
     C                   movel     q1user        hpiusr
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
    C*
     C                   movel     srdto         hpdt
     C                   movel     srdto         hpidt
     c                   movel     dstime        hptime
     c                   movel     dstime        hpitim
    C*
     C                   write     dbrcdf
    C*
     C                   feod      fmhpp
    C*
     C                   endsr
    C*
