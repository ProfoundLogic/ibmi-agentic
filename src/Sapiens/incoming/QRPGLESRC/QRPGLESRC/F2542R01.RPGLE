     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
    H**********************************************************************
    H* PROGRAM: F2542R01
    H* DESCRIPTION: CASE PAYMENT RATES
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 04/19/10  C:227  PROJECT: NY020
    H**********************************************************************
    H*  REVISIONS:
     H*
     H* 05/27/11  C:198 Project: NY026
     H*                 Recompiled.
     H*
    H**********************************************************************
     H*
     Ff2542f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffaiapny   uf a e             disk
     F                                     rename(pia01:dbrcdf)
     Ffaial1ny  if   e           k disk
     Fststl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Inpatient Case Payment Rates'
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
     C                   parm                    @@st              2
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
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*
     C     iaky          klist
     C                   kfld                    kkopct
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    iaopct        kkopct
     C     *like         define    iaefdt        kkefdt
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        iastat = 'I'
     C                   eval      iastat = 'A'
     C*
     C                   else
     C                   eval      iastat = 'I'
     C*
     C                   endif
     C*
     C                   eval      iauser = q1user
     C                   eval      iapgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         iadt
     C                   eval      iatime = dstime
     C*
     C                   update    dbrcdf
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* CLRFLDS - Clear fields on add
    C*================================================================
     C     clrflds       begsr
     C                   eval      scopct = *blanks
     C                   eval      scefdt = 0
     C                   eval      scdesc = *blanks
     C                   eval      scc001 = 0
     C                   eval      scc002 = 0
     C                   eval      scc003 = 0
     C                   eval      scc004 = 0
     C                   eval      scc005 = 0
     C                   eval      scc006 = 0
     C                   eval      scc007 = 0
     C                   eval      scc008 = 0
     C                   eval      scc009 = 0
     C                   eval      scc010 = 0
     C                   eval      scc011 = 0
     C                   eval      scc012 = 0
     C                   endsr
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
     C                   if        *in90 = *off
     C*
     C* OPCERT cannot be blank
     C*
     C                   if        scopct  = *blanks
     C                   seton                                        7190
     C                   goto      ck1end
     C                   endif
     C*
     C* Desc cannot be blank
     C*
     C                   if        scdesc = *blanks
     C                   seton                                        7290
     C                   goto      ck1end
     C                   endif
     C*
     C* Effective Date cannot be zero
     C*
     C                   if        scefdt = 0
     C                   seton                                        7390
     C                   goto      ck1end
     C                   endif
     C*
     C* Effective Date must be valid
     C*
     C                   eval      srdti = scefdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C                   if        srerr = '1'
     C                   seton                                        7490
     C                   goto      ck1end
     C                   endif
     C                   endif
     C     ck1end        endsr
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
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     mksthd        begsr
     C*
     C* Display state code and name in header
     C*
     C                   eval      kkkey = @@st
     C                   eval      scst  = @@st
     C     stky          chain     pst01                              99
     C                   eval      scstds = stname
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C* Add Mode
     C*
     C                   if        *in40 = *on
     C                   exsr      clrflds
     C                   endif
     C*
     C* Not add mode (change, display, etc)
     C*
     C                   if        *in40 = *off
     C*
     C                   eval      scopct = iaopct
     C                   eval      scdesc = iadesc
     C                   eval      scstat = iastat
     C                   eval      srdti = iaefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   eval      scc001 = iac001
     C                   eval      scc002 = iac002
     C                   eval      scc003 = iac003
     C                   eval      scc004 = iac004
     C                   eval      scc005 = iac005
     C                   eval      scc006 = iac006
     C                   eval      scc007 = iac007
     C                   eval      scc008 = iac008
     C                   eval      scc009 = iac009
     C                   eval      scc010 = iac010
     C                   eval      scc011 = iac011
     C                   eval      scc012 = iac012
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      iaopct = scopct
     C                   eval      iadesc = scdesc
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         iaefdt
     C*
     C                   eval      iac001 = scc001
     C                   eval      iac002 = scc002
     C                   eval      iac003 = scc003
     C                   eval      iac004 = scc004
     C                   eval      iac005 = scc005
     C                   eval      iac006 = scc006
     C                   eval      iac007 = scc007
     C                   eval      iac008 = scc008
     C                   eval      iac009 = scc009
     C                   eval      iac010 = scc010
     C                   eval      iac011 = scc011
     C                   eval      iac012 = scc012
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
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   exsr      mksthd
     C*
     C                   if        @@mode = 'D'
     C                   eval      *in09 = *on
     C                   endif
     C*
     C                   if        @@mode = 'P'
     C                   seton                                        42
     C                   endif
     C*
     C                   if        @@mode = 'A'
     C                   seton                                        40
     C                   seton                                        42
     C                   else
     C                   setoff                                       40
     C                   setoff                                       42
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C*
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C*
     C                   eval      iapgm  = q1pgm
     C                   eval      iauser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         iadt
     C                   movel     dstime        iatime
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
     C                   eval      iastat = 'A'
     C                   eval      iaipgm = q1pgm
     C                   eval      iaiusr = q1user
     C                   eval      iapgm  = q1pgm
     C                   eval      iauser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         iaidt
     C                   movel     srdto         iadt
     C                   movel     dstime        iaitim
     C                   movel     dstime        iatime
     C*
     C                   write     dbrcdf
     C                   feod      faiapny
     C                   endsr
