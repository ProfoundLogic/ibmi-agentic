     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F2541R01
    H* DESCRIPTION: APR DRG's
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 04/19/10  C:227  PROJECT: NY020
    H**********************************************************************
    H*  REVISIONS:
    H*                                                                    *
    H*  05/17/19  C:212  Project: NY045                                   *
    H*                   Added screen field for Outlier and related       *
    H*                   processing                                       *
    H*                                                                    *
    H*  06/14/13  C:274  Project: NY029                                   *
    H*                   2012 NY Hospital FS Update                       *
    H*                                                                    *
    H**********************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2541f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftadpny   uf a e             disk
     F                                     rename(pad01:dbrcdf)
     Fftadl1ny  if   e           k disk
     Fststl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'APR DRGs'
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
     C     adky          klist
     C                   kfld                    kkdrg
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    addrg         kkdrg
     C     *like         define    adefdt        kkefdt
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        adstat = 'I'
     C                   eval      adstat = 'A'
     C*
     C                   else
     C                   eval      adstat = 'I'
     C*
     C                   endif
     C*
     C                   eval      aduser = q1user
     C                   eval      adpgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         addt
     C                   eval      adtime = dstime
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
     C                   eval      scdrg  = *blanks
     C                   eval      scefdt = 0
     C                   eval      scdesc = *blanks
     C                   eval      scsiw1 = 0
     C                   eval      scsiw2 = 0
     C                   eval      scsiw3 = 0
     C                   eval      scsiw4 = 0
     C*** Beg Chg ***** 06/14/13 ***********************************
     C                   eval      scsiw5 = 0
     C                   eval      scsiw6 = 0
     C                   eval      scsiw7 = 0
     C                   eval      scsiw8 = 0
     C*** End Chg ***** 06/14/13 ***********************************
     C                   eval      scalos = 0
     C***** Beg Add *** 05/17/19 ***********************************
     C                   eval      scoutl = 0
     C***** End Add *** 05/17/19 ***********************************
     C                   eval      scstat = 'A'
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
     C* APR DRG cannot be blank
     C*
     C                   if        scdrg  = *blanks
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
     C*
     C                   endif
     C*
     C* Check for Duplicate record
     C*
     C                   if        *in40 = *on
     C                   eval      kkdrg = scdrg
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C     adky          chain     pad01                              61
     C                   if        %found
     C                   seton                                        7590
     C                   endif
     C                   endif
     C*
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
     C                   eval      scdrg  = addrg
     C                   eval      scdesc = addesc
     C                   eval      srdti = adefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   eval      scsiw1 = adsiw1
     C                   eval      scsiw2 = adsiw2
     C                   eval      scsiw3 = adsiw3
     C                   eval      scsiw4 = adsiw4
     C*** Beg Chg ***** 06/14/13 ***********************************
     C                   eval      scsiw5 = adsiw5
     C                   eval      scsiw6 = adsiw6
     C                   eval      scsiw7 = adsiw7
     C                   eval      scsiw8 = adsiw8
     C*** End Chg ***** 06/14/13 ***********************************
     C                   eval      scalos = adalos
     C***** Beg Add *** 05/17/19 ***********************************
     C                   eval      scoutl = adoutl
     C***** End Add *** 05/17/19 ***********************************
     C                   eval      scstat = adstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      addrg  = scdrg
     C                   eval      addesc = scdesc
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         adefdt
     C                   eval      adsiw1 = scsiw1
     C                   eval      adsiw2 = scsiw2
     C                   eval      adsiw3 = scsiw3
     C                   eval      adsiw4 = scsiw4
     C*** Beg Chg ***** 06/14/13 ***********************************
     C                   eval      adsiw5 = scsiw5
     C                   eval      adsiw6 = scsiw6
     C                   eval      adsiw7 = scsiw7
     C                   eval      adsiw8 = scsiw8
     C*** End Chg ***** 06/14/13 ***********************************
     C                   eval      adalos = scalos
     C***** Beg Add *** 05/17/19 ***********************************
     C                   eval      adoutl = scoutl
     C***** End Add *** 05/17/19 ***********************************
     C                   eval      adstat = scstat
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
     C*                  setoff                                       42
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
     C                   eval      adpgm  = q1pgm
     C                   eval      aduser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         addt
     C                   movel     dstime        adtime
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
     C                   eval      adstat = 'A'
     C                   eval      adipgm = q1pgm
     C                   eval      adiusr = q1user
     C                   eval      adpgm  = q1pgm
     C                   eval      aduser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         adidt
     C                   movel     srdto         addt
     C                   movel     dstime        aditim
     C                   movel     dstime        adtime
     C*
     C                   write     dbrcdf
     C                   feod      ftadpny
     C                   endsr
