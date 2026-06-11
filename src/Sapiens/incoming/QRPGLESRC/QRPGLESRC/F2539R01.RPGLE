     H*  (C) Copyright Progressive Data Solutions, Inc., 2004
    H**********************************************************************
    H* PROGRAM: F2539R01
    H* DESCRIPTION: ASC's BY PAS GROUP
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 09/07/04  C:79   PROJECT: NY009
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
     H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2539f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftdgpny   uf a e             disk
     F                                     rename(pdg01:dbrcdf)
     Fststl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'DRGs'
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
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        dgstat = 'I'
     C                   eval      dgstat = 'A'
     C*
     C                   else
     C                   eval      dgstat = 'I'
     C*
     C                   endif
     C*
     C                   eval      dguser = q1user
     C                   eval      dgpgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgdt
     C                   eval      dgtime = dstime
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
     C                   eval      scdrg  = 0
     C                   eval      scefdt = 0
     C                   eval      scdesc = *blanks
     C                   eval      scsiwc = 0
     C                   eval      scsiwd = 0
     C                   eval      scltrm = 0
     C                   eval      schtrm = 0
     C                   eval      sculos = 0
     C                   eval      scdlos = 0
     C                   eval      sctp20 = *blank
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
     C* DRG cannot be zero
     C*
     C                   if        scdrg  = *zeros
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
     C                   eval      scdrg  = dgdrg
     C                   eval      scdesc = dGdesc
     C                   eval      srdti = dgefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   eval      scsiwc = dgsiwc
     C                   eval      scsiwd = dgsiwd
     C                   eval      scltrm = dgltrm
     C                   eval      schtrm = dghtrm
     C                   eval      sculos = dgulos
     C                   eval      scdlos = dgdlos
     C                   eval      sctp20 = dgtp20
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      dgdrg  = scdrg
     C                   eval      dgdesc = scdesc
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgefdt
     C                   eval      dgsiwc = scsiwc
     C                   eval      dgsiwd = scsiwd
     C                   eval      dgltrm = scltrm
     C                   eval      dghtrm = schtrm
     C                   eval      dgulos = sculos
     C                   eval      dgdlos = scdlos
     C                   eval      dgtp20 = sctp20
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
    C* 0GMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   exsr      mksthd
     C*
     C                   if        @@mode = 'D'
     C                   eval      *in09 = *on
     C                   endif
     C*
     C                   if        @@mode <> 'D'
     C                   seton                                        41
     C                   endif
     C*
     C                   if        @@mode = 'A'
     C                   seton                                        40
     C                   seton                                        31
     C                   else
     C                   setoff                                       40
     C                   setoff                                       31
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
     C                   eval      dgpgm  = q1pgm
     C                   eval      dguser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgdt
     C                   movel     dstime        dgtime
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
     C                   eval      dgstat = 'A'
     C                   eval      dgipgm = q1pgm
     C                   eval      dgiusr = q1user
     C                   eval      dgpgm  = q1pgm
     C                   eval      dguser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dgidt
     C                   movel     srdto         dgdt
     C                   movel     dstime        dgitim
     C                   movel     dstime        dgtime
     C*
     C                   write     dbrcdf
     C                   feod      ftdgpny
     C                   endsr
