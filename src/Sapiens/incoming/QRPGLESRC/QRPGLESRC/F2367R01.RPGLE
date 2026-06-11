     H* (c) 2011 StoneRiver, Inc. All rights Reserved.                                     *
     H* StoneRiver is a trademark of StoneRiver, Inc.                                      *
     H actgrp(*caller)
     H/TITLE F2367R01                                                                      *
     H**************************************************************************************
     H*  Description:  Medicare GPCI Information Maintenance                               *
     H*                                                                                    *
     H*  Created: 07/03/09   Programmer C:227   Project:AR038                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/11/11   C:280    Project : SEQ78229                                            *
     H*                      Added option 20 for status change                             *
     H*                                                                                    *
     H*  10/01/10  C:198  Project : WV009                                                  *
     H*                   Added medicare release level on WW and maintenance screen.       *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2367f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftgpp     uf a e             disk
     F                                     rename(pgp01:dbrcdf)
     F*** BEG ADD *** 10/01/10 ****************************************
     Fftgpl1    if   e           k disk
     F*** END ADD *** 10/01/10 ****************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare GPCI Table'
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
     D kkefdt          s                   like(gpefdt)
     D kkst            s                   like(gpst)
     D kkloc           s                   like(gploc)
     D kkcont          s                   like(gpcont)
     D*** BEG ADD *** 10/01/10 ****************************************
     D kkmrel          s                   like(gpmrel)
     D*** END ADD *** 10/01/10 ****************************************
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
     C     gpky          klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    kkcont
     C                   kfld                    kkefdt
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   kfld                    kkmrel
     C*** END ADD *** 10/01/10 ****************************************
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
     C                   if        gpstat = 'I'
     C                   eval      gpstat = 'A'
     C                   else
     C                   eval      gpstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        gpuser
     C                   movel     q1pgm         gppgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         gpdt
     C                   movel     dstime        gptime
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
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   setoff                                       6667
     C*** END ADD *** 10/01/10 ****************************************
     C* State must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scst = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   endif
     C* Locality must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scloc = *zeros
     C                   seton                                        729088
     C                   leavesr
     C                   endif
     C* Contractor must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             sccont = *zeros
     C                   seton                                        749086
     C                   leavesr
     C                   endif
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        739087
     C                   leavesr
     C                   else
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779087
     C                   leavesr
     C                   endif
     C                   endif
     C                   endif
     C*** BEG ADD *** 10/01/10 ****************************************
     C* Release must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             scmrel = *blanks
     C                   seton                                        669067
     C                   leavesr
     C                   endif
     C* Check for duplicate record in FTGPP
     C                   if        *in40 = *on
     C                   eval      kkst = scst
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C                   eval      kkloc = scloc
     C                   eval      kkcont = sccont
     C                   eval      kkmrel = scmrel
     C     gpky          chain     pgp01
     C                   if        %found
     C                   seton                                        769089
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C*** END ADD *** 10/01/10 ****************************************
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
     C                   clear                   sccont
     C                   clear                   scefdt
     C                   clear                   sclnam
     C                   clear                   scwkgp
     C                   clear                   scpegp
     C                   clear                   scmpgp
     C                   clear                   scstat
     C                   if        *in40 = *on
     C                   eval      scst = *blanks
     C                   eval      scloc = *zeros
     C                   eval      sccont = *zeros
     C                   eval      scefdt = *zeros
     C                   eval      sclnam = *blanks
     C                   eval      scwkgp = *zeros
     C                   eval      scpegp = *zeros
     C                   eval      scmpgp = *zeros
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   eval      scmrel = *blanks
     C*** END ADD *** 10/01/10 ****************************************
     C                   eval      scstat = 'A'
     C*
     C                   else
     C*
     C                   eval      scst = gpst
     C                   eval      scloc = gploc
     C                   eval      sccont = gpcont
     C                   z-add     gpefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   movel     gplnam        sclnam
     C                   eval      scwkgp = gpwkgp
     C                   eval      scpegp = gppegp
     C                   eval      scmpgp = gpmpgp
     C                   eval      scstat = gpstat
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   eval      scmrel = gpmrel
     C*** END ADD *** 10/01/10 ****************************************
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      gpst = scst
     C                   eval      gploc = scloc
     C                   eval      gpcont = sccont
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         gpefdt
     C                   eval      gplnam = sclnam
     C                   eval      gpwkgp = scwkgp
     C                   eval      gppegp = scpegp
     C                   eval      gpmpgp = scmpgp
     C                   eval      gpstat = scstat
     C*** BEG ADD *** 10/01/10 ****************************************
     C                   eval      gpmrel = scmrel
     C*** END ADD *** 10/01/10 ****************************************
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
     C                   movel     q1user        gpuser
     C                   movel     q1pgm         gppgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         gpdt
     c                   movel     dstime        gptime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        gpuser
     C                   movel     q1pgm         gppgm
     C                   movel     q1pgm         gpipgm
     C                   movel     q1user        gpiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         gpdt
     C                   movel     srdto         gpidt
     c                   movel     dstime        gptime
     c                   movel     dstime        gpitim
     C                   write     dbrcdf
     C                   feod      ftgpp
     C                   endsr
