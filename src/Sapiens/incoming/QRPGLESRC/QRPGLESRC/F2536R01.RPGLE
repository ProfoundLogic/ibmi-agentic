     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2003-2008
    H**********************************************************************
    H* PROGRAM: F2536R01
    H* DESCRIPTION: PHYSICAL THERAPY PER VISIT INFORMATION
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 04/07/03  C:79   PROJECT: PA020
    H**********************************************************************
    H*  REVISIONS:
     H*
     H* 10/29/13 C:274 PROJECT PA067
     H*                Added Rev Codes 651,652,655,656 on the screen.
     H*
     H* 11/18/08 C:198 PROJECT PA046
     H*                Added NPI# after the medicare# on the screen
     H*                SCR01.
     H*
    H**********************************************************************
     H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     Ff2536f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffaptppa   uf a e             disk
     F                                     rename(ppt01:dbrcdf)
     Fststl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Physical Therapy Per Visit'
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
     C                   if        ptstat = 'I'
     C                   eval      ptstat = 'A'
     C*
     C                   else
     C                   eval      ptstat = 'I'
     C*
     C                   endif
     C*
     C                   eval      ptuser = q1user
     C                   eval      ptpgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ptdt
     C                   eval      pttime = dstime
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
     C                   eval      scmed# = *blanks
     C                   eval      scname = *blanks
     C                   eval      scefdt = 0
     C                   eval      scrvc1 = 0
     C                   eval      scrvc2 = 0
     C                   eval      scrvc3 = 0
     C                   eval      scrvc4 = 0
     C                   eval      scrvc5 = 0
     C                   eval      scrvc6 = 0
     C*** Beg Add *** 10/29/13 **********************************
     C                   eval      scrvc7 = 0
     C                   eval      scrvc8 = 0
     C                   eval      scrvc9 = 0
     C                   eval      scrvc0 = 0
     C*** Beg End *** 10/29/13 **********************************
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
     C* Medicare# cannot be blank
     C*
     C                   if        *in90 = *off
     C*
     C                   if        scmed# = *blanks and *in40 = *on
     C                   seton                                        9071
     C                   goto      ck1end
     C                   endif
     C*
     C* Name cannot be blank
     C*
     C                   if        scname = *blanks
     C                   seton                                        9072
     C                   goto      ck1end
     C                   endif
     C*
     C* Effective date cannot be blank
     C*
     C                   if        scefdt = *zeros and *in40 = *on
     C                   seton                                        9073
     C                   goto      ck1end
     C                   endif
     C*
     C* Effective date must be valid
     C*
     C                   if        scefdt <> *zeros and *in40 = *on
     C                   eval      srdti = scefdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C                   if        srerr = *on
     C                   seton                                        7490
     C                   goto      ck1end
     C                   endif
     C*
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
     C                   eval      scmed# = ptmed#
     C                   eval      scname = ptname
     C                   eval      srdti = ptefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*** BEG ADD *** 11/18/08 **********************************
     C                   eval      scnpi# = ptnpi#
     C*** END ADD *** 11/18/08 **********************************
     C                   eval      scrvc1 = ptrvc1
     C                   eval      scrvc2 = ptrvc2
     C                   eval      scrvc3 = ptrvc3
     C                   eval      scrvc4 = ptrvc4
     C                   eval      scrvc5 = ptrvc5
     C                   eval      scrvc6 = ptrvc6
     C*** Beg Add *** 10/29/13 **********************************
     C                   eval      scrvc7 = ptrvc7
     C                   eval      scrvc8 = ptrvc8
     C                   eval      scrvc9 = ptrvc9
     C                   eval      scrvc0 = ptrvc0
     C*** End Add *** 10/29/13 **********************************
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      ptmed# = scmed#
     C                   eval      ptname = scname
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ptefdt
     C*** BEG ADD *** 11/18/08 **********************************
     C                   eval      ptnpi# = scnpi#
     C*** END ADD *** 11/18/08 **********************************
     C                   eval      ptrvc1 = scrvc1
     C                   eval      ptrvc2 = scrvc2
     C                   eval      ptrvc3 = scrvc3
     C                   eval      ptrvc4 = scrvc4
     C                   eval      ptrvc5 = scrvc5
     C                   eval      ptrvc6 = scrvc6
     C*** Beg Add *** 10/29/13 **********************************
     C                   eval      ptrvc7 = scrvc7
     C                   eval      ptrvc8 = scrvc8
     C                   eval      ptrvc9 = scrvc9
     C                   eval      ptrvc0 = scrvc0
     C*** End Add *** 10/29/13 **********************************
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
     C                   eval      ptpgm  = q1pgm
     C                   eval      ptuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ptdt
     C                   movel     dstime        pttime
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
     C                   eval      ptstat = 'A'
     C                   eval      ptipgm = q1pgm
     C                   eval      ptiusr = q1user
     C                   eval      ptpgm  = q1pgm
     C                   eval      ptuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ptidt
     C                   movel     srdto         ptdt
     C                   movel     dstime        ptitim
     C                   movel     dstime        pttime
     C*
     C                   write     dbrcdf
     C                   feod      faptppa
     C                   endsr
