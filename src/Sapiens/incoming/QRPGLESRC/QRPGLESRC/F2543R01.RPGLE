     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
    H**********************************************************************
    H* PROGRAM: F2543R01
    H* DESCRIPTION: Inpatient Case Exempt Unit Rates
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 04/19/10  C:227  PROJECT: NY020
    H**********************************************************************
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H*  02/25/21  C:274  Project: NY053                                   *
    H*                   2020 NY Hospital FS Update                       *
    H*                                                                    *
    H*  06/14/13  C:274  Project: NY029                                   *
    H*                   2012 NY Hospital FS Update                       *
    H*                                                                    *
    H**********************************************************************
     H*
     Ff2543f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftempny   uf a e             disk
     F                                     rename(pem01:dbrcdf)
     Ffteml1ny  if   e           k disk
     Fststl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Inpatient Exempt Unit Rates'
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
     C     emky          klist
     C                   kfld                    kkopct
     C                   kfld                    kkefdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    emopct        kkopct
     C     *like         define    emefdt        kkefdt
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        emstat = 'I'
     C                   eval      emstat = 'A'
     C*
     C                   else
     C                   eval      emstat = 'I'
     C*
     C                   endif
     C*
     C                   eval      emuser = q1user
     C                   eval      empgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         emdt
     C                   eval      emtime = dstime
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
     C                   eval      scc003 = 0
     C                   eval      scc005 = 0
     C                   eval      scc007 = 0
     C                   eval      scc009 = 0
     C                   eval      scc011 = 0
     C                   eval      scc012 = 0
     C                   eval      scc013 = 0
     C                   eval      scc014 = 0
     C                   eval      scc015 = 0
     C                   eval      scc016 = 0
      //*** Beg Add *** 02/25/21/*******************************************
       Clear scc005;
       Clear scPsyDd;
      //*** End Add *** 02/25/21/*******************************************
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
     C                   eval      scopct = emopct
     C                   eval      scdesc = emdesc
     C                   eval      scstat = emstat
     C                   eval      srdti = emefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   eval      scc001 = emc001
     C                   eval      scc003 = emc003
     C                   eval      scc005 = emc005
     C                   eval      scc007 = emc007
     C                   eval      scc009 = emc009
     C                   eval      scc011 = emc011
     C                   eval      scc012 = emc012
     C                   eval      scc013 = emc013
     C                   eval      scc014 = emc014
     C                   eval      scc015 = EMNOPER
     C                   eval      scc016 = EMECTP
     C                   endif
      //*** Beg Add ***** 02/25/21 *********************************
       scc004  = EMc004;
       scPsyDd = EMpsyDd;
      //*** End Add ***** 02/25/21 *********************************
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      emopct = scopct
     C                   eval      emdesc = scdesc
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         emefdt
     C*
     C                   eval      emc001 = scc001
     C                   eval      emc003 = scc003
     C                   eval      emc005 = scc005
     C                   eval      emc007 = scc007
     C                   eval      emc009 = scc009
     C                   eval      emc011 = scc011
     C                   eval      emc012 = scc012
     C                   eval      emc013 = scc013
     C                   eval      emc014 = scc014
     C                   eval      EMNOPER = scc015
     C                   eval      EMECTP  = scc016
      //*** Beg Add ***** 02/25/21 *********************************
       emc004  = scc004;
       EMPSYDD = scPsyDd;
      //*** End Add ***** 02/25/21 *********************************
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
     C                   eval      empgm  = q1pgm
     C                   eval      emuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         emdt
     C                   movel     dstime        emtime
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
     C                   eval      emstat = 'A'
     C                   eval      emipgm = q1pgm
     C                   eval      emiusr = q1user
     C                   eval      empgm  = q1pgm
     C                   eval      emuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         emidt
     C                   movel     srdto         emdt
     C                   movel     dstime        emitim
     C                   movel     dstime        emtime
     C*
     C                   write     dbrcdf
     C                   feod      ftempny
     C                   endsr
