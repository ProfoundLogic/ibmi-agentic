     H*******************************************************************
     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*******************************************************************
     H/TITLE F2440R01 - WORK WITH TAXONOMY CODES
     H*
     H*  DESCRIPTION: This program shows the Detail Screen for FTTCP
     H*
     H*   CALLED BY: F2440R
     H*
     H* Created: 03/04/09 C:198 Project: MC330
     H*
     H*******************************************************************
     H*  REVISIONS:
     H*
     H*  09/09/25 C:342 Project: MC453
     H*                 Provider Taxonomy Codes Update
    H*
     H*  01/08/10 C:198 Support: 71199
     H*                 Compilation only
    H*
     H*******************************************************************
    H*
     Ff2440f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffttcp     uf a e             disk
     F                                     rename(ptc01:dbrcdf)
     Ffttcl1    if   e           k disk
     F
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Taxonomy Codes'
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
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
      *
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
      *
     C     dtmmdy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
    C*================================================================
    C*  KLIST definitions
     C     tcky          klist
     C                   kfld                    sccode
     C                   kfld                    xxefdt
    C*================================================================
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
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
     C*** error check for effective date and deactivation date
     C                   eval      *in41 = *off
     C                   eval      *in42 = *off
     C                   eval      *in28 = *off
     C                   eval      *in29 = *off
     C                   eval      *in30 = *off
     C                   eval      *in31 = *off
     C                   eval      *in32 = *off
     C                   eval      *in33 = *off
     C* if add mode
     C                   if        *in27 = *on
     C* required taxonomy Code
     C                   if        sccode = *blanks
     C                   eval      *in28 = *on
     C                   eval      *in90 = *on
     C                   endif
     C* required effective date
     C                   if        scefdt = *zeros and *in90 = *off
     C                   eval      *in29 = *on
     C                   eval      *in90 = *on
     C                   endif
     C* valid effective date
     C                   if        scefdt <> *zeros and *in90 = *off
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C*
     C                   if        srerr = '1'
     C                   eval      *in41 = *on
     C                   eval      *in90 = *on
     C                   endif                                                  scerr = '1'
     C                   endif                                                  scefdt <> 0
     C* TCCODE + TCEFDT combination must be unique
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxefdt            7 0
     C     tcky          chain     ptc01                              99
     C                   if        *in99 = *off and *in90 = *off
     C                   eval      *in30 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   endif                                                  *in27 = *on
     C* valid deactivation date
     C                   if        scdedt <> *zeros and *in90 = *off
     C                   z-add     scdedt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   eval      *in42 = *on
     C                   eval      *in90 = *on
     C                   endif                                                  scerr = '1'
     C*** deactivation date must be greater than effective date
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxefdt            7 0
     C*
     C                   z-add     scdedt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxdedt            7 0
     C                   if        xxefdt > xxdedt and *in90 =*off
     C                   eval      *in31 = *on
     C                   eval      *in90 = *on
     C                   endif
     C                   endif                                                  scdedt <> 0
     C* required classification
     C                   if        scclas = *blanks and *in90 = *off
     C                   eval      *in32 = *on
     C                   eval      *in90 = *on
     C                   endif
     C* required specialization
     C                   if        scspcl = *blanks and *in90 = *off
     C                   eval      *in33 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C*** Beg Add *** 09/09/25 ****************************************
     C* required display name
     C                   if        scdspn = *blanks and *in90 = *off
     C                   eval      msdspn= 'MCR0503'
     C                   eval      erdspn= *on
     C                   eval      *in90 = *on
     C                   endif
     C* required section
     C                   if        scsect = *blanks and *in90 = *off
     C                   eval      mssect= 'MCR0503'
     C                   eval      ersect= *on
     C                   eval      *in90 = *on
     C                   endif
     C*** End Add *** 09/09/25 ****************************************
     C                   z-add     1             scrn#             1 0
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
     C                   select
     C*
     C                   when      @@mode = 'C' or @@mode = 'D'
     C*
     C                   movel     tccode        sccode
     C                   movel     tcclas        scclas
     C                   movel     tcspcl        scspcl
     C                   z-add     tcefdt        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         scefdt
     C                   z-add     tcdedt        srdti
     C                   call      'SRCVTDAT'    dtmmdy
     C                   Movel     srdto         scdedt
     C                   Movel     tcstat        scstat
     C*** Beg Add *** 09/09/25 ****************************************
     C                   movel     tcdspn        scdspn
     C                   movel     tcsect        scsect
     C*** End Add *** 09/09/25 ****************************************
     C*
     C                   when      @@mode = 'A'
     C*
     C                   move      *blanks       sccode
     C                   move      *blanks       scclas
     C                   move      *blanks       scspcl
     C                   move      *zeros        scefdt
     C                   move      *zeros        scdedt
     C                   movel     'A'           scstat
     C*** Beg Add *** 09/09/25 ****************************************
     C                   move      *blanks       scdspn
     C                   move      *blanks       scsect
     C*** End Add *** 09/09/25 ****************************************
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C*** Beg Add *** 09/09/25 ****************************************
     C                   movel     scdspn        tcdspn
     C                   movel     scsect        tcsect
     C*** End Add *** 09/09/25 ****************************************
     C                   movel     sccode        tccode
     C                   movel     scclas        tcclas
     C                   movel     scspcl        tcspcl
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         tcefdt
     C                   z-add     scdedt        srdti
     C                   call      'SRCVTDAT'    dtyymd
     C                   Movel     srdto         tcdedt
     C*PUIMOD
     C*                  move      *date         sys_date
     C                   eval       sys_date = %date()
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      tcdt
     C                   eval      tcpgm=q1pgm
     C                   eval      tcuser=q1user
     C                   move      sys_time      tctime
     C*
     C                   if        @@mode = 'A'
     C                   movel     'A'           tcstat
     C     *cymd         move      sys_date      tcidt
     C                   eval      tcipgm=q1pgm
     C                   eval      tciusr=q1user
     C                   move      sys_time      tcitim
     C                   endif
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
     C*** Beg Add *** 09/09/25 ****************************************
     C                   eval      msdspn = *blanks
     C                   eval      erdspn = *off
     C                   eval      mssect = *blanks
     C                   eval      ersect = *off
     C*** End Add *** 09/09/25 ****************************************
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
     C                   select
     C                   when      @@mode = 'A'
     C                   eval      *in27 = *on
     C                   when      (@@mode <> 'A') and (@@mode <> 'C')
     C                   eval      *in09 = *on
     C                   endsl
     C                   z-add     0             scrn#
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   if        @@mode = 'S'
     C                   exsr      chgsts
     C                   endif
     C                   endsr
    C*================================================================
    C*CHGSTS  - Change Status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   if        tcstat = 'I'
     C                   move      'A'           tcstat
     C                   else
     C                   move      'I'           tcstat
     C                   endif                                                  tcstat = 'I'
     C                   endif                                                  *in99 = *off
     C*PUIMOD
     C*                  move      *date         sys_date
     C                   eval      sys_date = %date()
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      tcdt
     C                   eval      tcpgm=q1pgm
     C                   eval      tcuser=q1user
     C                   move      sys_time      tctime
     C*
     C                   update    dbrcdf
     C                   return
     C*
     C                   endsr
    C*================================================================
    C*UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      fttcp
     C                   endsr
