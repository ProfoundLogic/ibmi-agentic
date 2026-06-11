     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H actgrp(*caller)
     H/TITLE F2369R01                                                                      *
     H**************************************************************************************
     H*  Description:  Medicare Ambulance Rates Maintenance                                *
     H*                                                                                    *
     H*  Created: 08/12/10   Programmer C:251   Project:114MWS                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/11/11   C:280    Project : SEQ78229                                            *
     H*                      Added option 20 for status change                             *
     H*                                                                                    *
     H*  09/02/11   Programmer C:274   Project: FS392                                      *
     H*             Add lowest Quartile field.                                             *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2369f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmamp     uf a e             disk
     F                                     rename(pam01:dbrcdf)
     Fststl     if   e           k disk
     Ffmaml1    if   e           k disk    prefix(tm)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare Ambulance Rates'
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
     D kkefdt          s                   like(amefdt)
     D kkst            s                   like(amst)
     D kkloc           s                   like(amloc)
     D kkhcpc          s                   like(amhcpc)
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
     C     fmky          klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    kkhcpc
     C                   kfld                    kkefdt
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*
     C     fmky1         klist
     C                   kfld                    tmamst
     C                   kfld                    tmamloc
     C                   kfld                    tmamhcpc
     C                   kfld                    tmamefdt
    C*================================================================
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
     C                   if        amstat = 'I'
     C                   eval      amstat = 'A'
     C                   else
     C                   eval      amstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        amuser
     C                   movel     q1pgm         ampgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         amdt
     C                   movel     dstime        amtime
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
     C*
     C* If in ADD mode
     C                   if        *in40 = *on
     C* State must not be blank
     C                   if        scst = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   else
     C                   eval      kkkey = scst
     C     stky          chain     pst01
     C                   if        not %found
     C                   seton                                        769089
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* HCPCS code must not be blank
     C                   if        schcpc = *blanks
     C                   seton                                        749086
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Description should not be blank
     C*
     C                   if        scdesc = *blanks
     C                   seton                                        759050
     C                   leavesr
     C                   endif
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        739087
     C                   leavesr
     C                   else
     C*
     C* Effective Date must be valid
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779087
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Check for duplicate records
     C*
     C                   eval      tmamst = scst
     C                   eval      tmamloc = scloc
     C                   eval      tmamhcpc = schcpc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tmamefdt
     C*
     C     fmky1         chain     pam01
     C                   if        %found
     C                   seton                                        789089
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C*
     C* Base rate should not be blank
     C*
     C                   if        scbrat = *zeros
     C                   seton                                        799051
     C                   leavesr
     C                   endif
     C*
     C* RVU should not be blank
     C*
     C                   if        scrvu = *zeros
     C                   seton                                        809052
     C                   leavesr
     C                   endif
     C*
     C* GPCI should not be blank
     C*
     C                   if        scgpci = *zeros
     C                   seton                                        819053
     C                   leavesr
     C                   endif
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
     C                   clear                   schcpc
     C                   clear                   scefdt
     C                   clear                   scdesc
     C                   clear                   scbrat
     C                   clear                   scrvu
     C                   clear                   scgpci
     C                   clear                   scurbn
     C                   clear                   scrurl
     C                   clear                   scrgrd
     C*** Begin Add *** 09/02/11 **************************************
     C                   clear                   sclqsr
     C*** End Add   *** 09/02/11 **************************************
     C                   if        *in40 = *on
     C                   eval      scst = *blanks
     C                   eval      scloc = *zeros
     C                   eval      schcpc = *blanks
     C                   eval      scefdt = *zeros
     C                   eval      scdesc = *blanks
     C                   eval      scbrat = *zeros
     C                   eval      scrvu  = *zeros
     C                   eval      scgpci = *zeros
     C                   eval      scurbn = *zeros
     C                   eval      scrurl = *zeros
     C                   eval      scrgrd = *zeros
     C*** Begin Add *** 09/02/11 **************************************
     C                   eval      sclqsr = *zeros
     C*** End Add   *** 09/02/11 **************************************
     C                   eval      scstat = 'A'
     C*
     C                   else
     C*
     C                   eval      scst = amst
     C                   eval      scloc = amloc
     C                   eval      schcpc = amhcpc
     C                   z-add     amefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   movel     amdesc        scdesc
     C                   eval      scbrat = ambrat
     C                   eval      scrvu  = amrvu
     C                   eval      scgpci = amgpci
     C                   eval      scurbn = amurbn
     C                   eval      scrurl = amrurl
     C                   eval      scrgrd = amrgrd
     C*** Begin Add *** 09/02/11 **************************************
     C                   eval      sclqsr = amlqsr
     C*** End Add   *** 09/02/11 **************************************
     C                   eval      scstat = amstat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      amst = scst
     C                   eval      amloc = scloc
     C                   eval      amhcpc = schcpc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         amefdt
     C                   eval      amdesc = scdesc
     C                   eval      ambrat = scbrat
     C                   eval      amrvu  = scrvu
     C                   eval      amgpci = scgpci
     C                   eval      amurbn = scurbn
     C                   eval      amrurl = scrurl
     C                   eval      amrgrd = scrgrd
     C*** Begin Add *** 09/02/11 **************************************
     C                   eval      amlqsr = sclqsr
     C*** End Add   *** 09/02/11 **************************************
     C                   eval      amstat = scstat
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
     C                   movel     q1user        amuser
     C                   movel     q1pgm         ampgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         amdt
     c                   movel     dstime        amtime
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   movel     q1user        amuser
     C                   movel     q1pgm         ampgm
     C                   movel     q1pgm         amipgm
     C                   movel     q1user        amiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         amdt
     C                   movel     srdto         amidt
     c                   movel     dstime        amtime
     c                   movel     dstime        amitim
     C                   write     dbrcdf
     C                   feod      fmamp
     C                   endsr
