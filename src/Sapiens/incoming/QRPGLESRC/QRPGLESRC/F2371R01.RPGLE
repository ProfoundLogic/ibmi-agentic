     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H actgrp(*caller)
     H/TITLE F2371R01                                                                      *
     H**************************************************************************************
     H*  Description:  Medicare DMEPOS Rates Maintenance                                   *
     H*                                                                                    *
     H*  Created: 08/12/10   Programmer C:251   Project:114MWS                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  01/06/16   C:079    Project : FS493                                               *
     H*                      Added Oklahoma and rural fields                               *
     H*                                                                                    *
     H*  10/11/11   C:280    Project : SEQ78229                                            *
     H*                      Added option 20 for status change                             *
     H*                      Added code to clear fields on add                             *
     H*                                                                                    *
     H*  08/16/11    C:274     Project : CT007                                             *
     H*                                  Added field for CT.                               *
     H*                                                                                    *
     H*  06/22/11    C:198     Project : 114MYS                                            *
     H*                                  Added field for PA.                               *
     H*                                                                                    *
     H*  01/17/11    C:198     Project : FS388                                             *
     H*                                  Added   AR/DC/TN/TX                               *
     H*                                                                                    *
     H*  10/01/10    C:198     Project : WV009                                             *
     H*                                  Added field for West Virginia.                    *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2371f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmdep     uf a e             disk
     F                                     rename(pde01:dbrcdf)
     Ffmdel1    if   e           k disk    prefix(tm)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Medicare DMEPOS Rates'
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
     D kkhcpc          s                   like(dehcpc)
     D kkmod           s                   like(demod)
     D kkmod1          s                   like(demod1)
     D kkefdt          s                   like(deefdt)
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
     C     deky          klist
     C                   kfld                    kkhcpc
     C                   kfld                    kkmod
     C                   kfld                    kkmod1
     C                   kfld                    kkefdt
     C*
     C     deky1         klist
     C                   kfld                    tmdehcpc
     C                   kfld                    tmdemod
     C                   kfld                    tmdemod1
     C                   kfld                    tmdeefdt
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
     C                   if        destat = 'I'
     C                   eval      destat = 'A'
     C                   else
     C                   eval      destat = 'I'
     C                   endif
    C*
     C                   movel     q1user        deuser
     C                   movel     q1pgm         depgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dedt
     C                   movel     dstime        detime
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
     C*
     C* HCPCS code must not be blank
     C                   if        schcpc = *blanks
     C                   seton                                        719086
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Description should not be blank
     C*
     C                   if        scdesc = *blanks
     C                   seton                                        749050
     C                   leavesr
     C                   endif
     C*
     C* Effective date not be blank (If in ADD mode)
     C                   if        *in40 = *on
     C                   if        scefdt = *zero
     C                   seton                                        739089
     C                   leavesr
     C                   else
     C*
     C* Effective Date must be valid
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        779089
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Check for duplicate records
     C*
     C                   eval      tmdehcpc = schcpc
     C                   eval      tmdemod = scmod
     C                   eval      tmdemod1 = scmod1
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         tmdeefdt
     C*
     C     deky1         chain     pde01
     C                   if        %found
     C                   seton                                        789086
     C                   leavesr
     C                   endif
     C*
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
     C                   clear                   schcpc
     C                   clear                   scmod
     C                   clear                   scmod1
     C                   clear                   scefdt
     C                   clear                   scdesc
     C                   clear                   scnj
     C                   clear                   scstat
     C*
     C                   if        *in40 = *on
     C                   eval      schcpc = *blanks
     C                   eval      scmod = *blanks
     C                   eval      scmod1 = *blanks
     C                   eval      scefdt = *zeros
     C                   eval      scdesc = *blanks
     C                   eval      scnj   = *zeros
     C                   eval      scstat = 'A'
     C*
     C*** Beg Add *** 10/11/11 ****************************************
     C*
     C                   eval      scar = *zeros
     C                   eval      scdc = *zeros
     C                   eval      scnj = *zeros
     C                   eval      sctn = *zeros
     C                   eval      sctx = *zeros
     C                   eval      scwv = *zeros
     C*
     C*** End Add *** 10/11/11 ****************************************
     C*** Beg Add *** 01/06/16 ****************************************
     C*
     C                   eval      scdcR = *zeros
     C                   eval      scnjR = *zeros
     C                   eval      sctnR = *zeros
     C                   eval      sctxR = *zeros
     C                   eval      scwvR = *zeros
     C                   eval      scokR = *zeros
     C                   eval      scok  = *zeros
     C*
     C*** End Add *** 01/06/16 ****************************************
     C*
     C                   else
     C*
     C                   eval      schcpc = dehcpc
     C                   eval      scmod = demod
     C                   eval      scmod1 = demod1
     C                   z-add     deefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C                   movel     dedesc        scdesc
     C                   eval      scnj = denj
     C*** Beg Add *** 01/17/11 ****************************************
     C                   eval      scar = dear
     C                   eval      scdc = dedc
     C                   eval      sctn = detn
     C                   eval      sctx = detx
     C*** End Add *** 01/17/11 ****************************************
     C*** Beg Add *** 10/01/10 ****************************************
     C                   eval      scwv = dewv
     C*** End Add *** 10/01/10 ****************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C                   eval      scpa = depa
     C*** End Add *** 06/22/11 ****************************************
     C*** Beg Add *** 08/16/11 ****************************************
     C                   eval      scct = dect
     C*** End Add *** 08/16/11 ****************************************
     C*** Beg Add *** 01/06/16 ****************************************
     C                   eval      scARR = DEarR
     C                   eval      scCTR = DEctR
     C                   eval      scDCR = DEdcR
     C                   eval      scNJR = DEnjR
     C                   eval      scPAR = DEpaR
     C                   eval      scTNR = DEtnR
     C                   eval      scTXR = DEtxR
     C                   eval      scWVR = DEwvR
     C                   eval      scOKR = DEokR
     C                   eval      scOK = DEok
     C*** End Add *** 01/06/16 ****************************************
     C                   eval      scstat = destat
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      dehcpc = schcpc
     C                   eval      demod = scmod
     C                   eval      demod1 = scmod1
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         deefdt
     C                   eval      dedesc = scdesc
     C                   eval      denj = scnj
     C*** Beg Add *** 01/17/11 ****************************************
     C                   eval      dear = scar
     C                   eval      dedc = scdc
     C                   eval      detn = sctn
     C                   eval      detx = sctx
     C*** End Add *** 01/17/11 ****************************************
     C*** Beg Add *** 10/01/10 ****************************************
     C                   eval      dewv = scwv
     C*** End Add *** 10/01/10 ****************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C                   eval      depa = scpa
     C*** End Add *** 06/22/11 ****************************************
     C*** Beg Add *** 08/16/11 ****************************************
     C                   eval      dect = scct
     C*** End Add *** 08/16/11 ****************************************
     C*** Beg Add *** 01/06/16 ****************************************
     C                   eval      DEarR = scARR
     C                   eval      DEctR = scCTR
     C                   eval      DEdcR = scDCR
     C                   eval      DEnjR = scNJR
     C                   eval      DEpaR = scPAR
     C                   eval      DEtnR = scTNR
     C                   eval      DEtxR = scTXR
     C                   eval      DEwvR = scWVR
     C                   eval      DEokR = scOKR
     C                   eval      DEok = scOK
     C*** End Add *** 01/06/16 ****************************************
     C                   eval      destat = scstat
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
     C*
     C                   movel     q1user        deuser
     C                   movel     q1pgm         depgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dedt
     C                   movel     dstime        detime
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
     C                   movel     q1user        deuser
     C                   movel     q1pgm         depgm
     C                   movel     q1pgm         deipgm
     C                   movel     q1user        deiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dedt
     C                   movel     srdto         deidt
     c                   movel     dstime        detime
     c                   movel     dstime        deitim
     C                   write     dbrcdf
     C                   feod      fmdep
     C                   endsr
