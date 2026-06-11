     H copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H actgrp(*caller)
     H/TITLE F2358R01                                                                      *
     H**************************************************************************************
     H*  Description:  Medicare ASC Payment Rates Maintenance                              *
     H*                                                                                    *
     H*  Created: 11/14/08   Programmer C:212   Project:TX043                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/11/11   C:280    Project : SEQ78229                                            *
     H*                      Added option 20 for status change                             *
     H*                                                                                    *
     H*  01/17/11  C:198  Project : FS388
     H*                   Added processing for F7=Calculate Medicare allowance
     H*
     H*  10/01/10  C:198  Project:WV009
     H*                   Added 2 fields:
     H*                   - Transitioned payment weight
     H*                   - Transitioned weight
     H*
     H*  06/30/09  C:227  Project: TX063
     H*                   -Added field edit functionality
     H*                   -Added field search
     H*                   -Added functionality for F6-Add
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2358f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmasp     uf a e             disk
     F                                     rename(pas01:dbrcdf)
     F*** BEG ADD *** 06/30/09 ****************************************
     Fftgtl     if   e           k disk
     Fftgtp     if   e             disk    rename(pgt01:pgt02)
     Ffmasl1    if   e           k disk
     F*** END ADD *** 06/30/09 ****************************************
     F*** Beg Add *** 01/17/11 ****************************************
     Fftsal1    if   e           k disk    rename(psa01:psa0l)
     Fftsap     if   e             disk
     Fststl1    if   e           k disk
     Fftwgl1    if   e           k disk
     Fftwgl2    if   e           k disk    rename(pwg01:pwg02)
     F*** End Add *** 01/17/11 ****************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'MEDICARE ASC PAYMENT RATES INFO'
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
     D*** BEG ADD *** 06/30/09 ****************************************
     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
     D kkhcpc          s                   like(ashcpc)
     D kkefdt          s                   like(asefdt)
     D kkpi            s                   like(aspi)
     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
     D ##tbid          s                   like(gttbid)
     D*** END ADD *** 06/30/09 ****************************************
     D*** Beg Add *** 01/17/11 ****************************************
     D kkst            s                   like(sast)
     D kkcty           s                   like(sacty)
     D kkcbsa          s                   like(sacbsa)
     D kknam           s                   like(wgnam)
     D kkkey2          s                   like(stkey)
     D xxefdt          s                   like(saefdt)
     D*** End Add *** 01/17/11 ****************************************
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
     C*** BEG ADD *** 06/30/09 ****************************************
     C     gtpl          plist
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##kc
     C                   parm      *blanks       ##kl
     C                   parm      *zeros        ##rrn
     C                   parm                    ##tbid
     C*** END ADD *** 06/30/09 ****************************************
     C*** Beg Add *** 01/17/11 ****************************************
     C     statl         plist
     C                   parm                    @@call            1
     C                   parm                    @@kc2             1
     C                   parm                    @@kl2             1
     C                   parm                    ##rrn2            9 0
     C*** End Add *** 01/17/11 ****************************************
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*** BEG ADD *** 06/30/09 ****************************************
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C     asky          klist
     C                   kfld                    kkhcpc
     C                   kfld                    kkefdt
     C*                  kfld                    kkpi
     C*** END ADD *** 06/30/09 ****************************************
     C*** Beg Add *** 01/17/11 ****************************************
     C* ftsal1 key
     C     saky          klist
     C                   kfld                    kkcty
     C                   kfld                    kkst
     C                   kfld                    xxefdt
     C     saky1         klist
     C                   kfld                    kkcty
     C                   kfld                    kkst
     C* ftwgl1 key
     C     wgky          klist
     C                   kfld                    kkcbsa
     C                   kfld                    xxefdt
     C     wgky1         klist
     C                   kfld                    kkcbsa
     C* ftwgl2 key
     C     wgky2         klist
     C                   kfld                    kknam
     C                   kfld                    xxefdt
     C     wgky3         klist
     C                   kfld                    kknam
     C*
     C*** End Add *** 01/17/11 ****************************************
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
     C                   if        asstat = 'I'
     C                   eval      asstat = 'A'
     C                   else
     C                   eval      asstat = 'I'
     C                   endif
    C*
     C                   movel     q1user        asuser
     C                   movel     q1pgm         aspgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         asdt
     C                   movel     dstime        astime
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
     C*** BEG ADD *** 06/30/09 ****************************************
     C* HCPC code must not be blank (If in ADD mode)
     C                   if        *in40 = *on and
     C                             schcpc = *blanks
     C                   seton                                        719089
     C                   leavesr
     C                   endif
     C* HCPC desc must not be blank (If in ADD mode)
     C                   if        scdesc = *blanks
     C                   seton                                        729088
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
     C* Payment Indicator can not be blank (If in ADD mode)
     C                   if        scpi = *blanks
     C                   seton                                        749086
     C                   leavesr
     C                   else
     C                   eval      kktbid = 'ASPI'
     C                   eval      kkkey = scpi
     C     gtky          chain     pgt01
     C                   if        not %found
     C                   seton                                        759086
     C                   leavesr
     C                   endif
     C                   endif
     C* Check for duplicate record in FMASP(If in ADD mode)
     C                   eval      kkhcpc = schcpc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C*                  eval      kkpi = scpi
     C     asky          chain     pas01
     C                   if        %found
     C                   seton                                        769089
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C*** END ADD *** 06/30/09 ****************************************
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
     C*** BEG ADD *** 06/30/09 ****************************************
    C*================================================================
    C* KJDO - F10=Searches
    C*================================================================
     C     kjdo          begsr
     C                   setoff                                       31
     C                   if        hfld = 'SCPI'
     c*
     C                   eval      ##tbid = 'ASPI'
     c                   call      'F2380R'      gtpl
     c                   if        ##kc = *on or ##kl = *on
     c                             or ##rrn = *zeros
     c                   seton                                        8690
     c                   else
     c     ##rrn         chain     pgt02
     c                   movel(p)  gtkey         scpi
     c                   seton                                        8690
     c                   endif
     c*
     C                   endif
     C                   endsr
     C*** END ADD *** 06/30/09 ****************************************
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C*** BEG CHG *** 06/30/09 ****************************************
     C***                if        scmode = moded and @@rrn = 0
     C                   if        *in40 = *on
     C*** END CHG *** 06/30/09 ****************************************
     C*
     C                   move      *blanks       schcpc
     C                   move      *zeros        scefdt
     C                   move      *blanks       scdesc
     C                   move      *blanks       scpi
     C                   move      *zeros        scpmwt
     C                   move      *zeros        scalow
     C                   move      *blanks       scmult
     C                   move      *blanks       scanc
     C                   move      *blanks       scdev
     C*** Beg Add *** 10/01/10 **********************************
     C                   move      *zeros        sctpwt
     C                   move      *zeros        sctpmt
     C*** End Add *** 10/01/10 **********************************
     C                   move      'A'           scstat
     C*
     C                   else
     C*
     C                   movel     ashcpc        schcpc
     C                   z-add     asefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C*** Beg Add *** 01/17/11 ****************************************
     C                   eval      *in51 = *off
     C                   if        asefdt >= 1100101
     C                   eval      *in51 = *on
     C                   else
     C                   eval      *in51 = *off
     C                   endif
     C*** End Add *** 01/17/11 ****************************************
     C                   movel     asdesc        scdesc
     C                   movel     aspi          scpi
     C                   eval      scpmwt =  aspmwt
     C                   eval      scalow =  asalow
     C                   movel     asmult        scmult
     C                   movel     asanc         scanc
     C                   movel     asdev         scdev
     C                   movel     asstat        scstat
     C*** Beg Add *** 10/01/10 **********************************
     C                   eval      sctpwt =  astpwt
     C                   eval      sctpmt =  astpmt
     C*** End Add *** 10/01/10 **********************************
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*** BEG ADD *** 06/30/09 ****************************************
     C                   eval      ashcpc = schcpc
     C                   eval      asdesc = scdesc
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         asefdt
     C                   eval      aspi = scpi
     C                   eval      asmult = scmult
     C                   eval      aspmwt = scpmwt
     C                   eval      asalow = scalow
     C                   eval      asanc = scanc
     C                   eval      asdev = scdev
     C**                 eval      asdper = scdper
     C                   eval      asstat = scstat
     C*** Beg Add *** 10/01/10 **********************************
     C                   eval      astpwt =  sctpwt
     C                   eval      astpmt =  sctpmt
     C*** End Add *** 10/01/10 **********************************

     C*** END ADD *** 06/30/09 ****************************************
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
     C*** BEG ADD *** 06/30/09 ****************************************
     C     *INKJ         caseq     *on           KJDO
     C*** Beg Add *** 01/17/11 ****************************************
     C   51*inkg         caseq     *on           kgdo
     C*** End Add *** 01/17/11 ****************************************
     C                   endcs
     C*** END ADD *** 06/30/09 ****************************************
     C                   endsr
     C*** Beg Add *** 01/17/11 ****************************************
    C*================================================================
    C* KGDO   - Processing for F7=Calculate Medicare Allowance
    C*================================================================
     C     kgdo          begsr
     C*
     C                   dou       *inkc = *on or *inkl = *on
     C*
     C                   if        *inkg = *on
     C                   clear                   sscty
     C                   clear                   ssst
     C                   endif
     C*
     C                   exfmt     wdw01
     C                   movea     '000'         *in(54)
     C*
     C                   if        *inkj = *on
     C*
     C                   if        hfld = 'SSCTY' or hfld = 'SSST'
     C*** call F2359R
     C                   eval      @@call  = '2'
     C                   eval      ##rrn2 = *zeros
     C                   call      'F2359R'      statl
     C*
     C                   if        @@kc2 = '1'
     C                             or @@kl2 = '1'
     C                             or ##rrn2 = *zeros
     C*
     C                   select
     C                   when      hfld = 'SSCTY'
     C                   eval      *in80 = *on
     C                   when      hfld = 'SSST'
     C                   eval      *in78 = *on
     C                   endsl
     C*
     C                   endif                                                  #rrn2 = *zeroes
     C*
     C                   if        ##rrn2 <> 0
     C     ##rrn2        chain     psa01                              99
     C                   if        *in99 = *off
     C                   movel     sacty         sscty
     C                   movel     sast          ssst
     C                   movel     sacbsa        kkcbsa
     C                   write     wdw01
     C                   endif                                                  *in99 = '0'
     C                   endif                                                  @@rrn2 <> 0
     C*
     C                   endif                                                  hfld = 'SSCTY/SSST'
     C*
     C                   endif                                                  if *inkj = *on
     C                   eval      *in90 = *off
     C*
     C                   if        *inkj  <> '1' and *inkc <> '1' and
     C                             *inkl <> '1'
     C*
     C                   movea     '000'         *in(54)
     C*
     C                   exsr      errwdw1
     C                   if        *in90 = *off
     C                   exsr      calmed
     C                   endif                                                  *in90 = *off
     C*
     C                   endif                                                  *inkj/*inkc/*inkl=1
     C*
     C                   enddo                                                  dou *inkc = '1'
     C*
     C                   endsr
    C*================================================================
    C* ERRWDW1   - Error check on window 1
    C*================================================================
     C     errwdw1       begsr
     C*
     C*** city and state combination not in ftsap
     C                   if        sscty =  ' '
     C                   eval      *in54 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        ssst =  ' '
     C                   eval      *in55 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        ssst <> ' ' and sscty <> ' '
     C                   movel     sscty         kkcty
     C                   movel     ssst          kkst
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxefdt
     C     saky          setgt     psa0l
     C     saky1         readpe    psa0l                                  99
     C*
     C                   if        *in99 = *on
     C                   eval      *in56 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   else
     C                   movel     sacbsa        kkcbsa
     C                   endif                                                  *in99 = *on
     C*
     C                   endif                                                  ssst & sscty <> ' '
     C*
     C                   endsr
    C*================================================================
    C* CALMED   - Processing to calculate Medicare Allowance
    C*================================================================
     C     calmed        begsr
     C*
     C     wgky          setgt     pwg01
     C     wgky1         readpe    pwg01                                  99
     C                   if        *in99 = '0'
     C                   eval      wdalow = (asalow * .50 * wgwind) +
     C                                      (asalow * .50)
     C                   else
     C**** use state default wage index
     C                   movel     kkst          kkkey2
     C     kkkey2        chain     pst01                              99
     C                   if        *in99 = *off
     C                   movel     stname        kknam
     C     wgky2         setgt     pwg02
     C     wgky3         readpe    pwg02                                  99
     C                   if        *in99 = '0'
     C                   eval      wdalow = (asalow * .50 * wgwind) +
     C                                      (asalow * .50)
     C                   endif                                                  *in99 = *off
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   movel     schcpc        wdprcd
     C                   movel     wgbsa         wdcbsa
     C                   movel     wgwind        wdwind
     C                   movel     sscty         wdcty
     C                   movel     ssst          wdst
     C                   dou       *inkc = *on or *inkl = *on
     C                   exfmt     wdw02
     C                   enddo                                                  *inkc or *inkl = *on
     C                   eval      *inkl = *off
     C*
     C                   endsr
     C*** End Add *** 01/17/11 ****************************************
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*** BEG ADD *** 06/30/09 ****************************************
     C     @@mode        comp      'D'                                    09    Inquiry
     C     @@mode        comp      'A'                                    40    Add
    C*** Beg Add *** 10/11/11 ****************************************
     C     @@mode        comp      'C'                                    10    change
    C*** End Add *** 10/11/11 ****************************************
     C                   setoff                                           31
     C*** END ADD *** 06/30/09 ****************************************
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
     C*** BEG ADD *** 06/30/09 ****************************************
     C                   movel     q1user        asuser
     C                   movel     q1pgm         aspgm
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         asdt
     c                   movel     dstime        astime
     C*** END ADD *** 06/30/09 ****************************************
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C*** BEG ADD *** 06/30/09 ****************************************
     C                   movel     q1user        asuser
     C                   movel     q1pgm         aspgm
     C                   movel     q1pgm         asipgm
     C                   movel     q1user        asiusr
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         asdt
     C                   movel     srdto         asidt
     c                   movel     dstime        astime
     c                   movel     dstime        asitim
     C*** END ADD *** 06/30/09 ****************************************
     C                   write     dbrcdf
     C                   feod      fmasp
     C                   endsr
