     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F2547R01
    H* DESCRIPTION: IL Region to Zip for Hospitals Maintenance
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 02/21/12  C:277  PROJECT: IL004
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
     Ff2548f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffthzpil   uf a e             disk
     F                                     rename(phz01:dbrcdf)
     Ffthzl1il  if   e           k disk
     Fftgtl     if   e           k disk
     Fftgtp     if   e             disk    rename(pgt01:pgt02)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IL Zip Codes to Regions (Hospital)'
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
     D ##call          s              1
     D ##kc            s              1
     D ##kl            s              1
     D ##rrn           s              9  0
     D xxrgn#          s                   like(scrgn#)
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
    C*
     C     gtpl          plist
     C                   parm      '2'           ##call
     C                   parm      *blanks       ##kc
     C                   parm      *blanks       ##kl
     C                   parm      *zeros        ##rrn
     C                   parm                    ##tbid
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     ilky          klist
     C                   kfld                    kkzip
     C                   kfld                    kkcnty
     C                   kfld                    kkrgn#
     C                   kfld                    kkefdt
     C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    hzrgn#        kkrgn#
     C     *like         define    hzzip         kkzip
     C     *like         define    hzefdt        kkefdt
     C     *like         define    hzcnty        kkcnty
     C     *like         define    gttbid        kktbid
     C     *like         define    gtkey         kkkey
     C     *like         define    gttbid        ##tbid
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        hzstat = 'I'
     C                   eval      hzstat = 'A'
     C*
     C                   else
     C                   eval      hzstat = 'I'
     C*
     C                   endif
     C*
     C                   eval      hzuser = q1user
     C                   eval      hzpgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hzdt
     C                   eval      hztime = dstime
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
     C*
     C                   eval      scrgn# = *blanks
     C                   eval      sccnty = *blanks
     C                   eval      sczip  = *blanks
     C                   eval      scstat = 'A'
     C                   eval      scefdt = 0
     C*
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
     C* Region cannot be blank
     C*
     C                   if        scrgn# = *blanks
     C                   seton                                        7190
     C                   goto      ck1end
     C                   endif
     C*
     C* Zip Code cannot be blank
     C*
     C                   if        sczip  = *blanks
     C                   seton                                        7290
     C                   goto      ck1end
     C                   endif
     C*
     C* County  cannot be blank
     C*
     C                   if        sccnty = *blanks
     C                   seton                                        7390
     C                   goto      ck1end
     C                   endif
     C*
     C* Effective Date cannot be zero
     C*
     C                   if        scefdt = 0
     C                   seton                                        7490
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
     C                   seton                                        7590
     C                   goto      ck1end
     C                   endif
     C*
     C* Check for duplicate record
     C*
     C*
     C                   move      sczip         kkzip
     C                   move      sccnty        kkcnty
     C                   move      scrgn#        kkrgn#
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   if        srerr = '0'
     C                   movel     srdto         kkefdt
     C                   endif
     C*
     C                   if        @@mode = 'A'
     C     ilky          chain     phz01                              99
     C                   if        *in99 = *off
     C                   eval      *in82 = *on
     C                   else
     C                   eval      *in82 = *off
     C                   endif
     C                   endif
     C*
     C                   if        @@mode = 'C' and xxrgn# <> scrgn#
     C     ilky          chain     phz01                              99
     C                   if        *in99 = *off
     C                   eval      *in82 = *on
     C                   else
     C                   eval      *in82 = *off
     C                   endif
     C                   endif
     C*
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
    C*================================================================
    C* KJDO - F10=Searches
    C*================================================================
     C     kjdo          begsr
     C                   setoff                                       31
     C                   select
     C*
     C                   when      hfld = 'SCRGN#'
     C                   seton                                        305090
     C*
     C                   when      hfld = 'SCZIP'
     C                   seton                                        315190
     C*
     C                   when      hfld = 'SCCNTY'
     C                   seton                                        325290
     C*
     C                   when      hfld = 'SCEFDT'
     C                   seton                                        335390
     C*
     C                   endsl
     c*
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
     C                   movel     hzrgn#        scrgn#
     C                   movel     hzrgn#        xxrgn#
     C                   eval      sccnty = hzcnty
     C                   move      hzzip         sczip
     C                   eval      scstat = hzstat
     C                   eval      srdti = hzefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   movel     scrgn#        hzrgn#
     C                   eval      hzcnty = sccnty
     C                   move      sczip         hzzip
     C                   eval      hzstat = scstat
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hzefdt
     C*
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
     C     *inkj         caseq     *on           kjdo
     C                   endcs
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
     C                   eval      hzpgm  = q1pgm
     C                   eval      hzuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hzdt
     C                   movel     dstime        hztime
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
     C                   eval      hzstat = 'A'
     C                   eval      hzipgm = q1pgm
     C                   eval      hziusr = q1user
     C                   eval      hzpgm  = q1pgm
     C                   eval      hzuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hzidt
     C                   movel     srdto         hzdt
     C                   movel     dstime        hzitim
     C                   movel     dstime        hztime
     C*
     C                   write     dbrcdf
     C                   feod      fthzpil
     C                   endsr
