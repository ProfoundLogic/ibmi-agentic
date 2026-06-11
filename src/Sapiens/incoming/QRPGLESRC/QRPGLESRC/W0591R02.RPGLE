     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0591f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdwbp     uf a e             disk
     F                                     rename(pwb01:dbrcdf)
     Fwdwbl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Max and Min Weekly Benefits'
     D comapi          c                   'WA001R'
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
     C     wbkey         klist
     C                   kfld                    ##co#
     C                   kfld                    scyear
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
     C* year
     C                   if        scyear <= 1900
     C                   move      *on           *in71
     C     *in71         cabeq     *on           ender1                   90
     C                   endif
     C* stdt
     C                   eval      srdti = scstdt
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   move      *on           *in73
     C     *in73         cabeq     *on           ender1                   90
     C                   else
     C                   movel     srdto         scstdt
     C                   endif
     C* endt
     C                   eval      srdti = scendt
     C                   call      'SRCVTDAT'    datchk
     C                   if        srerr = '1'
     C                   move      *on           *in74
     C     *in74         cabeq     *on           ender1                   90
     C                   else
     C                   movel     srdto         scendt
     C                   endif

     c* Dates should be in correct order:
     c                   z-add     scstdt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxstdt            7 0

     c                   z-add     scendt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxendt            7 0
     c                   if        (xxstdt > xxendt) or
     c                             (xxendt < xxstdt)
     C                   move      *on           *in59
     C     *in59         cabeq     *on           ender1                   90
     C                   endif

     C* saww
     C                   if        scsaww <= *zeros
     C                   move      *on           *in72
     C     *in72         cabeq     *on           ender1                   90
     C                   endif
     C* tibx
     C                   if        sctibx <= *zeros
     C                   move      *on           *in75
     C     *in75         cabeq     *on           ender1                   90
     C                   endif
     C* tibn
     C                   if        sctibn <= *zeros
     C                   move      *on           *in76
     C     *in76         cabeq     *on           ender1                   90
     C                   endif
     C* iibx
     C                   if        sciibx <= *zeros
     C                   move      *on           *in77
     C     *in77         cabeq     *on           ender1                   90
     C                   endif
     C* iibn
     C                   if        sciibn <= *zeros
     C                   move      *on           *in78
     C     *in78         cabeq     *on           ender1                   90
     C                   endif
     C* sibx
     C                   if        scsibx <= *zeros
     C                   move      *on           *in79
     C     *in79         cabeq     *on           ender1                   90
     C                   endif
     C* libx
     C                   if        sclibx <= *zeros
     C                   move      *on           *in80
     C     *in80         cabeq     *on           ender1                   90
     C                   endif
     C* libn
     C                   if        sclibn <= *zeros
     C                   move      *on           *in81
     C     *in81         cabeq     *on           ender1                   90
     C                   endif
     C* dthx
     C                   if        scdthx <= *zeros
     C                   move      *on           *in82
     C     *in82         cabeq     *on           ender1                   90
     C                   endif
     C* tthr
     C                   if        sctthr <= *zeros
     C                   move      *on           *in83
     C     *in83         cabeq     *on           ender1                   90
     C                   endif
     C* year exist on add
     C                   if        *in08 = *on
     C     wbkey         chain     pwb01                              99
     C                   if        *in99 = *off
     C                   move      *on           *in84
     C     *in84         cabeq     *on           ender1                   90
     C                   endif
     C                   endif
     C*
     C     ender1        endsr
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
     C                   eval      scyear = wbyear
     C                   z-add     wbstdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scstdt
     C                   eval      scendt = wbendt
     C                   z-add     wbendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scendt
     C                   eval      scsaww = wbsaww
     C                   eval      sctibx = wbtibx
     C                   eval      sctibn = wbtibn
     C                   eval      sciibx = wbiibx
     C                   eval      sciibn = wbiibn
     C                   eval      scsibx = wbsibx
     C                   eval      scsibn = wbsibn
     C                   eval      sclibx = wblibx
     C                   eval      sclibn = wblibn
     C                   eval      scdthx = wbdthx
     C                   eval      scdthn = wbdthn
     C                   eval      sctthr = wbtthr
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      wbco# = ##CO#
     C                   eval      wbyear = scyear
     C                   z-add     scstdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wbstdt
     C                   z-add     scendt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wbendt
     C                   eval      wbsaww = scsaww
     C                   eval      wbtibx = sctibx
     C                   eval      wbtibn = sctibn
     C                   eval      wbiibx = sciibx
     C                   eval      wbiibn = sciibn
     C                   eval      wbsibx = scsibx
     C                   eval      wbsibn = scsibn
     C                   eval      wblibx = sclibx
     C                   eval      wblibn = sclibn
     C                   eval      wbdthx = scdthx
     C                   eval      wbdthn = scdthn
     C                   eval      wbtthr = sctthr
     C                   movel     q1user        wbuser
     C                   movel     q1pgm         wbpgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wbdt
     C                   movel     dstime        wbtm
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
B01  C                   if        @@mode = 'A'
     C                   eval      *in08 = *on
E01  C                   endif
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
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
     C* Init user
     C                   movel     q1user        wbiusr
     C                   movel     q1pgm         wbipgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         wbidt
     C                   movel     dstime        wbitm
     C*
     C                   write     dbrcdf
     C                   feod      wdwbp
     C*
     C                   eval      outflg = *on
     C*
     C                   endsr
