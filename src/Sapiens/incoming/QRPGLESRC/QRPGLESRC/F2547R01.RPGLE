     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H**********************************************************************
    H* PROGRAM: F2547R01
    H* DESCRIPTION: IL FS by Region Maintenance
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 02/21/12  C:277  PROJECT: IL004
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
     Ff2547f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmf1pil   uf a e             disk
     F                                     rename(pf101:dbrcdf)
     Ffmf1l1il  if   e           k disk
     Fftgtl     if   e           k disk
     Fftgtp     if   e             disk    rename(pgt01:pgt02)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IL FS by Region Maintenance'
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
     C                   kfld                    kkrgn#
     C                   kfld                    kkcode
     C                   kfld                    kkefdt
     C                   kfld                    kkcat
     C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    f1rgn#        kkrgn#
     C     *like         define    f1code        kkcode
     C     *like         define    f1efdt        kkefdt
     C     *like         define    f1cat         kkcat
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
     C                   if        f1stat = 'I'
     C                   eval      f1stat = 'A'
     C*
     C                   else
     C                   eval      f1stat = 'I'
     C*
     C                   endif
     C*
     C                   eval      f1user = q1user
     C                   eval      f1pgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f1dt
     C                   eval      f1time = dstime
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
     C*                  clear                   scr01
     C                   eval      scrgn# = *blanks
     C                   eval      sccat  = *blanks
     C                   eval      sccode = *blanks
     C                   eval      scdsc1 = *blanks
     C                   eval      scstat = 'A'
     C                   eval      scefdt = 0
     C                   eval      scastc = 0
     C                   eval      scaspo = 0
     C                   eval      scrpt  = *blanks
     C                   eval      scnew  = 0
     C                   eval      scnwpo = 0
     C                   eval      scfsr  = *blanks
     C                   eval      scused = 0
     C                   eval      scuepo = 0
     C                   eval      scasst = *blanks
     C                   eval      scrnt  = 0
     C                   eval      scrnpo = 0
     C                   eval      sccosg = *blanks
     C                   eval      scciii = 0
     C                   eval      scc3po = 0
     C                   eval      sctesg = *blanks
     C                   eval      scoutp = 0
     C                   eval      scotpo = 0
     C                   eval      scmult = *blanks
     C                   eval      scptot = 0
     C                   eval      sctppo = 0
     C                   eval      scbila = *blanks
     C                   eval      scppc  = 0
     C                   eval      scpppo = 0
     C                   eval      scane  = *blanks
     C                   eval      scptc  = 0
     C                   eval      scptpo = 0
     C                   eval      scglob = 0
     C                   eval      scdrgs = 0
     C                   eval      scdspo = 0
     C                   eval      scdrgt = 0
     C                   eval      scdtpo = 0
     C                   eval      scanba = 0
     C                   eval      scancv = 0
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
     C* F1RGN# cannot be blank
     C*
     C                   if        scrgn# = *blanks
     C                   seton                                        7190
     C                   goto      ck1end
     C                   endif
     C*
     C* Code cannot be blank
     C*
     C                   if        sccode = *blanks
     C                   seton                                        7290
     C                   goto      ck1end
     C                   endif
     C*
     C* Category cannot be blank
     C*
     C                   if        sccat = *blanks
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
     C* Description cannot be blank
     C*
     C                   if        scdsc1 = *blanks
     C                   seton                                        7690
     C                   goto      ck1end
     C                   endif
     C*
     C* Validate Assistant Surgeon
     C*
     C                   if        scasst <> *blanks
     C                   eval      kktbid = 'MI80'
     C                   eval      kkkey = scasst
     C     gtky          chain     pgt01
     C                   if        not %found
     C                   seton                                        808190
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Validate Co Surgeon
     C*
     C                   if        sccosg <> *blanks
     C                   eval      kktbid = 'MI62'
     C                   eval      kkkey = sccosg
     C     gtky          chain     pgt01
     C                   if        not %found
     C                   seton                                        828390
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Validate Team Surgeon
     C*
     C                   if        sctesg <> *blanks
     C                   eval      kktbid = 'MI66'
     C                   eval      kkkey = sctesg
     C     gtky          chain     pgt01
     C                   if        not %found
     C                   seton                                        848590
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Validate Multi Procedure
     C*
     C                   if        scmult <> *blanks
     C                   eval      kktbid = 'IL51'
     C                   eval      kkkey = scmult
     C     gtky          chain     pgt01
     C                   if        not %found
     C                   seton                                        868790
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Validate Bilateral Procedure
     C*
     C                   if        scbila <> *blanks
     C                   eval      kktbid = 'MI50'
     C                   eval      kkkey = scbila
     C     gtky          chain     pgt01
     C                   if        not %found
     C                   seton                                        888990
     C                   leavesr
     C                   endif
     C                   endif
     C*
     C* Check for duplicate record
     C*
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
     C                   when      hfld = 'SCASST'
     c*
     C                   eval      ##tbid = 'MI80'
     c                   call      'F2380R'      gtpl
     c                   if        ##kc = *on or ##kl = *on
     c                             or ##rrn = *zeros
     c                   seton                                        8090
     c                   else
     c     ##rrn         chain     pgt02
     c                   movel(p)  gtkey         scasst
     c                   seton                                        9190
     c                   endif
     c*
     C                   when      hfld = 'SCCOSG'
     c*
     C                   eval      ##tbid = 'MI62'
     c                   call      'F2380R'      gtpl
     c                   if        ##kc = *on or ##kl = *on
     c                             or ##rrn = *zeros
     c                   seton                                        8290
     c                   else
     c     ##rrn         chain     pgt02
     c                   movel(p)  gtkey         sccosg
     c                   seton                                        9290
     c                   endif
     C*
     C                   when      hfld = 'SCTESG'
     C*
     C                   eval      ##tbid = 'MI66'
     C                   call      'F2380R'      gtpl
     C                   if        ##kc = *on or ##kl = *on
     C                             or ##rrn = *zeros
     C                   seton                                        8490
     C                   else
     C     ##rrn         chain     pgt02
     C                   movel(p)  gtkey         sctesg
     C                   seton                                        9390
     C                   endif
     C*
     C                   when      hfld = 'SCMULT'
     C*
     C                   eval      ##tbid = 'IL51'
     C                   call      'F2380R'      gtpl
     C                   if        ##kc = *on or ##kl = *on
     C                             or ##rrn = *zeros
     C                   seton                                        8690
     C                   else
     C     ##rrn         chain     pgt02
     C                   movel(p)  gtkey         scmult
     C                   seton                                        9490
     C                   endif
     C*
     C                   when      hfld = 'SCBILA'
     C*
     C                   eval      ##tbid = 'MI50'
     C                   call      'F2380R'      gtpl
     C                   if        ##kc = *on or ##kl = *on
     C                             or ##rrn = *zeros
     C                   seton                                        8890
     C                   else
     C     ##rrn         chain     pgt02
     C                   movel(p)  gtkey         scbila
     C                   seton                                        9590
     C                   endif
     C*
     C                   when      hfld = 'SCRGN#'
     C                   seton                                        305090
     C*
     C                   when      hfld = 'SCCODE'
     C                   seton                                        315190
     C*
     C                   when      hfld = 'SCCAT'
     C                   seton                                        325290
     C*
     C                   when      hfld = 'SCEFDT'
     C                   seton                                        335390
     C*
     C                   when      hfld = 'SCDSC1'
     C                   seton                                        345490
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
     C                   movel     f1rgn#        scrgn#
     C                   eval      sccat = f1cat
     C                   eval      sccode = f1code
     C                   eval      scdsc1 = f1dsc1
     C                   eval      scstat = f1stat
     C                   eval      srdti = f1efdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   eval      scastc = f1astc
     C                   eval      scaspo = f1aspo
     C                   eval      scrpt  = f1rpt
     C                   eval      scnew  = f1new
     C                   eval      scnwpo = f1nwpo
     C                   eval      scfsr  = f1fsr
     C                   eval      scused = f1used
     C                   eval      scuepo = f1uepo
     C                   eval      scasst = f1asst
     C                   eval      scrnt  = f1rnt
     C                   eval      scrnpo = f1rnpo
     C                   eval      sccosg = f1cosg
     C                   eval      scciii = f1ciii
     C                   eval      scc3po = f1c3po
     C                   eval      sctesg = f1tesg
     C                   eval      scoutp = f1outp
     C                   eval      scotpo = f1otpo
     C                   eval      scmult = f1mult
     C                   eval      scptot = f1ptot
     C                   eval      sctppo = f1tppo
     C                   eval      scbila = f1bila
     C                   eval      scppc  = f1ppc
     C                   eval      scpppo = f1pppo
     C                   eval      scane  = f1ane
     C                   eval      scptc  = f1ptc
     C                   eval      scptpo = f1ptpo
     C                   eval      scglob = f1glob
     C                   eval      scdrgs = f1drgs
     C                   eval      scdspo = f1dspo
     C                   eval      scdrgt = f1drgt
     C                   eval      scdtpo = f1dtpo
     C                   eval      scanba = f1anba
     C                   eval      scancv = f1ancv
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   movel     scrgn#        f1rgn#
     C                   eval      f1cat  = sccat
     C                   eval      f1code = sccode
     C                   eval      f1dsc1 = scdsc1
     C                   eval      f1stat = scstat
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f1efdt
     C*
     C                   eval      f1astc = scastc
     C                   eval      f1aspo = scaspo
     C                   eval      f1rpt  = scrpt
     C                   eval      f1new  = scnew
     C                   eval      f1nwpo = scnwpo
     C                   eval      f1fsr  = scfsr
     C                   eval      f1used = scused
     C                   eval      f1uepo = scuepo
     C                   eval      f1asst = scasst
     C                   eval      f1rnt  = scrnt
     C                   eval      f1rnpo = scrnpo
     C                   eval      f1cosg = sccosg
     C                   eval      f1ciii = scciii
     C                   eval      f1c3po = scc3po
     C                   eval      f1tesg = sctesg
     C                   eval      f1outp = scoutp
     C                   eval      f1otpo = scotpo
     C                   eval      f1mult = scmult
     C                   eval      f1ptot = scptot
     C                   eval      f1tppo = sctppo
     C                   eval      f1bila = scbila
     C                   eval      f1ppc  = scppc
     C                   eval      f1pppo = scpppo
     C                   eval      f1ane  = scane
     C                   eval      f1ptc  = scptc
     C                   eval      f1ptpo = scptpo
     C                   eval      f1glob = scglob
     C                   eval      f1drgs = scdrgs
     C                   eval      f1dspo = scdspo
     C                   eval      f1drgt = scdrgt
     C                   eval      f1dtpo = scdtpo
     C                   eval      f1anba = scanba
     C                   eval      f1ancv = scancv
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
     C                   eval      f1pgm  = q1pgm
     C                   eval      f1user = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f1dt
     C                   movel     dstime        f1time
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
     C                   eval      f1stat = 'A'
     C                   eval      f1ipgm = q1pgm
     C                   eval      f1iusr = q1user
     C                   eval      f1pgm  = q1pgm
     C                   eval      f1user = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f1idt
     C                   movel     srdto         f1dt
     C                   movel     dstime        f1itim
     C                   movel     dstime        f1time
     C*
     C                   write     dbrcdf
     C                   feod      fmf1pil
     C                   endsr
