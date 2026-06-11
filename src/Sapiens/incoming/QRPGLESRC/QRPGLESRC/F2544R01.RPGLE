     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H**********************************************************************
    H* PROGRAM: F2544R01
    H* DESCRIPTION: IL Fee Schedule Update/Display
    H**********************************************************************
    H* PROGRAMMER NOTES:
    H**********************************************************************
    H*  CREATED: 08/11/10  C:227  PROJECT: IL001
    H**********************************************************************
    H*  REVISIONS:
    H**********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2544f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffmfspil   uf a e             disk
     F                                     rename(pfs01:dbrcdf)
     Ffmfsl1il  if   e           k disk
     Fftgtl     if   e           k disk
     Fftgtp     if   e             disk    rename(pgt01:pgt02)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IL Fee Schedule'
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
     C                   kfld                    kkzip
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
     C     *like         define    fszip         kkzip
     C     *like         define    fscode        kkcode
     C     *like         define    fsefdt        kkefdt
     C     *like         define    fscat         kkcat
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
     C                   if        fsstat = 'I'
     C                   eval      fsstat = 'A'
     C*
     C                   else
     C                   eval      fsstat = 'I'
     C*
     C                   endif
     C*
     C                   eval      fsuser = q1user
     C                   eval      fspgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         fsdt
     C                   eval      fstime = dstime
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
     C                   eval      sczip  = *blanks
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
     C* FSZIP cannot be blank
     C*
     C                   if        sczip  = *blanks
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
     C                   when      hfld = 'SCZIP'
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
     C                   eval      sczip = fszip
     C                   eval      sccat = fscat
     C                   eval      sccode = fscode
     C                   eval      scdsc1 = fsdsc1
     C                   eval      scstat = fsstat
     C                   eval      srdti = fsefdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
     C                   eval      scastc = fsastc
     C                   eval      scaspo = fsaspo
     C                   eval      scrpt  = fsrpt
     C                   eval      scnew  = fsnew
     C                   eval      scnwpo = fsnwpo
     C                   eval      scfsr  = fsfsr
     C                   eval      scused = fsused
     C                   eval      scuepo = fsuepo
     C                   eval      scasst = fsasst
     C                   eval      scrnt  = fsrnt
     C                   eval      scrnpo = fsrnpo
     C                   eval      sccosg = fscosg
     C                   eval      scciii = fsciii
     C                   eval      scc3po = fsc3po
     C                   eval      sctesg = fstesg
     C                   eval      scoutp = fsoutp
     C                   eval      scotpo = fsotpo
     C                   eval      scmult = fsmult
     C                   eval      scptot = fsptot
     C                   eval      sctppo = fstppo
     C                   eval      scbila = fsbila
     C                   eval      scppc  = fsppc
     C                   eval      scpppo = fspppo
     C                   eval      scane  = fsane
     C                   eval      scptc  = fsptc
     C                   eval      scptpo = fsptpo
     C                   eval      scglob = fsglob
     C                   eval      scdrgs = fsdrgs
     C                   eval      scdspo = fsdspo
     C                   eval      scdrgt = fsdrgt
     C                   eval      scdtpo = fsdtpo
     C                   eval      scanba = fsanba
     C                   eval      scancv = fsancv
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      fszip  = sczip
     C                   eval      fscat  = sccat
     C                   eval      fscode = sccode
     C                   eval      fsdsc1 = scdsc1
     C                   eval      fsstat = scstat
     C                   eval      srdti  = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         fsefdt
     C*
     C                   eval      fsastc = scastc
     C                   eval      fsaspo = scaspo
     C                   eval      fsrpt  = scrpt
     C                   eval      fsnew  = scnew
     C                   eval      fsnwpo = scnwpo
     C                   eval      fsfsr  = scfsr
     C                   eval      fsused = scused
     C                   eval      fsuepo = scuepo
     C                   eval      fsasst = scasst
     C                   eval      fsrnt  = scrnt
     C                   eval      fsrnpo = scrnpo
     C                   eval      fscosg = sccosg
     C                   eval      fsciii = scciii
     C                   eval      fsc3po = scc3po
     C                   eval      fstesg = sctesg
     C                   eval      fsoutp = scoutp
     C                   eval      fsotpo = scotpo
     C                   eval      fsmult = scmult
     C                   eval      fsptot = scptot
     C                   eval      fstppo = sctppo
     C                   eval      fsbila = scbila
     C                   eval      fsppc  = scppc
     C                   eval      fspppo = scpppo
     C                   eval      fsane  = scane
     C                   eval      fsptc  = scptc
     C                   eval      fsptpo = scptpo
     C                   eval      fsglob = scglob
     C                   eval      fsdrgs = scdrgs
     C                   eval      fsdspo = scdspo
     C                   eval      fsdrgt = scdrgt
     C                   eval      fsdtpo = scdtpo
     C                   eval      fsanba = scanba
     C                   eval      fsancv = scancv
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
     C                   eval      fspgm  = q1pgm
     C                   eval      fsuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         fsdt
     C                   movel     dstime        fstime
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
     C                   eval      fsstat = 'A'
     C                   eval      fsipgm = q1pgm
     C                   eval      fsiusr = q1user
     C                   eval      fspgm  = q1pgm
     C                   eval      fsuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         fsidt
     C                   movel     srdto         fsdt
     C                   movel     dstime        fsitim
     C                   movel     dstime        fstime
     C*
     C                   write     dbrcdf
     C                   feod      fmfspil
     C                   endsr
