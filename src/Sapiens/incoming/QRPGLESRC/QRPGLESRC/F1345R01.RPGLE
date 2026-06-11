     H*  (C) Copyright Progressive Data Solutions, Inc., 2003
    H**********************************************************************
    H* PROMATCH USAGE INFORMATION
    H**********************************************************************
     H* CREATED ON 03/13/03 BY C:79 FORO PROJECT: 114MFH
     H**********************************************************************
     H* REVISIONS:
     H**********************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff1345f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftpup     uf a e             disk
     F                                     rename(ppu01:dbrcdf)
     Fftpul     if   e           k disk
     Fftgtp     if   e             disk
     Fftgtl     if   e           k disk    rename(pgt01:pgt02)
     Ffmfnp     if   e             disk
     Ffmfnl1    if   e           k disk    rename(pfn01:pfn02)
     Fststp     if   e             disk
     Fststl     if   e           k disk    rename(pst01:pst02)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Work With ProMatch Usage Info'
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
     D xxpatn          ds            68
     D  xxmspa                 1      2
     D  xxmjpa                 3      3
     D  xxsbpa                 4      5
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
     C*
     C     puky          klist
     C                   kfld                    kkpatn
     C                   kfld                    kkfnd
     C                   kfld                    kkast
     C                   kfld                    kkpmst
     C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
     C     stky          klist
     C                   kfld                    kksky
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C*
     C     *like         define    pupatn        kkpatn
     C     *like         define    pufnd         kkfnd
     C     *like         define    puast         kkast
     C     *like         define    pupmst        kkpmst
     C     *like         define    gtco#         kkco#
     C     *like         define    gttbid        kktbid
     C     *like         define    gtkey         kkkey
     C     *like         define    fnfnd         kkfnd
     C     *like         define    stkey         kksky
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99 = *off
     C*
     C                   if        pustat = 'I'
     C                   eval      pustat = 'A'
     C*
     C                   else
     C                   eval      pustat = 'I'
     C*
     C                   endif
     C*
     C                   eval      puuser = q1user
     C                   eval      pupgm  = q1pgm
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         pudt
     C                   eval      putime = dstime
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
     C                   eval      scpatn = *blanks
     C                   eval      scast  = *blanks
     C                   eval      scfnd  = 0
     C                   eval      scactn = *blanks
     C                   eval      scpmst = *blanks
     C                   eval      scie   = *blanks
     C                   eval      scastd = *blanks
     C                   eval      scfndd = *blanks
     C                   eval      scactd = *blanks
     C                   eval      scpmsd = *blanks
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
     C                   eval      xxpatn = scpatn
     C*
     C                   if        *in90 = *off
     C**********************************************************************
     C* Master pattern is required
     C**********************************************************************
     C                   if        xxmspa = *blanks
     C                   seton                                        9071
     C                   goto      ck1end
     C                   endif                                                  if scmspa = blanks
     C**********************************************************************
     C* Master pattern must be 01-16
     C**********************************************************************
     C                   if        xxmspa < '01' or xxmspa > '16'
     C                   seton                                        9072
     C                   goto      ck1end
     C                   endif                                                  if scmspa = blanks
     C**********************************************************************
     C* Major pattern must be A-Z if entered
     C**********************************************************************
     C                   if        xxmjpa <> *blanks
     C                   if        xxmjpa < 'A' or xxmjpa > 'Z'
     C                   seton                                        9072
     C                   goto      ck1end
     C                   endif                                                  if scmspa = blanks
     C                   endif                                                  if scmspa = blanks
     C**********************************************************************
     C* Sub pattern must be 00-99
     C**********************************************************************
     C                   if        xxsbpa <> *blanks
     C                   if        xxsbpa < '00' or xxsbpa > '99'
     C                   seton                                        9072
     C                   goto      ck1end
     C                   endif                                                  if scmspa = blanks
     C                   endif                                                  if scmspa = blanks
     C**********************************************************************
     C* Validate ProMatch results status must be valid
     C**********************************************************************
     C                   eval      scpmsd = *blanks
     C*
     C                   if        scpmst <> *blanks
     C*
     C                   eval      kktbid = 'PMST'
     C                   eval      kkkey  = scpmst
     C*
     C     gtky          chain     pgt02                              99
     C*
     C                   if        *in99 = *on or gtstat <> 'A'
     C                   seton                                        9073
     C                   goto      ck1end
     C                   else
     C                   eval      scpmsd = gtdesc
     C                   endif                                                  if in99 = on
     C*
     C                   endif                                                  if ##rrn <> 0
     C**********************************************************************
     C* Group must be valid if entered
     C**********************************************************************
     C                   eval      scfndd = *blanks
     C*
     C                   if        scfnd <> 0
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = scfnd
     C*
     C     fnky          chain     pfn02                              99
     C*
     C                   if        *in99 = *on or fnstat = 'I'
     C                   seton                                        9074
     C                   goto      ck1end
     C                   else
     C                   eval      scfndd = fnname
     C                   endif                                                  if scmspa = blanks
     C*
     C                   endif                                                  scfnd <> blanks
     C**********************************************************************
     C* State must be valid if entered
     C**********************************************************************
     C                   eval      scastd = *blanks
     C*
     C                   if        scast <> *blanks
     C                   eval      kksky = scast
     C*
     C     stky          chain     pst02                              99
     C*
     C                   if        *in99 = *on or ststat = 'I'
     C                   seton                                        9075
     C                   goto      ck1end
     C                   else
     C                   eval      scastd = stname
     C                   endif                                                  if scmspa = blanks
     C*
     C                   endif                                                  scfnd <> blanks
     C**********************************************************************
     C* Include/Exclude must be I or E
     C**********************************************************************
     C                   if        scie <> 'I' and scie <> 'E'
     C                   seton                                        9076
     C                   goto      ck1end
     C                   endif                                                  if scmspa = blanks
     C**********************************************************************
     C* Validate ProMatch action (required)
     C**********************************************************************
     C                   eval      scactd = *blanks
     C*
     C                   if        scactn = *blanks
     C                   seton                                        9077
     C                   goto      ck1end
     C                   endif
     C*
     C*
     C                   if        scactn <> *blanks
     C*
     C                   eval      kktbid = 'PMAC'
     C                   eval      kkkey  = scactn
     C*
     C     gtky          chain     pgt02                              99
     C*
     C                   if        *in99 = *on or gtstat <> 'A'
     C                   seton                                        9078
     C                   goto      ck1end
     C                   else
     C                   eval      scactd = gtdesc
     C                   endif                                                  if in99 = on
     C*
     C                   endif                                                  if ##rrn <> 0
     C**********************************************************************
     C* Check for duplicate records
     C**********************************************************************
     C*
     C                   if        *in40 = *on
     C*
     C                   eval      kkpatn = scpatn
     C                   eval      kkfnd  = scfnd
     C                   eval      kkast  = scast
     C                   eval      kkpmst = scpmst
     C*
     C     puky          chain     ppu01                              99
     C                   if        *in99 = *off
     C                   seton                                        7890
     C                   goto      ck1end
     C                   endif                                                  if *in90 = off
     C*
     C                   endif                                                  if *in40 = on
     C*
     C                   endif                                                  if *in90 = off
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
     C*
    C*================================================================
    C* GETDES - Get descriptions
    C*================================================================
     C     getdes        begsr
     C*
     C* Group
     C*
     C                   if        scfnd <> 0
     C                   eval      kkco# = ##co#
     C                   eval      kkfnd = scfnd
     C*
     C     fnky          chain     pfn02                              99
     C*
     C                   if        *in99 = *off and fnstat = 'A'
     C                   eval      scfndd = fnname
     C                   endif                                                  if scmspa = blanks
     C*
     C                   endif                                                  scfnd <> blanks
     C*
     C* State
     C*
     C                   eval      scastd = *blanks
     C*
     C                   if        scast <> *blanks
     C                   eval      kksky = scast
     C*
     C     stky          chain     pst02                              99
     C*
     C                   if        *in99 = *off and ststat = 'A'
     C                   eval      scastd = stname
     C                   endif                                                  if scmspa = blanks
     C*
     C                   endif                                                  scfnd <> blanks
     C*
     C* Action
     C*
     C                   if        scactn <> *blanks
     C*
     C                   eval      kktbid = 'PMAC'
     C                   eval      kkkey  = scactn
     C*
     C     gtky          chain     pgt02                              99
     C*
     C                   if        *in99 = *off and gtstat <> 'I'
     C                   eval      scactd = gtdesc
     C                   endif                                                  if in99 = on
     C*
     C                   endif                                                  if ##rrn <> 0
     C*
     C* Results status
     C*
     C                   if        scpmst <> *blanks
     C*
     C                   eval      kktbid = 'PMST'
     C                   eval      kkkey  = scpmst
     C*
     C     gtky          chain     pgt02                              99
     C*
     C                   if        *in99 = *off and gtstat <> 'I'
     C                   eval      scpmsd = gtdesc
     C                   endif                                                  if in99 = on
     C*
     C                   endif                                                  if ##rrn <> 0
     C*
     C                   endsr                                                  if ##rrn <> 0
     C*
    C*================================================================
    C* KJDO - Search routines
    C*================================================================
     C     kjdo          begsr
     C*
     C**********************************************************************
     C* ProMatch results status
     C**********************************************************************
     C*
     C                   select
     C*
     C                   when      hfld = 'SCPMST'
     C*
     C                   call      'F2380R'
     C                   parm      '2'           ##call            1
     C                   parm      ' '           ##kc              1
     C                   parm      ' '           ##kl              1
     C                   parm      0             ##rrn             9 0
     C                   parm      'PMST'        ##tbid            4
     C*
     C                   if        ##rrn <> 0
     C*
     C     ##rrn         chain     pgt01                              99
     C                   if        *in99 = *off
     C                   eval      scpmst = gtkey
     C                   eval      scpmsd = gtdesc
     C                   seton                                        22
     C                   endif                                                  if in99 = on
     C*
     C                   else
     C                   seton                                        21
     C*
     C                   endif                                                  if ##rrn <> 0
     C*
     C*
     C**********************************************************************
     C* Group
     C**********************************************************************
     C                   when      hfld = 'SCFND'
     C*
     C                   call      'F0060R'
     C                   parm      '2'           ##call            1
     C                   parm      ' '           ##kc              1
     C                   parm      ' '           ##kl              1
     C                   parm      0             ##rrn             9 0
     C*
     C                   if        ##rrn <> 0
     C*
     C     ##rrn         chain     pfn01                              99
     C                   if        *in99 = *off
     C                   eval      scfnd  = fnfnd
     C                   eval      scfndd = fnname
     C                   seton                                        23
     C                   endif                                                  if in99 = on
     C*
     C                   else
     C                   seton                                        22
     C*
     C                   endif                                                  if ##rrn <> 0
     C*
     C**********************************************************************
     C* State
     C**********************************************************************
     C*
     C                   when      hfld = 'SCAST'
     C*
     C                   call      'F0601R'
     C                   parm      '2'           ##call            1
     C                   parm      ' '           ##kc              1
     C                   parm      ' '           ##kl              1
     C                   parm      0             ##rrn             9 0
     C                   parm      0             ##rrn2            9 0
     C*
     C                   if        ##rrn <> 0
     C*
     C     ##rrn         chain     pst01                              99
     C                   if        *in99 = *off
     C                   eval      scast  = stkey
     C                   eval      scastd = stname
     C                   seton                                        24
     C                   endif                                                  if in99 = on
     C*
     C                   else
     C                   seton                                        23
     C*
     C                   endif                                                  if ##rrn <> 0
     C**********************************************************************
     C* ProMatch action
     C**********************************************************************
     C*
     C                   when      hfld = 'SCACTN'
     C*
     C                   call      'F2380R'
     C                   parm      '2'           ##call            1
     C                   parm      ' '           ##kc              1
     C                   parm      ' '           ##kl              1
     C                   parm      0             ##rrn             9 0
     C                   parm      'PMAC'        ##tbid            4
     C*
     C                   if        ##rrn <> 0
     C*
     C     ##rrn         chain     pgt01                              99
     C                   if        *in99 = *off
     C                   eval      scactn = gtkey
     C                   eval      scactd = gtdesc
     C                   endif                                                  if in99 = on
     C*
     C                   endif                                                  if ##rrn <> 0
     C*
     C                   seton                                        25
     C*
     C                   endsl
     C*
     C                   seton                                        90
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
     C                   eval      scpatn = pupatn
     C                   eval      scast  = puast
     C                   eval      scfnd  = pufnd
     C                   eval      scactn = puactn
     C                   eval      scpmst = pupmst
     C                   eval      scie   = puie
     C*
     C* Get descriptions
     C*
     C                   exsr      getdes
     C*
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      pupatn = scpatn
     C                   eval      puast  = scast
     C                   eval      pufnd  = scfnd
     C                   eval      puactn = scactn
     C                   eval      pupmst = scpmst
     C                   eval      puie   = scie
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
     C*
     C     *inkj         caseq     *on           kjdo
     C                   endcs
     C*
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
     C*
     C                   if        @@mode = 'D'
     C                   eval      *in09 = *on
     C                   endif
     C*
     C                   if        @@mode = 'A'
     C                   eval      *in40 = *on
     C                   else
     C                   eval      *in40 = *off
     C                   endif
     C*
     C                   eval      kkco# = ##co#
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
     C                   eval      pupgm  = q1pgm
     C                   eval      puuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         pudt
     C                   movel     dstime        putime
     C*
     C                   update    dbrcdf
     C*
E01  C                   endif
     C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C*
     C                   eval      pustat = 'A'
     C                   eval      puipgm = q1pgm
     C                   eval      puiusr = q1user
     C                   eval      pupgm  = q1pgm
     C                   eval      puuser = q1user
     C*
     C                   time                    dsdttm
     C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         puidt
     C                   movel     srdto         pudt
     C                   movel     dstime        puitim
     C                   movel     dstime        putime
     C*
     C                   write     dbrcdf
     C                   feod      ftpup
     C                   endsr
