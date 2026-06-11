     Hcopyright('(c) Copyright Fiserv, Inc., 2005 - 2007')
     Hdftactgrp(*no)
    H*
     *****************************************************************
     *    PROGRAM: W2282R02                                          *
     *                                                               *
     *    DESCRIPTION: Audit assignment maintenance                  *
     *                                                               *
     *    CREATED BY:  C:53   03/23/05   PROJECT# WC1196             *
     *                                                               *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  06/04/07  C:33  Project: WC1196                              *
      *                  Changes from Q/A testing.                    *
      *                                                               *
      *  10/02/06 C:33  Project: WC1196                               *
      *                 Do not allow overlapping premium ranges.      *
      *                                                               *
      *  08/04/06 C:132 Project: WC1196                               *
      *               - Rearrange the positions of lines on display.  *
      *               - Only allow chg to Audit Type field            *
      *                                                               *
      *  11/04/05 C:53  Support: 49715                                *
      *                 Relax edit on record already existing.
      *                                                               *
     *****************************************************************
     Fw2282f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtssp     uf a e             disk
     F                                     rename(pss01:dbrcdf)
     Fwtssl     if   e           k disk
     F                                     Prefix(s1:2)
     F***  Begin Add  ***  10/02/06  **************************************
     F                                     rename(pss01:pss00)
     Fwtssl1    if   e           k disk
     F                                     infds(ssfds1)
     F***  End   Add  ***  10/02/06  **************************************
     Fwttgl     if   e           k disk
     Fwttgp     if   e             disk
     F                                     rename(ptg01:ptg0p)
     F                                     Prefix(tg1:2)
     Fwttal     if   e           k disk
     Fwttap     if   e             disk
     F                                     rename(pta01:pta0p)
     F                                     Prefix(ta1:2)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Audit Type Assignment Control'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##rrn           s              9  0
     D ##mode          s              1
     D***  Begin Add  ***  06/04/07  **************************************
     D ##kc            s              1
     D ##kl            s              1
     D ##call          s              1
     D***  End   Add  ***  06/04/07  **************************************
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@call          s              1
     D @@rrn           s              9  0
     D addMode         s              1n   inz
     D chgMode         s              1n   inz
     D dspMode         s              1n   inz
     D xxbctg          s                   like(tgbctg)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     d***  Begin Add  ***  10/02/06  **************************************
     D ssfds1          ds
     D ssrrn1                397    400b 0
     D*
     dPremOverlap      pr                  like(*in90)
     d***  End   Add  ***  10/02/06  **************************************

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
     C***  Begin Chg  ***  06/04/07  **************************************
     C*****acdpl1        plist
     C**********         parm      '2'           @@call
     C******inkc         parm                    @@kc
     C******inkl         parm                    @@kl
     C**********         parm      *zeros        @@rrn
     C     srchPl        plist
     C                   parm      '2'           ##call
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      *zeros        ##rrn
     C***  End   Chg  ***  06/04/07  **************************************
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     tgky          klist
     C                   kfld                    ##co#
     C                   kfld                    xxbctg
     C*
     C     taky          klist
     C                   kfld                    ##co#
     C                   kfld                    sctypa
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99=*off
     C*
     C                   if        ssstat='A'
     C                   eval      ssstat =  'I'
     C                   else
     C                   eval      ssstat = 'A'
     C                   endif                                                  fnflag='I'
     C*
     C                   update    dbrcdf
     C                   endif                                                  *in99=*off
     C*
     C                   endsr                                                  chgsts
     C*
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
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         if        *in39 = *on
     C                   if        addMode
     C***  End   Chg  ***  06/04/07  **************************************
     C*
     C                   if        scbctg = *blanks
     C                   move      *on           *in71
     C     *in71         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   movel     scbctg        xxbctg
     C*
     C     tgky          chain     ptg01                              78
     C  N78tgstat        comp      'I'                                    78
     C     *in78         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   if        scnrbs <> 'N'
     C                             and scnrbs <> 'R'
     C                   move      *on           *in73
     C     *in73         cabeq     *on           ender1                   90
     C                   endif
     C*
     C     taky          chain     pta01                              74
     C  N74tastat        comp      'I'                                    75
     C     *in74         cabeq     *on           ender1                   90
     C     *in75         cabeq     *on           ender1                   90
     C*
     C                   if        scprmh <= 0
     C                   move      *on           *in76
     C     *in76         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   if        scprml >= scprmh
     C                   move      *on           *in77
     C     *in77         cabeq     *on           ender1                   90
     C                   endif
     C*
     C***  Begin Add  ***  10/02/06  **************************************
     C  n90              eval      *in79 = PremOverlap
     C***  End   Add  ***  10/02/06  **************************************
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
      *===============================================================
      * KJDO - Subroutine for F10
      *===============================================================
      *
     C     kjdo          begsr
      *
     C                   select
     C                   when      hfld =  'SCBCTG' and addMode
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         move      *blanks       scbcds
     C**********         call      'W2280R'      acdpl1
     C*****@@rrn         chain     ptg0p                              99
      **********
     C******in99         ifeq      *on
     C**********         eval      *in40 = *on
     C                   call      'W2280R'      srchPl
     C     ##rrn         chain     ptg0p                              99
     C     *in99         ifeq      *on
     C                   eval      *in80 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C*
     C                   else
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in41 = *on
     C                   eval      *in81 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C                   eval      scbctg = tg1bctg
     C                   movel(p)  tg1bcds       scbcds
     C                   endif
     C*
     C     hfld          wheneq    'SCTYPA'
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         move      *blanks       sctypd
     C**********         call      'W2281R'      acdpl1
     C*****@@rrn         chain     pta0p                              99
     C******in99         ifeq      *on
     C**********         eval      *in42 = *on
     C                   call      'W2281R'      srchPl
     C     ##rrn         chain     pta0p                              99
     C     *in99         ifeq      *on
     C                   eval      *in82 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C*
     C                   else
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in43 = *on
     C                   eval      *in83 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C                   eval      sctypa = ta1typa
     C                   movel(p)  ta1typd       sctypd
     C                   endif
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scbctg = ssbctg
     C                   eval      scnrbs = ssnrbs
     C                   eval      sctypa = sstypa
     C                   eval      scprml = ssprml
     C                   eval      scprmh = ssprmh
     C                   eval      scstat = ssstat
     C*
     C*** chain to get description
     C*
     C                   move      *blanks       scbcds
     C                   movel     scbctg        xxbctg
     C     tgky          chain     ptg01                              99
     C  N99              movel(p)  tgbcds        scbcds
     C*
     C                   move      *blanks       sctypd
     C     taky          chain     pta01                              99
     C  N99              movel(p)  tatypd        sctypd
     C*
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         if        *in39 = *on
     C                   if        addMode
     C***  End   Chg  ***  06/04/07  **************************************
     C                   eval      scstat = 'A'
     C                   endif
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      ssco# = ##co#
     C                   eval      ssbctg = scbctg
     C                   eval      ssnrbs = scnrbs
     C                   eval      sstypa = sctypa
     C                   eval      ssprml = scprml
     C                   eval      ssprmh = scprmh
     C                   eval      ssstat = scstat
     C*
     C                   eval      ssuser = q1user
     C                   eval      sspgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sstime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         ssdt
     C*
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         if        *in39 = *on
     C                   if        addMode
     C***  End   Chg  ***  06/04/07  **************************************
     C                   eval      ssiusr = q1user
     C                   eval      ssipgm = q1pgm
     C                   eval      ssitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         ssidt
     C                   endif
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
     C     *inkj         caseq     *ON           KJDO                     90
     C                   endcs
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
      *
     C***  Begin Chg  ***  06/04/07  **************************************
      ***If*Mode is Search, Change or Display, don't allow Changes
      ***except*for Audit Type in Change Mode.
      **********
     C**********         move      *OFF          *in09
      **********
     C**********         if        @@mode = '1' OR @@mode ='C' OR
     C**********                   @@mode = 'D'
     C**********         move      *ON           *in09
     C**********         endif
      **********
     C*****@@mode        comp      'A'                                    39
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C***  End   Chg  ***  06/04/07  **************************************
     C*
     C                   select
     C                   when      @@mode = 'A'
     C                   eval      addMode = *on
     C                   when      @@mode = 'C'
     C                   eval      chgMode = *on
     C                   when      @@mode = 'D'
     C                   eval      dspMode = *on
     C                   endsl
     C***  Begin Del  ***  06/04/07  **************************************
      **********
      ***Allow*Change to Audit Type if Add or Change
      **********
     C**********         move      *OFF          *in38
      **********
     C**********         if        @@mode = 'A' OR @@mode ='C'
     C**********         move      *ON           *in38
     C**********         endif
     C***  End   Del  ***  06/04/07  **************************************
      *
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C     @@mode        ifeq      'S'
     C                   exsr      chgsts
     C                   return
     C                   endif
     C*
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
     C                   write     dbrcdf
     C                   feod      wtssp
     C                   endsr





     p***  Begin Add  ***  10/02/06  **************************************
    p*====================================================================
    p* Check for overlapping premium values
    p*====================================================================
     pPremOverlap      b
     dPremOverlap      pi                  like(*in90)

     c     keyWTSSL1     klist
     c                   kfld                    ##Co#
     c                   kfld                    SCBCTG
     c                   kfld                    SCNRBS
     c                   kfld                    SCPRML

     c     key1WTSSL1    klist
     c                   kfld                    ##Co#
     c                   kfld                    SCBCTG
     c                   kfld                    SCNRBS

     c     keyWTSSL1     setgt     pss01
     c     key1WTSSL1    readpe    pss01
     c                   if        not %eof
     c                              and scPrmL <= SSPRMH
     c                              and ssRRN1 <> @@rrn
     c***  Begin Add  ***  06/04/07  **************************************
     c                              and SSSTAT <> 'I'
     c***  End   Add  ***  06/04/07  **************************************
     c                   eval      *in90 = *on
     c                   return    *on
     c                   endif

     c     keyWTSSL1     setll     pss01
     c     key1WTSSL1    reade     pss01
     c                   if        not %eof
     c                              and scPrmH >= SSPRML
     c                              and ssRRN1 <> @@rrn
     c***  Begin Add  ***  06/04/07  **************************************
     c                              and SSSTAT <> 'I'
     c***  End   Add  ***  06/04/07  **************************************
     c                   eval      *in90 = *on
     c                   return    *on
     c                   endif

     c                   return    *off

     pPremOverlap      e
     C***  End   Add  ***  10/02/06  **************************************
