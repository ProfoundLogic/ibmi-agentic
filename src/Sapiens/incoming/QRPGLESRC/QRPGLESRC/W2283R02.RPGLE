     Hcopyright('(c) Copyright Fiserv, Inc., 2005 - 2007')
     Hdftactgrp(*no)
     *****************************************************************
     *    PROGRAM: W2283R02                                          *
     *                                                               *
     *    DESCRIPTION: Audit company assignment                      *
     *                                                               *
     *    CREATED BY:  C:53   03/23/05   PROJECT# WC1196             *
     *                                                               *
     *****************************************************************
      * Revised:                                                      *
      *                                                               *
      *  09/08/07  C:33  Project: WC1196  Priss# 29                   *
      *                  Use State Table file instead of Generic Table*
      *                   file for validating state.                  *
      *                                                               *
      *  06/04/07  C:33  Project: WC1196                              *
      *                  Changes from Q/A testing.                    *
      *                                                               *
      *  10/02/06 C:33  Project: WC1196                               *
      *                 Do not allow overlapping premium ranges.      *
      *                                                               *
      *  11/04/05 C:53  Support: 49715                                *
      *                 Relax edits
      *                                                               *
     *****************************************************************
     Fw2283f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)-Audit Company Assignment
     Fwtaap     uf a e             disk
     F                                     rename(paa01:dbrcdf)
     F***  Begin Add  ***  10/02/06  **************************************
     Fwtaal1    if   e           k disk
     F***  Begin Add  ***  06/04/07  **************************************
     F                                     infds(aafds1)
     F***  End   Add  ***  06/04/07  **************************************
     F***  End   Add  ***  10/02/06  **************************************
    F* Business Category File
     Fwttgl     if   e           k disk
    F*
    F* Audit Type file
     Fwttal     if   e           k disk
     Fwttap     if   e             disk
     F                                     rename(pta01:pta0p)
     F                                     Prefix(ta1:2)
    F* Audit Company Table file
     Fwtacl     if   e           k disk
     Fwtacp     if   e             disk
     F                                     rename(pac01:pac0p)
    F* County Code table file
     Fwtcnl1    if   e           k disk
     Fwtcnp     if   e             disk
     F                                     rename(pcn01:pcn0p)
    F* State Code table file
     F***  Begin Chg  ***  09/08/07  **************************************
     F**********wtgtl     if   e           k disk
     Fststl3    if   e           k disk    rename(pst01:pst03)
     F***  End   Chg  ***  09/08/07  **************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D***  Begin Chg  ***  06/04/07  **************************************
     D*optnm****       c                   'Audit Company Assignment'
     D optnm           c                   'Audit Co. Assignment Control'
     D***  End   Chg  ***  06/04/07  **************************************
     D comapi          c                   'WA001R'
     D chgnum          c                   'SA020R'
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
     D @@call          s              1
     D @@rrn           s              9  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##call          s              1
     D ##rrn           s              9  0
     D***  Begin Add  ***  10/02/06  **************************************
     D addMode         s              1n   inz
     D chgMode         s              1n   inz
     D dspMode         s              1n   inz
     D***  End   Add  ***  10/02/06  **************************************
     D***  Begin Chg  ***  09/08/07  **************************************
     D*kktbid          s                   like(gttbid)
     D*kkkey           s                   like(gtkey)
     D xxKey           s                   like(STKEY)
     D***  End   Chg  ***  09/08/07  **************************************
     D kkcnt1          s                   like(cnkey)
     D ##num           s             80
     D ##mask          s             10
     D ##dec           s              3  0
     D ##thou          s              1
     D ##key           s             10
     D ##view          s              1  0
     D ##st            s              2
     D                 ds
     D cnkey                   1     10
     D   xcnkey                3      5
     D                 ds
     D kcnkey                  1     10
     D   kcnky2                3      5
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     D***  Begin Add  ***  06/04/07  **************************************
     D aafds1          ds
     D aaRRN1                397    400b 0
     D***  End   Add  ***  06/04/07  **************************************
     D*
     d***  Begin Add  ***  10/02/06  **************************************
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
     C     acdpl1        plist
     C                   parm      '2'           ##call
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      *zeros        ##rrn
    C*
     C     acdpl2        plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C                   parm                    ##key
     C                   parm      SCST          ##st
     C                   parm                    ##view
     C***  Begin Del  ***  09/08/07  **************************************
    C**********
    C*****acdpl3        plist
    C**********         parm                    ##prds           40
    C**********         parm      '2'           ##call
    C**********         parm      'WCST'        ##id              4
    C**********         parm                    ##key            10
    C**********         parm                    ##rtc             1
     C***  End   Del  ***  09/08/07  **************************************
    C*
     C     chgpl         plist
     C                   parm                    ##num
     C                   parm                    ##mask
     C                   parm                    ##dec
     C                   parm                    ##thou
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     acky          klist
     C                   kfld                    ##co#
     C                   kfld                    scaco
     C*
     C     cnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkcnt1
     C*
     C     taky          klist
     C                   kfld                    ##co#
     C                   kfld                    sctypa
     C*
     C***  Begin Chg  ***  09/08/07  **************************************
     C*****gtky*         klist
     C**********         kfld                    ##co#
     C**********         kfld                    kktbid
     C**********         kfld                    kkkey
     C     keySTSTL3     klist
     C                   kfld                    xxKey
     C***  End   Chg  ***  09/08/07  **************************************
      *
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C*
     C***  Begin Add  ***  06/04/07  **************************************
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99=*off
     C*
     C                   select
     C                   when      AASTAT = 'A'
     C                   eval      AASTAT = 'I'
     C                   when      AASTAT = 'I'
     C                   eval      AASTAT = 'A'
     C                   endsl                                                  fnflag='I'
     C*
     C                   eval      AAUSER = q1user
     C                   eval      AAPGM  = q1pgm
     C                   time                    dsdttm
     C                   eval      AATIME = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         AADT
     C*
     C                   update    dbrcdf
     C*
     C                   endif                                                  *in99=*off
     C*
     C                   endsr                                                  chgsts
     C***  End   Add  ***  06/04/07  **************************************
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
     C* Check Premium Levels
     C*
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         IF        NOT *IN09
     C                   if        addMode
     C***  End   Chg  ***  06/04/07  **************************************
     C*
     C                   IF        scprml <  *zeros OR
     C                             scprmh <  *zeros
     C                   Move      *on           *in70
     C     *in70         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   IF        scprml >  *zeros AND
     C                             scprmh <= scprml
     C                   Move      *on           *in71
     C     *in71         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   IF        scprml =  *zeros AND
     C                             scprmh <= *zeros
     C                   Move      *on           *in78
     C     *in78         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   IF        scprmh >  *zeros AND
     C                             scprmh <= scprml
     C                   Move      *on           *in71
     C     *in71         cabeq     *on           ender1                   90
     C                   endif
     C
     C     taky          chain     pta01                              72
     C  N72tastat        comp      'I'                                    72
     C     *in72         cabeq     *on           ender1                   90
     C*
     C                   if        scst <> *blanks
     C***  Begin Del  ***  09/08/07  **************************************
      **********
     C**********         move(p)   'WCST'        kktbid
     C**********         movel(p)  scst          kkkey
     C*****gtky*         chain     pgt01                              73
     C                   eval      xxKey = SCST
     C     keySTSTL3     chain     pst03                              73
     C***  End   Del  ***  09/08/07  **************************************
     C     *in73         cabeq     *on           ender1                   90
     C                   endIf
     C*
     C                   if        sccnty <> *blanks
     C                   movel(p)  scst          kcnkey
     C                   movel(p)  sccnty        kcnky2
     C                   movel(p)  kcnkey        kkcnt1
     C     cnky          chain     pcn01                              74
     C  N74cnstat        comp      'I'                                    74
     C     *in74         cabeq     *on           ender1                   90
     C                   endIf
     C*
     C                   ENDIF                                                  NOT *IN09
     C*
     C* Audit Company
     C***  Begin Del  ***  06/04/07  **************************************
     C**********         if        *in38 = *on
     C***  End   Del  ***  06/04/07  **************************************
     C                   if        scaco = *blanks
     C                   move      *on           *in75
     C     *in75         cabeq     *on           ender1                   90
     C                   endif
     C***  Begin Del  ***  06/04/07  **************************************
     C**********         endif
     C***  End   Del  ***  06/04/07  **************************************
     C*
     C     acky          chain     pac01                              76
     C  N76acstat        comp      'I'                                    76
     C     *in76         cabeq     *on           ender1                   90
     C*
     C***  Begin Add  ***  10/02/06  **************************************
     C                   if        addMode
     C  n90              eval      *in79 = PremOverlap
     C                   endif
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
     C
     C                   select
     C*
     C                   when      hfld = 'SCTYPA' and addMode
     C                   move      *blanks       scatyd
     C                   call      'W2281R'      acdpl1
     C     ##rrn         chain     pta0p                              99
     C     *in99         ifeq      *on
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in41 = *on
     C                   eval      *in81 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C*
     C                   else
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in42 = *on
     C                   eval      *in82 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C                   eval      sctypa = ta1typa
     C                   movel(p)  ta1typd       scatyd
     C                   endif
     C***  Begin Del  ***  09/08/07  **************************************
     C**********
     C**********         when      hfld = 'SCST' and addMode
     C**********         call      'W2493R'      acdpl3
      **********
     C*****##rtc         ifeq      'A'
     C**********         exsr      pgmend
     C**********         endif
      **********
     C**********         move(p)   'WCST'        kktbid
     C**********         movel(p)  ##key         kkkey
     C*****gtky*         chain     pgt01                              99
     C******in99         ifeq      *on
     C*****Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in42 = *on
     C**********         eval      *in82 = *on
     C*****End** Chg  ***  06/04/07  **************************************
     C**********
     C**********         else
     C*****Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in43 = *on
     C**********         eval      *in83 = *on
     C*****End** Chg  ***  06/04/07  **************************************
     C**********         movel(p)  gtkey         scst
     C**********         endif
     C***  End   Del  ***  09/08/07  **************************************
     C*
     C                   when      hfld = 'SCCNTY' and addMode
     C                   move      *blanks       sccntd
     C                   call      'W2215R'      acdpl2
     C     ##rrn         chain     pcn0p                              99
     C     *in99         ifeq      *on
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in43 = *on
     C                   eval      *in83 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C*
     C                   else
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in44 = *on
     C                   eval      *in84 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C                   movel(p)  xcnkey        sccnty
     C                   movel(p)  cndesc        sccntd
     C                   endif
      *
     C     hfld          wheneq    'SCACO'
     C                   move      *blanks       scacod
     C                   call      'W2290R'      acdpl1
     C     ##rrn         chain     pac0p                              99
     C     *in99         ifeq      *on
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in44 = *on
     C                   eval      *in84 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C                   else
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         eval      *in41 = *on
     C                   eval      *in81 = *on
     C***  End   Chg  ***  06/04/07  **************************************
     C                   movel(p)  ackey         scaco
     C                   movel(p)  acname        scacod
     C                   endif
     C*
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   move      *blanks       ##dec
     C                   move      *zeros        ##thou
     C*
     C                   eval      scstat = aastat
     C                   eval      scprml = aaprml
     C                   eval      scprmh = aaprmh
     C*
     C                   eval      scaco = aaaco
     C                   eval      sctypa = aatypa
     C                   eval      scst = aast
     C                   eval      sccnty = aacnty
     C*
     C*** chain to get description
     C*
     C                   move      *blanks       scacod
     C     acky          chain     pac01                              99
     C  N99              movel(p)  acname        scacod
     C*
     C                   move      *blanks       sccntd
     C                   movel(p)  scst          kcnkey
     C                   movel(p)  sccnty        kcnky2
     C                   movel(p)  kcnkey        kkcnt1
     C     cnky          chain     pcn01                              99
     C  N99              movel(p)  cndesc        sccntd
     C*
     C                   move      *blanks       scatyd
     C     taky          chain     pta01                              99
     C  N99              movel(p)  tatypd        scatyd
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
     C                   eval      aaco# = ##co#
     C                   eval      aaaco = scaco
     C                   eval      aatypa = sctypa
     C                   eval      aast = scst
     C                   eval      aacnty = sccnty
     C*
     C                   eval      aauser = q1user
     C                   eval      aapgm = q1pgm
     C                   time                    dsdttm
     C                   eval      aatime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         aadt
     C*
     C                   eval      aastat = scstat
     C                   eval      aaprml = scprml
     C                   eval      aaprmh = scprmh
     C*
     C***  Begin Chg  ***  06/04/07  **************************************
     C**********         if        *in39 = *on
     C                   if        addMode
     C***  End   Chg  ***  06/04/07  **************************************
     C                   eval      aaiusr = q1user
     C                   eval      aaipgm = q1pgm
     C                   eval      aaitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         aaidt
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
     C     *inkj         caseq     *ON           kjdo                     90
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
      ***except*for Audit Company in Change Mode.
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
     C***  Begin Add  ***  10/02/06  **************************************
     C                   select
     C                   when      @@Mode = 'A'
     C                   eval      addMode = *on
     C                   when      @@Mode = 'C'
     C                   eval      chgMode = *on
     C                   when      @@Mode = 'D'
     C                   eval      dspMode = *on
     C                   endsl
     C***  End   Add  ***  10/02/06  **************************************
     C***  Begin Del  ***  06/04/07  **************************************
      **********
      ***Allow*Change to Audit Company if Add or Change
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
     C***  Begin Add  ***  06/04/07  **************************************
     C                   select
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C***  End   Add  ***  06/04/07  **************************************
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
     C                   feod      wtaap
     C                   endsr





     p***  Begin Add  ***  10/02/06  **************************************
    p*====================================================================
    p* Check for overlapping premium values
    p*====================================================================
     pPremOverlap      b
     dPremOverlap      pi                  like(*in90)

     c     keyWTAAL1     klist
     c                   kfld                    ##Co#
     c                   kfld                    SCTYPA
     c                   kfld                    SCST
     c                   kfld                    SCCNTY
     c                   kfld                    SCPRML

     c     key1WTAAL1    klist
     c                   kfld                    ##Co#
     c                   kfld                    SCTYPA
     c                   kfld                    SCST
     c                   kfld                    SCCNTY

     c     keyWTAAL1     setgt     paa01
     c     key1WTAAL1    readpe    paa01
     c                   if        not %eof
     c                              and scPrmL <= AAPRMH
     c***  Begin Add  ***  06/04/07  **************************************
     c                              and aaRRN1 <> @@rrn
     c                              and AASTAT <> 'I'
     c***  End   Add  ***  06/04/07  **************************************
     c                   eval      *in90 = *on
     c                   return    *on
     c                   endif

     c     keyWTAAL1     setll     paa01
     c     key1WTAAL1    reade     paa01
     c                   if        not %eof
     c                              and scPrmH >= AAPRML
     c***  Begin Add  ***  06/04/07  **************************************
     c                              and aaRRN1 <> @@rrn
     c                              and AASTAT <> 'I'
     c***  End   Add  ***  06/04/07  **************************************
     c                   eval      *in90 = *on
     c                   return    *on
     c                   endif

     c                   return    *off

     pPremOverlap      e
     C***  End   Add  ***  10/02/06  **************************************
