      * (C) Copyright FISERV Insurance Solutions, Inc., 2007

      /title SR00509I2 - "Info/Maint" Additional Info program
      *****************************************************************
      * DESCRIPTION:
      *
      * WC000509A - Retrospective Premium End Endorsement
      *
      * "Info/Maint" - Endorsement "Addtional Information" program
      *              - Multiple, Manual with Additional Information.
      *
      * CALLED FROM: SR00509I
      * CALLS TO:    None
      *
      *****************************************************************
      * REVISED:
      *
      * 06/06/08 C:197 PROJECT C114WQG
      *                Created.
      *
      *****************************************************************

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fsr00509F2 cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    
     *  Physical file definition (for maintaining record)
     Fwlrpp     uf a e             disk
     F                                     rename(prp01:dbrcdf)
     Fwlrpl     if   e           k disk
    
     * State Code File
     Fststl3    if   e           k disk
    
      * group
     Fwmfnl     if   e           k disk
    
      * employer
     Fwmeml     if   e           k disk
    
      * employer policy year
     Fwdelp     if   e             disk
    
      * employer cross reference
     Fwzexl     if   e           k disk
     F                                     rename(pex01:pex02)
    

     D efDS            ds
     D  EMEFD1
     D  EMEFD2
     D  EMEFD3
     D  EMEFD4
     D  EMEFD5
     D ef                                  OVERLAY(efDS)
     D                                     LIKE(EMEFD1) DIM(5)

     D cxDS            ds
     D  EMCAD1
     D  EMCAD2
     D  EMCAD3
     D  EMCAD4
     D  EMCAD5
     D cx                                  OVERLAY(cxDS)
     D                                     LIKE(EMCAD1) DIM(5)


    D*================================================================
    D*  /COPY required constants
    D*================================================================

     D optnm           c                   'Retro Premium Changes End -
     D                                     Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   2


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
     D @@rrn2          s              9  0
     D @@qte           s              1
     d ##emp#          s                   like(ememp#)
     d ##div           s                   like(emdiv)
     d ##name          s                   like(emname)
     d ##fyr           s                   like(elfyr)
     d ##agt#          s              5  0
     d ##anam          s             60
     d ##prod          s              5  0
     d ##pnam          s             60
     d x               s              7  0
     d sysdt           s              7  0
     d xxstdt          s                   like(elpfdt)
     d x1div           s                   like(eldiv)
     d x1pfdt          s                   like(elpfdt)
     d x1andt          s                   like(elandt)
     d xspfdt          s                   like(elpfdt)
     d xsfyr           s                   like(elfyr)
     d xsandt          s                   like(elandt)
     d xsdiv           s                   like(eldiv)
     d x1seq#          s                   like(rpseq#)


    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------


     C/COPY SCOPYMT01C


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
      *                                     endorsement specific data file
     C                   parm                    @@rrn
      *                                     wdelp
     C                   parm                    @@rrn2
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     c     agtpl         plist
     c                   PARM                    ##CO#
     c                   PARM      elfnd         ##FND
     c                   PARM      elemp#        ##EMP#
     c                   PARM      eldiv         ##DIV
     c                   PARM      elfyr         ##FYR
     c     scagt#        PARM                    ##AGT#
     c                   PARM                    ##ANAM
     c     scprod        PARM                    ##PROD
     c                   PARM                    ##PNAM

     C*
     *================================================================
     *  KLIST definitions
     *================================================================

     c     rpky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#

     c     fnky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd

     c     emky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     c     zexky         klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv

     C     stky          Klist
     C                   Kfld                    XXkey

     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr

     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr
     C
     C*   Checking for Invalid state
     C
     C                   if        scels1 <> *blanks
     C                   MOVEL     scels1        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     C                   seton                                        4590
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scels1<>*blanks
     C*
     C                   if        scels2 <> *blanks
     C                   MOVEL     scels2        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        4690
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scels2<>*blanks
     c*
     C                   if        scels3 <> *blanks
     C                   MOVEL     scels3        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        4790
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     c                   endif                                                  scels3<>*blanks
     c
     C                   if        scna01 <> *blanks
     C                   MOVEL     scna01        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        4890
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c
     C                   if        scna02 <> *blanks
     C                   MOVEL     scna02        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        4990
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna03 <> *blanks
     C                   MOVEL     scna03        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5090
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna04 <> *blanks
     C                   MOVEL     scna04        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5190
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna05 <> *blanks
     C                   MOVEL     scna05        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5290
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna06 <> *blanks
     C                   MOVEL     scna06        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5390
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna07 <> *blanks
     C                   MOVEL     scna07        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5490
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna08 <> *blanks
     C                   MOVEL     scna08        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5590
     C                   leavesr
     c                   endif                                                  *IN60=*ON
     c                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna09 <> *blanks
     C                   MOVEL     scna09        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5690
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     C                   if        scna10 <> *blanks
     C                   MOVEL     scna10        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5790
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scna01<>*blanks
     c*
     c*     Check for invalid date
     C
     C                   if        sceld1 <> *zeros
     C                   z-add     sceld1        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         sceld1
     C                   endif                                                  sceld <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in72 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   if        sceld2 <> *zeros
     C                   z-add     sceld2        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         sceld2
     C                   endif                                                  sceld <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in73 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C
     C                   if        sceld3 <> *zeros
     C                   z-add     sceld3        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         sceld3
     C                   endif                                                  sceld3 <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in74 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C*
     C                   endsr                                                  ERRCK1
     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C*
     C*   Checking for Invalid state
     C                   if        scfcs1 <> *blanks
     C                   MOVEL     scfcs1        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5890
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scfcs1 <> *blanks
     C*
     C                   if        scfcs2 <> *blanks
     C                   MOVEL     scfcs2        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        5990
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scfcs1 <> *blanks
     C*
     C                   if        scfcs3 <> *blanks
     C                   MOVEL     scfcs3        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        6190
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  scfcs1 <> *blanks
     C*
     C                   if        sctms1 <> *blanks
     C                   MOVEL     sctms1        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        6290
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  sctms1 <> *blanks
     C*
     C                   if        sctms2 <> *blanks
     C                   MOVEL     sctms2        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        6390
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  sctms1 <> *blanks
     C*
     C                   if        sctms3 <> *blanks
     C                   MOVEL     sctms3        xxkey            10
     C     stky          chain     pst01                              60
     C                   if        *in60 = *on
     c                   seton                                        6490
     C                   leavesr
     C                   endif                                                  *IN60=*ON
     C                   endif                                                  sctms1 <> *blanks
     C*
     C                   if        scfcd1 <> *zeros
     C                   z-add     scfcd1        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         scfcd1
     C                   endif                                                  scfcd <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in75 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     c                   endif
     c*
     C                   if        scfcd2 <> *zeros
     C                   z-add     scfcd2        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         scfcd2
     C                   endif
     C*
     C                   if        srerr <> '0'
     C                   eval      *in76 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     c*
     C                   if        scfcd3 <> *zeros
     C                   z-add     scfcd3        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         scfcd3
     C                   endif                                                  scfcd <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in77 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     c*
     C                   if        sctmd1 <> *zeros
     C                   z-add     sctmd1        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         sctmd1
     C                   endif                                                  sctmd <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in78 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     c*
     C                   if        sctmd2 <> *zeros
     C                   z-add     sctmd2        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         sctmd2            8 0
     C                   endif                                                  sctmd <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in79 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     c*
     C                   if        sctmd3 <> *zeros
     C                   z-add     sctmd3        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   MOVEL     SRDTO         sctmd3            8 0
     C                   endif                                                  sctmd <> *zeros
     C*
     C                   if        srerr <> '0'
     C                   eval      *in80 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

     c                   eval      scels1 = rpels1
     c                   eval      scels2 = rpels2
     c                   eval      scels3 = rpels3
     c                   eval      scelf1 = rpelf1
     c                   eval      scelf2 = rpelf2
     c                   eval      scelf3 = rpelf3
     c*
     c                   z-add     rpeld1        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         sceld1
     c*
     c                   z-add     rpeld2        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         sceld2
     c*
     c                   z-add     rpeld3        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         sceld3
     c*
     c                   eval      scna01 = rpna01
     c                   eval      scna02 = rpna02
     c                   eval      scna03 = rpna03
     c                   eval      scna04 = rpna04
     c                   eval      scna05 = rpna05
     c                   eval      scna06 = rpna06
     c                   eval      scna07 = rpna07
     c                   eval      scna08 = rpna08
     c                   eval      scna09 = rpna09
     c                   eval      scna10 = rpna10
     c                   eval      scfcs1 = rpfcs1
     c                   eval      scfcs2 = rpfcs2
     c                   eval      scfcs3 = rpfcs3
     c                   eval      scfc11 = rpfc11
     c                   eval      scfc12 = rpfc12
     c                   eval      scfc13 = rpfc13
     c                   eval      scfc21 = rpfc21
     c                   eval      scfc22 = rpfc22
     c                   eval      scfc23 = rpfc23
     c                   eval      scfc31 = rpfc31
     c                   eval      scfc32 = rpfc32
     c                   eval      scfc33 = rpfc33
     c*
     c                   z-add     rpfcd1        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scfcd1
     c*
     c                   z-add     rpfcd2        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scfcd2
     c*
     c                   z-add     rpfcd3        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scfcd3
     c*
     c                   eval      sctmf1 = rptmf1
     c                   eval      sctmf2 = rptmf2
     c                   eval      sctmf3 = rptmf3
     c                   eval      sctmo1 = rptmo1
     c                   eval      sctmo2 = rptmo2
     c                   eval      sctmo3 = rptmo3
     c                   eval      sctmf1 = rptmf1
     c                   eval      sctmf2 = rptmf2
     c                   eval      sctmf3 = rptmf3
     c*
     c                   eval      sctms1 = rptms1
     c                   eval      sctms2 = rptms2
     c                   eval      sctms3 = rptms3
     c                   z-add     rptmd1        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         sctmd1
     c*
     c                   z-add     rptmd2        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         sctmd2
     c*
     c                   z-add     rptmd3        srdti
     c                   CALL      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         sctmd3
     c*
     c**
     c                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     c                   if        @@mode = 'A'
     C     rpky          setgt     prp01
     C     rpky          readpe    prp01

     C                   if        not %eof
     C                   eval      rpseq# = rpseq# + 1
     C                   else
     C                   eval      rpseq# = 1
     C                   endif

     c                   endif

     c                   eval      rpco#  = elco#
     c                   eval      rpfnd  = elfnd
     c                   eval      rpfyr  = elfyr
     c                   eval      rpemp# = elemp#
     c                   eval      rpdiv  = eldiv
     c                   eval      rpels1 = scels1
     c                   eval      rpels2 = scels2
     c                   eval      rpels3 = scels3
     c                   eval      rpelf1 = scelf1
     c                   eval      rpelf2 = scelf2
     c                   eval      rpelf3 = scelf3
     c
     c                   z-add     sceld1        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rpeld1
     c*
     c                   z-add     sceld2        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rpeld2
     c*
     c                   z-add     sceld3        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rpeld3
     c*
     c                   eval      rpna01 = scna01
     c                   eval      rpna02 = scna02
     c                   eval      rpna03 = scna03
     c                   eval      rpna04 = scna04
     c                   eval      rpna05 = scna05
     c                   eval      rpna06 = scna06
     c                   eval      rpna07 = scna07
     c                   eval      rpna08 = scna08
     c                   eval      rpna09 = scna09
     c                   eval      rpna10 = scna10
     c                   eval      rpfcs1 = scfcs1
     c                   eval      rpfcs2 = scfcs2
     c                   eval      rpfcs3 = scfcs3
     c                   eval      rpfc11 = scfc11
     c                   eval      rpfc12 = scfc12
     c                   eval      rpfc13 = scfc13
     c                   eval      rpfc21 = scfc21
     c                   eval      rpfc22 = scfc22
     c                   eval      rpfc23 = scfc23
     c                   eval      rpfc31 = scfc31
     c                   eval      rpfc32 = scfc32
     c                   eval      rpfc33 = scfc33
     c                   eval      rpfcd1 = scfcd1
     c*
     c                   z-add     scfcd1        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rpfcd1
     c*
     c                   z-add     scfcd2        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rpfcd2
     c*
     c                   z-add     scfcd3        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rpfcd3
     c*
     c                   eval      rptms1 = sctms1
     c                   eval      rptms2 = sctms2
     c                   eval      rptms3 = sctms3
     c                   eval      rptmo1 = sctmo1
     c                   eval      rptmo2 = sctmo2
     c                   eval      rptmo3 = sctmo3
     c                   eval      rptmf1 = sctmf1
     c                   eval      rptmf2 = sctmf2
     c                   eval      rptmf3 = sctmf3
     c*
     c                   z-add     sctmd1        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rptmd1
     c*
     c                   z-add     sctmd2        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rptmd2
     c*
     c                   z-add     sctmd3        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   MOVEL     SRDTO         rptmd3

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     c                   eval      *inlr = *on
     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr

     c                   select

     c                   when      @@mode = 'A'
     c                   eval      *in40 = *on

     c                   when      @@mode = 'D'
     c                   eval      *in09 = *on

     c                   endsl

      *  Load Screen Headings

     c     @@rrn2        chain     pel01                              60
     c     emky          chain     pem01                              60
     c                   eval      scfyr  = elfyr
     c                   eval      scfnd  = emfnd

      * get fund name

     c                   eval      scfnam = *blanks
     c     fnky          chain     pfn01
     c                   if        %found
     c                   if        fnanam <> *blanks
     c                   eval      scfnam = fnanam
     c                   else
     c                   eval      scfnam = fnname
     c                   endif
     c                   endif

     c                   if        elfaf = 'A'
     c                   eval      scpyst = 'Audited'
     c                   else
     c                   eval      scpyst = 'Estimated'
     c                   endif
      *
      *  Concat Employer/Division/Name
      *
     c                   CALL      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c                   parm                    ##edn            80
      *
     c                   eval      scedn = ##edn

      *  get Cancel date

     c                   eval      x = 5
      *
     c                   dow       (ef(x) = 0) and (x > 1)
     c                   eval      x = x - 1
     c                   enddo
      *
     c                   eval      *in41 = *off
      *

     c                   time                    DSDTTM
     c                   z-add     dsdate        srdti
     c                   CALL      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysdt

      *  Get Agent/Producer Information

     c                   call      'WA002R'      AGTPL
     c                   eval      scanam = ##anam
     c                   eval      scpnam = ##pnam


     C                   endsr


     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb

     C                   eval      rpuser = q1user
     C                   eval      rppgm = q1pgm
     C                   time                    dsdttm
     C                   eval      rptime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         rpdate
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb

     C                   eval      rpuser = q1user
     C                   eval      rpiusr = q1user
     C                   eval      rppgm  = q1pgm
     C                   eval      rpipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      rptime = dstime
     C                   eval      rpitim = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         rpdate
     C                   movel(p)  srdto         rpidat
     C                   write     dbrcdf
      *
     C                   feod      wlrpp
     C                   eval      outflg = *on
     C                   endsr

