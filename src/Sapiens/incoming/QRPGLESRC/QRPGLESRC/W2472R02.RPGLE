     H*  (C) Copyright Progressive Data Solutions, Inc., 2004
     H****************************************************************
     H* Loss Cost Multiplier for Rate Set Code Maintenance           *
     H*
     H*  10/20/04  C:132  PROJECT: WC1136                            *
     H*                                                              *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                              *
     H*  08/10/05 C:136 PROJECT: WC1136                              *
     H*                 ADD EXPIRATION DATE FIELD                    *
     H****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2472f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Loss Cost Multiplier file (for maintaining record)
     Fwdlmp     uf a e             disk
     F                                     rename(plm01:dbrcdf)
    F*
    F*  Rate Set Code  (for F10 Search)
     Fwmrsp     if   e             disk
     F                                     rename(prs01:prs02)
    F*
    F*  Rate Set Code
     Fwmrsl     if   e           k disk
      *
      *
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Loss Cost Multiplier-Rate Set Code'
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
     D @@JRST          s              3
     D @@RTCD          s              3
     D xxrtcd          s                   like(rsrtcd)
      *** beg add ***** 08/10/05 ***********************************************
     D xxefdt          s                   like(lmefdt)
     D xxexdt          s                   like(lmefdt)
      *** end add ***** 08/10/05 ***********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C/COPY SCOPYMT01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@JRST
     C                   parm                    @@rtcd
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
      *
     C     RSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXRTCD
      *
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================

    C*================================================================
    C* CHGSTS - Update WXPGP with New Status Flag
    C*================================================================
     C     Chgsts        Begsr

     C     @@rrn         Chain     Wdlmp

     C                   If        %found

     C                   If        LMSTAT = 'A'
     C                   Eval      LMSTAT = 'I'

     c                   else

     C                   Eval      LMSTAT = 'A'
     C                   Endif

     C                   Update    dbrcdf
     C                   Endif

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
     C                   eval      *IN61 = *off
     C*
     C* Check Bill Schedule Code & Eff Date if ADD Mode
     C                   If        *IN40  = *ON
     C*
     C                   If        SCRTCD = *blanks
     C                   move      *ON           *IN90
     C                   move      *ON           *IN30
     C                   endif
     C*
     C                   If        SCRTCD <> *blanks
     C                   movel(P)  scrtcd        xxrtcd
     C     RSKY          chain     prs01                              61
     C                   If        *IN61  =  *on
     C                   move      *ON           *IN90
     C                   move      *ON           *IN31
     C                   endif
     C                   endif
     C*
     C                   Z-ADD     scEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C                   eval      srdti = SCEFDT
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C                   if        (srerr = *on)
     C                   move      *ON           *IN90
     C                   move      *on           *in32
     C                   endif
     C*
     C* The Loss Cost Multiplier cannot be Zero or Less than Zero
     C                   IF        NOT *IN90 AND SCLCM <= *ZEROS
     C                   move      *ON           *IN90
     C                   move      *ON           *IN33
     C                   ENDIF
     C*
     C                   endif                                                  End *IN40 = *ON
     C*
      *** BEG ADD ***** 08/10/05 **************************************
     C  n90              if        scexdt <> 0
     C                   eval      srdti = scexdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scexdt
     C                   if        (srerr = *on)
     C                   seton                                        71  90
     C                   endif
     C                   endif

     c  n90              if        scexdt <> 0
     c                   eval      srdti = scefdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxefdt
     c                   eval      srdti = scexdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxexdt
     c                   if        xxexdt < xxefdt
     c                   seton                                        72  90
     c                   endif
     c                   endif
      *** END ADD ***** 08/10/05 **************************************
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
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     kjdo          begsr
      *
     C                   select
      *
     C                   when      hfld = 'SCRTCD' AND *IN40 = *ON
     C                   CALL      'W2471R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##RTCD            3
     C*
     C*****##RRN         CHAIN     WMRSP                              60
     C*
     C                   IF        ##RTCD <> *BLANKS AND RSSTAT <> 'I'
     C                   MOVEL(P)  ##RTCD        SCRTCD
     C                   Eval      scstat = 'A'
     C                   ELSE
     C                   move      *ON           *IN90
     C                   move      *ON           *IN31
     C                   endif
      *
     C                   ENDSL                                                  *IN60=*OFF
     C*
     C                   endsr
      *
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   IF        NOT *IN40
     C                   move      lmrtcd        scrtcd
     C                   move      lmlcm         sclcm
     C                   move      lmSTAT        scSTAT
     C*
     C                   Z-ADD     LMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   ELSE
     C                   CLEAR                   SCLCM
     C                   CLEAR                   SCSTAT
     C                   CLEAR                   SCEFDT
     C                   ENDIF
     C*
      *** beg add ***** 08/10/05 ***********************************************
     c                   eval      srdti = lmexdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scexdt
      *** end add ***** 08/10/05 ***********************************************
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   move      sclcm         lmlcm
     C*
     C                   MOVE      Q1PGM         LMPGM
     C                   MOVE      Q1USER        LMUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LMDT
     C                   MOVEL     DSTIME        LMTIME
     C*
     C                   IF        not *IN40
      *
     C                   move      scrtcd        lmrtcd
     C                   move      scSTAT        lmSTAT
      *
     C                   Endif
     C*
     C                   MOVE      Q1PGM         LMPGM
     C                   MOVE      Q1USER        LMUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LMDT
     C                   MOVEL     DSTIME        LMTIME
     C*
     C                   IF        *IN40
     C                   MOVEL     SRDTO         LMIDT
     C                   MOVE      Q1PGM         LMIPGM
     C                   MOVE      Q1USER        LMIUSR
     C                   MOVE      'A'           LMSTAT
     C                   MOVE      ##CO#         LMCO#
     C                   MOVEL     SCRTCD        LMRTCD
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LMEFDT
     C                   Endif
     C*
      *** beg add ***** 08/10/05 ***********************************************
     c                   eval      srdti = scexdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         lmexdt
      *** end add ***** 08/10/05 ***********************************************
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
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
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
     C
     C                   MOVE      *OFF          *IN40
     C                   IF        @@MODE = 'A'
     C                   MOVE      *ON           *IN40
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN35
     C                   IF        @@RTCD <> *BLANKS
     C                   EVAL      SCRTCD = @@RTCD
     C                   MOVE      *ON           *IN35
     C                   EVAL      SCJRST = @@JRST
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
      *
     C                   If        @@mode = 'S'
     C                   Exsr      Chgsts
     C                   Return
     C                   Endif

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
     C                   feod      wdlmp
     C                   endsr
