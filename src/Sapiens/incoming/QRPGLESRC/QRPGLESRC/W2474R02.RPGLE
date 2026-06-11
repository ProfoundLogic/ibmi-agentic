     H****************************************************************
     H*  (C) Copyright Progressive Data Solutions, Inc., 2005
     H****************************************************************
    H*                                                              *
     H* Description: Restriction of User IDs access to specific      *
     H*              claims maintenance program.                     *
     H*                                                              *
     H*  04/12/05  C:132  PROJECT: 114WIH                            *
     H*                                                              *
     H****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2474f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmxrp     uf a e             disk
     F                                     rename(pxr01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Claims Restriction Maintenance'
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
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================

    C*================================================================
    C* CHGSTS - Update WMXRP with New Status Flag
    C*================================================================
     C     Chgsts        Begsr

     C     @@rrn         Chain     Wmxrp

     C                   If        %found

     C                   If        XRSTAT = 'A'
     C                   Eval      XRSTAT = 'I'

     c                   else

     C                   Eval      XRSTAT = 'A'
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
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C
     C                   eval      scusid = XRusid
     C                   eval      scfnd  = XRfnd
     C                   eval      scfyr  = XRfyr
     C                   eval      sccase = XRcase
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
    C
     C                   eval      XRusid = scusid
     C                   eval      XRfnd  = scfnd
     C                   eval      XRfyr  = scfyr
     C                   eval      XRcase = sccase
     C*
     C                   MOVE      Q1PGM         XRPGM
     C                   MOVE      Q1USER        XRUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRDT
     C                   MOVEL     DSTIME        XRTIME
     C*
     C                   IF        *IN40
     C                   MOVE      'A'           XRSTAT
     C                   MOVEL     SRDTO         XRIDT
     C                   MOVE      Q1PGM         XRIPGM
     C                   MOVE      Q1USER        XRIUSR
     C                   MOVE      'A'           XRSTAT
     C                   MOVE      ##CO#         XRCO#
     C                   EndIF
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
     C     @@rrn         chain(N)  dbrcdf                             99
     C
     C                   IF        @@mode <> 'A' And @@MODE <> 'C'
     C                   move      *ON           *IN09
     C                   EndIF
     C
     C                   IF        @@mode = 'A'
     C                   move      *ON           *IN40
     C                   EndIF
     C
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
     C                   feod      wmxrp
     C                   endsr
