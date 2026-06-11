     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
     H**************************************************************************************
     H* H/TITLE - Work with OPPS Hospital Impacts Detail program.                          *
     H*                                                                                    *
     H* DESCRIPTION: This program shows option 5(Detail) for Work with OPPS Hospital       *
     H*  Impacts.                                                                          *
     H*                                                                                    *
     H* CALLED BY: F2351R                                                                  *
     H*                                                                                    *
     H* CALLS TO :                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 04/03/08   Programmer C:198   Project:TX039                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2351f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftoip     uf a e             disk
     F                                     rename(poi01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'OPPS HOSPITAL IMPACT INFORMATION'
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
     C                   movel     oiprov        scprov
     C                   z-add     oiefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C                   movel     oiCBSA        scCBSA
     C                   eval      SCTDU  =  OITDU
     C                   movel     oiRSCH        scRSCH
     C                   eval      SCPRWI =  OIPRWI
     C                   movel     oiAR          scAR
     C                   movel     oiURGE        scURGE
     C                   movel     oiRGN         scRGN
     C                   eval      SCDSH  =  OIDSH
     C                   eval      SCOCCR =  OIOCCR
     C                   eval      SCEPP1 =  OIEPP1
     C                   eval      SCEPP2 =  OIEPP2
     C                   eval      SCEOUT =  OIEOUT
     C                   eval      SCBEDS =  OIBEDS
     C                   movel     oiTEAC        scTEAC
     C                   movel     oiOWN         scOWN
     C                   movel     oistat        scstat
     C                   eval      SCPLIN =  OIPLIN
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   movel     scprov        oiprov
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   Movel     srdto         oiefdt
     C                   movel     scCBSA        oiCBSA
     C                   eval      oiTDU  =  scTDU
     C                   movel     scRSCH        oiRSCH
     C                   eval      oiPRWI =  scPRWI
     C                   movel     scAR          oiAR
     C                   movel     scURGE        oiURGE
     C                   movel     scRGN         oiRGN
     C                   eval      oiDSH  =  scDSH
     C                   eval      oiOCCR =  scOCCR
     C                   eval      oiEPP1 =  scEPP1
     C                   eval      oiEPP2 =  scEPP2
     C                   eval      oiEOUT =  scEOUT
     C                   eval      oiBEDS =  scBEDS
     C                   movel     scTEAC        oiTEAC
     C                   movel     scOWN         oiOWN
     C                   eval      oiPLIN =  scPLIN
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
     C                   write     dbrcdf
     C                   feod      ftoip
     C                   endsr
