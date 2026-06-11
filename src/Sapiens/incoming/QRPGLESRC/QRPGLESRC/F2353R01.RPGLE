     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H* H/TITLE - Work with IPPS Hospital Impacts Detail program.                          *
     H*                                                                                    *
     H* DESCRIPTION: This program shows option 5(Detail) for Work with IPPS Hospital       *
     H*  Impacts.                                                                          *
     H*                                                                                    *
     H* CALLED BY: F2353R                                                                  *
     H*                                                                                    *
     H* CALLS TO :                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 04/03/08   Programmer C:189   Project:TX039                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff2353f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftiip     uf a e             disk
     F                                     rename(pii01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IPPS Hospital Impacts'
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
     C                   movel     IIPROV        SCPROV
     C                   z-add     iiefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C                   movel     IINAME        SCNAME
     C                   movel     IIGLMA        SCGLMA
     C                   movel     IICLMA        SCCLMA
     C                   movel     IIPLMA        SCPLMA
     C                   z-add     IIW505        SCW505
     C                   movel     IIE505        SCE505
     C                   movel     IIH401        SCH401
     C                   z-add     IIPRWI        SCPRWI
     C                   z-add     IICOLA        SCCOLA
     C                   z-add     IIRBED        SCRBED
     C                   movel     IISCC         SCSCC
     C                   z-add     IIHRAT        SCHRAT
     C                   z-add     IIRDAY        SCRDAY
     C                   z-add     IIBEDS        SCBEDS
     C                   z-add     IIADC         SCADC
     C                   z-add     IIOCCR        SCOCCR
     C                   z-add     IICCCR        SCCCCR
     C                   z-add     IIDSH         SCDSH
     C                   movel     IIPRLM        SCPRLM
     C                   z-add     IIPRW1        SCPRWI1
     C                   movel     IIPRTY        SCPRTY
     C                   movel     IILUGR        SCLUGR
     C                   movel     IIRECL        SCRECL
     C                   z-add     IIBILL        SCBILL
     C                   z-add     IITA24        SCTA24
     C                   z-add     IICM24        SCCM24
     C                   z-add     IISC24        SCSC24
     C                   z-add     IITA25        SCTA25
     C                   z-add     IICM25        SCCM25
     C                   z-add     IISC25        SCSC25
     C                   movel     IIRGN         SCRGN
     C                   movel     IIURBN        SCURBN
     C                   movel     IIURPA        SCURPA
     C                   z-add     IITCOP        SCTCOP
     C                   z-add     IITCCP        SCTCCP
     C                   z-add     IIPRGA        SCPRGA
     C                   z-add     IIPRFP        SCPRFP
     C                   z-add     IIAKHI        SCAKHI
     C                   z-add     IIODSH        SCODSH
     C                   z-add     IICDSH        SCCDSH
     C                   z-add     IIEOPM        SCEOPM
     C                   z-add     IIECPM        SCECPM
     C                   z-add     IIMDPC        SCMDPC
     C                   movel     IISTAT        SCSTAT
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   movel     SCPROV        IIPROV
     C                   z-add     SCEFDT        IIEFDT
     C                   movel     SCNAME        IINAME
     C                   movel     SCGLMA        IIGLMA
     C                   movel     SCCLMA        IICLMA
     C                   movel     SCPLMA        IIPLMA
     C                   z-add     SCW505        IIW505
     C                   movel     SCE505        IIE505
     C                   movel     SCH401        IIH401
     C                   z-add     SCPRWI        IIPRWI
     C                   z-add     SCCOLA        IICOLA
     C                   z-add     SCRBED        IIRBED
     C                   movel     SCSCC         IISCC
     C                   z-add     SCHRAT        IIHRAT
     C                   z-add     SCRDAY        IIRDAY
     C                   z-add     SCBEDS        IIBEDS
     C                   z-add     SCADC         IIADC
     C                   z-add     SCOCCR        IIOCCR
     C                   z-add     SCCCCR        IICCCR
     C                   z-add     SCDSH         IIDSH
     C                   movel     SCPRLM        IIPRLM
     C                   z-add     SCPRWI1       IIPRW1
     C                   movel     SCPRTY        IIPRTY
     C                   movel     SCLUGR        IILUGR
     C                   movel     SCRECL        IIRECL
     C                   z-add     SCBILL        IIBILL
     C                   z-add     SCTA24        IITA24
     C                   z-add     SCCM24        IICM24
     C                   z-add     SCSC24        IISC24
     C                   z-add     SCTA25        IITA25
     C                   z-add     SCCM25        IICM25
     C                   z-add     SCSC25        IISC25
     C                   movel     SCRGN         IIRGN
     C                   movel     SCURBN        IIURBN
     C                   movel     SCURPA        IIURPA
     C                   z-add     SCTCOP        IITCOP
     C                   z-add     SCTCCP        IITCCP
     C                   z-add     SCPRGA        IIPRGA
     C                   z-add     SCPRFP        IIPRFP
     C                   z-add     SCAKHI        IIAKHI
     C                   z-add     SCODSH        IIODSH
     C                   z-add     SCCDSH        IICDSH
     C                   z-add     SCEOPM        IIEOPM
     C                   z-add     SCECPM        IIECPM
     C                   z-add     SCMDPC        IIMDPC
     C                   movel     SCSTAT        IISTAT
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
     C                   feod      ftiip
     C                   endsr
