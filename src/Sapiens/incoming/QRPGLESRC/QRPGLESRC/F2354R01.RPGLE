     H*  (C) Copyright Fiserv insurance Solutions, Inc., 2008
     H**************************************************************************************
     H* H/TITLE - Work with MS-DRG's Detail Screen program.                                *
     H*                                                                                    *
     H* DESCRIPTION: This program shows option 5(Detail) for Work with MS-DRG's            *
     H*                                                                                    *
     H* CALLED BY: F2354R                                                                  *
     H*                                                                                    *
     H* CALLS TO :                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 04/03/08   Programmer C:189   Project:TX039                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/30/15 C:212  Project: FS468                                        *
     H*                  Load ALOS                                             *
     H*                                                                                    *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H**sy266_ovr*** DFTACTGRP(*NO)
     H DFTACTGRP(*NO) actgrp(*caller)
      /ENDIF
     Ff2354f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fftdgp     uf a e             disk
     F                                     rename(pdg01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'IPPS MS-DRG''s'
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
     C                   Z-add     dgdrg#        scdrg#
     C
     C                   If        %len(dgdesc) > 55
     C                   movel     dgdesc        scdesc
     C                   eval      scdesc1= %triml(%subst(dgdesc :55:44))
     C                   else
     C                   movel     dgdesc        scdesc
     C                   endif
     C
     C                   z-add     dgefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C                   z-add     dgrelw        screlw
     C                   z-add     dgglos        scglos
     C                   z-add     dgoutl        scoutl
     C                   Movel     dgpsac        scpsac
     C                   Movel     dgspec        scspec
     C                   Movel     dgmdc         scmdc
     C                   Movel     dgtype        sctype
     C**** Beg Add *** 10/30/15 *******************************************
      /free
         scalos = DGalos;
      /end-free
     C**** End Add *** 10/30/15 *******************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   Z-add     scdrg#        dgdrg#
     C                   movel     scdesc        dgdesc
     C                   z-add     dgefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         scefdt
     C                   Movel     scpsac        dgpsac
     C                   Movel     scspec        dgspec
     C                   Movel     scmdc         dgmdc
     C                   Movel     sctype        dgtype
     C                   z-add     screlw        dgrelw
     C                   z-add     scglos        dgglos
     C                   z-add     scoutl        dgoutl
     C**** Beg Add *** 10/30/15 *******************************************
      /free
         DGalos = scalos;
      /end-free
     C**** End Add *** 10/30/15 *******************************************
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
     C                   feod      ftdgp
     C                   endsr
