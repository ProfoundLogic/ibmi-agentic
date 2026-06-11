    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
    H*
     H**************************************************************************
    H*  F8285R01 - First Health EOR Financial Work with Transfer History Update
    H*
    H*  Called from: F8285R - First Health EOR Financial Work with Transfer His
    H*  Calls to   : None
    H*
     H**************************************************************************
    H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
    H*  Based on F8265R01 from Client 135
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
    H*  Indicators used:
    H*
    H****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff8285f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffax9p     uf a e             disk
     F                                     rename(px901:dbrcdf)
    D*
     D******/COPY SCOPYMT01I
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('Bill Transfer Histor-
     D                                     y')
     D comapi          c                   const('WA001R')
     D #ofscr          c                   const(1)
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
    C*
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
    C*
     C                   movel(p)  x9dcid        scdcid
     C                   movel(p)  x9mbr#        scmbr#
    C*
     C                   eval      srdti = x9uldt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sculdt
    C*
     C                   eval      scultm = x9ultm
    C*
     C                   eval      srdti = x9cvdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sccvdt
    C*
     C                   eval      sccvtm = x9cvtm
    C*
     C                   eval      srdti = x9prdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scprdt
    C*
     C                   eval      scprtm = x9prtm
    C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
    C*
    C*
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
    C*
     C                   eval      *inlr = *on
    C*
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
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
B01  C                   if        *in99 = *off
    C*
     C                   exsr      mvscdb
     C                   update    dbrcdf
    C*
E01  C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      fax9p
    C*
     C                   endsr
