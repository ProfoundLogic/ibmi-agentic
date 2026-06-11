    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE W8275R01
     H**************************************************************************
    H*  W8275R01   : First Health Claims Ack - Transfer History
    H*
     H*  Description:
     H*
    H*  Called from: W8275R - First Health Claims Ack - Work with Transfer Hist
    H*  Calls to   : W8276R - First Health Claims Ack - Work with Claim Errors
    H*
     H**************************************************************************
     H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
    H*  Indicators used:
    H*
     H**************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8275f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwzc9p     uf a e             disk
     F                                     rename(pc901:dbrcdf)
    D*
     D*****************/COPY SCOPYMT01I
    D*
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('First Health Claims Ack -
     D                                     Transfer History')
     D comapi          c                   const('WA001R')
     D #ofscr          c                   const(1)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##call          s              1
     D ##dcid          s                   like(c9dcid)
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
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
     C******************/COPY SCOPYMT01C
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
     C                   movel(p)  c9dcid        scdcid
     C                   movel(p)  c9mbr#        scmbr#
    C*
     C                   eval      srdti = c9uldt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sculdt
    C*
     C                   eval      scultm = c9ultm
    C*
     C                   eval      srdti = c9cvdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sccvdt
    C*
     C                   eval      sccvtm = c9cvtm
    C*
     C                   eval      srdti = c9prdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scprdt
    C*
     C                   eval      scprtm = c9prtm
    C*
     C                   eval      sc#err = c9#err
    C*
     C                   eval      *in71  = *off
     C                   if        c9prfl = 'E'
     C                   eval      *in71  = *on
     C                   endif
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
     C                   select
     C                   when      *inkg = *on
     C                   exsr      kgdo
     C                   endsl
     C                   endsr
    C*================================================================
    C* KGDO - Work with Claim Errors
    C*================================================================
     C     kgdo          begsr
    C*
     C                   call      'W8276R'
     C                   parm                    ##kc
     C                   parm                    ##kl
     C                   parm                    ##call
     C                   parm      scdcid        ##dcid
    C*
     C                   if        ##kl  = *on
     C                   eval      *inkl = *on
     C                   endif
    C*
     C                   if        ##kc  = *on
     C                   eval      *inkc = *on
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
     C                   if        *inkl = *on
     C                   eval      *inkl = *off
     C                   endif
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
     C                   feod      wzc9p
    C*
     C                   endsr
