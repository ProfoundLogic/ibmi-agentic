     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
     H*/TITLE W7946R02 -
    H*********************************************************************
    H*  Description: State Workfile Maintenance.                         *
    H*               Basically the chg status option is the only use.    *
    H*                                                                   *
    H*  Called from: W7946R01, W7946R                                    *
    H*                                                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*  Created: C:296  Project: 114MQA   Date: 12/09/14                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
    H**************************************************************************************
     FW7946f02  cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
    F*
    F*  Physical file definition (for maintaining record)
     Fwxsselp   uf a e             disk    rename(pss01:dbrcdf)
     F
     Fwmd1l     if   e           k disk
     Fwdf2l     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Renewal Tracking State Select'
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
     D xxiefl          s                   like(ssiefl)
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
     c                   parm                    @@pgm            10
     c                   parm                    @@car#            5 0
    C*
    C*  Company file/state retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99=*off
     C*
     C                   if        ssiefl='E'
     C                   eval      ssiefl =  'I'
     C                   else
     C                   eval      ssiefl = 'E'
     C                   endif                                                  fnflag='I'
     C*
     C                   update    dbrcdf
     C*
     C                   endif                                                  *in99=*off
     C*
     C                   endsr                                                  chgsts
     C*
     C*================================================================
     C* CHGSTS1 - SUBROUTINE FOR CHANGING STATUS (ALL)
     C*================================================================
     C*
     C     chgsts1       begsr
     C*
     C     1             chain     dbrcdf                             99
     C*
     C     *in99         doweq     *off
     C*
     C                   If        %parms > 5 and  @@car#<> 0
     C                   endif
     C                   if        ssiefl='E'
     C                   eval      ssiefl =  'I'
     C                   else
     C                   eval      ssiefl = 'E'
     C                   endif                                                  fnflag='I'
     c                   if        ssiefl = 'I' and xxpgm = 'W7480R'
     c
     c                   endif
     C*
     C                   update    dbrcdf
     C     next          tag
     C*
     C                   read      dbrcdf                                 99
     C                   enddo                                                  *in99=*off
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
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
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
     c                   move      *blanks       xxpgm            10
     c                   if        q1prms > 4
     c                   move      @@pgm         xxpgm
     c                   endif
     C                   select
     C
     C                   when      @@mode = 'A'
     C                   exsr      chgsts1
      /free
        *inlr = *on;
      /end-free
     C                   return
     C
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
      /free
        *inlr = *on;
      /end-free
     C                   return
     C
     C                   endsl
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
     C                   feod      wxsselp
     C                   endsr
