     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
     H/Title W7945R02 - Bank selection change status
    H*********************************************************************
    H*  Description: Work With Bank Select Workfile                      *
    H*               Allows selection of multiple banks for EFT/ACH      *
    H*                                                                   *
    H*  Called from: W7945R                                              *
    H*                                                                   *
    H*  Calls to: None                                                   *
    H*                                                                   *
    H*  Created: C:191  Project: WC1806  Dated: 06/29/09                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*
    H*  05/28/14 C:136 Project: 129WTG
    H*                 Fix 'Change Status on All' function
    H*
    H**************************************************************************************
    H*
     Fw7945f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwxckp     uf a e             disk
     F                                     rename(pck01:dbrcdf)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Bank select'
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
     C                   if        ckinex='E'
     C                   eval      ckinex =  'I'
     C                   else
     C                   eval      ckinex = 'E'
     C                   endif                                                  ckinex='I'
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
      *** beg chg ***** 05/28/14 ***************************************
     C*****1*****        chain     dbrcdf                             99
     C     1             setll     dbrcdf
     C                   read      dbrcdf                                 99
      *** end chg ***** 05/28/14 ***************************************
     C*
     C     *in99         doweq     *off
     C*
     C                   if        ckinex='E'
     C                   eval      ckinex =  'I'
     C                   else
     C                   eval      ckinex = 'E'
     C                   endif                                                  ckinex='I'
     C*
     C                   update    dbrcdf
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
     C                   select
     C
     C                   when      @@mode = 'A'
     C                   exsr      chgsts1
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
     C                   feod      wxckp
     C                   endsr
