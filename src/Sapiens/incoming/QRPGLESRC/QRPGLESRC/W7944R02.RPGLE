     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
     H*/TITLE W7944R02 -
    H*********************************************************************
    H*  Description: Renewal Tracking Group Workfile Maintenance.        *
    H*               Basically the chg status option is the only use.    *
    H*                                                                   *
    H*  Called from: W7944R01                                            *
    H*                                                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*  Created: C:53   Project: 129WJW/WC1277  03/16/05                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  01/08/14  C:279 Project: 114WKC                                                   *
     H*                  Display all group for User Group restriction setup.               *
     H*                                                                                    *
     H*  11/23/10  C:189 Project 139WPQ                                                    *
     H*                  'Select All' should work on the subsetted list if Car# entered on *
     H*                   on subset screen                                                 *
     H*                                                                                    *
     H*  11/02/09  C:65  Project: WC1819                                                   *
    H*                  Only allow a record to be set for included from Medicare process  *
     H*                  if a prior extract has been updated with response date, or no     *
     H*                  prior extract.                                                    *
     H*                                                                                    *
     H*  08/16/06  C:33  Project: WC1143                                                   *
    H*                  Change status for all groups correctly.                           *
    H**************************************************************************************
     FW7944f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwxgsp     uf a e             disk
     F                                     rename(pgs01:dbrcdf)
     f***  begin add  ***  11/02/09  **************************************
     Fwmd1l     if   e           k disk
     f***  end add ******  11/02/09  **************************************
     f*** beg Add *** 11/23/10 ******************************************
     Fwdf2l     if   e           k disk
     f*** end Add *** 11/23/10 ******************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Renewal Tracking Group Select'
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
     D xxiefl          s                   like(gsiefl)
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
     c*** begin add ***  11/02/09  ***************************************
     c                   parm                    @@pgm            10
     c*** end add *****  11/02/09  ***************************************
     c*** begin add ***  11/23/10  ***************************************
     c                   parm                    @@car#            5 0
     c*** end add *****  11/23/10  ***************************************
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
     f***  begin add  ***  11/02/09  **************************************
     C     d1ky          klist
     c                   kfld                    gsco#
     c                   kfld                    gsfnd
     f***  end add ******  11/02/09  **************************************
     C*** beg add *** 11/23/10 ****************************************
     C     f2ky          klist
     C                   kfld                    ##co#
     C                   kfld                    gsfnd
     C*** End add *** 11/23/10 ****************************************
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
     C                   if        gsiefl='E'
     C                   eval      gsiefl =  'I'
     C                   else
     C                   eval      gsiefl = 'E'
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
     C***  Begin Del  ***  08/16/06  **************************************
     C**********
     C**********         if        gsiefl='E'
     C**********         eval      xxiefl =  'I'
     C**********
     C**********         else
     C**********         eval      xxiefl = 'E'
     C**********         endif                                                  fnflag='I'
     C***  End   Del  ***  08/16/06  **************************************
     C*
     C     *in99         doweq     *off
     C*
     c*** begin add ***  11/23/10  **************************************************
     C                   If        %parms > 5 and  @@car#<> 0
     C     f2ky          chain     pf201                              60
     C                   If        *in60 = *off
     C                   If        @@car#<> 0 and f2car#<> @@car#
     C                   Goto      Next
     C*
     C                   endif
     C                   else
     C                   Goto      Next
     C                   endif
     C                   endif
     c*** End   add ***  11/23/10  **************************************************
     C***  Begin Chg  ***  08/16/06  **************************************
     C**********         eval      gsiefl = xxiefl
     C                   if        gsiefl='E'
     C                   eval      gsiefl =  'I'
     C                   else
     C                   eval      gsiefl = 'E'
     C                   endif                                                  fnflag='I'
     C***  End   Chg  ***  08/16/06  **************************************
     c*** begin add ***  11/02/09  ***************************************
     c                   if        gsiefl = 'I' and xxpgm = 'W7480R'
     c
     c     d1ky          setgt     pd101
     c     d1ky          readpe    pd101                                  60
     c
     c                   if        *in60 = *off and d1rsdt = 0
     C                   eval      gsiefl = 'E'
     c                   endif
     c
     c                   endif
     c*** end add *****  11/02/09  ***************************************
     C*
     C                   update    dbrcdf
     c*** begin add ***  11/23/10  **************************************************
     C     next          tag
     c*** End   add ***  11/23/10  **************************************************
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
     c*** begin add ***  11/02/09  ***************************************
     c                   move      *blanks       xxpgm            10
     c                   if        q1prms > 4
     c                   move      @@pgm         xxpgm
     c                   endif
     c*** end add *****  11/02/09  ***************************************
     C                   select
     C
     C                   when      @@mode = 'A'
     C                   exsr      chgsts1
      /free
       //****** beg add ******* 01/08/14 *************************
        *inlr = *on;
       //****** end add ******* 01/08/14 *************************
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
       //****** beg add ******* 01/08/14 *************************
        *inlr = *on;
       //****** end add ******* 01/08/14 *************************
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
     C                   feod      wxgsp
     C                   endsr
