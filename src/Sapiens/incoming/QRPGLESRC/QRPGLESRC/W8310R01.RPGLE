     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W8310R01 - Maintain DCI Data Export                      *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Maintain DCI Data Export                *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: W8310R                                              *
     H*                                                               *
     H* CREATED: 05/21/09  C:191 PROJECT WC1792                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/08/11 C:220  PROJECT: WC1987                               *
     H*                 - Add caode for below fields                  *
     H*                 Claims "Reported to Insurer Date" is          *
     H*                 before/after 9/2009                           *
     H*                                                               *
     H*****************************************************************
    H*
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw8310f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwxf1p     uf a e             disk
     F                                     rename(pf101:dbrcdf)
     Fwxf1l     if   e           k disk
     Fwmfnl     if   e           k disk
      *  GENERIC TABLE FILE
     Fwtgtl     if   e           k disk
     F*  User id file definition (for getting user param)
     Fwmupl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'DCI Data Export'
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
     D xxfnd           s              3  0
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
     C*
     C     dtcmdp        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *blanks       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*  Group Master File
     C*
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    xxfnd
     C*
      *   keylist for the generic table file
      *
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    xxtbid
     C                   kfld                    xxkey
      *
     C*  DCI Extract submission LF
     C*
     C     f1ky          klist
     C                   kfld                    ##co#
     C                   kfld                    scfnd
     C                   kfld                    scst
      *
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
     C*
     C* Validate Fund # - Should not be zero
     C                   if        scfnd = *zeros
     C                   seton                                        9031
     C                   leavesr
     C                   endif                                                  scfnd = *zeros
     C*
     C*** --- Must exist in fund file
     C                   move      scfnd         xxfnd
     C     fnky          chain     pfn01                              60
     C                   if        not %found
     C                   seton                                        9032
     C                   leavesr
     C                   endif                                                  not %found
     C*
     C                   movel(p)  fnname        xxname
     C                   movel(p)  fnname        scname
     C*
     C* Validate State# - Should not be blank
     C                   if        scst = *blank
     C                   seton                                        9033
     C                   leavesr
     C                   endif                                                  scst = *blank
     C*
     C*** --- Must exist in table
     C                   movel(p)  'WCST'        xxtbid
     C                   movel(p)  scst          xxkey
     C     gtky          chain     pgt01                              60
     C                   if        not %found or gtstat <> 'A'
     C                   seton                                        9034
     C                   leavesr
     C                   endif                                                  not %found
     C*
     C* Edit for duplicate record - Fund + State
     C                   if        *in40 or scst <> f1st
     C     f1ky          setll     pf101                                  60
     C                   if        %equal
     C                   seton                                        9035
     C                   leavesr
     C                   endif                                                  %equal
     C                   endif                                                  *in40
     C*
     C* Edit for State Sampling Ratio
     C                   if        not *in40 and scrtio <= 0 and f1flag = 'I'
     C                   seton                                        9036
     C                   leavesr
     C                   endif                                                  not *in40
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
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C  N40              eval      ##co# = f1co#
     C                   eval      scfnd = f1fnd
     C                   movel(p)  f1name        scname
     C                   movel(p)  f1name        xxname           60
     C                   eval      scst = f1st
     C                   eval      scrtio = f1rtio
     C                   eval      scflag = f1flag
     C***** Beg Add ***** 03/08/11 ************************************
     C                   eval      scrtop = f1rtop
     C                   eval      scrtcl = f1rtcl
     C***** End Add ***** 03/08/11 ************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      f1co# = ##co#
     C                   eval      f1fnd = scfnd
     C                   movel(p)  xxname        f1name
     C                   eval      f1st = scst
     C                   eval      f1rtio = scrtio
     C***** Beg Add ***** 03/08/11 ************************************
     C                   eval      f1rtop = scrtop
     C                   eval      f1rtcl = scrtcl
     C***** End Add ***** 03/08/11 ************************************
     C*
     C                   if        *in40
     C                   if        f1rtio > 0
     C                   eval      f1flag = 'I'
     C                   else
     C                   eval      f1flag = 'E'
     C                   endif                                                  f1rtio > 0
     C                   endif                                                  *in40
     C*
     C* Update maintenance fields
     C                   eval      f1pgm = q1pgm
     C                   eval      f1user = q1user
     C                   exsr      getdtm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         f1dt
     C                   eval      f1time = dstime
     C*
     C* If Add mode - Update initial fields
     C                   if        *in40
     C                   eval      f1ipgm = q1pgm
     C                   eval      f1iusr = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         f1idt
     C                   eval      f1itm = dstime
     C                   endif                                                  *in40
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
     C***
     C* Process search key
     C                   select
     C                   when      *inkj = *on
     C                   exsr      kjdo
     C                   endsl
     C***
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
     C*
     C* get company no. from user parameter
     C     q1user        chain     pup01
     C                   if        %found
     C                   eval      ##co# = upco#
     C                   endif                                                  %found
     C*
     C                   eval      *in40 = *off
     C                   select
     C                   when      @@mode = 'A' or @@mode = 'P'
     C                   eval      *in40 = *on
     C                   endsl
     C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C**** processing for 20 option, it will have @@mode = 'S'
     C**** change status
     C                   select
     C                   when      @@mode = 'S'
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C* toggle Status
     C                   if        f1flag = 'I'
     C                   eval      f1flag = 'E'
     C                   else
     C                   eval      f1flag = 'I'
     C                   endif                                                  f1flag = 'I'
     C*
     C                   eval      f1pgm = q1pgm
     C                   eval      f1user = q1user
     C                   exsr      getdtm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         f1dt
     C                   eval      f1time = dstime
     C                   update    dbrcdf
E01  C                   endif
     C                   return
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
     C                   feod      wxf1p
     C                   endsr
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
     C     kjdo          begsr
     C***
     C                   setoff                                       414243
     C                   select
     C* Search Fund
     C                   when      hfld = 'SCFND'
     C                   move      scfnd         #@fnd
     C                   move      ##co#         #@co#
     C*
     C                   call      'W0010R'                             99
     C     *inkc         parm                    ##kc              1
     C     *inkl         parm                    ##kl              1
     C                   parm      '2'           ##call            1
     C                   parm                    #@co#             3
     C                   parm      *blanks       #@fnd             3
     C*
     C                   if        #@fnd <> *blanks
     C* Search successful
     C                   move      #@fnd         scfnd
     C                   move      scfnd         xxfnd
     C* Fund found load fund name
     C     fnky          chain     pfn01                              99
     C                   movel(p)  fnname        scname
     C                   movel(p)  fnname        xxname
     C                   eval      *in42 = *on
     C                   else
     C* Search un-successful
     C                   eval      *in41 = *on
     C                   endif                                                  #@fnd <> *blanks
     C* Search Fund
     C                   when      hfld = 'SCST'
     C                   movel(p)  'WCST'        xxtbid
     C                   move(p)   *blanks       xxkey
     C*
     C                   CALL      'W2493R'
     C                   parm                    xxprds           40
     C                   parm                    xxcall            1
     C                   parm                    xxtbid            4
     C                   parm                    xxkey            10
     C                   parm                    xxrtcd            1
     C*
     C                   select
     C                   when      xxrtcd = 'A'
     C                   eval      *inkc = *on
     C                   when      xxrtcd = 'B' or xxkey = *blanks
     C                   eval      *in42=*on
     C                   when      xxrtcd = '*'
     C                   movel(p)  xxkey         scst
     C                   eval      *in43 = *on
     C                   endsl                                                  xxrtcd
     C*
     C                   endsl                                                  hfld
     C                   seton                                        90
     C
     C                   endsr
