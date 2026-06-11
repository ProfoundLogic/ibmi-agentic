     H*  (C) Copyright Fiserv, Inc., 2007
     H/TITLE W0067R02 - Claim User Security Maintenance
    H*
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Claim User Security Maintenance         *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO: W0067R02                                            *
     H*                                                               *
     H* CREATED: 05/25/07  C:191 PROJECT WC1515                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0067f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmcup     uf a e             disk
     F                                     rename(pcu01:dbrcdf)
    F*  logical file definition (for validating duplicate)
     Fwmcul     if   e           k disk
    F*  Group file definition (for validating group)
     Fwmfnl     if   e           k disk
    F*  Claims file definition (for validating claim#)
     Fwmcml1    if   e           k disk
    F*  User id file definition (for validating user id)
     Fwmupl     if   e           k disk
    F*  Claims physical file definition (for search)
     Fwmcmp     if   e             disk    rename(pcm01:pcm01p)
    F*  User id physical file definition (for search)
     Fsmupp     if   e             disk    rename(pup01:pup01p)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Claims User Security'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##co#a          s              3
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fnda          s              3
     D ##fyr           s              3  0
     D ##fyra          s              3
     D ##kc            s              1
     D ##kl            s              1
     D ##call          s              1
     D ##mode          s              1
     D ##sort          s              2
     D ##view          s              1  0
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D xxco#           s                   like(cuco#)
     D kkco#           s                   like(cuco#)
     D kkfnd           s                   like(cufnd)
     D kkfyr           s                   like(cufyr)
     D kkcase          s                   like(cucase)
     D kkausr          s                   like(cuausr)
     D kkcnam          s                   like(cucnam)
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
    C* key list for claim security file logical
     C     cuky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkausr
    C* key list for group file
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C* key list for claims file
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
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
     C***
     C                   eval      kkco# = xxco#
     C                   eval      kkfnd = scfnd
     C                   eval      kkfyr = scfyr
     C                   eval      kkcase = sccase
     C                   eval      kkausr = scausr
     C
     C*** if mode is ADD
     C* Validate duplicate records
     C                   if        @@mode = 'A'
     C     cuky          chain     pcu01
     C                   if        %found
     C                   eval      *in71 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  %found
     C                   endif                                                  @@mode = 'A'
     C***
     C* Validate group
     C     fnky          chain     pfn01
     C                   if        not %found
     C                   eval      *in72 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  not %found
     C***
     C* Validate Claim number
     C     cmky          chain     pcm01
     C                   if        not %found
     C                   eval      *in73 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   else
     C                   eval      sccnam = %trim(cmlsn) + ', ' + %trim(cmfsn)
     C                                    + ' ' + %trim(cmmid)
     C                   endif                                                  not %found
     C***
     C* Validate User ID
     C     scausr        chain     pup01
     C                   if        not %found
     C                   eval      *in74 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif                                                  not %found
     C***
     C* Validate status
     C                   if        scstat <> 'A' and scstat <>'I' and
     C                             scstat <> 'a' and scstat <>'i'
     C                   eval      *in75 = *on
     C                   eval      *in90 = *on
     C                   leavesr
     C                   endif
     C***
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
     C***
     C  n40              eval      xxco# = cuco#
     C                   eval      scfnd = cufnd
     C                   eval      scfyr = cufyr
     C                   eval      sccase = cucase
     C                   eval      sccnam = cucnam
     C                   eval      scausr = cuausr
     C                   eval      scstat = custat
     C***
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C***
     C                   eval      cuco# = xxco#
     C                   eval      cufnd = scfnd
     C                   eval      cufyr = scfyr
     C                   eval      cucase = sccase
     C                   eval      cucnam = sccnam
     C                   eval      cuausr = scausr
     C                   eval      custat = scstat
     C***
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
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C***
     C*
     C                   select
     C                   when      @@mode = 'A'
     C                   eval      *in40 = *on
     C* get company no. from user parameter
     C     q1user        chain     pup01
     C                   if        %found
     C                   eval      xxco# = upco#
     C                   endif                                                  %found
     C                   endsl
     C***
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C***
     C                   exsr      getdtm
     C                   select
     C                   when      @@mode = 'S'
     C     @@rrn         chain     dbrcdf
     C                   if        %found
     C                   if        custat = 'A'
     C                   eval      custat = 'I'
     C                   else
     C                   eval      custat = 'A'
     C                   endif
     C*
     C                   eval      cupgm = q1pgm
     C                   eval      cuuser = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         cudt
     C                   eval      cutime = dstime
     C*
     C                   update    dbrcdf
     C                   endif
     C                   return
     C                   endsl
     C***
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C***
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      cupgm = q1pgm
     C                   eval      cuuser = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         cudt
     C                   eval      cutime = dstime
     C*
     C                   update    dbrcdf
E01  C                   endif                                                  *in99=*off
     C***
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      cupgm = q1pgm
     C                   eval      cuuser = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         cudt
     C                   eval      cutime = dstime
     C                   eval      cuipgm = q1pgm
     C                   eval      cuiusr = q1user
     C                   eval      cuidt = cudt
     C                   eval      cuitm = dstime
     C                   write     dbrcdf
     C                   feod      wmcup
     C                   endsr
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
     C     kjdo          begsr
     C***
     C                   select
     C*
     C                   when      hfld = 'SCFND'
     C                   move      xxco#         ##co#a
     C                   call      'W0010R'
     C     *inkc         parm      *blanks       ##kc
     C     *inkl         parm      *blanks       ##kl
     C                   parm      '2'           ##call
     C                   parm                    ##co#a
     C                   parm      *zeros        ##fnda
     C                   parm      *zeros        ##fyra
     C                   parm      1             ##view
     C*
     C                   if        *inkl <> *on and ##fnda <> *blanks
     C                   move(p)   ##fnda        scfnd
     C                   seton                                        22
     C                   else
     C                   seton                                        21
     C                   endif                                                  *inkl <> *on
     C*
     C                   when      hfld = 'SCFYR' or
     C                             hfld = 'SCCASE'
     C                   call      'W0070R'
     C                   parm      '2'           ##call
     C     *inkc         parm      *blanks       ##kc
     C     *inkl         parm      *blanks       ##kl
     C                   parm      ' '           ##mode
     C                   parm      *zeros        ##rrn
     C                   parm      'CN'          ##sort
     C                   parm      *zeros        ##rrn2
     C                   parm      *zeros        ##fyr
     C*
     C                   if        *inkl <> *on and ##rrn <> *zeros
     C     ##rrn         chain     pcm01p
     C                   if        %found
     C                   eval      scfnd = cmfnd
     C                   eval      scfyr = cmfyr
     C                   eval      sccase = cmcase
     C                   seton                                        24
     C                   endif                                                  %found
     C                   else
     C                   seton                                        22
     C                   endif                                                  *inkl <> *on
     C*
     C                   when      hfld = 'SCAUSR'
     C                   call      'S0005R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C                   parm      3             ##view
     C*
     C                   if        ##rrn <> *zeros
     C     ##rrn         chain     pup01p
     C                   if        %found
     C                   eval      scausr = upusr
     C                   seton                                        25
     C                   endif                                                  %found
     C                   else
     C                   seton                                        24
     C                   endif                                                  *inkl <> *on
     C*
     C                   endsl
     C                   seton                                        90
     C
     C                   endsr
