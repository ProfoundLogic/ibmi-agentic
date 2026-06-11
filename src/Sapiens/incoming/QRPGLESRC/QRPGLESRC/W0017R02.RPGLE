     H*  (C) Copyright Progressive Data Solutions, Inc., 2001
    H*
     H/TITLE W0017R02 - Maintenance of Loss Constants
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* MAINTENANCE PROGRAM FOR LOSS CONSTANTS                        *
     H*                                                               *
     H* CALLED BY: E0017R                                             *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 09/28/01
     H*                                                               *
     H* 09/28/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       SC_ST - CANNOT BE BLANK (WC$0503)                  *
     H*  72       SC_ST - INVALID STATE (WCR0504)                     *
     H*  73       SC_EFDT - CANNOT BE BLANK (WC0503)                  *
     H*  74       SC_EFDT - INVALID DATE (WCR0500)                   *
     H*  75       SC_PRBS - CANNOT BE LESS THAN ZERO (WCR0547)        *
     H*  76       SC_STAT - CANNOT BE LESS THAN ZERO (WCR0547)        *
     H*  77       SC_ST - RECORD ALREADY EXISTS (C30005)
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0017f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdscp     uf a e             disk
     F                                     rename(psc01:dbrcdf)
    F*  Loss constant logical (for validation)
     Fwdscl     if   e           k disk
     F*  Fund file (for getting name of group and for validation)
     Fwmfnl     if   e           k disk
     F*  State table (for validation)
     Fstabl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Loss Constants'
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
     D @@fnd           s              3  0
     D kkfnd           s              3  0
     D kkstky          s             10
     D kkst            s              2
     D kkefdt          s              7  0
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
     C                   parm                    @@fnd
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
      *
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
      *
     C     stky          klist
     C                   kfld                    kkstky
      *
     C     scky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkefdt
      *
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
      *
     C                   if        *in90 = *off and sc_st = *blanks             ensure field isn't
     C                   seton                                        7190      blank
     C                   else
     C                   eval      kkstky = sc_st
     C     stky          chain     pst01                              60        validate state
     C   60              seton                                        7290
     C                   endif
      *
     C                   if        *in90 = *off and sc_efdt = 0                 ensure field isn't
     C                   seton                                        7390      blank
     C                   else
     C                   eval      srdti = sc_efdt
     C                   call      'SRCVTDAT'    datchk                         validate date
     C                   movel(p)  srdto         sc_efdt
     C                   if        srerr = '1'
     C                   seton                                        7490
     C                   endif                                                  srerr = '1'
     C                   endif                                                  sc_efdt = 0
      *
     C                   if        *in90 = *off and sc_prbs < 0                 prem basis amt not
     C                   seton                                        7590      less than zero
     C                   endif
      *
     C                   if        *in90 = *off and sc_stat < 0                 stat code amt not
     C                   seton                                        7690      less than zero
     C                   endif
      *
     C                   if        *in90 = *off and *in40 = *on
     C                   eval      kkst = sc_st
     C                   eval      srdti = sc_efdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         kkefdt
     C     scky          chain     psc01                              60
     C                   if        not *in60
     C                   seton                                        7790
     C                   endif
     C                   endif
      *
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
      *
     C                   eval      sc_st = scst
     C                   eval      srdti = scefdt
     C                   call      'SRCVTDAT'    DTMDYY
     C                   movel(p)  srdto         sc_efdt
     C                   eval      sc_prbs = scprbs
     C                   eval      sc_stat = scstat
      *
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
      *
     C                   eval      scco# = ##co#
     C                   eval      scfnd = sc_fnd
     C                   eval      scst = sc_st
     C                   eval      srdti = sc_efdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         scefdt
     C                   eval      scprbs = sc_prbs
     C                   eval      scstat = sc_stat
      *
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
      *
     C                   if        @@mode = 'A'
     C                   seton                                        40
     C                   eval      sc_fnd = @@fnd
     C                   eval      kkfnd = @@fnd
     C                   else
     C     @@rrn         chain     dbrcdf                             60
     C                   if        not *in60
     C                   eval      sc_fnd = scfnd
     C                   eval      kkfnd = scfnd
     C                   endif
     C                   endif
      *
     C     fnky          chain     pfn01                              60
     C                   if        not *in60
     C                   eval      scfname = fnname
     C                   else
     C                   eval      sc_fnd = 0
     C                   eval      scfname = *blanks
     C                   endif
      *
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
     C                   eval      scuser = q1user
     C                   eval      scpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sctime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         scdt
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      scuser = q1user
     C                   eval      sciusr = q1user
     C                   eval      scpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      sctime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         scdt
     C                   movel(p)  srdto         scidt
     C                   write     dbrcdf
     C                   feod      wdscp
     C                   endsr
