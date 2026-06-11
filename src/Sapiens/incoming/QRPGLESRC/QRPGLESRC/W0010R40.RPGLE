     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE - GROUP CARRIER INFORMATION
    H*********************************************************************
    H*  DESCRIPTION: This option will be used to enter carrier specific  *
    H*               information as well as control additional carrier   *
    H*               number processing.                                  *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    W0010R                                                         *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    NONE                                                           *
    H*                                                                   *
    H*  CREATED: 09/28/01      C:90      PROJECT: WC1054                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
     H* 03/22/17  C:33  Project: 148WAB                                   *
     H*                 Do not display (M) for CLT148                     *
    H*                                                                   *
     H* 04/19/13  C:180 SUPPORT WA146 82141                               *
     H*                 Re-compiled only                                  *
     H*                                                                   *
    H* 03/26/02  C:90  Project: WC1054                                   *
    H*                 Change "MOVEL" statements in the ERRCK subroutine *
    H*                 to "EVAL" statements so that the field is padded  *
    H*                 with blanks.                                      *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0010f40  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmgcp     uf a e             disk    rename(pgc01:dbrcdf)
     Fwmgcl     if a e           k disk
     F                                     infds(dbfds)
     Fwmfnp     if   e             disk

     Fwtgtl     if   e           k disk
     Fwtabl     if   e           k disk    include(ptt01)
     Fwtavl     if   e           k disk    rename(pav01:pav00)
    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'Group Carrier Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   2
    D*---------------------------------------------------------------------
    D* Data structures
    D*---------------------------------------------------------------------
     Ddbfds            ds
     D dbrrn                 397    400b 0
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
     D @@rrn2          s              9  0
     D c#f             s              1    dim(18)
     D***** xxkey           s                   like(TTKEY)
     D***** FN_rrn          s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     D/copy SPRDATE
     D/copy SPRRUNCMD
     D/copy SPRSTRING

     D/copy WPRGROUP
     D/copy WPREMPLR

     C/copy SCOPYMT41C

    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
    C*
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn2
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
    C*
     C*****table_key     klist
     C*****              kfld                    ##co#
     C*****              kfld                    xxkey
    C*
     C     gcky          klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
    C*
     C     gtky          klist
     C                   kfld                    ##co#
     C                   kfld                    qqid              4
     C                   kfld                    qqkey            10
    C*
    C*====================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*====================================================================
    C*
     C     dltpre        begsr
     C                   endsr
    C*
    C*====================================================================
    C* DSPLYR - Processing after displaying information
    C*====================================================================
    C*
     C     dsplyr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     errck1        begsr
    C*
    C* Default carrier code is required
    C*
     C     sccrcd        ifeq      *blanks
     C                   seton                                        30  90
     C                   endif
    C*
    C* Validate default carrier code
    C*
      **** begin chg **** 03/26/02 *****************************************
     C****************** movel     'CRR '        qqid
     C****************** movel     sccrcd        qqkey
     C                   eval      qqid = 'CRR'
     C                   eval      qqkey = sccrcd
      **** end   chg **** 03/26/02 *****************************************
     C     gtky          chain     pgt01                              60
     C  n90*in60         ifeq      *on
     C                   seton                                        31  90
     C                   endif
    C*
    C* Validate policy symbol
    C*
     C  n90scpsym        ifne      *blanks
      **** begin chg **** 03/26/02 *****************************************
     C****************** move      'PSYM'        qqid
     C****************** movel     scpsym        qqkey
     C                   eval      qqid = 'PSYM'
     C                   eval      qqkey = scpsym
      **** end   chg **** 03/26/02 *****************************************
     C     gtky          chain     pgt01                              60
     C     *in60         ifeq      *on
     C                   seton                                        32  90
     C                   endif
     C                   endif
    C*
    C* Validate company code
    C*
     C  n90sccocd        ifne      *blanks
      **** begin chg **** 03/26/02 *****************************************
     C****************** move      'CCOD'        qqid
     C****************** movel     sccocd        qqkey
     C                   eval      qqid = 'CCOD'
     C                   eval      qqkey = sccocd
      **** end   chg **** 03/26/02 *****************************************
     C     gtky          chain     pgt01                              60
     C     *in60         ifeq      *on
     C                   seton                                        33  90
     C                   endif
     C                   endif
    C*
    C* Carrier number required must be Y or N
    C*
     C  n90sccreq        ifne      'Y'
     C     sccreq        andne     'N'
     C                   seton                                        34  90
     C                   endif
    C*
    C* Assign carrier # be Y or N
    C*
     C  n90scasnc        ifne      'Y'
     C     scasnc        andne     'N'
     C                   seton                                        35  90
     C                   endif
    C*
    C* Update carrier # be Y or N
    C*
     C  n90scupdc        ifne      'Y'
     C     scupdc        andne     'N'
     C                   seton                                        36  90
     C                   endif
    C*
    C* Edit for duplicate carrier # be Y or N
    C*
     C  n90scedtc        ifne      'Y'
     C     scedtc        andne     'N'
     C                   seton                                        37  90
     C                   endif
    C*
    C* Edit carrier number format
    C*
     C  n90              movea     sccfmt        c#f
    C*
     C
     C     '@'           lookup    c#f                                    71
      *
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
    C*
     C     errck2        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
    C*
     C     errck3        begsr
     C                   endsr
    C*
    C*====================================================================
    C* KJDO - Subroutine for F10
    C*====================================================================
    C*
     C     kjdo          begsr
    C*
     C                   select
    C*
    C* Default carrier code search
    C*
     C     hfld          wheneq    'SCCRCD'
      *
     C                   move      *blanks       qqrtc
     C                   move      *blanks       qqkey
      *
     C                   call      'W2493R'
     C                   parm                    qqprds           40
     C                   parm                    qqcall            1
     C                   parm      'CRR'         qqid              4
     C                   parm                    qqkey            10
     C                   parm                    qqrtc             1
      *
     C     qqrtc         ifeq      'A'
     C                   exsr      pgmend
     C                   endif
      *
     C     qqrtc         ifeq      'B'
     C     qqrtc         oreq      ' '
     C                   seton                                        51
     C                   exsr      pgmstr
     C                   endif
      *
     C     qqrtc         ifeq      '*'
     C                   movel     qqkey         sccrcd
     C                   seton                                        52
     C                   endif
    C*
    C* Policy symbol search
    C*
     C     hfld          wheneq    'SCPSYM'
      *
     C                   move      *blanks       qqrtc
     C                   move      *blanks       qqkey
      *
     C                   call      'W2493R'
     C                   parm                    qqprds           40
     C                   parm                    qqcall            1
     C                   parm      'PSYM'        qqid              4
     C                   parm                    qqkey            10
     C                   parm                    qqrtc             1
      *
     C     qqrtc         ifeq      'A'
     C                   exsr      pgmend
     C                   endif
      *
     C     qqrtc         ifeq      'B'
     C     qqrtc         oreq      ' '
     C                   seton                                        52
     C                   exsr      pgmstr
     C                   endif
      *
     C     qqrtc         ifeq      '*'
     C                   movel     qqkey         scpsym
     C                   seton                                        53
     C                   endif
    C*
    C* Company code search
    C*
     C     hfld          wheneq    'SCCOCD'
      *
     C                   move      *blanks       qqrtc
     C                   move      *blanks       qqkey
      *
     C                   call      'W2493R'
     C                   parm                    qqprds           40
     C                   parm                    qqcall            1
     C                   parm      'CCOD'        qqid              4
     C                   parm                    qqkey            10
     C                   parm                    qqrtc             1
      *
     C     qqrtc         ifeq      'A'
     C                   exsr      pgmend
     C                   endif
      *
     C     qqrtc         ifeq      'B'
     C     qqrtc         oreq      ' '
     C                   seton                                        53
     C                   exsr      pgmstr
     C                   endif
      *
     C     qqrtc         ifeq      '*'
     C                   movel     qqkey         sccocd
     C                   seton                                        54
     C                   endif
      *
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   eval      sccrcd     = GCDCAR
     C                   eval      scpsym     = GCPSYM
     C                   eval      sccocd     = GCCCOD
     C                   eval      scpgid     = GCPGID
     C                   eval      sccreq     = GCCRFL
     C                   eval      scasnc     = GCACFL
     C                   eval      scupdc     = GCUCFL
     C                   eval      scedtc     = GCDCFL
     C                   eval      sccfmt     = GCCFMT
     C                   eval      sccfrm     = GCFRCN
     C                   eval      sccrto     = GCTOCN
     C                   eval      scnxtc     = GCNACN
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*====================================================================
    C*
     C     mvscdb        begsr
    C*
     C                   eval      GCCO#      = fnco#
     C                   eval      GCFND      = fnfnd
     C                   eval      GCDCAR     = sccrcd
     C                   eval      GCPSYM     = scpsym
     C                   eval      GCCCOD     = sccocd
     C                   eval      GCPGID     = scpgid
     C                   eval      GCCRFL     = sccreq
     C                   eval      GCACFL     = scasnc
     C                   eval      GCUCFL     = scupdc
     C                   eval      GCDCFL     = scedtc
     C                   eval      GCCFMT     = sccfmt
     C                   eval      GCFRCN     = sccfrm
     C                   eval      GCTOCN     = sccrto
     C                   eval      GCNACN     = scnxtc
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHERR - Processing for other modes
    C*====================================================================
    C*
     C     otherr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     othfks        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     pgmend        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
    C*
     C     pgmstr        begsr
     C*
     C***  Begin Add  ***  03/22/17  **************************************
     C                   eval      visM = *on
     C*
     C                   call      'RTVCLTID'
     C                   parm                    cltID             3
     C*
     C                   select
     c                   when      cltID = '148'
     C                   eval      visM = *off
     C                   endsl
     C***  End   Add  ***  03/22/17  **************************************
     C*
     C     @@rrn2        chain     wmfnp                              60
     C  n60              eval      scfnd    = fnfnd
     C  n60              movel     fnname        scfnam
    C*
     C     gcky          chain     pgc01                              61
     C  n61              z-add     dbrrn         @@rrn
     C   61              z-add     0             @@rrn
     C  n61              eval      @@mode  = 'C'
     C   61              eval      @@mode  = 'A'
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PREOTH - Pre-processing for other modes
    C*====================================================================
    C*
     C     preoth        begsr
     C                   endsr
    C*
    C*====================================================================
    C* UPDATR - Update data base record and after processing
    C*====================================================================
    C*
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
B01  C                   if        *in99 = *off
    C*
     C                   exsr      mvscdb
    C*
     C                   eval      GCUSER = q1user
     C                   eval      GCPGM  = q1pgm
     C                   time                    dsdttm
     C                   eval      GCTIME = dstime
     C                   eval      GCDT   = dtCYMD(dsdate)
    C*
     C                   update    dbrcdf
    C*
E01  C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WRITER - Write data base record and after processing
    C*====================================================================
    C*
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
    C*
     C                   exsr      mvscdb
    C*
     C                   eval      GCUSER = q1user
     C                   eval      GCIUSR = q1user
     C                   eval      GCPGM  = q1pgm
     C                   eval      GCIPGM = q1pgm
     C                   time                    dsdttm
     C                   eval      GCTIME = dstime
     C                   eval      GCDT   = dtCYMD(dsdate)
     C                   eval      GCIDT  = dtCYMD(dsdate)
    C*
     C                   write     dbrcdf
    C*
     C                   feod      wmgcp
    C*
     C                   endsr
    C*


