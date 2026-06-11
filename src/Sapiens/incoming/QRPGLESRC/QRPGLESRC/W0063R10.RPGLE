     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/TITLE W0063R10 - Claim Fee Information
     H*********************************************************************
     H*  CREATED: 02/25/10    C:33    Project: WC1851                     *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 02/11/11  C:33  Project: WC1851                                   *
     H*                 Re-write.  No comments.                           *
     H*                                                                   *
     H* 05/26/10  C:61  Project: WC1851                                   *
     H*                 Fix. No comments.                                 *
     H*********************************************************************
     Fw0063f10  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtfip     uf a e             disk    rename(pfi01:dbrcdf)
     Fwtftl     if   e           k disk    rename(pft01:pft00)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D comapi          c                   'WA001R'
     D #ofScr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D @@co#           s              3  0
     D @@fnd           s              3  0
     D @@fnm           s             60
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D optNm           s             40
     D xxdat1          s                   like(fiefdt)
     D xxdat2          s                   like(fiexdt)
     D xztbid          s                   like(gttbid)
     D xxgroup         s                   like(fifnd)
     D xxco#           s                   like(fico#)
     D xxst            s                   like(fist)
     D xxefdt          s                   like(fiefdt)
     D xxKey           s                   like(GTKEY)
     d addMode         s              1n
     d cpyMode         s              1n
     d dspMode         s              1n
     d updMode         s              1n
     d cpyModeMass     s              1n

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D/copy sPrDate
     D/copy sPrString
     D/copy wPrGroup
    C*
     C/copy sCopyMT41c
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
     C     keyWTFTL      klist
     c                   KFLD                    FICO#
     c                   KFLD                    FIFND
     c                   KFLD                    FITYPE
     c                   KFLD                    FIDESC
     c*
     C     gtky          klist
     c                   KFLD                    scCo#
     c                   KFLD                    xztbid
     c                   KFLD                    xxkey

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
    C* erRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr

      *  Allow Override at Emp level- Allow for Y or N.

     c  n90              if        scaoel <> 'N'
     c                             AND scaoel <> 'Y'
     c                   seton                                        5790
     c                   endif                                                  (scaoel <> 'N')

      * Billing fee setup

      * Indemnity Only.. Billing based on should be valid..
      *
     C  n90              if        (SCINBT <> *BLANKS)
     C                   movel(P)  'CFBB'        xztbid
     C                   movel(P)  SCINBT        xxkey
     C     gtky          chain     pgt00                              60
     C                   if        (*in60 = *on) or (gtstat <> 'A')
     C                   seton                                        314590
     C                   endif
     C                   endif

      * Validation on Indeminity Only.. Percentage/Flat Fee

     C  N90              if        scInBt <> *blanks
     C                              and (scInBt = 'C' or scInBt = 'I')
     C                   select
     C                   when      scInPn = *ZEROS
     C                   seton                                        7190
     C                   when      scInfF <> *ZEROS
     C                   seton                                        7490
     C                   endsl
     C                   endif
      *
     C  N90              if        scInBt <> *blanks
     C                              and scInBt = 'F'
     C                   select
     C                   when      scInPn <> *zeros
     C                   seton                                        7290
     C                   when      scInfF = *zeros
     C                   seton                                        7390
     C                   endsl
     C                   endif
      *
      * If Indeminity Only.. billing based on is blank..
      * then Percentage and flat fee should be blank.
      *
     C  N90              if        scInBt = *blanks
     C                   select
     C                   when      scInpN <> *zeros
     C                   seton                                        7290
     C                   when      scInfF <> *zeros
     C                   seton                                        7490
     C                   endsl
     C                   endif
      *
      * Medical Only.. Billing based on should be valid..
     C  n90              if        SCMOBT <> *BLANKS
     C                   movel(P)  'CFBB'        xztbid
     C                   movel(P)  SCMOBT        xxkey
     C     gtky          chain     pgt00                              60
     C                   if        (*in60 = *on) or (gtstat <> 'A')
     C                   seton                                        284490
     C                   endif
     C                   endif
     C*
      * Validation on Medical Only.. Percentage/Flat Fee
      *
     C  N90              if        scMoBt <> *blanks
     C                              and (scMoBt = 'C' or scMoBt = 'I')
     C                   select
     C                   when      scMoPn = *zeros
     C                   seton                                        9790
     C                   when      scMofF <> *zeros
     C                   seton                                        9990
     C                   endsl
     C                   endif
      *
     C  N90              if        scMoBt <> *blanks
     C                              and scMoBt = 'F'
     C                   select
     C                   when      scMoPn <> *zeros
     C                   seton                                        9890
     C                   when      scMofF = *zeros
     C                   seton                                        9190
     C                   endsl
     C                   endif
      *
      * If Medical Only.. billing based on is blank..
      * then Percentage and flat fee should be blank.
      *
     C  N90              if        scMoBt = *blanks
     C                   select
     C                   when      scMoPn <> *zeros
     C                   seton                                        9890
     C                   when      scMofF <> *zeros
     C                   seton                                        9990
     C                   endsl
     C                   endif
      *
      * Record Only.. Billing based on should be valid..
      *
     C  n90              if        (SCROBT <> *BLANKS)
     C                   movel(P)  'CFBB'        xztbid
     C                   movel(P)  SCROBT        xxkey
     C     gtky          chain     pgt00                              60
     C                   if        (*in60 = *on) or (gtstat <> 'A')
     C                   seton                                        254390
     C                   endif
     C                   endif

      * Valid value of Record Only is F.
      *
     C  N90              if        scRobt <> *blanks
     C                              and scRobt <> 'F'
     C                   seton                                        254790
     C                   endif
      *
     C  N90              if        scRobt <> *blanks
     C                              and scRobt = 'F'
     C                              and scRofF = *zeros
     C                   seton                                        9590
     C                   endif
     C*
      * If Record Only.. billing based on is blank..
      * flat fee should be blank.
      *
     C  N90              if        scRobt = *blanks
     C                              and scRofF <> *zeros
     C                   seton                                        9690
     C                   endif

      * All Claims... Billing based on should be valid..
      *
     C  N90              if        (SCALBT <> *BLANKS)
     C                   movel(P)  'CFBB'        xztbid
     C                   movel(P)  SCALBT        xxkey
     C     gtky          chain     pgt00                              60
     C                   if        (*in60 = *on) or (gtstat <> 'A')
     C                   seton                                        344690
     C                   endif
     C                   endif

      * Valid value of All Claims is only F.
      *
     C  N90              if        scAlBt <> *blanks
     C                              and scAlBt <> 'F'
     C                   SETON                                        344890
     C                   ENDIF
     C*
     c* If Y is entered denoting claims fees, at least one claim setup entry must exist.
     c  n90              if        (scrobt = *blanks and
     c                              SCROFF = *zeros   and
     c                              SCMOBT = *blanks  and
     c                              SCMOPN = *zeros   and
     c                              SCMOFF = *zeros   and
     c                              SCINBT = *blanks  and
     c                              SCINPN = *zeros   and
     c                              SCINFF = *zeros   and
     c                              SCALBT = *blanks  and
     c                              SCALFF = *zeros )
     C                   seton                                          6790
     c                   endif

     C*  If All Claims is selected, entry for Record Only Medical Only and Indemnity will
     C*  not be allowed.
     C*
     C  n90              if        (scalbt <> *blanks)
     c                             and (scrobt <> *blanks or
     c                                 SCROFF <> *zeros   or
     c                                 SCMOBT <> *blanks  or
     c                                 SCMOPN <> *zeros   or
     c                                 SCMOFF <> *zeros   or
     c                                 SCINBT <> *blanks  or
     c                                 SCINPN <> *zeros   or
     c                                 SCINFF <> *zeros )
     C                   seton                                          6690
     C                   endif

      * Validation on All Claims Only.. Percentage/Flat Fee
      *
     C  N90              if        scAlBt <> *blanks
     C                              and (scAlBt = 'C' or scAlBt = 'I')
     C                              and scAlfF <> *zeros
     C                   seton                                        7890
     C                   endif
      *
      * Validation on All Claims Only.. Percentage/Flat Fee
      *
     C  N90              if        scAlBt <> *blanks
     C                              and scAlBt = 'F'
     C                              and scAlfF = *zeros
     C                   seton                                        7790
     C                   endif
      *
      * If All Claims ... billing based on is blank..
      * then  flat fee should be blank.
      *
     C  N90              if        scAlBt = *blanks
     C                              and scAlfF <> *zeros
     C                   seton                                        7890
     C                   endif

     C                   endsr

    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     c     errck2        begsr
     C                   endsr

    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr

     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     kjdo          begsr
     C                   movea     '00000000'    *IN(25)
     C                   movea     '00000000'    *IN(33)
     C*
     C                   select
     C*
     C                   when      hfld = 'SCROBT'
     C                              or hfld = 'SCMOBT'
     C                              or hfld = 'SCINBT'
     C                              or hfld = 'SCALBT'
     C*
     C                   call      'W2493R'
     C                   parm                    ##PRDS           40
     C                   parm      '2'           ##CALL            1
     C                   parm      'CFBB'        ##ID              4
     C                   parm      *BLANKS       ##KEY            10
     C                   parm                    ##RTCD            1
     C*
     C                   select
     C                   when      ##Key <> *blanks
     C                   select
     C                   when      hfld = 'SCROBT'
     C                   seton                                        27
     C                   eval      scRoBt = ##Key
     C                   when      hfld = 'SCMOBT'
     C                   seton                                        29
     C                   eval      scMoBt = ##Key
     C                   when      hfld = 'SCINBT'
     C                   seton                                        32
     C                   eval      scInBt = ##Key
     C                   when      hfld = 'SCALBT'
     C                   seton                                        36
     C                   eval      scAlBt = ##Key
     C                   endsl
     C                   other
     C                   select
     C                   when      hfld = 'SCROBT'
     C                   seton                                        25
     C                   when      hfld = 'SCMOBT'
     C                   seton                                        28
     C                   when      hfld = 'SCINBT'
     C                   seton                                        31
     C                   when      hfld = 'SCALBT'
     C                   seton                                        34
     C                   endsl
     C                   endsl

     C                   endsl

     C                   endsr

    C*================================================================
    C* Load Maintenance Fields
    C*================================================================
     C     MaintFlds     begsr

     c                   eval      FIDT   = DateTo7(SystemDate)
     c                   eval      FITIME = TimeTo6(SystemTime)                 Maintaince time
     c                   eval      FIUSER = q1user                              Maintaince user
     c                   eval      FIPGM  = q1pgm                               Maintaince Pgm

     C                   endsr

    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     c                   eval      scCo#  = FICO#
     c                   eval      scFnd  = FIFND
     c                   eval      scFNam = GetGrpName(FIFND)
     c                   eval      scSt   = FIST
     c                   eval      scEfDt = DtMDYY(FIEFDT)
     c                   eval      scExDt = DtMDYY(FIEXDT)

     c* Screen 1
     c                   movel     fiaoel        scaoel                         Allow override to

     c* Record Only Claims
     c                   movel     firobt        scrobt                         Record(Bill based)
     c                   movel     firoff        scroff                         Record(Flat fee)

     c* Medical Only Claims
     c                   movel     fimobt        scmobt                         Medical(Bill based)
     c                   movel     fimopn        scmopn                         Medical(%)
     c                   movel     fimoff        scmoff                         Medical(Flat fee)

     c* Indemnity Claims
     c                   movel     fiinbt        scinbt                         Indemn(Bill based)
     c                   movel     fiinpn        scinpn                         Indemn(%)
     c                   movel     fiinff        scinff                         Indemn(Flat fee)

     c* All claims
     c                   movel     fialbt        scalbt                         All claims based)
     c                   movel     fialff        scalff                         All claims(Flat fee)

     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     c     mvscdb        begsr

     c** Screen 1
     c                   movel     scaoel        fiaoel                         Allow override to

     c* Record Only Claims
     c                   movel     scrobt        firobt                         Record(Bill based)
     c                   movel     scroff        firoff                         Record(Flat fee)

     c* Medical Only Claims
     c                   movel     scmobt        fimobt                         Medical(Bill based)
     c                   movel     scmopn        fimopn                         Medical(%)
     c                   movel     scmoff        fimoff                         Medical(Flat fee)

     c* Indemnity Claims
     c                   movel     scinbt        fiinbt                         Indemn(Bill based)
     c                   movel     scinpn        fiinpn                         Indemn(%)
     c                   movel     scinff        fiinff                         Indemn(Flat fee)

     c* All claims
     c                   movel     scalbt        fialbt                         All claims based)
     c                   movel     scalff        fialff                         All claims(Flat fee)

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

     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS

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

     C                   eval      addMode = @@Mode = 'A'
     C                   eval      cpyMode = @@Mode = 'P'
     C                   eval      dspMode = @@Mode = 'D'
     C                   eval      updMode = @@Mode = 'C'
     C                   eval      cpyModeMass = @@Mode = 'M'

     C                   eval      *in40 = @@Mode = 'A'

     C                   select
     C                   when      addMode
     C                   eval      cpyFlg = *on
     C                   endsl
     C*
     C     @@rrn         chain     wtfip
     C     keyWTFTL      chain     wtftl
     C                   eval      optNm = %trim(FIDESC) + ' State Detail'
     C                   exsr      GetPgT

     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr

    C*================================================================
    C* UpdatR - Update data base record and after processing
    C*================================================================
     C     UpdatR        begsr

     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
E01  C                   endif

     C                   endsr

    C*================================================================
    C* WriteR - Write data base record and after processing
    C*================================================================
     C     WriteR        begsr

     c                   exsr      UpdatR

     C                   eval      outFlg = *on

     C                   endsr
