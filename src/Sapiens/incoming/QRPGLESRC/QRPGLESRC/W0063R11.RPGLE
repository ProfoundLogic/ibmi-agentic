     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*********************************************************************
     H/TITLE W0063R11 - Collateral Information
     H*********************************************************************
     H*  Created: 02/25/10    C:33    Project: WC1851                     *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 02/11/11  C:33  Project: WC1851                                   *
     H*                 Re-write.  No comments.                           *
     H*                                                                   *
     H* 05/26/10  C:61  Project: WC1851                                   *
     H*                 Fix. No comments.                                 *
     H*********************************************************************
     Fw0063f11  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwtfip     uf a e             disk    rename(pfi01:dbrcdf)
     Fwtftl     if   e           k disk    rename(pft01:pft00)
     Fwtgtl     if   e           k disk
     Fwmfnl     if   e           k disk    rename(pfn01:pfn02)
     Fwdf1l1    if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D comapi          c                   'WA001R'
     D #ofScr          c                   1
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
     DClaimsOnly       pr             1n
     D inDate                          d   const
     D*
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
     C     fnky1         klist
     C                   kfld                    scCo#
     C                   kfld                    scFnd
     c*
     C* KEY LIST FOR THE GENERIC TABLE FILE
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
    C* ErrCk1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     ErrCk1        begsr
     C*
     c* Allow Override to Emp level(Y/N)
     c                   if        scCaOe <> 'N'
     c                             AND scCaOe <> 'Y'
     c                   seton                                        7990
     c                   leavesr
     c                   endif                                                  (scCaOe <> 'N')
     C*
     c*  Collateral related to claims fee must be Y or N.
     c                   if        scCrCf <> 'N'
     c                             AND scCrCf <> 'Y'
     c                   seton                                        7090
     c                   leavesr
     c                   endif                                                  (scCrCf <> 'N')
     C*
     c* Billing based on
     C                   if        scCoBs = *blanks
     c                   seton                                        7890
     c                   leavesr
     c                   endif                                                  (scclfe = 'Y')
     C*
     C                   if        scCoBs <> *blanks
     C                   movel(P)  'COBB'        xztbid
     C                   movel(P)  scCoBs        xxkey
     C     gtky          chain     pgt01                              60
     c                   if        not %found or GTSTAT <> 'A'
     C                   seton                                        89  90
     c                   leavesr
     C                   endif
     C                   endif
     C*
     C                   if        ClaimsOnly(CvtToDate(scEfDt))
     C                              and (scCoBs = 'E' or scCoBs = 'P')
     c                   seton                                        77  90
     c                   leavesr
     C                   endif
     C*
      * Validation on Collateral Fee ... Percentage/Flat Fee
     C                   if        scCoBs <> *BLANKS
     C                              and (scCoBs = 'C' OR scCoBs = 'E' OR
     C                                   scCoBs = 'I' OR scCoBs = 'P')
     C                   select
     C                   when      scCoPn = *ZEROS
     C                   SETON                                        7190
     c                   leavesr
     C                   when      scCoFf <> *ZEROS
     C                   SETON                                        7490
     c                   leavesr
     C                   endsl
     C                   ENDIF
      *
     C                   if        scCoBs <> *BLANKS
     C                              and scCoBs = 'F'
     C                   select
     C                   when      scCoPn <> *ZEROS
     C                   SETON                                        7290
     c                   leavesr
     C                   when      scCoFf = *ZEROS
     C                   SETON                                        7390
     c                   leavesr
     C                   endsl
     C                   ENDIF
      *
      * IF Collateral flat fee is entered, the Minimum Collateral  amount should be Zeros
     C                   if        scCoFf <> *zeros
     C                              and SCCOMA <> *ZEROS
     C                   SETON                                        7690
     c                   leavesr
     C                   ENDIF

     C                   endsr


    C*================================================================
    C* ErrCk2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     c     ErrCk2        begsr
     C                   endsr

    C*================================================================
    C* ErrCk3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     ErrCk3        begsr
     C                   endsr

     C*===============================================================
     C* KJDO - Subroutine for F10=Search
     C*===============================================================
     C     kjdo          begsr
     C*
     C                   select

     C     HFLD          WHENEQ    'SCCOBS'
     C*
     C                   CALL      'W2493R'
     C                   parm                    QQPRDS           40
     C                   parm      '2'           QQCALL            1
     C                   parm      'COBB'        QQID              4
     C                   parm      *BLANKS       QQKEY            10
     C                   parm                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        88
     C                   MOVEL     QQKEY         scCoBs
     C                   else
     C                   seton                                        87
     C                   endif

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
     c                   eval      scCrCf = FICRCF
     c                   eval      scCoBs = FICOBB
     c                   eval      scCoPn = FICOPN
     c                   eval      scCoFf = FICOFF
     c                   eval      scCoMa = FIMNCA
     c                   eval      scCaOe = FICAOE

     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     c     mvscdb        begsr

     c** Screen 1
     c                   eval      FICRCF = scCrCf
     c                   eval      FICOBB = scCoBs
     c                   eval      FICOPN = scCoPn
     c                   eval      FICOFF = scCoFf
     c                   eval      FIMNCA = scCoMa
     c                   eval      FICAOE = scCaOe

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
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr

     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
E01  C                   endif

     C                   endsr

    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr

     c                   exsr      Updatr

     C                   eval      outFlg = *on

     C                   endsr





    P*================================================================
    P* Get group year as of a date
    P*================================================================
     PClaimsOnly       b
     DClaimsOnly       pi             1n
     D inDate                          d   const
     D*
     Dx7Date           s                   like(F1STPO)

     C     keyWDF1L1     klist
     C                   kfld                    scCo#
     C                   kfld                    scFnd
     C*
     C     key1WDF1L1    klist
     C                   kfld                    scCo#
     C                   kfld                    scFnd
     C                   kfld                    x7Date
     C*
     C                   eval      x7Date = DateTo7(inDate)
     C*
     C                   clear                   F1CLMS
     C     key1WDF1L1    setgt     wdf1l1
     C     keyWDF1L1     readpe    wdf1l1
     C                   return    F1CLMS = 'Y'

     PClaimsOnly       e
