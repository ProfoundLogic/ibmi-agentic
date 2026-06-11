     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H/title Medicare Query File Program
     H*====================================================================
     H*  Created: 08/14/09   C:232  Project: WC1819 Phase 5.
     H*====================================================================
    H*  Revisions:
     H*
     H*  03/06/25 C:333  Project WC2998
     H*                  Changes to CMS Settlement Reporting.
     H*                  - Recompiled.
     H*                                                                                    *
     H*  01/19/11   C:53   Project: WC1963 Support 75560                                   *
     H*                    Use HIC# in the WMCDP per WC1963 changes                        *
     H*
    H*  12/13/10   C:198  Project: WC1963
     H*                    Made changes in Medicare query submission file process.
     H*
    H*  10/05/10   C:251  Project: WC1944
     H*                    Add *Entry parms and processing for query selection screen
     H*
     H*  05/18/10   C:198  Project: WC1879
     H*                    Added processing for DCN1 and DCN2.
     H*                    Changed loading of QTCNT so that it has leading zeros.
     H*                    Corrected record count to be increased only when record is
     H*                    present in wmcmp and WMCDP.
     H*
     H*  04/16/10   C:198  Project: WC1819 Phase 5 Priss 73
     H*                    Removed restriction of picking on open Lost time/
     H*                    Medical claims.
     H*
     H*====================================================================

    F*  Physical file definition (for maintaining record)
     FWXGSL     if   e           k disk
     F*** Beg Chg *** 04/16/10 ********************************************
     F*******WMCMLH    if   e           k disk
     FWMCML1    if   e           k disk
     F*** End Chg *** 04/16/10 ********************************************
     FWMCDL     if   e           k disk
     FWDF2L     if   e           k disk
     FWDD1L     uf a e           k disk
     F*** Beg Add *** 10/05/10 **************************************************
     FWMCHL     if   e           k disk
     F*** End Add *** 10/05/10 **************************************************
     FWXMQP     o    e             disk
     F*** Beg Add *** 12/13/10 **************************************************
     FWMCXL     if   e           k disk
     F*** End Add *** 12/13/10 **************************************************

    D*================================================================
    D*  External Data Structure
    D*================================================================
     DPSDS           ESDS                  extname(SZQ1P)
     DNGQH_DS        e ds                  extname(MED2NGQH)
     DNGQD_DS        e ds                  extname(MED2NGQD)
     DNGQT_DS        e ds                  extname(MED2NGQT)

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     D                 DS
     D  DSDATE1                1      8  0
     D  @MONTH                 1      2  0
     D  @DAY                   3      4  0
     D  @YEAR                  5      8  0

     D                 DS
     D  DSDATE2                1      8  0
     D  @YEAR2                 1      4  0
     D  @MONTH2                5      6  0
     D  @DAY2                  7      8  0
     D*** Beg Add *** 05/18/10 **********************************
     D                 DS
     D  XXDCN1                 1     13
     D  X1FND                  1      3  0
     D  X1FYR                  4      6  0
     D  X1CASE                 7     13  0
     D*** End Add *** 05/18/10 **********************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D @@rcdcnt        s              9s 0 inz
     D CurrDate        S               d   DatFmt(*Iso)
     D rcd_chg         S               n
     D comapi          c                   const('WA001R')
     D X1DATE          S                   LIKE(D1EXDT)
     Dxxmode           S              1a   inz(' ')
     D svRRE#          S                   LIKE(F2RRE#)
     D*** Beg Add *** 05/18/10 **********************************
     DXXCNT            S              9A   inz(' ')
     D*** End Add *** 05/18/10 **********************************
     D*** Beg Add *** 10/05/10 ***********************************************
     D GtNxt           s              1
     D XXefdt          s              7  0
     D XXstat          s                   LIKE(CMSTAT)
     D*** End Add *** 10/05/10 ***********************************************
     D*** Beg Add *** 12/13/10 ***********************************************
     D xxmbs           s                   LIKE(CXMBQF)
     D*** End Add *** 12/13/10 ***********************************************

    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C***  *ENTRY        PLIST
     C***                PARM                    CO#               3
     C***                PARM                    QUE              10
     C*** Beg Add *** 10/05/10 ******************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@STAT            1
     C                   PARM                    @@EFDT            7
     C                   PARM                    @@LTFL            1
     C*
     C                   MOVE      @@EFDT        XXefdt
     C*** End Add *** 10/05/10 ******************************************
     C
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40

     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1

     C     datchk1       plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1

    C*================================================================
    C*  KLIST definitions
    C*================================================================

      *  KEY LIST FOR WDF2L
     C     f2ky          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    gsFND

      *  KEY LIST FOR WXGSL
     C     gsky          KLIST
     C                   KFLD                    ##CO#


      *  KEY LIST FOR WMCMLH
     C     Cmlhky        KLIST
     C                   KFLD                    GSCO#
     C                   KFLD                    GSFND

      *  KEY LIST FOR WMCDL
     C     Cdlky         KLIST
     C                   KFLD                    CMSS#

      *  KEY LIST FOR WDD1L
     C     D1lky         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    X1date
     C*** Beg Add *** 10/05/10 ******************************************
     **
     **  KEY LIST FOR Claim history master file
     **
     C     CHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*** End Add *** 10/05/10 ******************************************
     C*** Beg Add *** 12/13/10 ******************************************
     C     cxky          klist
     C                   kfld                    f2co#
     C                   kfld                    f2fnd
     C*** Beg Add *** 12/13/10 ******************************************

    C*================================================================
    C*                  M A I N L I N E
    C*================================================================
     C                   Eval      CurrDate = %date(UDATE)
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
     C                   EVAL      DSDATE1 =     DSDATE
     C                   EVAL      @MONTH2 = @MONTH
     C                   EVAL      @DAY2   = @DAY
     C                   EVAL      @YEAR2  = @YEAR
      *
      * get company number via company api
     C                   call      comapi        compl
     C                   clear                   @@rcdcnt
      * Read WDF2L for F2RRE#
     C     gsky          setll     pgs01
     C     gsky          reade     PGS01                                  90
     C                   Dow       *in90 = *off
     C                   if        gsiefl = 'I'
     C     f2ky          chain     pf201

     C*** Beg Add ** 12/13/10 ************************************
     C     cxky          chain     pcx01                              90
     C                   if        *in90 = *off
     C                   movel     cxmbqf        xxmbs
     C                   endif
     C*** End Add ** 12/13/10 ************************************
      * write header record to WXMQP flat file
     C                   clear                   PMQ01
     C                   eval      QHHI = 'HO'
     C                   eval      QHRRE# = F2RRE#
     C                   eval      svrre# = F2rre#
     C                   eval      QHTYPE = 'NGHQ'
     C                   move      DSDATE2       QHDATE
     C                   eval      QHFILL = *blanks
     C                   eval      MZDATA = NGQH_DS
     C                   write     PMQ01
     C                   leave
     C                   endif
     C     gsky          reade     PGS01                                  90
     C                   enddo
      * read through Fund work file WXGSL
     C     gsky          setll     pgs01
     C     gsky          reade     PGS01                                  90
     C                   Dow       *in90 = *off
     C*
     C                   if        gsiefl = 'I'
     C*
      * Load the key list for the claim file (WMCMLH) with the
      * company number and group to get all open claims
     C     CmlhKy        setll     PCM01
     C     Cmlhky        reade     PCM01                                  91
     C                   Dow       *in91 = *off
     C*
     C*** Beg Del *** 10/05/10 ****************************************
     C********           If        CMLTFL = 'I' or
     C********                     CMLTFL = 'M'
     C*** End Del *** 10/05/10 ****************************************
     C*
     C*** Beg Add *** 10/05/10 ****************************************
     C*
     C* Add Logic for selection as per second screen- Query selection
     C*
     C                   eval      Gtnxt = *blanks
     C                   eval      XXstat = *blanks
     C*
     C                   if        @@STAT = ' '
     C                   eval      XXstat = CMSTAT
     C                   else
     C                   eval      XXstat = @@STAT
     C                   endif
     C*
     C*** Check for claim status on screen with status in database
     C*
     C                   if        XXSTAT = CMSTAT
     C                   select
     C*
     C                   when      CMSTAT = 'O' or
     C                             CMSTAT = 'C' and XXEFDT = *zeros
     C                   exsr      SRsel2
     C*
     C                   when      CMSTAT ='C'  and XXEFDT <> *zeros
     C*
     C     chky          setgt     pch01
     C     chky          readpe    pch01                                  90
     C*
     C                   if        *in90 = *off
     C*
     C                   dow       *in90 = *off and CHOCST <> 'C'
     C     chky          readpe    pch01                                  90
     C                   enddo
     C*
     C                   if        (CHEFDT > XXEFDT) and CHOCST ='C'
     C                   exsr      srsel2
     C                   else
     C                   eval      GtNxt = 'Y'
     C                   endif
     C*
     C** If no record is present in file - *in90 = *on in first run
     C                   else
     c                   eval      GtNxt = 'Y'
     C                   endif
     C*
     C                   endsl
     C*
     C                   else                                                   XXSTAT <> CMSTAT
     C*
     C                   eval      GtNxt = 'Y'
     C                   endif                                                  XXSTAT <> CMSTAT
     C*
     C*** Beg Add *** 12/13/10 ****************************************
     C     CDLky         chain     PCD01
     C                   If        %found(WMCDL)
     C                   If        CDMDBN = 'Y' and xxmbs <> 'Y'
     C                   eval      GtNxt = 'Y'
     C                   EndIf                                                  cdmdbn = 'Y'
     C                   Else
     C                   eval      GtNxt = 'Y'
     C                   EndIf                                                  record in WMCDP
     C*** End Add *** 12/13/10 ****************************************
     C* If claim does not meet the selection criteria, get the next claim record
     C*
     C                   if        GtNxt = 'Y'
     C     Cmlhky        reade     PCM01                                  91
     C                   Iter
     C                   else
     C*
     C*** End Add *** 10/05/10 ****************************************
     C*
      *  Update the total claim count.
     C*** Beg Del *** 05/18/10 ****************************************
     C*******            eval      @@rcdcnt = @@rcdcnt + 1
     C*** End Del *** 05/18/10 ****************************************
     C*** Beg Del *** 12/13/10 ****************************************
     C*** I added chain to WMCDP above and made gtnxt = 'Y' if record does not exist
     C*** in WMCDP so that record count is not incremented when claimant is not in WMCDP
      *  Chain to demographics file (WMCDL)
     C**** CDLky         chain     PCD01
     C****               If        %found(WMCDL)
     C*** End Del *** 12/13/10 ****************************************
     C
     C*** Beg Add *** 05/18/10 ****************************************
      *  Update the total claim count.
     C                   eval      @@rcdcnt = @@rcdcnt + 1
     C*** End Add *** 05/18/10 ****************************************
      *  Check to see if record is log file (WDD1L)
     C     D1lky         chain     PD101
     C                   If        %found(WDD1L)
     C                             and d1type = 'Q'
     C                   eval      xxmode = 'U'
     C                   eval      rcd_chg = *off
     C                   Select
     C*** Beg Chg *** 01/19/11 ****************************************
     C**********         When      D1HIC# <> CMHIC#
     C                   When      D1HIC# <> CDHICN
     C*** End Chg *** 01/19/11 ****************************************
     C                   eval      rcd_chg = *on

     C                   When      D1LSN <> CDLSN
     C                   eval      rcd_chg = *on

     C                   When      D1FSN <> %subst(CDFSN:1:1)
     C                   eval      rcd_chg = *on

     C                   When      D1DTOB  <> CDDTOB
     C                   eval      rcd_chg = *on

     C                   When      D1SS# <> CDSS#
     C                   eval      rcd_chg = *on

     C                   other
     C                   select
     C                   when      cdsex = 'M'
     C                   if        d1sex <> '1'
     C                   eval      rcd_chg = *on
     C                   endif
     C                   when      cdsex = 'F'
     C                   if        d1sex <> '2'
     C                   eval      rcd_chg = *on
     C                   endif
     C                   other
     C                   if        d1sex <> '0'
     C                   eval      rcd_chg = *on
     C                   endif
     C                   endsl

     C                   Endsl

     C                   If        rcd_chg
     C                   exsr      updwrtlogf
     C                   Endif
     C                   exsr      wrtdtl

     C                   else
      * If record not found...
      * Write to medicare extraxt log file WDD1P
     C                   eval      xxmode = 'A'
     C                   exsr      updwrtlogf
     C                   exsr      wrtdtl
     C                   Endif                                                  rcd present in WDD1P
     C
     C*** Beg Del *** 12/13/10 *******************************************
     C********           Endif                                                  rcd present in WMCDP
     C*** End Del *** 12/13/10 *******************************************
     C
     C                   Endif                                                  CMLTFL = 'I'/'M'
     C     Cmlhky        reade     PCM01                                  91
     C                   Enddo

     C                   endif                                                  gsiefl = 'I'

     C     gsky          reade     PGS01                                  90
     C                   Enddo

      * Read WDF2L for F2RRE#
     C     gsky          setll     pgs01
     C     gsky          reade     PGS01                                  90
     C                   Dow       *in90 = *off
     C                   if        gsiefl = 'I'

      * Write the trailer record to WXMQP flat file
     C                   clear                   PMQ01
     C                   eval      QTTI = 'TO'
     C                   eval      QTRRE# = svRRE#
     C                   eval      QTTYPE = 'NGHQ'
     C                   move      DSDATE2       QTDATE
     C*** Beg Chg *** 05/18/10 ****************************************
     C*******            eval      QTCNT = @@rcdcnt
     C                   eval      XXCNT = %editc(@@rcdcnt:'X')
     C                   move      XXCNT         QTCNT
     C*** End Chg *** 05/18/10 ****************************************
     C                   eval      QTFILL = *blanks
     C                   eval      MZDATA = NGQT_DS
     C                   write     PMQ01

     C                   leave
     C                   endif
     C     gsky          reade     PGS01                                  90
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C                   return
     *===============================================================*
     C     updwrtlogf    begsr

     C                   if        xxmode = 'A'
     C                   clear                   PD101
     C                   endif
     C*
     C                   movel     x1date        D1EXDT
     C*** Beg Chg *** 01/19/11 ****************************************
     C**********         movel     CMHIC#        D1HIC#
     C                   movel     CDHICn        D1HIC#
     C*** End Chg *** 01/19/11 ****************************************
     C                   movel     CDSS#         D1SS#
     C                   movel     CDLSN         D1LSN
     C                   eval      D1FSN = %subst(CDFSN:1:1)
     c                   select
      *
     C                   when      CDSEX = 'M'
     C                   move      '1'           D1SEX
     C                   when      CDSEX = 'F'
     C                   move      '2'           D1SEX
     C                   other
     C                   move      '0'           D1SEX
     C                   endsl
      *
     C                   move      CDDTOB        D1DTOB
      *
     C                   MOVE      Q1PGM         D1PGM
     C                   MOVE      Q1USER        D1USER
     C                   MOVE      X1DATE        D1DT
     C                   MOVE      DSTIME        D1TIME
      *
     C                   if        xxmode = 'A'
     C                   MOVE      'Q'           D1TYPE
     C                   move      CMCO#         D1CO#
     C                   move      CMFND         D1FND
     C                   move      CMFYR         D1FYR
     C                   move      CMCASE        D1CASE
     C                   MOVE      Q1PGM         D1IPGM
     C                   MOVE      Q1USER        D1IUSR
     C                   MOVE      X1DATE        D1IDT
     C                   MOVE      DSTIME        D1ITIM
     C*** Beg Add *** 05/18/10 **********************************
     C                   move      CMFND         X1FND
     C                   move      CMFYR         X1FYR
     C                   move      CMCASE        X1CASE
     C                   Movel     XXDCN1        D1DCN1
     C*** End Add *** 05/18/10 **********************************
      *
     C                   write     PD101
     C                   else                                                   xxmode = 'A'
     C                   update    PD101
     C                   endif                                                  xxmode = 'A'
      *
     C                   endsr
     *===============================================================*
     C     WrtDtl        Begsr
      * Write the Query Input Detail record to WXMQP flat file
     C*** Beg Chg *** 01/19/11 ****************************************
     C**********         eval      QDHI   =  CMHIC#
     C                   eval      QDHI   =  CDHICN
     C*** End Chg *** 01/19/11 ****************************************
     C                   movel     CDLSN         QDLSN
     C                   eval      QDFSN  = %subst(CDFSN:1:1)
     C                   z-add     cdDTOB        srdti
     C                   CALL      'SRCVTDAT'    DATCHK1
     C                   MOVEL     SRDTO         QDDTOB
     C*** Beg Add *** 05/18/10 **********************************
     C                   move      CMFND         X1FND
     C                   move      CMFYR         X1FYR
     C                   move      CMCASE        X1CASE
     C                   Movel     XXDCN1        QDDCN1
     C*** End Add *** 05/18/10 **********************************
      *
     C                   select
     C                   when      CDSEX = 'M'
     C                   move      '1'           D1SEX
     C                   when      CDSEX = 'F'
     C                   move      '2'           D1SEX
     C                   other
     C                   move      '0'           D1SEX
     C                   endsl
     C                   move      D1SEX         QDSEX
      *
     C                   eval      QDSS#  = CDSS#
     C                   eval      QDFILL = *blanks
     C                   eval      MZDATA = NGQD_DS
     C                   write     PMQ01
     C                   endsr                                                  wrtdtl subroutine
     C*** Beg Add *** 10/05/10 ******************************************************************
     C*******************************************************************************************
     C*
     C*  Selection process for new query screen
     C*
     C*******************************************************************************************
     C     Srsel2        begsr
     C*
     C* Selection process as per record lost time flag
     C*
     C                   if        CMLTFL =  'R'
     C                   eval      GtNxt = 'Y'
     C                   else
     C                   if        @@LTFL <> *blanks and @@LTFL <> CMLTFL
     C                   eval      GtNxt = 'Y'
     C                   endif
     C                   endif
     C*
     C                   endsr
     C*** End Add *** 10/05/10 *******************************************************************
