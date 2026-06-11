    Hcopyright('(C) Copyright Fiserv, Inc., 2006 - 2008')
      *=========================================================================
      /title W4921R - California Quarterly CIRB Report
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *=========================================================================
      *  CALLED FROM: W4921C
      *
      *  CREATED 07/10/06   C:136   PROJECT: 132WGK/136WCH
      *=========================================================================
      * REVISIONS:
      * 01/22/13  C:287 SEQ#: SY266_OVR
      *                 Added module source to change compiler options of object.
      *
      * 04/01/08 C:183 PROJECT: WC1619
      *                Call SREFFD to set the effective date to be
      *                used on SETGT.
      *
      * 10/31/07 C:136 Support: WA136 60447
      *                Correct split pay flag logic for exp mod.
      *                Prevent carry over if premium history record doesn't exist.
      *
      * 10/15/07 C:136 Support: WA132 60560
      *                SETLL on PEU01 missing. Replaced needed line of code.
      *
      * 11/29/06 C:136 Support: WA136 55570
      *                Change pure premium calculation to not use a blended exp mod if the
      *                group policy year is flagged to use payrolls split on the exp mod.
      *
      * 10/27/06 C:136 Support: WA136 55570
      *                Correct the GetPureRate subprocedure to use the policy effective date,
      *                or the anniversary rating dates to get payroll rates. Also, corrected
      *                selection on EHDT instead of EHIDT.  There is a bug in SRINPR that
      *                looks like it randomly doesn't populate EHIDT.  EHDT is suitable for this
      *                purpose.  SRINPR doesn't need correction at this time.
      *=========================================================================
     fwfeul4    if   e           k disk
     fwfcap1l   if a e           k disk
     fwfcal     uf a e           k disk    prefix(big)
     fwmepl7    if   e           k disk
     fwdehl6    if   e           k disk
     fsmcol     if   e           k disk
     fwmprl1    if   e           k disk
     fwtpol     if   e           k disk
      *** beg add ***** 11/29/06 ***********************************************
     fwdf1l     if   e           k disk
      *** end add ***** 11/29/06 ***********************************************
    d*=========================================================================
    d* Stand Alone Fields - TOP
    d*=========================================================================
     d kkco#           s                   like(EUCO#)
     d kkjrst          s                   like(EUJRST) inz('CA')
     d svfnd           s                   like(EUFND)
     d svemp#          s                   like(EUEMP#)
     d svdiv           s                   like(EUDIV)
     d svfyr           s                   like(EUFYR)
     d kkrpdt7         s                   like(EHDT)
     d xxTotWrtnPrem   s                   like(EUYDWR)
     d xxPureWrtn      s                   like(EUYDWR)
     d xxTotYTDWrtn    s                   like(EUYDWR)
     d xxTotYTDEarn    s                   like(EUYDWR)
     d xxPureYTDWrtn   s                   like(EUYDWR)
     d xxPureYTDEarn   s                   like(EUYDWR)
     d xxPureRatio     s             16  9
     d kkrtcd          s                   like(PRRTCD)
     d wkDate          s               d
     d svCACO#         s                   like(bigCACO# )
     d svCAFND         s                   like(bigCAFND )
     d svCARPDT        s                   like(bigCARPDT)
     d svCAWRTN        s                   like(bigCAWRTN)
     d svCAEARN        s                   like(bigCAEARN)
     d svCAPUWR        s                   like(bigCAPUWR)
     d svCAPUEA        s                   like(bigCAPUEA)
    d*=========================================================================
    d* Data Structures and Arrays
    d*=========================================================================
     d psds          esds                  extname(szq1p)
     d                 ds
     d  expModDates            1     21  0 dim(3)
     d   EHEMD3                1      7  0
     d   EHEMD2                8     14  0
     d   EHEMD1               15     21  0
     d                 ds
     d  expModRates            1     15  4 dim(3)
     d   EHEXM3                1      5  4
     d   EHEXM2                6     10  4
     d   EHEXM1               11     15  4
    d*=========================================================================
    d* Procedure Prototypes
    d*=========================================================================
     d*entry
     d W4921R          pr
     d  @@co#                         3
     d  @@rpdt                        8
     d W4921R          pi
     d  @@co#                         3
     d  @@rpdt                        8
     d writeDetail     pr
     d writeCIRBfile   pr
     d getPureWrtn     pr                  like(EUYDWR)
     d getPureEarn     pr                  like(EUYDWR)
     d getPureCode     pr                  like(PRRTCD)
     d getPureRate     pr                  like(PRRT)
     d getBlendExpMod  pr                  like(EHEXM1)
     d getTotWrtn      pr                  like(EUYDWR)
     d subLeapDay      pr             1  0
     d  fromDt                        8  0
     d  toDate                        8  0
     d  leapDay                       8  0
      *** beg add **** 11/29/06 ********************************************************
     d getSplitFlag    pr             1
     d getExpMod       pr                  like(EHEXM1)
     d  effDt                         7  0
      *** end add **** 11/29/06 ********************************************************
     d/copy sprdate
    c*=========================================================================
      *  KLIST's
    c*=========================================================================
     c     euky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkjrst
    c*=========================================================================
      *  MAINLINE PROCESSING
    c*=========================================================================
     c                   move      @@co#         kkco#
     c                   eval      wkDate = cvtToDate(numDate(@@rpdt))
     c     *iso          move      wkDate        CARPDT
     c     *cymd         move      wkDate        kkrpdt7
     c                   eval      kkrtcd = getPureCode
      *
      *  Write to the California Quarterly CIRB Report Detail file
      *
      *** beg add ***** 10/15/07 ***********************************************
     c     euky          setll     peu01
      *** end add ***** 10/15/07 ***********************************************
     c     euky          reade     peu01
     c                   eval      svfnd  = EUFND
     c                   eval      svemp# = EUEMP#
     c                   eval      svdiv  = EUDIV
     c                   eval      svfyr  = EUFYR
     c                   dow       not(%eof)
     c                   if        svfnd  <> EUFND  or
     c                             svemp# <> EUEMP# or
     c                             svdiv  <> EUDIV  or
     c                             svfyr  <> EUFYR
     c                   callp     writeDetail
     c                   eval      svfnd  = EUFND
     c                   eval      svemp# = EUEMP#
     c                   eval      svdiv  = EUDIV
     c                   eval      svfyr  = EUFYR
     c                   eval      xxTotYTDWrtn  =
     c                             (EUYDWR - (EUYDWT + EUYDWD))
     c                   eval      xxTotYTDEarn  =
     c                             (EUYDEA - (EUYDET + EUYDED))
     c                   else
     c                   eval      xxTotYTDWrtn  = xxTotYTDWrtn  +
     c                             (EUYDWR - (EUYDWT + EUYDWD))
     c                   eval      xxTotYTDEarn  = xxTotYTDEarn  +
     c                             (EUYDEA - (EUYDET + EUYDED))
     c                   endif
     c     euky          reade     peu01
     c                   if        %eof
     c                   callp     writeDetail
     c                   endif
     c                   enddo
      *
      *  Read the Detail file to write the California Quarterly CIRB Report file
      *
     c     kkco#         reade     pca0p1
     c                   eval      svCACO#  = CACO#
     c                   eval      svCAFND  = CAFND
     c                   eval      svCARPDT = CARPDT
     c                   dow       not(%eof)
     c                   if        bigCAFND <> CAFND
     c                   callp     writeCIRBfile
     c                   eval      svCAFND  = CAFND
     c                   eval      svCAWRTN = CAWRTN
     c                   eval      svCAEARN = CAEARN
     c                   eval      svCAPUWR = CAPUWR
     c                   eval      svCAPUEA = CAPUEA
     c                   else
     c                   add       CAWRTN        svCAWRTN
     c                   add       CAEARN        svCAEARN
     c                   add       CAPUWR        svCAPUWR
     c                   add       CAPUEA        svCAPUEA
     c                   endif
     c     kkco#         reade     pca0p1
     c                   if        %eof
     c                   callp     writeCIRBfile
     c                   endif
     c                   enddo
     c
     c                   seton                                        lr
     p*=========================================================================
     p* writeDetail - write to WFCAP1
     p*=========================================================================
     p writeDetail     b
     d writeDetail     pi

     c                   if        xxTotYTDWrtn <> 0 or
     c                             xxTotYTDEarn <> 0
     c                   eval      xxTotWrtnPrem = getTotWrtn
     c                   endif

     c                   eval      CACO#  = kkco#
     c                   eval      CAFND  = svfnd
     c                   eval      CAEMP# = svemp#
     c                   eval      CADIV  = svdiv
     c                   eval      CAFYR  = svfyr
     c                   eval      CAWRTN = xxTotYTDWrtn
     c                   eval      CAEARN = xxTotYTDEarn

     c                   if        xxTotWrtnPrem <> 0
     c                   eval      CAPUWR = getPureWrtn
     c                   eval      CAPUEA = getPureEarn
     c                   else
     c                   eval      CAPUWR = 0
     c                   eval      CAPUEA = 0
     c                   endif

     c                   eval      CAUSER = Q1USER
     c                   eval      CAPGM  = Q1PGM
     c                   eval      CADT   = dateTo7(SystemDate)
     c                   eval      CAIUSR = Q1USER
     c                   eval      CAIPGM = Q1PGM
     c                   eval      CAIDT  = CADT
     c                   write     pca0p1

     p writeDetail     e
     p*=========================================================================
     p* getTotWrtn - returns total written premium on the policy
     p*=========================================================================
     p getTotWrtn      b
     d getTotWrtn      pi                  like(EUYDWR)

     d flatCxFlag      s              1    inz(*off)

     c     ehky          klist
     c                   kfld                    kkco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr

     c     ehky1         klist
     c                   kfld                    kkco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
     c                   kfld                    kkrpdt7

      *** beg add ***** 10/31/07 ******************************************************
     c                   eval      EHWAPR = 0
      *** end add ***** 10/31/07 ******************************************************

     c     ehky1         setgt     peh01
     c     ehky          readpe    peh01
     c                   dow       not(%eof)
     c                   if        EHFAF = 'A' and EHWAPR = 0
     c                   eval      flatCxFlag = *on
     c                   leave
     c                   endif
     c                   if        EHWAPR <> 0
     c                   leave
     c                   endif
     c     ehky          readpe    peh01
     c                   enddo

     c                   if        flatCxFlag = *off and EHWAPR <> 0
     c                   eval      xxTotWrtnPrem =
     c                             (EHWAPR - EHWTER - EHWDTR)
     c                   return    xxTotWrtnPrem
     c                   endif

     c                   if        flatCxFlag = *on
     c     ehky1         setgt     peh01
     c     ehky          readpe    peh01
     c                   dow       not(%eof)
     c                   if        EHFAF <> 'A' and EHWAPR <> 0
     c                   leave
     c                   endif
     c     ehky          readpe    peh01
     c                   enddo
     c                   endif

     c                   if        EHWAPR <> 0
     c                   eval      xxTotWrtnPrem =
     c                             (EHWAPR - EHWTER - EHWDTR)
     c                   return    xxTotWrtnPrem
     c                   else
     c                   return    0
     c                   endif

     p getTotWrtn      e
     p*=========================================================================
     p* getPureWrtn - returns pure premium value
     p*=========================================================================
     p getPureWrtn     b
     d getPureWrtn     pi                  like(EUYDWR)

     d payrollAmt      s                   like(EPEAPR) inz(0)
     d pureRate        s                   like(PRRT)
     d xxTotPureWrtn   s                   like(EUYDWR) inz(0)
     d xxRoundField    s             11  0 inz(0)
     d xxBlendExpMod   s                   like(EHEXM1) inz(1)
      *** beg add ***** 11/29/06 ***********************************************
     d expMod          s                   like(EHEXM1) inz(1)
      *** end add ***** 11/29/06 ***********************************************
      *** beg chg ***** 10/31/07 ***********************************************
     d********** splitPayFlag    s              1
     d splitPayFlag    s              1    inz('N')
      *** end chg ***** 10/31/07 ***********************************************

     c     epky          klist
     c                   kfld                    kkco#
     c                   kfld                    CAFND
     c                   kfld                    CAFYR
     c                   kfld                    CAEMP#
     c                   kfld                    CADIV

     c     poky          klist
     c                   kfld                    kkco#
     c                   kfld                    PRRTCD
     c                   kfld                    PRCLAS
     c                   kfld                    PREFDT

     c     poky1         klist
     c                   kfld                    kkco#
     c                   kfld                    PRRTCD
     c                   kfld                    PRCLAS
     c                   kfld                    PREFDT
     c                   kfld                    payrollAmt

     c     epky          setll     pep01
     c     epky          reade     pep01
     c                   dow       not(%eof)
      *** beg chg ***** 10/27/06 ************************************************
     c***********        if        EPIDT <= EHIDT
     c                   if        EPIDT <= EHDT
      *** end chg ***** 10/27/06 ************************************************
     c     EHFAF         ifeq      'A'
     c                   eval      payrollAmt = EPAAPR
      *** beg add ***** 11/29/06 ************************************************
     c                   eval      splitPayFlag = getSplitFlag
      *** end add ***** 11/29/06 ************************************************
     c                   else
     c                   eval      payrollAmt = EPEAPR
     c                   endif
     c                   eval      pureRate = getPureRate
     c                   select
     c                   when      PRPCF = 'P'
      *** beg add ***** 11/29/06 *******************************************************
     c                   if        splitPayFlag = 'Y'
     c                   eval      expMod = getExpMod(EPEFDT)
     c                   eval(h)   xxTotPureWrtn =
     c                             xxTotPureWrtn +
     c                             (payrollAmt * pureRate)/100 * expMod
     c                   else
      *** end add ***** 11/29/06 *******************************************************
     c                   eval(h)   xxTotPureWrtn =
     c                             xxTotPureWrtn + (payrollAmt * pureRate)/100
      *** beg add ***** 11/29/06 *******************************************************
     c                   endif
      *** end add ***** 11/29/06 *******************************************************
     c                   when      PRPCF = 'C'
      *** beg add ***** 11/29/06 *******************************************************
     c                   if        splitPayFlag = 'Y'
     c                   eval      expMod = getExpMod(EPEFDT)
     c                   eval(h)   xxTotPureWrtn =
     c                             xxTotPureWrtn +
     c                             (payrollAmt * pureRate) * expMod
     c                   else
      *** end add ***** 11/29/06 *******************************************************
     c                   eval(h)   xxTotPureWrtn =
     c                             xxTotPureWrtn + (payrollAmt * pureRate)
      *** beg add ***** 11/29/06 *******************************************************
     c                   endif
      *** end add ***** 11/29/06 *******************************************************
     c                   when      PRPCF = 'O'
     c     poky1         setgt     ppo01
     c     poky          reade     ppo01
     c                   if        not(%eof)
      *** beg add ***** 11/29/06 *******************************************************
     c                   if        splitPayFlag = 'Y'
     c                   eval      expMod = getExpMod(EPEFDT)
     c                   eval(h)   xxTotPureWrtn =
     c                             xxTotPureWrtn + (POAMT * expMod)
     c                   else
      *** end add ***** 11/29/06 *******************************************************
     c                   eval(h)   xxTotPureWrtn =
     c                             xxTotPureWrtn + POAMT
      *** beg add ***** 11/29/06 *******************************************************
     c                   endif
      *** end add ***** 11/29/06 *******************************************************
     c                   endif
     c                   endsl
     c                   endif                                                  EPIDT <= EHIDT
     c     epky          reade     pep01
     c                   enddo

      *** beg add ***** 11/29/06 *******************************************************
     c                   if        splitPayFlag = 'N'
      *** end add ***** 11/29/06 *******************************************************
     c                   eval      xxBlendExpMod = getBlendExpMod
     c                   eval(h)   xxTotPureWrtn = xxTotPureWrtn * xxBlendExpMod
      *** beg add ***** 11/29/06 *******************************************************
     c                   endif
      *** end add ***** 11/29/06 *******************************************************

     c                   eval(h)   xxRoundField  = xxTotPureWrtn * 1
     c                   eval      xxTotPureWrtn = xxRoundField

     c                   if        EHFAF <> 'A'
     c                   eval(h)   xxTotPureWrtn =
     c                             xxTotPureWrtn * EHPROR
     c                   eval(h)   xxRoundField  = xxTotPureWrtn * 1
     c                   eval      xxTotPureWrtn = xxRoundField
     c                   endif

     c                   if        xxTotWrtnPrem <> 0
     c                   eval      xxPureRatio   = (xxTotPureWrtn/xxTotWrtnPrem)
     c                   eval(h)   xxPureYTDWrtn =
     c                             xxTotYTDWrtn  *  xxPureRatio
     c                   eval(h)   xxRoundField  = xxPureYTDWrtn * 1
     c                   eval      xxPureYTDWrtn = xxRoundField
     c                   else
     c                   eval      xxPureYTDWrtn = 0
     c                   eval      xxPureRatio   = 0
     c                   endif

     c                   return    xxPureYTDWrtn

     p getPureWrtn     e
     p*=========================================================================
     p* getPureEarn - returns pure earned premium value
     p*=========================================================================
     p getPureEarn     b
     d getPureEarn     pi                  like(EUYDWR)
     d xxRoundField    s             11  0 inz(0)

     c                   eval(h)   xxPureYTDEarn = xxTotYTDEarn * xxPureRatio
     c                   eval(h)   xxRoundField  = xxPureYTDEarn * 1
     c                   eval      xxPureYTDEarn = xxRoundField
     c                   return    xxPureYTDEarn

     p getPureEarn     e
     p*=========================================================================
     p* writeCIRBfile - write to WFCAP
     p*=========================================================================
     p writeCIRBfile   b
     d writeCIRBfile   pi

     c     bigCAKY       klist
     c                   kfld                    kkco#
     c                   kfld                    svCARPDT
     c                   kfld                    svCAFND

     c     bigCAKY       chain     pca01
     c                   eval      bigCACO#  = svCACO#
     c                   eval      bigCAFND  = svCAFND
     c                   eval      bigCARPDT = svCARPDT
     c                   eval      bigCAWRTN = svCAWRTN
     c                   eval      bigCAEARN = svCAEARN
     c                   eval      bigCAPUWR = svCAPUWR
     c                   eval      bigCAPUEA = svCAPUEA

     c                   if        %found
     c                   eval      bigCAUSER = Q1USER
     c                   eval      bigCAPGM  = Q1PGM
     c                   eval      bigCADT   = dateTo7(SystemDate)
     c                   update    pca01
     c                   else
     c                   eval      bigCAUSER = Q1USER
     c                   eval      bigCAPGM  = Q1PGM
     c                   eval      bigCADT   = dateTo7(SystemDate)
     c                   eval      bigCAIUSR = Q1USER
     c                   eval      bigCAIPGM = Q1PGM
     c                   eval      bigCAIDT  = bigCADT
     c                   write     pca01
     c                   endif

     p writeCIRBfile   e
     p*=========================================================================
     p* getPureCode - get pure rate set code from SMCOP
     p*=========================================================================
     p getPureCode     b
     d getPureCode     pi                  like(COCART)
     c     kkco#         chain     pco01
     c                   return    COCART
     p getPureCode     e
     p*=========================================================================
     p* getPureRate -  get CA pure rate
     p*=========================================================================
     p getPureRate     b
     d getPureRate     pi                  like(PRRT)
     d kkefdt          s                   like(EHPFDT)
     d xxandt          s                   like(EHANDT)

     c     prky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
     c                   kfld                    EPCLAS

     c     prky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
     c                   kfld                    EPCLAS
     c                   kfld                    kkefdt

     c                   if        EHANDT = *zero
      *** beg chg ***** 10/27/06 *******************************************************
     c*****************  eval      kkefdt = EPEFDT
     c                   eval      kkefdt = EHPFDT
      *** end chg ***** 10/27/06 *******************************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             KKEFDT
     C                   MOVE(P)   KKCO#         ##CO#             3
     C                   MOVE(P)   SVFND         ##FND             3
     C                   MOVE(P)   SVFYR         ##FYR             3
     C                   MOVE(P)   SVEMP#        ##EMP#            9
     C                   MOVE(P)   SVDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DATE        KKEFDT
     C*** END ADD *****  04/01/08  *******************************************
     c     prky1         setgt     ppr01
     c     prky          readpe    ppr01
     c                   if        %eof
     c                   eval      PRRT = 0
     c                   endif
     c                   else
     c                   eval      xxandt = EHANDT + 10000
     c                   if        EPEFDT < xxandt
     c                   eval      kkefdt = EHANDT
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             KKEFDT
     C                   MOVE(P)   KKCO#         ##CO#             3
     C                   MOVE(P)   SVFND         ##FND             3
     C                   MOVE(P)   SVFYR         ##FYR             3
     C                   MOVE(P)   SVEMP#        ##EMP#            9
     C                   MOVE(P)   SVDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DATE        KKEFDT
     C*** END ADD *****  04/01/08  *******************************************
     c     prky1         setgt     ppr01
     c     prky          readpe    ppr01                                  60
     c                   if        %eof
     c                   eval      PRRT = 0
     c                   endif
     c                   else
      *** beg chg ***** 10/27/06 *******************************************************
     c*****************  eval      kkefdt = EPEFDT
     c                   eval      kkefdt = xxandt
      *** end chg ***** 10/27/06 *******************************************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             KKEFDT
     C                   MOVE(P)   KKCO#         ##CO#             3
     C                   MOVE(P)   SVFND         ##FND             3
     C                   MOVE(P)   SVFYR         ##FYR             3
     C                   MOVE(P)   SVEMP#        ##EMP#            9
     C                   MOVE(P)   SVDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DATE        KKEFDT
     C*** END ADD *****  04/01/08  *******************************************
     c     prky1         setgt     ppr01
     c     prky          readpe    ppr01                                  60
     c                   if        %eof
     c                   eval      PRRT = 0
     c                   endif
     c                   endif
     c                   endif
     c                   return    PRRT
     p getPureRate     e
     p*=========================================================================
     p* getBlendExpMod -   get blended rate for Experience Mods
     p*=========================================================================
     p getBlendExpMod  b
     d getBlendExpMod  pi                  like(EHEXM1)
     d xxPfDt          s              8  0 inz(0)
     d xxPtDt          s              8  0 inz(0)
     d xxLeapCk        s              4  0 inz(0)
     d xxLeapDt        s              8  0 inz(0)
     d xxPolDays       s              5  0
     d xxBlend         s                   like(EHEXM1) inz(1)
     d xxEMD1          s              8  0 inz(0)
     d xxEMD2          s              8  0 inz(0)
     d xxEMD3          s              8  0 inz(0)
     d xxM1Days        s              5  0
     d xxM2Days        s              5  0
     d xxM3Days        s              5  0
     d xxM1Ratio       s              5  4
     d xxM2Ratio       s              5  4
     d xxM3Ratio       s              5  4
     d xxLeapYrFlg     s               n   inz(*off)
     d polFromDt       s               d
     d polToDate       s               d
     d leapDate        s               d

     c                   eval      xxPfDt = dtMDYY(EHPFDT)
     c                   eval      xxPtDt = dtMDYY(EHPTDT)
     c                   eval      polFromDt = cvtToDate(xxPfDt)
     c                   eval      polToDate = cvtToDate(xxPtDt)
     c                   eval      xxPolDays = dtDiff(xxPfDt:xxPtDt)

     c                   move      xxPfDt        xxLeapCk
     c                   eval      xxLeapCk = %rem(xxLeapCk:4)
     c                   if        xxLeapCk = 0
     c                   eval      xxLeapDt = xxPfDt
     c                   movel     '0229'        xxLeapDt
     c                   eval      leapDate = cvtToDate(xxLeapDt)
     c                   if        leapDate > polFromDt and leapDate < polToDate
     c                   sub       1             xxPolDays
     c                   eval      xxLeapYrFlg = *on
     c                   endif
     c                   else
     c                   move      xxPtDt        xxLeapCk
     c                   eval      xxLeapCk = %rem(xxLeapCk:4)
     c                   if        xxLeapCk = 0
     c                   eval      xxLeapDt = xxPtDt
     c                   movel     '0229'        xxLeapDt
     c                   eval      leapDate = cvtToDate(xxLeapDt)
     c                   if        leapDate > polFromDt and leapDate < polToDate
     c                   sub       1             xxPolDays
     c                   eval      xxLeapYrFlg = *on
     c                   endif
     c                   endif
     c                   endif

     c                   if        xxPolDays <> 0

     c                   eval      xxEMD1 = dtMDYY(EHEMD1)
     c                   eval      xxEMD2 = dtMDYY(EHEMD2)
     c                   eval      xxEMD3 = dtMDYY(EHEMD3)

     c                   if        xxEMD1 <> 0 and EHEXM1 <> 0
     c                   if        EHEMD1 < EHPFDT
     c                   eval      xxEMD1 = xxPfDt
     c                   endif
     c                   if        xxEMD2 <> 0
     c                   eval      xxM1Days = dtDiff(xxEMD1:xxEMD2)
     c                   if        xxLeapYrFlg = *on
     c                   eval      xxM1Days =
     c                             xxM1Days - subLeapDay(xxEMD1:xxEMD2:xxLeapDt)
     c                   endif
     c                   else
     c                   eval      xxM1Days = dtDiff(xxEMD1:xxPtDt)
     c                   if        xxLeapYrFlg = *on
     c                   eval      xxM1Days =
     c                             xxM1Days - subLeapDay(xxEMD1:xxPtDt:xxLeapDt)
     c                   endif
     c                   endif
     c                   eval(h)   xxM1Ratio = xxM1Days/xxPolDays
     c                   eval(h)   xxBlend = xxM1Ratio * EHEXM1
     c                   endif

     c                   if        xxEMD2 <> 0 and EHEXM2 <> 0
     c                   if        xxEMD3 <> 0
     c                   eval      xxM2Days = dtDiff(xxEMD2:xxEMD3)
     c                   if        xxLeapYrFlg = *on
     c                   eval      xxM2Days =
     c                             xxM2Days - subLeapDay(xxEMD2:xxEMD3:xxLeapDt)
     c                   endif
     c                   else
     c                   eval      xxM2Days = dtDiff(xxEMD2:xxPtDt)
     c                   if        xxLeapYrFlg = *on
     c                   eval      xxM2Days =
     c                             xxM2Days - subLeapDay(xxEMD2:xxPtDt:xxLeapDt)
     c                   endif
     c                   endif
     c                   eval(h)   xxM2Ratio = xxM2Days/xxPolDays
     c                   eval(h)   xxBlend =
     c                             xxBlend + xxM2Ratio * EHEXM2
     c                   endif

     c                   if        xxEMD3 <> 0 and EHEXM3 <> 0
     c                   eval(h)   xxM3Days = dtDiff(xxEMD2:xxPtDt)
     c                   if        xxLeapYrFlg = *on
     c                   eval      xxM3Days =
     c                             xxM3Days - subLeapDay(xxEMD3:xxPtDt:xxLeapDt)
     c                   endif
     c                   eval(h)   xxM3Ratio = xxM3Days/xxPolDays
     c                   eval(h)   xxBlend =
     c                             xxBlend + xxM3Ratio * EHEXM3
     c                   endif

     c                   endif

     c                   return    xxBlend

     p getBlendExpMod  e
     p*=========================================================================
     p* subLeapDay - returns 1 if leap day is in range passed in else returns 0
     p*=========================================================================
     p subLeapDay      b
     d subLeapDay      pi             1  0
     d  fromDate                      8  0
     d  toDate                        8  0
     d  leapDate                      8  0
     d  date1          s               d
     d  date2          s               d
     d  date3          s               d

     c                   eval      date1 = cvtToDate(fromDate)
     c                   eval      date2 = cvtToDate(toDate)
     c                   eval      date3 = cvtToDate(leapDate)
     c                   if        date3 > date1 and date3 < date2
     c                   return    1
     c                   else
     c                   return    0
     c                   endif

     p subLeapDay      e
      *** beg add ***** 11/29/06 *******************************************************
     p*=========================================================================
     p* getSplitFlag - return 'Y' if payrolls are split on exp mod in group detail, else return 'N'
     p*=========================================================================
     p getSplitFlag    b
     d getSplitFlag    pi             1

     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    svfnd
     c                   kfld                    svfyr

     c     fnky          chain     pf101
     c                   if        %found and F1SPLA = 'Y'
     c                   return    'Y'
     c                   else
     c                   return    'N'
     c                   endif

     p getSplitFlag    e
     p*=========================================================================
     p* getExpMod - returns exp mod effective for split payrolls
     p*=========================================================================
     p getExpMod       b
     d getExpMod       pi                  like(EHEXM1)
     d  effDt                         7  0
     d  x              s              1  0

     c                   do        3             x
     c                   if        effDt >= expModDates(x) and
     c                             expModDates(x) <> 0
     c                   return    expModRates(x)
     c                   endif
     c                   enddo

     c                   return    1

     p getExpMod       e
      *** end add ***** 11/29/06 *******************************************************
