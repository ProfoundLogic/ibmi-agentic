     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W8005R - Fee invoice process program
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   This prompt program to invoke claim fee/collateral          *
     H*   invoice and detail report                                   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   NONE                                                        *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:189                                             *
     H* CREATED: 01/25/10               PROJECT: WC1851               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/23/11 C:65  WC1851                                         *
     H*                Change incurred and paid plans to accumulate   *
     H*                amounts from the reserve history and payment   *
     H*                transaction files respectively, in order to    *
     H*                pull information up to the invoice as of date. *
     H*                                                               *
     H* 09/14/11 C:65  WC1851                                         *
     H*                Write fee detail records if a claim has been   *
     H*                charged previously but there is no current     *
     H*                billing. This is to allow prior billings to    *
     H*                be accumulated for fee detail report when      *
     H*                collateral is charged on a per claim basis.    *
     H*                                                               *
     H* 09/08/11 C:65  WC1851                                         *
     H*                Implement the fee setup flag to charge         *
     H*                collateral based on claims fees.               *
     H*                                                               *
     H* 08/29/11 C:65  WC1851                                         *
     H*                Set up a new batch for every invoice job.      *
     H*                                                               *
     H* 08/19/11 C:65  WC1851                                         *
     H*                Use the premium from premium history as of the *
     H*                as of date passed in for collateral prem calc. *
     H*                                                               *
     H* 08/01/11 C:65  WC1851                                         *
     H*                Change the minimum collateral calc to be on a  *
     H*                per policy basis rather than per division.     *
     H*                                                               *
     H* 06/01/11 C:65  WC1851 Priss 189                               *
     H*                Fix time stamps on a/r batch and a/r category. *
     H*                                                               *
     H* 05/09/11 C:65  WC1851                                         *
     H*                Load from group for non-override policies.     *
     H*                Return batch # for a/r batch report.           *
     H*                                                               *
     H* 04/20/11 C:65  WC1851                                         *
     H*                Changes to calculations.                       *
     H*                                                               *
     H* 02/15/11 C:061 WC1851                                         *
     H*                (Changes not commented)                        *
     H*               -Eliminate SrFnd# (use SrFnd instead)           *
     H*               -Eliminate GOTOs.                               *
     H*               -Use date routines from SPRDATE                 *
     H*               -Check Posting Date prior to processing         *
     H*                Collateral Adjustments.                        *
     H*               -Create new procedure "Calculate".              *
     H*               -SRPRBL use A/R codes from WTFTP.               *
     H*               -Claim Fee and Colateral calculations will be   *
     H*                made using procedure Calculate.                *
     H*                                                               *
     H* 05/20/10 C:190 WC1851                                         *
     H*                If claim fee exclusion flag is set skip the    *
     H*                employer.                                      *
     H*                Corrected 'Call' parameters for SRFUTR8.        *
     H*                Corrected 'Call' parameters for SRINV#.         *
     H*                                                               *
     H* 03/05/10 C:136 25WQL PRISS 13                                 *
     H*                Default post date to system date plus EXDFID   *
     H*                (Deductible Fee Interim Days) for each group   *
     H*                processed if no post date is passed into the   *
     H*                program.                                       *
     H*****************************************************************

     FWDFCL     IF A E           K DISK
     FWDFCL1    UF   E           K DISK    RENAME(PFC01:PFC02)
     F                                     PREFIX(B)
     FWDFCL3    IF   E           K DISK    RENAME(PFC01:PFC03)
     F                                     PREFIX(A)
     FWXGSL     IF   E           K DISK
     FWDEAL     IF   E           K DISK
     FWDEAL4    UF A E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWDEAL5    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PPA01)
     Fwtftl     if   e           k disk    rename(pft01:pft00)
     FWMRSL     IF   E           K DISK
     FWMCML14   IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDEHL6    IF   E           K DISK
     FWACTL2    IF   E           K DISK
     FWDEFL2    IF   E           K DISK
     FWDEFL1    IF   E           K DISK    rename(pef01:pef02)
     FWAETL1    IF   E           K DISK
     FWTFIL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK    rename(pf101:pf100)
     FWMEAL     IF   E           K DISK    RENAME(PEA01:PEA00)
     F                                     prefix($:1)

     DCalculate        pr            11s 2
     D clmLTFlag                      1a   value
     D billType                       1a   value
     D percentage                     7s 4 value
     D flatFee                       11s 2 value
     D minimum                       11s 2 value options(*nopass)

     DStopCollateral   pr             1n
     D inDate                         7  0 const

     DValidClaim       pr             1n

     DsaveWDEFP        ds                  likerec(pef01:*input) inz

     DstRptWDELP       ds                  likerec(pel01:*input) inz
     DkeyWDELL         ds                  likerec(pel01:*key) inz

     D                 DS
     DCMA                      1     90  2 DIM(10)
     DCMAR01                   1      9  2
     DCMAR02                  10     18  2
     DCMAR03                  19     27  2
     DCMAR04                  28     36  2
     DCMAR05                  37     45  2
     DCMAR06                  46     54  2
     DCMAR07                  55     63  2
     DCMAR08                  64     72  2
     DCMAR09                  73     81  2
     DCMAR10                  82     90  2
     D*
     D                 DS
     DCMR                      1     90  2 DIM(10)
     DCMRR01                   1      9  2
     DCMRR02                  10     18  2
     DCMRR03                  19     27  2
     DCMRR04                  28     36  2
     DCMRR05                  37     45  2
     DCMRR06                  46     54  2
     DCMRR07                  55     63  2
     DCMRR08                  64     72  2
     DCMRR09                  73     81  2
     DCMRR10                  82     90  2
     D dsWDEFP       E DS                  EXTNAME(WDEFP)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FYR             S              3  0 DIM(999)
     D FDT             S              7  0 DIM(999)                             POLICY EFF DATE
     D ST              S              2    DIM(999)                             RATING STATE
     D CLM             S             11  2 DIM(999)                             CLAIM FEE
     D COL             S             11  2 DIM(999)                             COLATERAL FEE
     D CLMAMT          S             11  2 DIM(999)                             CLAIM FEE AT YR LVL
     D COLAMT          S             11  2 DIM(999)                             COL FEE AT YR LVL
     D XXPAMT          S             15  2
     DCOLTYPE          S              1
     DCLMTYPE          S              1
     DsingleEmp        S              1n
     DpriorRec         S              1n
     Dfirst            S              1n   inz('1')
     Dx7PrDt           S              7  0
     Dx7LsDt           S              7  0
     DsysDate7         S              7  0
     Dx7PoDt           S              7  0
     DsysTime6         s              6  0
     DxxFyr            s                   like(EFFYR)
     DxxRtCd           s                   like(ELRTCD)
     DxFeeCod          s                   like(EAARCD)
     Dxxbt             S                   like(fcbt)
     DsaveEFEMP#       s                   like(EFEMP#)
     DsaveEFDIV        s                   like(EFDIV)
     DsaveEFFYR        s                   like(EFFYR)
     DsaveType         s                   like(EFTYPE)
     DsaveDesc         s                   like(EFDESC)
     Dtotalclm         s                   like(FCCFCB)
     Dtotalcol         s                   like(FCCOFB)
     Dminichk          S              1n   inz('0')

     D/copy sPrDate
     D/copy wPrEmplr
      *
      *  Group
      *
     C     FNKY          klist
     C                   kfld                    xxco#
     C*
     C     keyWDEFL2     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C*
     C     key1WDEFL2    klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C*
     C     keyWDEFL1     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xsDiv

     C     FCKEY3        klist
     C                   kfld                    XXCO#
     C                   kfld                    xxFnd
     C                   kfld                    x7PoDt
     C*
     C     EAKEY         klist
     C                   kfld                    XXCO#
     C                   kfld                    XXSTAT
     C*
     C     keyWTFTL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    SaveType
     C                   kfld                    SaveDesc
     C*
     C     keyWMEXL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C*
     C     TBKEY         klist
     C                   kfld                    XXCO#
     C                   kfld                    XXKEY
     C*
     C     CMKEY         klist
     C                   kfld                    XXCO#
     C                   kfld                    XXFND
     C                   kfld                    XXEMP#
     C                   kfld                    XXDIV
     C                   kfld                    xxFyr
     C*
     C     CIKY          klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     c                   kfld                    EFTYPE
     c                   kfld                    EFDESC
     C                   kfld                    XXST
     C                   kfld                    ELPFDT
     C*
     C     CIKY1         klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     c                   kfld                    EFTYPE
     c                   kfld                    EFDESC
     C                   kfld                    XXST
     C*
     C     EFKY          klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C*
     C     RSKY1         klist
     C                   kfld                    ELCO#
     C                   kfld                    xxRtCd
     C*
     C     CTKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    xxFnd
     C                   kfld                    CMFYR
     C                   kfld                    CMCASE
      *
     C     keyWDF1L      klist
     C                   kfld                    EFCO#
     C                   kfld                    EFFND
     C                   kfld                    EFFYR
     C*
     C     keyWMEAL      klist
     C                   kfld                    XXCO#
     C                   kfld                    xxFnd
     C                   kfld                    CMEMP#
     C                   kfld                    CMDIV

     C     INSTAL        PLIST
     C                   PARM                    @@WCIF            1
     C                   PARM                    @@MCIF            1
     C                   PARM                    @@WCIN            1
     C                   PARM                    @@AR              1
     C                   PARM                    @@NTWK            1
     C                   PARM                    @@UR              1
     C                   PARM                    @@MED             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@WCDK            1
     C                   PARM                    @@RHAB            1
     C                   PARM                    @@MSS             1
     C                   PARM                    @@FSCH            1
     C                   PARM                    @@PRVB            1
     C                   PARM                    @@NPWC            1
     C                   PARM                    @@OS              1
     C                   PARM                    @@ARLS            1
     C                   PARM                    @@CCOD            1
     C                   PARM                    @@CRVW            1
     C                   PARM                    @@PROM            1
     C                   PARM                    @@MCRV            1
     C                   PARM                    @@FREQ            1
     C                   PARM                    @@CRAC            1
     C                   PARM                    @@PREO            1
     C                   PARM                    @@PARN            1
     C                   PARM                    @@MGRI            1
     C                   PARM                    @@KCIF            1
     C                   PARM                    @@CURS            1
     C                   PARM                    @@PCIB            1
     C                   PARM                    @@CLT#            3
     C                   PARM                    @@UCPF            1
     C                   PARM                    @@DCF             1
     C                   PARM                    @@AUTB            1
     C*
     C*
     C     ELKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    XXFND
     C                   kfld                    XXEMP#
     C                   kfld                    XXDIV
     C                   kfld                    XXFYR
     C*
     C     EHKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    XXFND
     C                   kfld                    XXEMP#
     C                   kfld                    XXDIV
     C                   kfld                    XXFYR
     C                   kfld                    X7LSDT
     C*
     C     EHKY1         klist
     C                   kfld                    XXCO#
     C                   kfld                    XXFND
     C                   kfld                    XXEMP#
     C                   kfld                    XXDIV
     C                   kfld                    XXFYR
     C*
     C     EAKEY1        klist
     C                   kfld                    XXCO#
     C                   kfld                    XXBAT#
     C                   kfld                    XXFND
     C                   kfld                    saveEFEMP#
     C                   kfld                    saveEFDIV
     C                   kfld                    XXBFYR
     C*
     C     EAKEY2        klist
     C                   kfld                    XXCO#
     C                   kfld                    XXBAT#
     C*
     C     epky          klist
     C                   kfld                    CMCO#
     C                   kfld                    CMFND
     C                   kfld                    CMEMP#
     C                   kfld                    CMDIV
     C                   kfld                    CMFYR
     C*
     C     FCKY          klist
     C                   kfld                    EACO#
     C                   kfld                    EAFND
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C                   kfld                    EAFYR
     C                   kfld                    EAPODT
     C*
     C     FCKY1         klist
     C                   kfld                    FCCO#
     C                   kfld                    FCFND
     C                   kfld                    FCEMP#
     C                   kfld                    FCDIV
     C                   kfld                    FCFYR
     C                   kfld                    FCCASE
     C*
     C     FCKY2         klist
     C                   kfld                    FCCO#
     C                   kfld                    FCFND
     C                   kfld                    FCEMP#
     C                   kfld                    FCDIV
     C                   kfld                    FCFYR
     C*
     C     FCKY3         klist
     C                   kfld                    FCCO#
     C                   kfld                    FCFND
     C                   kfld                    FCEMP#
     C*
     C     FCKY4         klist
     C                   kfld                    XXCO#
     C                   kfld                    XXFND
     C                   kfld                    XXEMP#
     C                   kfld                    XXDIV
     C                   kfld                    XXFYR
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    FCCFCB        @@CFCB
     C     *LIKE         DEFINE    FCCOFE        @@COFE
     C     *LIKE         DEFINE    FCCOFB        @@COFB
     C     *LIKE         DEFINE    FCCASE        @@CASE
     C     *LIKE         DEFINE    *IN60         @ALLLVL
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    EADESC        XXDESC
     C     *LIKE         DEFINE    EAFYR         XXBFYR
     C     *LIKE         DEFINE    EAAMT         XXBATA
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    EASYCD        XXSYCD
     C     *LIKE         DEFINE    FCBAT#        XXBAT#
     C     *LIKE         DEFINE    FCCFCH        CLMFEE
     C     *LIKE         DEFINE    FCCFCH        CLMFEE1
     C     *LIKE         DEFINE    FCCFCH        CLMFEE2
     C     *LIKE         DEFINE    FCCOFE        COLFEE
     C     *LIKE         DEFINE    FCCOFE        COLFEE1
     C     *LIKE         DEFINE    FCCOFE        COLFEE2
     C     *LIKE         DEFINE    @TOTALB2      @TOTALB3
     C     *LIKE         DEFINE    @TOTALB2      @TOTALB4
     C     *LIKE         DEFINE    ELDIV         XSDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQPBDT            8
     C                   PARM                    QQLSDT            8
     C                   PARM                    QQPODT            8
     C                   PARM                    QQPRDT            8
     C                   PARM                    SCCOM1            2
     C                   PARM                    SCCOM2            2
     C                   PARM                    SCCOM3            2
     C                   PARM                    SCCOM4            2
     C                   PARM                    QQOUTQ           10
     C                   PARM                    SCDFLG            1
     C                   PARM                    SCDDRM            5
     C                   PARM                    QQASSF            1
     C                   PARM                    QQEXCL            1
     C                   PARM                    QQBAT#            9
     C*
     C
     C*Move parameter values to numeric variables
     C
     C                   Move      QQCO#         XXCO#             3 0
     C                   Move      QQFND         XXFND             3 0
     C                   Move      QQEMP#        XXEMP#            9 0
     C                   Move      QQDIV         XXDIV             5 0
     C                   Move      QQPBDT        XXPBDT            8 0
     C                   Move      QQLSDT        x8LsDt            8 0
     C                   Move      QQPODT        x8PoDt            8 0
     C                   Move      QQPRDT        x8PrDt            8 0
     c                   eval      x7PrDt = DtCYMD(x8PrDt)
     c                   eval      x7LsDt = DtCYMD(x8LsDt)
     c                   eval      x7PoDt = DtCYMD(x8PoDt)
     c                   eval      sysDate7 = DateTo7(SystemDate)
     c                   eval      sysTime6 = TimeTo6(SystemTime)
      *
     C                   MOVEL     sysDate7      XXCYM1            5 0
     C                   MOVE      *BLANKS       XXRDBL            2
     C
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C*
     c                   z-add     0             xxbat#
     C                   MOVE      *ON           FIRST1            1
      /EJECT
      *================================================================
      *  MAINLINE CODE START HERE
      *================================================================
     C*
     C                   CALL      'SA001R'      INSTAL
     C*
     C*  FIND A VALID SYSTEM TABLE CODE
     C*
     C     XXCO#         SETLL     PSY01
     C     *IN60         DOUEQ     *ON
     C     XXCO#         READE     PSY01                                  60
     C     SYTYPE        IFEQ      'Y'
     C     SYSTAT        ANDNE     'I'
     C                   MOVEL     SYKEY         XXSYCD
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   ENDDO
     C*
     C                   IF        XXEMP# = 0
      *
     C*  Read through file to make sure at least one group is included
      *
     C     FNKY          CHAIN     PGS01                              60

     C                   DOW       *IN60 = *OFF

     C                   IF        GSIEFL =  'I'
     c                   eval      singleEmp = *off
     C                   eval      xxFnd = GSFND
     C                   EXSR      SrFnd
     C                   ENDIF

     C     FNKY          READE     PGS01                                  60
     C                   ENDDO

     C                   ELSE
      *
     c                   eval      singleEmp = *on
     C                   EXSR      SrFnd
     C
     C                   ENDIF                                              QQEMP# = ' '
     C*
      *  UPDATE ALL BATCH RECORDS CREATED WITH THE TOTAL BATCH AMOUNT
      *
     C                   clear                   xxTotB
     C     EAKEY2        CHAIN(n)  PEA02                              60
     C     *IN60         DOWEQ     '0'
     C                   eval      xxTotB += EAAMT
     C     EAKEY2        READE(n)  PEA02                                  60
     C                   END
     C*
     C     EAKEY2        CHAIN     PEA02                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXTOTB        EABATA
     C                   UPDATE    PEA02
     C     EAKEY2        READE     PEA02                                  60
     C                   END
      *  Delete the A/R batch dummy record.
      *
     C                   MOVE      *HIVAL        XXFND
     C                   MOVE      *HIVAL        saveEFEMP#
     C                   MOVE      *HIVAL        saveEFDIV
     C                   MOVE      *HIVAL        XXBFYR
     C     EAKEY1        CHAIN     PEA02                              60
      *
     C  N60              DELETE    PEA02
      *
     c                   eval      qqPoDt = %editc(DtMDYY(x7PoDt):'X')
     C                   move      xxbat#        QQBAT#
      *
     C                   SETON                                        LR

      /EJECT
     C*====================================================================
      * Process Records for each group
     C*====================================================================
     C     SrFnd         BEGSR
     C*
     C                   CLEAR                   CLM
     C                   CLEAR                   COL
     C                   CLEAR                   CLMAMT
     C                   CLEAR                   COLAMT
     C                   select
     C                   when      singleEmp
     C     key1WDEFL2    setll     wdefl2
     C                   other
     C     keyWDEFL2     setll     wdefl2
     C                   endsl
     C*
     C                   dou       %eof(wdefl2)
     C*
     C                   select
     C                   when      singleEmp
     C     key1WDEFL2    reade     wdefl2
     C                   other
     C     keyWDEFL2     reade     wdefl2
     C                   endsl
     c*
     C                   select
     C                   when      first = *on
     C                   eval      saveEFEMP# = EFEMP#
     C                   eval      saveEFDIV  = EFDIV
     C                   eval      saveEFFYR  = EFFYR
     C                   eval      saveType   = EFTYPE
     C                   eval      saveDesc   = EFDESC
     C                   eval      first = *off

     C                   when      %eof(wdefl2) or
     C                              (EFEMP# <> saveEFEMP# or EFDIV <> saveEFDIV
     C                              or EFTYPE <> SaveType or EFDESC <> saveDesc)
     C                   exsr      FyrBrk
     C                   exsr      Emp_Break

     C                   when      EFFYR <> saveEFFYR
     C                   exsr      FyrBrk
     C                   endsl
     C*
     C                   clear                   clmType
     C                   clear                   colType
     C*
     C                   if        not %eof(wdefl2)
     C                   eval      xxCo#  = EFCO#
     C                   eval      xxFnd  = EFFND
     C                   eval      xxEmp# = EFEMP#
     C                   eval      xxDiv  = EFDIV
     C                   eval      xxFyr  = EFFYR
     C                   exsr      SrFees
     C                   endif
     C*
     C                   enddo
     C*
     C                   endsr

      /EJECT
     C*====================================================================
      * Process fee records for each employer
     C*====================================================================
     C     SrFees        BEGSR

     c                   if        xxemp# = 0
     c                   eval      *in75 = *on
     c                   else
     c                   eval      *in75 = *off
     c                   endif
     C*
     C                   exsr      GetBat#
     C*
     C*    Start Reading the Cliam file
     C*
     C                   MOVE      *OFF          I@BILL            1
     c                   move      *off          minichk
     C*
     C*  READ CLAIM FILE FOR ALL EMPLOYERS
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C     *IN60         DOWEQ     '0'

      *VALIDATE THAT CLAIM IS VALID

     c                   if        ValidClaim
     C
     C*CLAIM REPORTED TILL INVOICE DATE SHOULD ONLY BE CONSIDERED
     C
     C                   if        CMDTRP  <=  x7LsDt
     C
     C*CALCULATE THE CLAIM & COLATERAL FEE FOR EACH RECORD AT CASE LEVEL
     C                   EXSR      FEECOL
     C
      *
      * ACCUMULATE THE COLFEE  & CLMFEE FOR EACH RECORD (CASE LEVEL)
      *
     C                   Z-ADD     CMFYR         XXBFYR
     C                   ADD       1             P                 3 0
     C                   MOVE      totalclm      CLM(P)
     C                   MOVE      totalcol      COL(P)
     c                   eval      *in75 = *on
     c                   endif
     c                   endif
      *
     C     CMKEY         READE     PCM01                                  60
     C                   ENDDO                                                  *IN50
     C*
     C                   ENDSR                                                  SrFnd

      /EJECT
     C*====================================================================
     C* Get the batch #
     C*====================================================================
     C     GetBat#       BEGSR
     C*
     C*  CHECK TO SEE IF BATCH RECORDS ALREADY EXIST FOR THIS JOB.
     C*   IF SO, UPDATE CURRENT BATCH RECORDS OTHERWISE RETRIEVE
     C*   A NEW BATCH NUMBER
     C*
     C                   clear                   EXDFID
     c     keyWMEXL      chain     pex01
     C                   select
     C                   when      x8PoDt <> *zero
     C                   eval      x7PoDt = x7PoDt
     c                   when      EXDFID <> 0
     C                   eval      x7PoDt = DtCYMD(DtFuture( +
     C                                             DateTo8(SystemDate):EXDFID))
     C                   other
     C                   eval      x7PoDt = DtCYMD(DateTo8(SystemDate))
     C                   endsl
     C*
     C     FIRST1        IFEQ      *ON
     C                   MOVE      *OFF          FIRST1
      *
     C*  RETRIEVE NEXT AVAILABLE BATCH NUMBER
     C*
     C                   MOVE      *BLANKS       XXSTAT            1
     C     EAKEY         SETGT     PEA01
     C                   READP     PEA01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL1            9 0
     C                   ENDIF
     C*
     C                   MOVE      'T'           XXSTAT
     C     EAKEY         SETGT     PEA01
     C                   READP     PEA01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL2            9 0
     C                   ENDIF
     C*
     C     XXVAL1        IFGT      XXVAL2
     C                   Z-ADD     XXVAL1        XXBAT#
     C                   ELSE
     C                   Z-ADD     XXVAL2        XXBAT#
     C                   ENDIF
      *
     C     XXBAT#        IFEQ      *ZEROS
     C                   Z-ADD     1             XXBAT#
     C                   ENDIF
      *
      *  Write an A/R batch dummy record, to hold the bat#.
      *
     C                   CLEAR                   PEA02
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     0             EABATA
     c                   eval      eaachf = 'F'
     C                   MOVE      *HIVAL        EAFND
     C                   MOVE      *HIVAL        EAFYR
     C                   MOVE      *HIVAL        EAEMP#
     C                   MOVE      *HIVAL        EADIV
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   Z-ADD     *ZEROS        EAPODT
     C                   Z-ADD     *ZEROS        EACHK#
     C                   Z-ADD     *ZEROS        EACKAM
     C                   Z-ADD     *ZEROS        EAAMT
     C                   MOVE      *BLANKS       EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   eval      sysTime6 = sysTime6 + 1
     C                   eval      EATIME = sysTime6
     c                   eval      EADT   = sysDate7
     c                   eval      EAIDT  = EADT
     C                   MOVE      Q1USER        EAIUSR
     C                   MOVE      Q1PGM         EAIPGM
     C                   Z-ADD     *ZEROS        EAINV#
     C                   Z-ADD     *ZEROS        EATRDT
     C                   MOVE      *BLANKS       EALATE
     C                   WRITE     PEA02
      *
     C                   ENDIF                                                  if FIRST = *ON
     C*
     C                   ENDSR                                                  SrFnd

      /EJECT
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    EAAMT         XXTOTB
     C     *LIKE         DEFINE    EACO#         #ICO#
     C     *LIKE         DEFINE    EAFND         #IFND
     C     *LIKE         DEFINE    EAEMP#        #IEMP#
     C     *LIKE         DEFINE    EADIV         #IDIV
     C     *LIKE         DEFINE    EAFYR         #IFYR
     C     *LIKE         DEFINE    EAPODT        #IDT
     C     *LIKE         DEFINE    EAINV#        #IINV#
     C*
     C*
     C                   CLEAR                   TTTYPE
     C                   MOVEL     EAARCD        XXKEY
     C*
     C     TBKEY         CHAIN     PTT01                              60
     C*
     ***   TTTYPE        IFEQ      'A'
     C*    TTTYPE        OREQ      'B'
     C*    TTTYPE        OREQ      'D'
     C*    TTTYPE        OREQ      'F'
     C*    TTTYPE        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      EACO#         #ICO#
     C                   PARM      EAFND         #IFND
     C                   PARM      EAEMP#        #IEMP#
     C                   PARM      EADIV         #IDIV
     C                   PARM      EAFYR         #IFYR
     C                   PARM      x7PoDt        #IDT
     C                   PARM                    #IINV#
     C                   PARM      *ON           ##CRTN            1
     C*
     C*                  ENDIF
     C                   ENDSR

      /EJECT
     C*====================================================================
     C* Get the state reporting division information
     C*====================================================================
     C     GetStRptInfo  begsr
      /free
       keyWDELL.ELCO#  = ELCO#;
       keyWDELL.ELFND  = ELFND;
       keyWDELL.ELEMP# = ELEMP#;
       keyWDELL.ELDIV  = GetStRptDv(ELCO#:ELFND:ELEMP#);
       keyWDELL.ELFYR  = ELFYR;

       //Chain to file loads the fields to stRptWDELP;
       chain %kds(keyWDELL) pel01 stRptWDELP;

       xxRtCd = stRptWDELP.ELRTCD;

      /end-free
     C                   endsr

      /EJECT
     C*====================================================================
     C*  FEECOL - THIS SUBROUTINE WILL EXECUTE OTHER FEE ROUTINES
     C*====================================================================
     C     FEECOL        BEGSR
     C*
     C* CLEAR THE RECORD FORMAT
     C*
     C                   CLEAR                   PFC01
     C                   CLEAR                   colfee
     C                   CLEAR                   clmfee
     C                   z-add     0             totalclm
     C                   z-add     0             totalcol
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     ELKY          CHAIN     PEL01                              60
     c*
     c* Set premium for collateral calculation based on prem as of the losses date.
     c*
     c     EHKY          setgt     peh01
     c     ehky1         readpe    peh01                                  60
     c*
     c                   z-add     elanpr        xxprem           11 2
     c*
     c                   if        *in60 = *off
     c                   z-add     ehwapr        xxprem
     c                   else
     c                   if        elfaf = 'A'
     c                   z-add     elaapr        xxprem
     c                   endif
     c                   endif
     c*
     C     keyWDF1L      chain     wdf1l
     c                   eval      xxRtCd = ELRTCD
     C*
     C
     C* GET THE RATING STATE TO ACESS THE WTFIP RECORDS APPLICABLE TO EMPLOYER
     C*
     C                   clear                   RSJRST
     C     RSKY1         CHAIN     PRS01                              60
     C                   MOVEL     RSJRST        XXST              2
      *
     C* ACCESS THE GROUP LEVEL CHARGE SETUP FILE  & VALIDATE EXPIRY DATE
      *
     C     CIKY          SETGT     PFI01                                           WTFIP
     C     CIKY1         READPE    PFI01                                  60
     C                   DOW       *IN60 =*OFF
     C
     C                   IF        FIEXDT = 0  OR  FIEXDT>ELPFDT
     C                   LEAVE
     C                   ENDIF
     C
     C     CIKY1         READPE    PFI01                                  60
     C                   ENDDO
     C*
     C* If setup is missing, skip
     C                   if        *in60
     C                   leavesr
     C                   endif
     C*
     c                   clear                   @cal_clmfee
     c                   clear                   @cal_colfee
     C*
     C* CHECK IF WE SHOULD CALCULATE THE "BILL CLAIM FEE"
     C*
     C                   if        EFTYPE = 'CLF' and EFEXCF <> 'Y'
     C                   MOVE      'Y'           @CAL_CLMFEE       1
     C                   ENDIF
     C*
     C* CHECK IF WE WILL CALCULATE THE General Fees
     C*
     C                   if        EFTYPE = 'GEN' and EFEXCO <> 'Y'
     C                   MOVE      'Y'           @CAL_COLFEE       1
     C                   ENDIF
     C*
     C* " IF OVERRIDE GROUP VALUES SET AS 'Y' AND CLAIM FEE ALLOW OVERRIDE AT EMPLOYER
     C* LEVEL 'Y' THEN USE CLAIM FEE VALUES FROM WDEFP ELSE USE FROM WTFIP
     C*
     c                   if        FIAOEL <> 'Y'
     C                   eval      EFINBT = FIINBT
     C                   eval      EFINPN = FIINPN
     C                   eval      EFINFF = FIINFF
     C                   eval      EFMOBT = FIMOBT
     C                   eval      EFMOPN = FIMOPN
     C                   eval      EFMOFF = FIMOFF
     C                   eval      EFROBT = FIROBT
     C                   eval      EFROPN = FIROPN
     C                   eval      EFROFF = FIROFF
     C                   eval      EFALBT = FIALBT
     C                   eval      EFALPN = FIALPN
     C                   eval      EFALFF = FIALFF
     c                   endif
     C*
     C*" IF OVERRIDE GROUP VALUES SET AS 'Y' AND COLLATERAL ALLOW OVERRIDE AT EMPLOYER
     C*LEVEL 'Y' THEN USE COLATERAL FEE VALUES FROM WDEFP ELSE USE FROM WTFIP
     C*
     c                   if        FICAOE <> 'Y'
     C                   eval      EFCOBS = FICOBB
     C                   eval      EFCOMA = FIMNCA
     C                   eval      EFCOPN = FICOPN
     C                   eval      EFCOFF = FICOFF
     c                   endif
     C*
     C*" CLAIMS ONLY GRPS  (WMFNP/FNCLMS) DO NOT CALCULATE COLLATERAL FEE IF BILL BASED ON IS E -
     C*% OF ESTIMATED PREIUM  OR P - % OF PAYROLL.
     C*
     C                   If        (FNCLMS ='Y') and (EFCOBS='E' or EFCOBS='P')
     C                   Move      *blanks       @CAL_COLFEE
     C                   endif
     C*
     C* Do not calculate claim fee for the minimum collateral calculation run.
     C*
     C                   If        minichk
     C                   Move      *blanks       @CAL_CLMFEE
     C                   endif
     C
     C                   If        @CAL_CLMFEE  = 'Y'
     C                   CLEAR                   CLMFEE
     C                   EXSR      GETFEE
     C                   endif
     C
     C* CHECK THE STOP COLATERAL FLAG AT EMPLYER LEVEL(ELSTCO)
     C
     C                   IF        ELSTCO = ' '  AND @CAL_COLFEE  = 'Y'
     C                   CLEAR                   COLFEE
     C                   if        not StopCollateral(x7PoDt)
     C                   EXSR      GETCOL
     c                   endif

     C                   ENDIF

     C                   ENDSR
      /EJECT
     C*====================================================================
     C* EMP_BREAK SUBROUTINE              ** SUBROUTINE **   E M P B R K *
     C*====================================================================
     C     EMP_BREAK     BEGSR
     C*
     c                   exsr      EMPBRK
     C*
     c                   exsr      EMPBRK2
     C*
     C                   eval      saveEFEMP# = EFEMP#
     C                   eval      saveEFDIV  = EFDIV
     C                   eval      saveType   = EFTYPE
     C                   eval      saveDesc   = EFDESC
     C                   eval      saveEFFYR  = EFFYR
     C*
     C                   ENDSR
     C*

      /EJECT
     C*====================================================================
     C* EMPBRK SUBROUTINE              ** SUBROUTINE **   E M P B R K *
     C*====================================================================
     C     EMPBRK        BEGSR
     C*
     C*  WRITE A/R BATCH RECORD FOR THE EMPLOYER, IF THIS IS NOT THE FIRST
     C*  TIME THRU
     C*
     C     saveEFEMP#    IFNE      *ZEROS
     C                   Z-ADD     1             Z
     C                   Z-ADD     FYR(Z)        XXBFYR
     C     XXBFYR        DOWNE     0
     C                   Z-ADD     *ZEROS        EAAMT
     C                   Z-ADD     *ZEROS        EACKAM
     C*
     C*  Get the A/R billing code
     C                   clear                   FTBLAR
     C     keyWTFTL      chain     WTFTL
     C                   eval      xFeeCod = FTBLAR
     C*
     C* FIRST WRITE CLAIM FEE A/R RECORD
     C     EAKEY1        CHAIN     PEA02                              30
     C*
     C     *IN30         DOWEQ     *OFF
     C     EAARCD        ANDNE     xFeeCod
     c     *in30         OREQ      *OFF
     C     EAACHF        andne     'F'
     C     EAKEY1        READE     PEA02                                  30
     C                   ENDDO                                                  *IN30 = *OFF
     C*
     C     *IN30         IFEQ      '1'
     C     EATFFL        OREQ      'Y'
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     saveEFEMP#    EAEMP#
     C                   Z-ADD     saveEFDIV     EADIV
     C*
     C* OVERRIDE DIV IF PEO & SPECIFIC CLIENT
     C*
     C     FNPEOF        IFNE      *BLANKS
     C     @@CLT#        ANDEQ     '135'
     C                   Z-ADD     *ZEROS        EADIV
     C                   Z-ADD     saveEFDIV     EARDIV
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      XXSYCD        EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXBFYR        EAFYR
     C                   MOVEL     xFeeCod       EAARCD
     C*
     C                   MOVEL     xFeeCod       XXKEY
     C*
     C                   clear                   TTDESC
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTDESC        XXDESC
     C*
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL     XXDESC        EADESC
     C                   Z-ADD     *ZEROS        EABATA
     C                   Z-ADD     0             EAAMT
     C                   Z-ADD     0             EACKAM
     C                   ENDIF

     c                   Z-ADD     0             XXWK12           11 2
     C                   select
     C                   when      SaveType = 'CLF'
     C                   Z-ADD     CLMAMT(Z)     XXWK12
     C                   when      SaveType = 'GEN'
     C                   Z-ADD     COLAMT(Z)     XXWK12
     C                   endsl
     C                   ADD       XXWK12        EAAMT
     C                   ADD       XXWK12        XXTOTB

     c                   eval      eaachf = 'F'

     c                   eval      eapodt = x7PoDt
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   eval      sysTime6 = sysTime6 + 1
     C                   eval      EATIME = sysTime6
     c                   eval      EADT   = sysDate7
     C*
     C*  IF THE A/R BATCH RECORD ALREADY EXISTS AND HAS NOT BEEN TRANSFERRED
     C*  THAN UPDATE OTHERWISE WRITE A NEW RECORD
     C*
     C                   select
     C*
     C                   when      not *in30 and EATFFL <> 'Y'
     c                   if        eaamt <> 0
     C                   UPDATE    PEA02
     c                   else
     c                   DELETE    PEA02
     c                   endif
     C*
     C                   when      EAAMT <> *zero and #IInv# <> *zero
     C                   eval      EAINV# = #IInv#
     C                   exsr      MaintFldsEA
     C                   WRITE     PEA02
     C                   MOVE      *ON           I@BILL
     C*
     C                   when      EAAMT <> *zero
     C                   EXSR      GETINV
     C                   EVAL      EAINV#  = #IINV#
     C*    UPDATE WDFCP WITH THE SAME INVOICE NUMBER
     C     FCKY          CHAIN     PFC02                              60
     C                   DOW       *IN60 = *OFF
     C                   EVAL      FCINV#  = EAINV#
     C                   UPDATE    PFC02
     C     FCKY          READE     PFC02                                  60
     C                   ENDDO
     C                   exsr      MaintFldsEA
     C                   WRITE     PEA02
     C                   MOVE      *ON           I@BILL
     C*
     C                   endsl
     C*
     C                   ADD       1             Z
     C                   Z-ADD     FYR(Z)        XXBFYR
     C*
     C                   enddo                                                  FYR,Z DOWNE
     C*
     C                   endif                                                  XXSVEM <> 0
     C*
     C                   Z-ADD     0             Z                 3 0
     C                   CLEAR                   FYR
     C                   CLEAR                   CLMAMT
     C                   CLEAR                   COLAMT
     C*
     C                   ENDSR
      /EJECT
     C*====================================================================
     C* EMPBRK2 SUBROUTINE              ** SUBROUTINE **   E M P B R K 2 *
     C*====================================================================
     C     EMPBRK2       BEGSR
     C*
     C*  Read over WDEFL1 for the state reporting division in order to
     C*  process minimum collateral charges for the entire policy.
     C*
     c                   if        efemp# <> saveEFEMP#
     C                             or %eof(wdefl2)
     C*
     c                   move      dsWDEFP       saveWDEFP
     C*
     c                   eval      xsDIV  = GetStRptDv(XXCO#:XXFND:XXEMP#)
     C*
     c                   move      *on           minichk
     C*
     c     keyWDEFL1     chain     pef02                              60
     C*
     c                   eval      saveEFEMP# = efemp#
     c                   eval      saveEFDIV  = efdiv
     c                   eval      saveEFFYR  = effyr
     c                   eval      saveType   = eftype
     c                   eval      saveDesc   = efdesc
     C*
     C* Loop over all records for the state reporting division, breaking on each FYR.
     C*
     c                   dow       *in60 = *off
     C*
     c                   if        effyr <> saveEFFYR
     c                   exsr      FYRBRK
     c                   endif
     C*
     c                   clear                   pcm01
     c                   z-add     0             cmcase
     C*
     C                   eval      xxCo#  = EFCO#
     C                   eval      xxFnd  = EFFND
     C                   eval      xxEmp# = EFEMP#
     C                   eval      xxDiv  = EFDIV
     C                   eval      xxFyr  = EFFYR
     C*
     C                   EXSR      FEECOL
     C
     C                   Z-ADD     XXFYR         XXBFYR
     C                   ADD       1             P                 3 0
     C                   MOVE      totalcol      COL(P)
     c
     c                   if        totalcol <> 0
     c                   eval      saveType   = eftype
     c                   eval      saveDesc   = efdesc
     c                   endif
     C*
     c     keyWDEFL1     reade     pef02                                  60
     c                   enddo
     C*
     c                   exsr      FYRBRK
     c                   exsr      EMPBRK
     c
     c                   move      *off          minichk
     C*
     c                   move      saveWDEFP     dsWDEFP
     C*
     c                   endif                                                  efemp# <> saveEFEMP#
     C*
     C                   ENDSR
      /EJECT
     C*====================================================================
     C* GETFEE SUBROUTINE              ** SUBROUTINE **   G E T F E E *
     C*====================================================================
     C     GETFEE        BEGSR
     C*
     c                   eval      clmfee =
     c                             calculate(' ':efalbt:efalpn:efalff)
     c                   eval      xxbt = efalbt
     C                   exsr      wrtfil
     C*
     c                   eval      clmfee =
     c                             calculate('R':efrobt:efropn:efroff)
     c                   eval      xxbt = efrobt
     C                   exsr      wrtfil
     C*
     c                   eval      clmfee =
     c                             calculate('I':efinbt:efinpn:efinff)
     c                   eval      xxbt = efinbt
     C                   exsr      wrtfil
     C*
     c                   eval      clmfee =
     c                             calculate('M':efmobt:efmopn:efmoff)
     c                   eval      xxbt = efmobt
     C                   exsr      wrtfil


     C                   ENDSR

      /EJECT
     C*====================================================================
     C* GETCOL SUBROUTINE              ** SUBROUTINE **   G E T C O L *
     C*====================================================================
     C     GETCOL        BEGSR
     C*
     C                   eval      colType = EFCOBS
     C*
     c                   eval      colfee =
     c                             calculate(' ':efcobs:efcopn:
     c                                       efcoff:efcoma)
     c                   eval      xxbt = efcobs
     c*
     c* Override calculated collateral to be instead based on claims fees if
     c* flagged at group.
     c*
     c                   if        ficrcf = 'Y' and not minichk
     c
     c                   z-add     0             colfee
     c
     C     FCKY4         SETLL     PFC02
     C     FCKY4         READE     PFC02
     C
     C                   DOW       NOT %EOF
     c
     c                   select
     c
     c                   when      xxbt = 'I' or xxbt = 'F' or xxbt = 'C'
     c                   if        bfccase = cmcase
     C                   ADD       BFCCFCB       colfee
     c                   endif
     c
     c                   when      xxbt = 'P' or xxbt = 'E'
     C                   ADD       BFCCFCB       colfee
     c
     c                   endsl
     c
     C     FCKY4         READE     PFC02
     C                   ENDDO
     c
     c                   endif

     C                   exsr      wrtfil

     C                   ENDSR

      /EJECT
     C*====================================================================
     C* Update WDEAP maintenance fields
     C*====================================================================
     C     MaintFldsEA   begsr
     C*
     C                   eval      EAIDT  = EADT
     C                   eval      EAIUSR = EAUSER
     C                   eval      EAIPGM = EAPGM
     C*
     C                   endsr

      /EJECT
     C*====================================================================
     C* WRTFIL SUBROUTINE              ** SUBROUTINE **   G E T C O L *
     C*====================================================================
     C     WRTFIL        BEGSR
     C*
     C                   if        colFee = *zero
     C                              and clmFee = *zero
     C                   leavesr
     C                   endif
     C*
     C                   select
     C                   when      clmFee <> *zero
     C                   eval      clmType = xxBt
     C                   when      colFee <> *zero
     C                   eval      colType = xxBt
     C                   endsl
     C*
     c                   clear                   pfc01
     C
     c                   eval      FCTYPE = EFTYPE
     c                   eval      FCDESC = EFDESC
     c                   eval      FCBT   = xxbt
     C
     c                   if        FICRCF = 'Y'
     c                   eval      FCBT   = 'R'
     c                   endif
     C
     c                   eval      FCASOF = x7LsDt
     C                   Z-ADD     XXCO#         FCCO#
     C                   Z-ADD     XXFND         FCFND
     C                   Z-ADD     XXFYR         FCBFYR
     C                   Z-ADD     XXEMP#        FCEMP#
     C                   Z-ADD     XXDIV         FCDIV
     C                   Z-ADD     XXFYR         FCFYR
     C                   Z-ADD     CMCASE        FCCASE
     C                   Z-ADD     x7PoDt        FCPODT
     C                   Z-ADD     CLMFEE        FCCFCH
     C                   Z-ADD     COLFEE        FCCOFE
     C                   Z-ADD     XXBAT#        FCBAT#
     C                   Z-ADD     x7PrDt        FCINDT
     C
     C                   eval      FCDT   = sysDate7
     C                   eval      FCTIME = TimeTo6(SystemTime)
     C                   eval      FCUSER = q1User
     C                   eval      FCPGM  = q1Pgm
     C*
     c                   eval      FCIDT  = FCDT
     C                   eval      FCITM  = FCTIME
     C                   eval      FCIUSR = FCUSER
     C                   eval      FCIPGM = FCPGM
     C
     C**  CALCULATE THE PREVIOUSLY BILLED AMOUNTS FOR COL FEE
     C
     c                   move      *off          priorRec
     C
     C                   CLEAR                   @TOTALB2         11 2          (COL TOTAL BILLED P)
     C
     C                   IF        (COLTYPE = 'C' and not minichk)               (CASE LEVEL)
     C                             OR (COLTYPE = 'I' and not minichk)
     C                             OR COLTYPE = 'F'
     C
     C*----------------------------
     C*WHEN COLATERAL IS  CASE LEVEL WE NEED TO BE SURE THAT THE SAME EMPLOYER HAS NOT BEEN PROSESS
     C*ED AT EMP LEVEL BEFORE FOR THAT COLATERAL. IF IT IS THEN CONSIDER THAT AMOUNT AS WELL
     C*----------------------------
     C                   EVAL      @@CASE = FCCASE
     C                   CLEAR                   FCCASE
     C                   CLEAR                   @TOTALB3
     C     FCKY1         SETLL     PFC03
     C     FCKY1         READE     PFC03                                  60

     C                   DOW       *IN60 = *OFF
     C                   ADD       AFCCOFB       @TOTALB3
     C     FCKY1         READE     PFC03                                  60
     C                   ENDDO
     C
     C* WRITE A NEGATIVE RECORD  FOR EMP LEVEL RECORD
     C
     C                   IF        @TOTALB3 <> 0
     C                   EXSR      NEGAR1
     C                   ENDIF
     C
     C                   EVAL      FCCASE = @@CASE
     C     FCKY1         SETLL     PFC03
     C     FCKY1         READE     PFC03                                  60
     C
     C                   DOW       *IN60 = *OFF
     C                   ADD       AFCCOFB       @TOTALB2
     c                   if        afcpodt < x7podt
     c                   move      *on           priorRec
     c                   endif
     C     FCKY1         READE     PFC03                                  60
     C                   ENDDO
     C
     C                   EVAL      FCCOFB  =  FCCOFE - @TOTALB2
     C
     C                   ELSE                                                   COLTYP <> 'C' 'I'
     C
     C                   IF        COLTYPE = 'E' OR  COLTYPE = 'P'               (YEAR LEVEL)
     C                             OR (COLTYPE = 'C' and minichk)
     C                             OR (COLTYPE = 'I' and minichk)
     C
     c                   if        not minichk
     C     FCKY2         SETLL     PFC02
     C     FCKY2         READE     PFC02
     C
     C                   DOW       NOT %EOF
     C                   ADD       BFCCOFB       @TOTALB2
     c                   if        bfcpodt < x7podt
     c                   move      *on           priorRec
     c                   endif
     C     FCKY2         READE     PFC02
     C                   ENDDO
     C
     c                   else
     C
     C     FCKY3         SETLL     PFC02
     C     FCKY3         READE     PFC02
     C
     C                   DOW       NOT %EOF
     c                   if        BFCFYR = FCFYR
     C                   ADD       BFCCOFB       @TOTALB2
     c                   endif
     C     FCKY3         READE     PFC02
     C                   ENDDO
     c                   endif
     C
     c                   if        minichk
     c
     c                   if        @TOTALB2 >= FCCOFE
     c                   leavesr
     c                   else
     c                   EVAL      FCCOFE = FCCOFE - @TOTALB2
     c                   clear                   @TOTALB2
     c                   ENDIF
     c
     c                   ENDIF                                                  if minichk
     c
     C                   EVAL      FCCOFB  =  FCCOFE - @TOTALB2
     C                   ENDIF
     C                   ENDIF
     C
     C**  CALCULATE THE PREVIOUSLY BILLED AMOUNTS FOR CLM FEE SIMILARLY
     C
     C                   CLEAR                   @TOTALB1         11 2
     C
     C                   IF        CLMTYPE = 'C' OR  CLMTYPE = 'I'
     C                             OR CLMTYPE = 'F'

     C     FCKY1         SETLL     PFC03
     C     FCKY1         READE     PFC03                                  60
     C
     C                   DOW       *IN60 = *OFF
     C                   ADD       AFCCFCB       @TOTALB1
     c                   if        afcpodt < x7podt
     c                   move      *on           priorRec
     c                   endif
     C     FCKY1         READE     PFC03                                  60
     C                   ENDDO
     C
     C                   EVAL      FCCFCB  =  FCCFCH - @TOTALB1
     C                   ENDIF                                                  CLTYPE = C, I, F
     C
     C*IF BOTH OF COLATERAL & CLAIM FEE BILLED AMOUNTS ARE ZERO NO NEED TO WRITE THE RECORD
     C
     C                   IF        CLMTYPE = ' '
     C                   EVAL      FCCFCB  = 0
     C                   ENDIF
     C
     C                   IF        COLTYPE = ' '
     C                   EVAL      FCCOFB  = 0
     C                   ENDIF
     C
     C                   If        FCCFCB  = 0  AND  FCCOFB = 0
     c                   if        (priorRec and coltype = 'C') or
     c                             (priorRec and coltype = 'I')
     c* Only write 0 record in these cases, otherwise leave subroutine.
     c                   else
     C                   leavesr
     c                   endif
     C                   ENDIF

     C**COMPARE THE LEVELS
     C* COLLATERAL APPLIED AT CLAIM LEVEL, CLAIM FEE IS ALWAYS AT CLAIM LEVEL

     C*******************************************************************
     C*WRITE COLATERAL FEE  AT "EMP" LEVEL AND  CLAIM FEE AT "CASE" LEVEL
     C*******************************************************************
     C
     C                   IF        (COLTYPE = 'E' OR  COLTYPE = 'P')
     C
     C* FOR WRITING COLATERAL AT "EMP" LEVEL ----- MAKE CLAIM BILLED AS ZERO ALSO
     C*                                      ----- MAKE CASE NO AS ZERO
     C*
     C                   EVAL      @@CFCB  = FCCFCB
     C                   EVAL      @@CASE  = FCCASE
     C                   CLEAR                   FCCFCB
     C                   CLEAR                   FCCASE

     C                   IF        FCCOFB <> 0
     C                   WRITE     PFC01
     C                   add       fccofb        totalcol
     C                   ENDIF
     C*
     C* FOR WRITING CLAIM RCD AT CASE LEVEL  ----- MAKE COLATERAL AS ZERO
     C*
     C                   EVAL      @@COFB  = FCCOFB
     C                   EVAL      FCCOFB  = 0
     C                   EVAL      FCCFCB  = @@CFCB
     C                   EVAL      FCCASE  = @@CASE
     C
     C                   IF        FCCFCB <> 0
     C                   WRITE     PFC01
     C                   add       fccfcb        totalclm
     C                   ENDIF
     C
     C                   EVAL      FCCOFB  = @@COFB
     C                   leavesr
     C                   ENDIF
     C
     C                   WRITE     PFC01
     C                   add       fccfcb        totalclm
     C                   add       fccofb        totalcol
     C                   ENDSR

      /EJECT
     C*====================================================================
     C* FYRBRK SUBROUTINE              ** SUBROUTINE **   G E T C O L *
     C*====================================================================
     C     FYRBRK        BEGSR
     C
     C                   ADD       1             Z
     C                   Z-ADD     saveEFFYR     FYR(Z)
     C*
     C* WRITE THE ACCUMULATED VALUE OF CLAIM  FEE & COLATERAL AT THE YEAR LEVEL IN THE YEAR LEVEL
     C* ARRAY
     C*
     C                   XFOOT     CLM           CLMAMT(Z)
     C                   XFOOT     COL           COLAMT(Z)
     C                   CLEAR                   CLM
     C                   CLEAR                   COL
     C                   CLEAR                   P
     C*
     C                   eval      saveEFFYR  = EFFYR
     C*
     C                   ENDSR

      /EJECT
     C*====================================================================
      * NEGAR1- PROCESS NEGATIVE PAYMENT FOR COLATERAL
     C*====================================================================
     C     NEGAR1        BEGSR
     C*
     C                   EVAL      @@CFCB  = FCCFCB
     C                   EVAL      @@COFE  = FCCOFE
     C*
     C                   EVAL      FCCOFE = -(@TOTALB3)
     C                   EVAL      FCCOFB = -(@TOTALB3)
     C                   CLEAR                   FCCFCB
     C                   CLEAR                   FCCASE
     C                   WRITE     PFC01
     C                   add       fccofb        totalcol
     C*
     C                   EVAL         FCCFCB  = @@CFCB
     C                   EVAL         FCCOFE  = @@COFE
     C                   ENDSR
      /EJECT

      /EJECT
      *====================================================================
      * Calculate
      *====================================================================
     PCalculate        b
     DCalculate        pi            11s 2
     D clmLTFlag                      1a   value
     D billType                       1a   value
     D percentage                     7s 4 value
     D flatFee                       11s 2 value
     D minimum                       11s 2 value options(*nopass)

     DAmount           s             11s 2


     c                   if        ((CMLTFL <> clmLTFlag)
     c                             and (clmLTFlag <> *blank))
     c                   return    *zero
     c                   endif

     c                   if        not minichk
     c
     c                   select
      * % of claims paid
     c                   when      billType = 'C'
     c                   eval      amount = %xfoot(cma) *
     c                                      (percentage / 100)
      * % of claims incurred
     c                   when      billType = 'I'
     c                   eval      amount = (%xfoot(cma) + %xfoot(cmr)) *
     c                                      (percentage / 100)
      * % of estimated premium
     c                   when      billType = 'E'
     c                   eval      amount = XXPREM * percentage / 100
      * % of payroll
     c                   when      billType = 'P'

     c                   eval      Amount = 0
     c     epky          setll     pep01
     c     epky          reade     pep01

     c                   dow       not %eof

     c                   if        elfaf = 'A'
     c                   eval      amount = amount + EPAAPR
     c                   else
     c                   eval      amount = amount + EPEAPR
     c                   endif

     c     epky          reade     pep01
     c                   enddo

     c                   eval      amount = amount * percentage / 100
      * Flat fee amount
     c                   when      billType = 'F'
     c                   eval      Amount = flatfee

     c                   endsl

     c                   else                                                   if not minichk
     c
     c                   if        (%parms >= 5) and
     c                             (Amount < minimum)
     c                   eval      Amount = minimum
     c                   endif

     c                   endif                                                  if not minichk

     c                   z-add(h)  Amount        xxwk11           11 0
     c                   z-add     xxwk11        Amount
     c                   return    Amount

     pCalculate        e




      /EJECT
      *====================================================================
      * Determine if collateral should be stopped based on date
      *====================================================================
     PStopCollateral   b
     DStopCollateral   pi             1n
     D inDate                         7  0 const

     C                   select
     c                   when      EFSCBD <> *zero and EFSCBD <= inDate
     C                   return    *on
     c                   when      F1SCDT <> *zero and F1SCDT <= inDate
     C                   return    *on
     c                   when      ELSCDT <> *zero and ELSCDT <= inDate
     C                   return    *on
     c                   when      stRptWDELP.ELSTCO = 'Y'
     C                              and stRptWDELP.ELSCDT <> *zero
     C                              and stRptWDELP.ELSCDT <= inDate
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl

     pStopCollateral   e




      /EJECT
      *====================================================================
      * ValidClaim - Function returns *on if claim is valid.
      *====================================================================
     PValidClaim       b
     DValidClaim       pi             1n

      *  DO NOT INCLUDE THE CLAIM IF IT HAS BEEN DELETED, TRANSFERRED
      *  TO ANOTHER CLAIM #, OR TRANSFERRED TO ANOTHER EMPLOYER.

     C     CTKY          CHAIN     PCT01                              60

     C     *IN60         DOWEQ     '0'

     C*  Do not include the claim only if the transfer was processed
     C*    in the current month.

     C                   MOVEL     CTDT          XXCYM2            5 0
     C     XXCYM1        IFEQ      XXCYM2

     C     CTDLTR        IFNE      *BLANK
     C     CTFYR         ORNE      CMFYR
     C     CTCASE        ORNE      CMCASE
     C     CTEMP#        ORNE      CMEMP#
     C     CTDIV         ORNE      CMDIV
     C                   return    *off
     C                   ENDif

     C                   ENDif

     C     CTKY          READE     PCT01                                  60
     C                   END

     C*  Do not include the claim if it was transferred using the
     C*    Group to Group transfer process in the current month.

     C     CTKY          CHAIN     PET01                              60

     C     *IN60         DOWEQ     '0'
     C                   MOVEL     ETDT          XXCYM2

     C     XXCYM1        IFEQ      XXCYM2
     C                   return    *off
     C                   END

     C     CTKY          READE     PET01                                  60
     C                   END

     C                   return    *on

     pValidClaim       e
