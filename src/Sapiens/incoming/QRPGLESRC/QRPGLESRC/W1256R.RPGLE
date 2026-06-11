     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H/TITLE W1256R - CREATE EFT BATCH ENTRIES
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H BndDir('WTBNDDIR')
     H**************************************************************************
     H*     Program description:                                               *
     H*        This program creates batch entries for EFT                      *
     H*        employers based upon criteria set up at the group and           *
     H*        employer levels.  Option 12 in W/W A/R Batch Entries            *
     H*        will created the transmission files. The EFT                    *
     H*        history file will be written.                                   *
     H*                                                                        *
     H* CALLED FROM:                                                           *
     H*   W1256C                                                               *
     H* CALLS TO:                                                              *
     H*   W9200C                                                               *
     H*                                                                        *
     H* PROGRAMMER: LARRY KORZON                                               *
     H* CREATED: 12/18/91               PROJECT: AKU                           *
     h* Rewritten 04/11/03 by C:136 for project WC1116                         *
     H**************************************************************************
     H* INDICATORS:                                                            *
     H*   30  ENTRY TO TRANSMIT                                                *
     H*   31  NEW BATCH# CREATED                                               *
     H*   32  PRENOTIFICATION TRANSMISSION                                     *
     H*   35  HEADING FOR CREDIT BALANCE REPORT                                *
     H*   40  UPDATE OR WRITE                                                  *
     H*   60  GENERAL CHAIN INDICATOR                                          *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*                                                                        *
     H* 05/09/25 C:300 Support: 118851                                         *
     H*                Create Dummy batch record with 99999999                 *
     H*                in Check number and Check amount                        *
     H*                Similar to a Master AR batch recod is created manually  *
     H*                in AR Batch Entry screen                                *
     H*                                                                                            *
     H* 06/24/24 C:079 Project: 146WKR
     H*                Collateral Processing - Recomile
     H*                                                                        *
     H* 11/08/23 C:062 Support: 114493(140)                                    *
     H*                Populate the category type.                             *
     H*                                                                        *
     H* 10/24/22 C:079 Support: 133/104008                                     *
     H*                Fix logic for deposit SR Online                         *
     H*                                                                        *
     H* 12/02/21 C:136 Support: 99810                                          *
     H*                Fix logic for inst fees                                 *
     H*                                                                        *
     H* 04/12/21 C:136 Project: WC1986                                         *
     H*                Use GetEftStatus for verification                       *
     H*                Exclude S/R policies                                    *
     H*                Exclude 3rd Party if deposit not included in balance.   *
     H*                                                                        *
     H* 08/05/15 C:307 Project: SY354                                          *
     H*                -Adding 'Route To:'                                     *
     H*                                                                        *
     H* 08/26/14 C:298 Project: SY354                                          *
     H*                -Implement Routing Page Changes.                        *
     H*                                                                        *
     h*  04/17/12 C:255 Project: WC2044                                                    *
     h*                 Replace "Direct Debit" with EFT.                                   *
     h*                 Call GetBankAcc in place of WA023R.                                *
     h*                                                                                    *
     H*  11/28/11 C:136 Support: WA099 78619                                   *
     H*                 Do not select audited policies for EFT per GW.         *
     H*                                                                        *
     H*  04/21/11 C:190 Project: 114WYI                                        *
     H*                 ACH for Premium Payments via Agent.                    *
     H*                                                                        *
     H*  08/09/10 C:191 Project: 71WKU                                         *
     H*                 Amount of each Retro Credit installment is subtracted  *
     H*                 frm corresponding prm installment written to A/R Batch *
     H*                                                                        *
     H*  04/09/10 C:191 Support: WA146 72036                                   *
     H*                 Use SETGT and READPE to get latest effective record    *
     H*                 from WMIFP.                                            *
     H*                                                                        *
     H* 01/28/10  C:249  Project: SY215                                        *
     H*           C:189  Retrieve brand Name                                   *
     H*                                                                        *
     H*  06/29/09 C:191 Project: WC1806                                        *
     H*                 -Change to use WMEFP instead of WMDBP                  *
     H*                 -Get A/R codes from Company Master                     *
     H*                 -Get ACH values from Company Master                    *
     H*                 -Get ACH interim days from Bank Master                 *
     H*                 -Remove pre-note processing                            *
     H*                                                                        *
     H*  01/07/09 C:136 Support: WA139 66641                                   *
     H*                 Added indicator 60 to chain to PDB01 so that run       *
     H*                 across groups would work without having a routing      *
     H*                 record.                                                *
     H*                                                                        *
     H*  06/12/08 C:136 Project: CI019                                         *
     H*                 Ensure that S/R policies are skipped.                  *
     H*                                                                        *
     H*  01/26/07 C:136 Support: WA129 56860                                   *
     H*                 Batch total not equal to written amount in batch       *
     H*                                                                        *
     H*  12/22/06 C:132 Project: 129WPH                                        *
     H*                 Modify the ACH processing to Exclude fee type/tax      *
     H*                 category A/R entries if the A/R code on transaction    *
     H*                 matches the default A/R code for Tax Billed on Audit.  *
     H*                                                                        *
     H*  11/22/04 C:136 Support: WA129 44736                                   *
     H*                 strDtErr flag not getting cleared for each year break. *
     H*                                                                        *
     H*  09/23/04 C:15  Project: 121WBJ                                        *
     H*                 Added code to pickup additional transaction types for  *
     H*                 the ACH transmission.                                  *
     H*                                                                        *
     H*  01/13/04 C:136 Project: WC1116                                        *
     H*                 Use the new debit expiration date if not zero to       *
     H*                 determine whether to capture a transaction or not.     *
     H*                                                                        *
     H*  12/30/03 C:136 Support: WA129 40579                                   *
     H*                 Correct the debit start date calculation for weekly    *
     H*                 billing cycle.                                         *
     H*                                                                        *
     H*  12/01/03 C:136 Support: 40157                                         *
     H*                 Correct year being written to batch if more than       *
     H*                 one year exists.                                       *
     H*                                                                        *
     H*  09/30/03 C:136 Support: 39276 (WC1116)                                *
     H*                 If called from job scheduler, use the previous day     *
     H*                 as the run date if the time is before 5:00PM.          *
     H*                                                                        *
     H*  09/24/03 C:136 Support: 39075 (WC1116)                                *
     H*                 Add processing for initial maintenance fields.         *
     H*                 Use the runDate field as the initial date field.       *
     H*                                                                        *
     H*  07/07/03 C:136 Project: WC1116                                        *
     H*                 Exclude test groups from ACH processing.               *
     H*                                                                        *
     H*  06/30/03 C:136 Project: WC1116                                        *
     H*                 If no records are selected for ACH transmission,       *
     H*                 create a *zero batch record per KK.                    *
     H*                                                                        *
     H*  06/09/03 C:136 PROJECT: WC1116                                        *
     H*                 Correct calculation of debit start for monthly billing *
     H*                 plans. Change run date to equal system date if         *
     H*                 called from job scheduler.                             *
     H**************************************************************************
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMEFL7    IF   E           K DISK
     FWSCKL     IF   E           K DISK
     fwmeal7    if   e           k disk
     fwmexl     if   e           k disk
     Fwmdhp     o    e             disk
     fwdeal     if   e           k disk    rename(pea01:pea02) prefix(dl_)
     fwdeal4    uf a e           k disk    rename(pea01:pea03) prefix(do_)
     FWTABL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     Fwmifl     if   e           k disk
     Fwmrsl     if   e           k disk    rename(prs01:prs02)                  rate set code
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*
     d toPODT          s                   like(EAPODT)
     d transDate       s                   like(EATRDT)
     d grpARCD         s                   like(EAARCD)
     d arCdDsc         s                   like(TTDESC)
     d svfnd           s                   like(EAFND)
     d runDate         s              7  0
     d runDt8          s              8  0
     d toPODT8         s              8  0
     d pexFlag         s               n
     d pdcFlag         s               n
     d xxco#           s                   like(ELCO#)
     d xxfnd           s                   like(ELFND)
     d x1fnd           s                   like(ELFND)
     d xxfyr           s                   like(ELFYR)
     d xxemp#          s                   like(ELEMP#)
     d xxbat#          s                   like(do_EABAT#)
     d xxdiv           s                   like(ELDIV)
     d xxkey           s                   like(BSKEY)
     d xxpodt          s                   like(DHPODT)
     d xxamt           s                   like(EAAMT)
     d xxval1          s                   like(do_EABAT#)
     d xxval2          s                   like(do_EABAT#)
     d accumBatTot     s                   like(do_EABATA)
     d svipgm          s                   like(do_eaipgm)
     d sviusr          s                   like(do_eaiusr)
     d svfyr           s                   like(do_eafyr)
     d batchCount      s              7  0
     d q@dt            s                   like(eadt)
     d elbsdt8         s              8  0
     d bsdtDOW4        s              4
     d bsdtDOW2        s              2
     d instDOW2        s              2
     d instStrDt8      s              8  0
     d debStrDt8       s              8  0
     d t1              s              1  0
     d t2              s              1  0
     d dowDiff         s              5  0
     d numofDays       s              5  0
     d strDtErr        s               n
     d startDate       s               d
     dtbaCode          s                   like(EXTBAC)

     d                 ds
     d debStrDt                       7  0
     d debYear                        3  0 overlay(debStrDt)
     d debMth                         2  0 overlay(debStrDt:4)
     d debDay                         2  0 overlay(debStrDt:6)

     d pren            s             30    dim(1) ctdata perrcd(1)
     d day             s              2a   dim(7) ctdata perrcd(7)
     d amount          s              9  2 dim(13)
     d ar_code         s              2a   dim(13)
     d ar_desc         s             30a   dim(13)
     d x               s              3  0
     d xxifbl          s                   like(ififbl)
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     d/copy scopypr01d
     d/copy SPRDATE
     d/copy SPRSTRING
     D/copy WPRBANK
     D*** Begin Add *** 11/08/23 *******************************************
     D/copy WPRAR
     D*** End   Add *** 11/08/23 *******************************************
     d ##co#           s              3
     d ##fnd           s              3
     d ##appl          s              2
     d ##srdt          s              8
     d ##acct          s             20
     d ##call          s              1
     d ##hdcy          s              1
     d ##chkq          s             10
     d ##ftyp          s             10
     d ##rtcd          s              1
     d ##efdt          s              7
     d ##cpyq          s             10
     d ##cktp          s             10
     d ##coid          s             20
     d ##eftc          s              1
       //*** beg add ***** 04/12/21 **************************
       Dcl-S chrType Char(1) Inz('E');
       Dcl-S chrEnv  Char(1);
       Dcl-S chrSrdFlg  Char(1);
       //*** end add ***** 04/12/21 **************************
     C*=========================================================================
     C*   KLIST
     C*=========================================================================
     C*   user parm file
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     coky          klist
     C                   kfld                    xxco#

     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C*
     C     efky          klist
     C                   kfld                    xxtype
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    x1key            30
     C*
     C     efky1         klist
     C                   kfld                    xxtype
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C*
     C     efky2         klist
     C                   kfld                    xxtype
     C                   kfld                    xxco#
     C*
     C*
     C     dbky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C*
     C     tbky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxkey
     C*
     C     eaky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxbat#
     C*
     C     eaky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxstat            1
     C*
     C     eaky2         klist
     C                   kfld                    xxco#
     C                   kfld                    xxbat#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     c                   kfld                    svfyr
     C*
     c     eaky9         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c     f1ky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    eafyr
      *
     c     elky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    EAFYR
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     rsky          klist
     C                   kfld                    xxco#
     C                   kfld                    elrtcd
     C*
     c     ifky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    rsjrst
     c                   kfld                    elpfdt
     c*
     c     ifky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    rsjrst
     c*
     C     biky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxappl            2
     C                   kfld                    xxcktp           10
     C                   kfld                    xxfnd
     C*
     C     ckky          klist
     C                   kfld                    xxco#
     C                   kfld                    ##acct
      *
      *
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRNDT            8
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXAMT
     C                   Z-ADD     0             XXTENT            4 0
     C                   Z-ADD     0             XXPREN            4 0
     C                   MOVE      'E'           XXTYPE            1
      *
      * if called interactively, qqrndt will have a value; otherwise the current day will be used if
      * called from job scheduler.
      *
     c                   move      qqco#         xxco#
     c                   move      qqfnd         x1fnd

     c                   if        qqrndt <> *blanks
     c                   move      qqrndt        runDt8
     c                   eval      runDate = dtCYMD(runDt8)
     c                   else
     c                   time                    dsdttm
     c                   if        dstime < 170000

     c                   call      'SRPAST8'
     c                   parm      dsdate        runDt8
     c                   parm      1             numdays           5 0
     c                   parm                    err               1

     c                   eval      runDate = dtCYMD(runDt8)
     c                   else

     c                   eval      runDt8  = dsdate
     c                   eval      runDate = dtCYMD(runDt8)
     c                   endif
     c                   endif
     c
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     coky          chain     pco01                              60
     C                   MOVE      CONAME        XXCONM           40
      *
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
      *

     c                   clear                   ar_code
     c                   clear                   ar_desc

     c* use Company master file now
     c                   eval      ar_code(1) = COARCD
     c                   eval      ar_code(2) = COECAR
     c                   eval      ar_code(3) = COIFAR
     c                   eval      ar_code(4) = COMFAR
     c                   eval      ar_code(5) = COSFAR
     c                   eval      ar_code(6) = COTXAR
     c                   eval      ar_code(7) = CO1AAR
     c                   eval      ar_code(8) = CO2AAR
     c                   eval      ar_code(9) = CO3AAR
     c                   eval      ar_code(10) = CO4AAR
     c                   eval      ar_code(11) = CO5AAR
     c                   eval      ar_code(12) = CO6AAR
     c                   eval      ar_code(13) = CO7AAR

     c                   do        13            x

     c                   eval      xxkey = ar_code(x)
     c     tbky          chain     ptt01
     c                   if        %found
     c                   eval      ar_desc(x) = TTDESC
     c                   endif                                                  if ARCD in table

     c                   enddo

     C*
     C*   CHAIN TO THE FUND FILE
     C*
     c                   if        qqfnd <> *zeros and
     c                             qqfnd <> *blanks
     c                   move      qqfnd         x1fnd
     c                   eval      xxfnd = x1fnd
     c                   move      qqfnd         x2fnd             4
     C     fnky          chain     pfn01                              60
     c                   else
     c                   eval      xxfnd = 0
     c                   move      '*ALL'        x2fnd
     c                   move      *blanks       FNNAME
     c                   endif
      *
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C************       EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN35
     C*
     C*   Read the Direct Debit Employer's File
     C*
     c                   if        x1fnd <> 0
     C* Change process to read thru EFT control file
     C     efky1         chain     pef01                              60
     c                   else
     c     efky2         chain     pef01                              60
     c                   endif                                                  xxfnd <> 0

     C                   dow       not *in60

     c                   eval      xxfnd  = EFFND
     c                   eval      xxemp# = EFEMP#
     c                   eval      xxdiv  = EFDIV
     c                   eval      x1key  = EFKEY
      *
     c     efky          setgt     pef01
     c                   dou       runDate >= EFEFDT or %eof
     c     efky          readpe    pef01
     c                   enddo

     c                   if        not %eof and efexdt = 0 or
     c                             not %eof and runDate < efexdt
      *
     c     fnky          chain     pfn01
     c                   if        %found and FNTEST <> 'Y'
      *
     c                   if        xxfnd <> svfnd
     c                   eval      svfnd = xxfnd
      *
     c                   eval      pexFlag = *off
     c                   eval      tbaCode = *blanks
     c     fnky          chain     pex01
      * Retrieve bank account number using API WA023R
     C                   move      xxco#         ##co#
     C                   move      xxfnd         ##fnd
     C                   move      rundt8        ##srdt
     C                   eval      ##appl = 'WP'
     C                   eval      ##cktp = 'EFT'
      *
      /Free
       ##acct = *Blanks;
       ##call = *Blanks;
       ##hdcy = *Blanks;
       ##chkq = *Blanks;
       ##ftyp = *Blanks;
       ##rtcd = *Blanks;
       ##efdt = *Zeros ;
       ##cpyq = *Blanks;
       ##cktp = *Blanks;
       ##coid = *Blanks;
       ##eftc = 'E'    ;

       ##acct= GetBankAcc(##co#: ##fnd: ##appl: ##srdt: ##call:
                ##hdcy: ##chkq: ##ftyp: ##efdt: ##cpyq:
                ##cktp: ##eftc: ##coid: ##rtcd);
      /End-Free
     C*
     C                   if        ##rtcd <> '1'
      * Retrieve EFT interim days from Bank account master
     C     ckky          chain     pck01
     c                   if        %found
     c                   eval      toPODT8 = dtFuture(runDt8:CKACID)
     c                   eval      toPODT  = dtCYMD(toPODT8)
     c                   eval      tbaCode = EXTBAC
     c                   eval      pexFlag = *on
     c                   endif                                                  %found
     c                   endif                                                  ##rtcd<>'1'

      * Make sure group EFT Control Info is set up.
     c
     c                   endif                                                  xxfnd <> svfnd
     c*
     c*  Clear work fields
     c*
     c                   eval      xxamt = 0
     c                   clear                   amount
     c*
     c*  If prenotification sent and group info is set up and employer info is set up,
     c*  accumulate AR amounts and write batch.
     c*
     c                   if        (efpdt <> 0  or ckbypn ='Y')
     c                              and pexFlag = *on

     c                   eval      svfyr = 0
     c                   eval      batchCount = 0

     c     eaky9         chain     pea01                              60
     c                   dow       not *in60

     c                   if        EAFYR <> svfyr

     c                   if        batchCount > 0
     c                   eval      batchCount = 0
     c                   exsr      wrtBatch
     c                   eval      xxamt = 0
     c                   clear                   amount
     c                   endif

     c                   eval      svfyr = EAFYR
     c                   exsr      fyrBrk
     c                   endif                                                  eafyr <> svfyr
      *
     c                   if        tbaCode <> *BLANKS and
     c                             tbaCode <> EAARCD  or
     c                             tbaCode =  *BLANKS

     c                   if        EAPODT >= runDate  and
     c                             EAPODT <= toPODT   and
     c                             EATRDT  = runDate   or
     c                             EAPODT  = toPODT   and
     c                             EATRDT <> runDate
      *
      * Get the employer's direct debit start date.  Installments on or after this date will be
      * written to A/R batch.

     c                   if        not strDtErr

     c                   select

     c                   when      EAARTY = 'B' and EAPODT >= debStrDt
     c                             and EACTYP <> 'E'
     c                             or EAARTY = 'A' and EAPODT >= debStrDt
     c                   add       EAAMT         amount(1)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARCD = F1RCCD and EAPODT >= debStrDt
     c                   sub       EAAMT         amount(1)
     c                   sub       EAAMT         xxamt
     c                   when      EAARTY = 'B' and EACTYP = 'E'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(2) <> *blanks
     c                   add       EAAMT         amount(2)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EAARCD = xxifbl
     c                             and EAPODT >= debStrDt
     c                             and ar_code(3) <> *blanks
     c                   add       EAAMT         amount(3)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = 'M'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(4) <> *blanks
     c                   add       EAAMT         amount(4)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = 'S'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(5) <> *blanks
     c                   add       EAAMT         amount(5)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = 'T'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(6) <> *blanks
     c                   add       EAAMT         amount(6)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = '1'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(7) <> *blanks
     c                   add       EAAMT         amount(7)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = '2'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(8) <> *blanks
     c                   add       EAAMT         amount(8)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = '3'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(9) <> *blanks
     c                   add       EAAMT         amount(9)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = '4'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(10) <> *blanks
     c                   add       EAAMT         amount(10)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = '5'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(11) <> *blanks
     c                   add       EAAMT         amount(11)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = '6'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(12) <> *blanks
     c                   add       EAAMT         amount(12)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   when      EAARTY = 'F' and EACTYP = '7'
     c                             and EAPODT >= debStrDt
     c                             and ar_code(13) <> *blanks
     c                   add       EAAMT         amount(13)
     c                   add       EAAMT         xxamt
     c                   add       1             batchCount

     c                   endsl

     c                   endif                                                  not strDt Err


     c                   endif                                                  EAPODT >= runDate
      *
     c                   EndIf                                                  tbaCode <> *BLANKS

     c     eaky9         reade     pea01                                  60

     c                   if        *in60 and batchCount > 0
     c                   eval      batchCount = 0
     c                   exsr      wrtBatch
     c                   eval      xxamt = 0
     c                   clear                   amount
     c                   endif                                                  *in60

     c                   enddo                                                  looping A/R Batch

     c                   endif                                                  DBPRED <> 0
     C                   end                                                    fntest <> 'y'
      *
     C                   end                                                    DBSTAT = 'A'

     c     efky          setgt     pef01

     c                   if        x1fnd = *zero
     c     efky2         reade     pef01                                  60
     c                   else
     C     efky1         reade     pef01                                  60
     c                   endif                                                  xxfnd <> 0

     c                   enddo                                                  *in60 = *off

     c                   if        not *in30
     c                   exsr      srcrtd
     c                   eval      xxtent = 1
     c                   seton                                        3031

     c     eaky          chain     pea03
     c                   eval      do_EATFAP = 'N'
     c                   eval      do_EATFFL = 'N'
     c                   eval      do_EAACHF = 'A'
     c                   update    pea03
     c                   endif

     c                   if        xxbat# <> 0
     c     eaky          chain     pea03                              60
     c                   dow       not *in60
     c                   eval      do_EABATA = accumBatTot
     c                   update    pea03
     c     eaky          reade     pea03                                  60
     c                   enddo
     c                   endif

     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   except    detail

     C     *IN30         IFEQ      '1'
     C                   MOVE      XXBAT#        QQBAT#            9
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W9200C'
     C                   PARM                    QQCO#
     C                   PARM                    QQBAT#
     C                   PARM                    QQOUTQ
     C                   END

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
         xxPrt = '0';
         Chain orKey POR01;
         If %found;
            Except Route;
         Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   seton                                        lr

      *** beg chg ***** 04/12/21 **************************
     C*=========================================================================
     C* fyrBrk - subroutine to process a break in FYR
     C*=========================================================================
     c***     fyrBrk        begsr
      ****
      **** Get the employer's direct debit start date.  Installments on or after this date will be
      **** written to A/R batch.
      ****
     c***     f1ky          chain     pf101                              60
     c***                   eval      strDtErr = *off
     c***     elky          chain     pel01
     c***                   if        not %found
     c***                   eval      strDtErr = *on
     c***                   else
     c***                   if        elaeft =  'A'
     c***                   if        efeatp = 'AG'
     c***                   eval      do_eaeatp = 'AG'
     c***                   else
     c**** Call WA023R3 to validate active EFT.
     C***                   time                    dsdttm
     C***                   eval      q@dt = dtCYMD(dsdate)
     C***                   move      q@dt          q3dt
      ***
     C***                   move      elco#         q3co#             3
     C***                   move      elfnd         q3fnd
     C***                   movel(p)  elemp#        q3emp#            9
     C***                   movel(p)  eldiv         q3div             5
     c***     q3emp#        cat(p)    q3div:0       xxwk30           30
     c***     xxwk30        cat(p)    'AG':0        xxwk30
     c****
     C***                   call      'WA023R3'
     C***                   PARM                    Q3CO#
     C***                   PARM      'E'           Q3TYPE            1
     C***                   PARM                    Q3FND             3
     C***                   PARM      xxwk30        Q3KEY            30
     C***                   PARM                    Q3DT              7
     C***                   PARM      ' '           Q3EFT             1
     C***                   PARM      ' '           Q3PRE             1
     c***                   if        q3eft = *on
     c***                   eval      do_eaeatp = 'AG'
     c***                   else
     c***                   eval      do_eaeatp = '  '
     c***                   endif                                                  q3eft = *on
     c***                   endif                                                  efeatp = 'AG'
     c***                   else                                                   elaeft =  'A'
     c***                   eval      do_eaeatp = '  '
     c***                   endif                                                  elaeft =  'A'
     c***                   movel(p)  ELBSCD        xxkey
     c***     tbky          chain     pbs01
     c***                   if        not %found
     c***                             or (%found and ELFAF = 'A')
     c***                   eval      strDtErr = *on
     c***                   else
      ***
     c***                   if        BSSEFL = 'E'
     c***                   exsr      getStrDt
     C***                   clear                   xxifbl
     C***     rsky          chain     prs02                              60
     C***     ifky          setgt     pif01                              60
     C***     ifky1         readpe    pif01                                  60
     C***  n60              eval      xxifbl = ififbl
     c***                   else
     c***                   eval      strDtErr = *on
     c***                   endif
      ***
     c***                   endif
     c***                   endif
      ***
     c***                   endsr
       Begsr FyrBrk;
        Chain elky pel01;
        strDtErr = *Off;
       // Skip if Audited
        If ELfaf = 'A';
           strDterr = *On;
           LeaveSr;
        Endif;
       // Skip if Self-Reporting
        xxKey = %Editc(ELbscd:'X');
        Chain tbKy pbs01;
        If BSsefl = 'S';
           strDtErr = *On;
           LeaveSr;
        Endif;
       // Skip transaction if deposit not included in the
       // online A/R Balance Calc and active on 3rd Party.
        If EXindp = 'Y';
           //*** Beg Add *** 10/24/22 *************************************
           //***strDtErr = Not(Has3rdPartyAcct(ELco#:ELfnd:ELemp#:
           //*** ELdiv:chrType:runDate:ELfyr:chrEnv:chrSrdFlg));
           strDtErr = (Has3rdPartyAcct(ELco#:ELfnd:ELemp#:
            ELdiv:chrType:runDate:ELfyr:chrEnv:chrSrdFlg));
           //*** End Add *** 10/24/22 *************************************
           If strDtErr = *On;
              LeaveSr;
           Endif;
        Endif;
        Exsr GetStrDt;
        Chain rsKy prs02;
        Setgt ifKy pif01;
       //*** beg chg ***** 12/02/21 ***********************
       // Readpe ifKy pif01;
        Readpe ifKy1 pif01;
       //*** end chg ***** 12/02/21 ***********************
        If Not(%Eof);
           xxIfBl = Ififbl;
        Endif;
       Endsr;
       //*** end chg ***** 04/12/21 ***********************
     C*=========================================================================
     C* getStrDt - subroutine to get direct debit start date based off ELBSDT and BSFREQ
     C*=========================================================================
     c     getStrDt      begsr

     c                   select

     c                   when      BSFREQ = 'W'

     c                   eval      elbsdt8 = dtMDYY(ELBSDT)
     c                   eval      bsdtDow4 = dowShort(elbsdt8)
     c                   movel     bsdtDow4      bsdtDow2
     c                   eval      bsdtDow2 = upper(bsdtDow2)
     c                   eval      instDow2 = BSDOWK
     c                   eval      t1 = 1
     c                   eval      t2 = 1
     c     bsdtDow2      lookup    day(t1)                                60
     c     instDow2      lookup    day(t2)                                61

     c                   if        *in60 and *in61

     c                   eval      dowDiff = t2 - t1
     c                   if        dowDiff < 0
     c                   add       7             dowDiff
     c                   endif

     c                   if        dowDiff = 0
     c                   eval      instStrDt8 = dtMDYY(ELBSDT)
     c                   else
     c                   eval      instStrDt8 = dtFuture(elbsdt8:dowDiff)
     c                   endif

     c                   eval      numofDays = (BSDBSD - 1) * 7
     c                   eval      debStrDt8 = dtFuture(instStrDt8:numofDays)
     c                   eval      debStrDt  = dtCYMD(debStrDt8)

     c                   else
     c                   eval      strDtErr = *on
     c                   endif

     c                   when      BSFREQ = 'T'

     c                   eval      debStrDt = ELBSDT

     c                   select

     c                   when      debDay >  01 and
     c                             debDay <  15
     c                   eval      debDay = 15

     c                   when      debDay > 15
     c                   eval      debDay = 01
     c     *cymd         move      debStrDt      startDate
     c                   adddur    1:*m          startDate
     c     *cymd         move      startDate     debStrDt

     c                   endsl

     c     BSDBSD        sub       1             startPeriod       3 0

     c                   if        startPeriod > 0

     c                   do        startPeriod

     c                   select
     c                   when      debDay = 1
     c                   eval      debDay = 15
     c                   when      debDay = 15
     c                   eval      debDay = 01
     c     *cymd         move      debStrDt      startDate
     c                   adddur    1:*m          startDate
     c     *cymd         move      startDate     debStrDt
     c                   endsl

     c                   enddo

     c                   endif

     c                   when      BSFREQ = 'M'
     c                   eval      debStrdt = ELBSDT
     c     BSDBSD        sub       1             startPeriod

     c                   if        startPeriod > 0

     c                   do        startPeriod
     c     *cymd         move      debStrDt      startDate
     c                   adddur    1:*m          startDate
     c     *cymd         move      startDate     debStrDt
     c                   move      '01'          debStrDt

     c                   enddo

     c                   endif

     c                   endsl

     c                   endsr
     C*=========================================================================
     C*    Subroutine to create batch entries                                  *
     C*=========================================================================
     C     srcrtb        begsr

     C                   if        xxbat# = 0
     C                   exsr      srcrtd
     C                   exsr      srcrtr
     C                   seton                                        31
     C                   endif

     c                   do        13            x

     c                   if        amount(x) > 0
     C*
     C*  Update employer A/R Batch file
     C*
     C     eaky          chain     pea03
     C                   if        do_EAEMP# = 0

     c                   eval      accumBatTot = amount(x) + accumBatTot
     c                   eval      do_EACO#  = xxco#
     c                   eval      do_EAFND  = xxfnd
     c                   eval      do_EAFYR  = svfyr                            gets yr of last rec
     c                   eval      do_EAEMP# = xxemp#
     c                   eval      do_EADIV  = xxdiv
     c                   eval      do_EAARCD = ar_code(x)
     c*** Begin Add *** 11/08/23 *******************************************
     c                   eval      do_EAcTyp = getArCat(do_EAARCD)
     c*** End   Add *** 11/08/23 *******************************************
     c                   eval      do_EADESC = ar_desc(x)
     C                   eval      do_EAPODT = toPODT
     C                   eval      do_EATRDT = toPODT
     C                   eval      do_EACHK# = 0
     C                   eval      do_EACKAM = amount(x)
     C                   eval      do_EAAMT  = amount(x)
     C                   eval      do_EASYCD = COSYCD
     C                   eval      do_EATFAP = 'N'
     C                   eval      do_EATFFL = 'N'
     C                   eval      do_EATFDT = 0
     C                   eval      do_EATFAM = 0
     C                   eval      do_EASTAT = *blank
     c                   eval      do_EAACHF = 'A'
     C                   eval      do_EAUSER = Q1USER
     C                   eval      do_EAPGM  = Q1PGM
     C                   time                    DSDTTM
     C                   eval      do_EADT = dtCYMD(dsdate)
     C                   eval      do_EATIME = dstime
     C                   update    pea03
     C                   else

     C     eaky2         chain     pea03                              40
     C                   dow       not *in40 and do_eaarcd <> ar_code(x)
     C     eaky2         reade     pea03                                  40
     C                   enddo
     c                   eval      accumBatTot = amount(x) + accumBatTot
     c                   eval      do_EACO#  = xxco#
     c                   eval      do_EAFND  = xxfnd
     c                   eval      do_EAFYR  = svfyr                            gets yr of last rec
     c                   eval      do_EAEMP# = xxemp#
     c                   eval      do_EADIV  = xxdiv
     c                   eval      do_EAARCD = ar_code(x)
     c*** Begin Add *** 11/08/23 *******************************************
     c                   eval      do_EAcTyp = getArCat(do_EAARCD)
     c*** End   Add *** 11/08/23 *******************************************
     c                   eval      do_EADESC = ar_desc(x)
     C                   eval      do_EAPODT = toPODT
     C                   eval      do_EATRDT = toPODT
     C                   eval      do_EACHK# = 0
     C                   eval      do_EACKAM = amount(x)
     C                   eval      do_EAAMT  = amount(x)
     C                   eval      do_EASYCD = COSYCD
     C                   eval      do_EATFAP = 'N'
     C                   eval      do_EATFFL = 'N'
     C                   eval      do_EATFDT = 0
     C                   eval      do_EATFAM = 0
     C                   eval      do_EASTAT = *blank
     c                   eval      do_EAACHF = 'A'
     C                   eval      do_EAUSER = Q1USER
     C                   eval      do_EAPGM  = Q1PGM
     C                   time                    DSDTTM
     C                   eval      do_EADT   = dtCYMD(dsdate)
     C                   eval      do_EATIME = dstime
     C                   eval      do_eaipgm = svipgm
     C                   eval      do_eaiusr = sviusr
     C                   eval      do_eaidt  = runDate
     C*
     C                   if        not *in40
     C                   update    pea03
     C                   else
     C                   write     pea03
     C                   end

     C                   end                                                    do_emp# = 0

     C                   add       1             xxtent
     C                   seton                                        30

     c                   endif                                                  amount(x) > 0

     c                   enddo                                                  do 11 x

     c                   endsr
     C*=========================================================================
     C*     SUBROUTINE TO CREATE DUMMY RECORD                         *
     C*=========================================================================
     C     SRCRTD        BEGSR
     C*
     C*  DETERMINE NEXT AVAILABLE BATCH NUMBER
     C*
     C                   MOVE      *BLANKS       XXSTAT
     C     EAKY1         SETGT     PEA02
     c                   READP     PEA02                                  61
     c     *IN61         IFEQ      '0'
     C     dl_EACO#      ANDEQ     XXCO#
     C                   eval      xxval1 = dl_EABAT#
     C                   END
     C*
     C                   MOVE      'T'           XXSTAT
     C     EAKY1         SETGT     PEA02
     c                   READP     PEA02                                  61
     c     *IN61         IFEQ      '0'
     C     dl_EACO#      ANDEQ     XXCO#
     C                   eval      xxval2 = dl_EABAT#
     C                   END
     C*
     C     XXVAL1        IFGT      XXVAL2
     C     1             ADD       XXVAL1        XXBAT#
     C                   ELSE
     C     1             ADD       XXVAL2        XXBAT#
     C                   END
     C                   MOVE      *BLANKS       XXSTAT

     C                   MOVE      XXCO#         do_EACO#
     C                   MOVE      XXBAT#        do_EABAT#
     C                   MOVE      *ZEROS        do_EABATA
     c                   MOVE      *ZEROS        do_EAFND
     C                   MOVE      *ZEROS        do_EAFYR
     C                   MOVE      *ZEROS        do_EAEMP#
     C                   MOVE      *ZEROS        do_EADIV
     C                   MOVE      *BLANKS       do_EAARCD
     c*** Begin Add *** 11/08/23 *******************************************
     C                   MOVE      *BLANKS       do_EAcTyp
     c*** End   Add *** 11/08/23 *******************************************
     C                   MOVE      *BLANKS       do_EADESC
     C                   MOVE      *ZEROS        do_EAPODT
     C                   MOVE      *ZEROS        do_EATRDT
     C                   Z-ADD     0             do_EACHK#
     C                   Z-ADD     0             do_EACKAM
      *** Begin Add *** 05/09/25 *******************************************
     C                   MOVE      *All'9'       do_EACHK#
     C                   MOVE      *all'9'       do_EACKAM
      *** End   Add *** 05/09/25 *******************************************
     C                   MOVE      *ZEROS        do_EAAMT
     C                   MOVE      *BLANKS       do_EASYCD
     C                   MOVE      *BLANKS       do_EATFAP
     C                   MOVE      *BLANKS       do_EATFFL
     C                   MOVE      *ZEROS        do_EATFDT
     C                   MOVE      *ZEROS        do_EATFAM
     C                   MOVE      XXSTAT        do_EASTAT
     C                   MOVE      Q1USER        do_EAUSER
     C                   MOVE      Q1PGM         do_EAPGM
     c                   time                    dsdttm
     C                   Z-ADD     DSTIME        do_EATIME
     C                   eval      do_EADT = dtCYMD(dsdate)
     C                   eval      do_eaiusr = q1user
     C                   eval      do_eaipgm = q1pgm
     C                   eval      do_eaidt  = runDate
     C                   eval      sviusr    = q1user
     C                   eval      svipgm    = q1pgm
     C                   WRITE     PEA03
     C*
     C                   ENDSR
     C*=========================================================================
     C*     SUBROUTINE TO CREATE WMDHP RECORD
     C*=========================================================================
     C     SRCRTR        BEGSR
     C*
     C                   MOVE      XXCO#         DHCO#
     C                   MOVE      X1FND         DHFND
     C                   MOVE      *zeros        DHFYR
     C                   MOVE      toPODT        DHPODT
     C                   MOVE      XXBAT#        DHBAT#
     C                   MOVE      *ZEROS        DHTMDT
     C                   MOVE      XXSTAT        DHSTAT
     C                   MOVE      Q1USER        DHUSER
     C                   MOVE      Q1PGM         DHPGM
     c                   time                    dsdttm
     C                   Z-ADD     DSTIME        DHTIME
     C                   eval      DHDT = dtCYMD(dsdate)
     C                   WRITE     PDH01
     C*
     C                   ENDSR
     c*=========================================================================
     c*   Subroutine to write to A/R Batch
     c*=========================================================================
     c     wrtBatch      begsr
     c*
     c*   If prenotification has not been sent or amount due is greater
     c*   than 0, write or update the employer A/R batch file and generate
     c*   a summary report.
     c*
     c* Remove pre-note processing
     c*
     c                   if        xxamt > 0
     c                   exsr      srcrtb
     c                   end
     c*
     c                   if        *inoa
     c                   except    head
     c                   setoff                                       35
     c                   end
     c*
     c                   if        xxamt < 0
     c     dbky          chain     pem01
     c                   except    head2
     c                   except    notice
     c                   seton                                        35
     c                   end

     c                   endsr                                                  wrtBatch
      *=================================================================================
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O**
     O**   SELECTION CRITERIA
     O**
     O          E            ROUTE          1
     O                                           20 'Company  . . :'
     O                       QQCO#               25
     O*
     O          E            ROUTE          1
     O                                           20 'Group  . . . :'
     O                       x2fnd               25
     O*
     O          E            ROUTE          1
     O                                           20 'Run date:'                 changed from podtMLD
     O                       runDt8              32 '  /  /    '
     O*
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*=========================================================================
     O* REPORT DETAIL
     O*=========================================================================
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              85
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'Page:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            6 'Group:'
     O                       x2fnd               12
     O                       FNNAME              73
     O          E            HEAD           3
     O                                           35 'Run date:'                 changed from PODT
     O                       runDt8              47 '  /  /    '                            mld
     O          E            DETAIL         2
     O              N30N31                       29 '*** No entries to trans'
     O              N30N31                       51 'mit, no batch# created'
     O              N30N31                       72 'for posting date ***'
     O               31                          16 '*** Batch#'
     O               31      XXBAT#        4     26
     O               31                          38 'created ***'
     O               30N31                       26 '*** Entries added to'
     O               30N31                       33 'batch#'
     O               30N31   XXBAT#        4     44
     O               30N31                       48 '***'
     O*
     O          E    30      DETAIL         2
     O                                           35 'Total # of entries added'
     O                                           36 ':'
     O                       XXTENT        4     42
     O          E            DETAIL         2
     O               30                          28 '*** A/R batch edit has'
     O               30                          47 'been submitted ***'
     O*=========================================================================
     O*  CREDIT BALANCE ACCOUNTS
     O*=========================================================================
     O          E   N35      HEAD2          2 12
     O                                           28 'The following accounts'
     O                                           50 'have a credit balance'
     O          E   N35      HEAD2          2
     O                                           29 'Employer#   Division'
     O                                           36 'Name'
     O                                           81 'Amount'
     O          E            NOTICE         2
     O                       EMEMP#        4     18
     O                       EMDIV         4     27
     O                       EMNAME              72
     O              N35                          75 '$'
     O                       XXAMT         4     84
**  TABLE PREN
PRENOTIFICATION TRANSMISSION
**  day
MOTUWETHFRSASU
