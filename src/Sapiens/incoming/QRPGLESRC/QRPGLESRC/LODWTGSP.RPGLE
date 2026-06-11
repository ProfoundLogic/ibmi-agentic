     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *=================================================================================
      * Load WTGSP (Group Process Select Program)                         *
     H*       !!!!!!!!  Add table entries to fv and fv1 !!!!!!!!!!        *
      *=================================================================================
     H* Revised:                                                          *
     H*                                                                   *
     H* 11/20/24 C:340 Project: WC2988                                    *
     H*                Load WTGSP with Print Certificate Of Insurance     *
     H*                                                                   *
     H* 10/28/20 C:090 Project: WC2671                                    *
     H*                Load WTGSP with Audit Request                      *
     H*                                                                   *
     H* 01/16/20 C:180 Project: CVT129                                    *
     H*                Load WTGSP with DTTFRRCSBM - Darts Remark tranf    *
     H*
     H* 12/19/19 C:079 Project: 150WEM                                    *
     H*                Default to Include in Policy Export Group Select   *
     H*                                                                   *
     H* 08/19/19 C:079 Project: WC2604                                    *
     H*                Load WTGSP with WTIDCTPMT,WTIDCQPMT and GSNCARGRP  *
     H*
     H* 04/04/17 C:062 Project: 148WAG                                    *
     H*                Load WTGSP with Carrier Policy #                   *
     H*
     H* 01/09/17 C:306 Project: WC2441                                    *
     H*                Load WTGSP with WTRPTAGACT                         *
     H*
     H* 11/08/16 C:307 Project: WC2459                                    *
     H*                Load WTGSP with WTRPTAGLOS                         *
     H*
     H* 10/21/16 C:308 Project: WC2425                                    *
     H*                Load WTGSP with WTRPTAGG                           *
     H*
     H* 09/28/16 C:300 Project: WC2456                                    *
     H*                Load WTGSP with WTRPTNRWB and WTRPTLTNOT           *
     H*
     H* 06/24/16 C:079 Project: C114MWK                                   *
     H*                Remove F6601R01                                    *
     H*
     H* 04/21/16 C:308 Project: C114MWK                                   *
     H*                Load WTGSP with F6601R01                           *
     H*
     H* 04/15/16 C:298 Project: WC2416                                    *
     H*                Load WTGSP with WTRPTRMBDD                         *
     H*
     H* 03/21/16 C:309 Project: WC2418                                    *
     H*                Load WTGSP with WTRPTARDTL                         *
     H*
     H* 09/11/15 C:295 Project: Project: WC2362                           *
     H*                Load WTGSP with 1099RFILE and 1099RIRS             *
     H*
     H* 07/20/15 C:295 Project: Phw# 36111 (In house)                     *
     H*                Change the value in the array fv1 and changed
     H*                the GSLIKECAR value for below entries Y to N.
     H*                W7417R1B  E  YYY  - W7417R1B  E  YYN               *
     H*                W7350R02B E  YYY  - W7350R02B E  YYN               *
     H*                W7350R02  E  YYY  - W7350R02  E  YYN               *
     H*                                                                   *
     H* 07/17/15 C:65  Project: Phw# 36100 (CSI)                          *
     H*                Change population of new fields.                   *
     H*                                                                   *
     H* 06/23/15 C:33  35519
     H*               -Add new fields.
     H*               -All reports should be excluded for 114.
     H*                                                                   *
     H* 12/01/14 C:296 Project: 114MQA                                    *
     H*                Add a record to compile                            *
     H*                time array for FV and FV1, FV2.                    *
     H*                (Proc id = F6300R)                                 *
     H*                                                                   *
     H* 10/14/14 C:279 Project: 114WNC                                    *
     H*                Add WTCOMCHK                                       *
     H*                                                                   *
     H* 09/12/14 C:280 Project: SYAOCVT                                   *
     H*                Add SYAOCVT                                        *
     H*                                                                   *
     H* 05/28/14 C:279 Project: 129WSV                                    *
     H*                Add A/R detail report.                             *
     H*                                                                   *
     H* 01/08/14 C:279 Project: 114WKC                                    *
     H*                Add F18 - Select Groups for Data Restrictions      *
     H*                                                                   *
     H* 10/17/11 C:53  Project: 139WQI                                    *
     H*                Add DARTS extract (SCEXTR)                         *
     H*                                                                   *
     H* 05/12/11 C:33  Project: WC1851                                    *
     H*                Activate F17 for W8005R.                           *
     H*                                                                   *
     H* 03/11/11 C:190 Project: 139WPQ                                    *
     H*                Add processing to create records for the WCPOLS,   *
     H*                WCSTAT, and Financial Data files processes.        *
     H*          Note: WCPOLS & WCSTAT are called up with 999 param so    *
     H*                that these pgm did not called up in W7944R to      *
     H*                avoid recursive call error.                        *
     H*                                                                   *
     H* 10/11/10 C:062 Project: SY190                                     *
     H*                Add Agent Loss Ratio Report (W7300R)               *
     H*                                                                   *
     H* 10/05/10 C:251 Project: WC1944                                    *
     H*                -Changed GSPPGM value in compile time array for    *
     H*                 W7488R from W7488C to W7488R2                     *
     H*                                                                   *
     H* 07/26/10 C:136 Project: WC1694                                    *
     H*                Add Default Inc/Exc Flag to table                  *
     H*                Add Condition Inc/Exc Flag for Policy Export       *
     H*                Add Condition Inc/Exc Flag for Medicare RRE#       *
     H*                Allow program to update existing WTGSP records     *
     H*                Revised logic for subset flag (WC1879)             *
     H*                                                                   *
     H* 07/02/10  C:257 Project: WC1790                                   *
     H*                -Add new record EDI Submission                     *
     H*                                                                   *
     H* 05/18/10  C:198 Project: WC1879                                   *
     H*                -Changed the structure of wtgsp to incorporate     *
     H*                 F17=Subset                                        *
     H*                                                                   *
     H* 06/23/10  C:235 Project: 114WVD                                   *
     H*                -Add new record PNC Bank Extract in array.         *
     H*                -Removed record SEPTA114 from array.               *
     H*                                                                   *
     H* 03/03/10 C:136 Project: WC1851                                    *
     H*                - Add record for Fee Invoice report                *
     H*                                                                   *
     H* 02/01/10  C:33  Project: 25WRB                                    *
     H*                -Add new records in array.                         *
     H*                                                                   *
     H* 12/21/09  C:53  Project: WC1819                                   *
     H*                -Add new record in array.                          *
     H*                                                                   *
     H* 11/10/09  C:235 Project: 114WVD                                   *
     H*                -Add new record in array.                          *
     H*                                                                   *
     H* 10/01/09  C:33  Project: 114WUO                                   *
     H*                -Add new record.                                   *
     H*                -Allow for program to be processed multiple times. *
    H*********************************************************************
     fwtgsp     uf a e           k disk

     d size            c                   50
     d x               s              3  0
     d fv              s             94    dim(size) ctdata perrcd(1)
     D fv1             s             10    dim(size) ctdata perrcd(1)
     D fv2             s             30    dim(size) alt(fv1)
     d flds          e ds                  extname(WTGSP)
     d sysDt           s               d
     d sysDt7          s              7  0

     d psds          esds                  extname(szq1p)
     d index                          2  0
     d aflgs           ds
     d  defInExFlg                    1
     d  conOnWMPEP                    1

     d***  Begin Add  ***  06/23/15  **************************************
     d xxClt#          s              3

     dRtvCltId         pr                  ExtPgm('RTVCLTID')
     d cltId                          3

      /Free
       RtvCltId(xxClt#);
      /End-free
     c***  End   Add  ***  06/23/15  **************************************

     c                   move      *date         sysDt
     c     *cymd         move      sysDt         sysDt7
     c     1             do        size          x
     c                   if        fv(x)  <> *blanks
     c                   eval      flds   = fv(x)
     C     GSID          chain     WTGSP
     c                   eval      GSPGM  = q1pgm
     c                   eval      GSUSER = q1user
     c                   eval      GSDT   = sysDt7
     c                   eval      index = %lookup(GSID : fv1)
     c                   if        index > 0
     c                   eval      GSDIEF = %subst(fv2(index) : 1 : 1)
     c                   eval      GSCOPE = %subst(fv2(index) : 2 : 1)
     c                   eval      GSCRRE = %subst(fv2(index) : 3 : 1)
     c                   eval      GSF17  = %subst(fv2(index) : 4 : 1)
     c                   eval      GSCAR# = %subst(fv2(index) : 5 : 1)
     C***  Begin Add  ***  06/23/15  **************************************
     c                   eval      GSlikeCar# = %subst(fv2(index) : 6 : 1)
     c                   eval      GSlikeRRE  = %subst(fv2(index) : 7 : 1)
     c                   eval      GScxPgmVal = %subst(fv2(index) : 8 : 10)
     C***  End   Add  ***  06/23/15  **************************************
     C***  Begin Add  ***  09/11/15  **************************************
     c                   eval      GSdspTCC   = %subst(fv2(index) : 18 : 1)
     c                   eval      GSReqTCC   = %subst(fv2(index) : 19 : 1)
     c                   eval      GSLikeTCC  = %subst(fv2(index) : 20 : 1)
     C***  End   Add  ***  09/11/15  **************************************
     C***  Begin Add  ***  08/19/19  **************************************
     c                   eval      GSnCarGrp = %subst(fv2(index) : 21 : 1)
     C***  End   Add  ***  08/19/19  **************************************
     c                   else
     c                   eval      GSDIEF = 'I'
     c                   endif
     C***  Begin Add  ***  06/23/15  **************************************
      /Free
       If xxClt# = '114';
          GSdIEF = 'E';
       //**  Begin Add  ***  07/17/15  **************************************
          GSlikeCar# = ' ';
       //**  end add ******  07/17/15  **************************************
       Endif;
       //*** Beg Add *** 12/19/19 *************************************
       If xxClt# = '150' and GSid = 'W8370R';
          GSdIef = 'E';
       Endif;
       //*** End Add *** 12/19/19 *************************************
      /End-free
     C***  Begin Add  ***  06/23/15  **************************************
     c                   if        %found(WTGSP)
     c                   update    pgs01
     c                   else
     c                   write     pgs01
     c                   endif
     c***  end chg ***** 07/26/10 *****************************************
     c***  Begin Add  ***  10/01/09  **************************************
     c*** beg del ***** 07/26/10 ******************************************
     C***********        endif
     c*** end del ***** 07/26/10 ******************************************
     c***  End   Add  ***  10/01/09  **************************************
     c                   else
     c                   leave
     c                   endif
     c                   enddo
     c***  Begin Del  ***  10/01/09  **************************************
     c**********         endif
     c***  End   Del  ***  10/01/09  **************************************
     c                   seton                                        lr
      *  1         2         3         4         5         6         7         8         9
123456*890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
** fv (field values)
W7942R    Renewal Tracking                        W7942R    001                            001
W7290R1   Aging Report                            W7290R1   001                            001
W7995R    Surplus Dividend                        W7995R    002                            001
W8370R    Policy Export                           W8370R    003  Group Policy Select      G001
W7480R    Medicare Extract                        W7480R    003                            002Y
W3017R    Agent Commission Analysis               W3017R    001                            001
W7084R    Ulico Claim Export                      W7084R    004                            002
W7935R1   Reimbursable Deductible Detail          W7935R1   001                            001
W8146R    Security fund Extract                   W8146R    001                            001
W8165R    NDIF Report Extract                     W8165R    000                            001
W9010C    Test Refresh (Transfer)                 W9010C    000                            001
W7480RE   Medicare Extract Edit Mode              W7480R    003                           E002Y
W8007R    Deductible Invoices                     W8007R    000                            001
W7324R    Deductible Invoice Reprint              W7324R    000                            001
W7935R    Deductable Detail Report                W7935R    000                            001
W8005R    Fee Invoices                            W8005R    003  Fee Invoices              001
W7300R    Agent Loss Ratio (PPR Report)           W7300R    000                            001
PNC114    PNC Bank Extract                        W7035R2   003                            001
W7488R    Medicare Query File Submission          W7488R2   000                            002Y
W0066R1   EDI Submission                          W0066R1   001                           E001
W8026R    Collateral Dissolve                     W8026R    004                            001
W7417R1B  WCPOLS Data Transfer                    W7417R1   999                            002Y
W7350R02B Statistical Submission update/display   W7350R02  999                            002Y
W7350R02  Financial Data Files                    W7350R02  005                           F002Y
SCEXTR    DARTS Extract                           SCEXTR    003                            001
S0009R    Select Groups for Data Restrictions     S0009R03  000                            001
W7380R    A/R Detail Report                       W7380R    003  A/R Detail Report         001
SYAOCVT   AO Conversion Command                   SYAOCVTS  000  AO Conversion Command     001
WTCOMCHK  Check Schedule Maintenance              WTCOMCHK01000                            001
W7946C    NCCI Medical Data Call                  W7946C    000                            001
1099RFILE Create 1099 Workfile & Report           W1099RP   005                           F001Y
1099RIRS  Create 1099 Submission                  W1099RP   005                           T001Y
WTRPTARDTLA/R Detail Report New compsuite         WTRPTARDTL003  A/R Detail Report         001
WTRPTRMBDDReimbursable Deductible Report New      WTRPTRMBDD003  Reim Ded Detail Report    001
WTRPTNRWB New/Renewal Business Report             WTRPTNRWB 001  New/Renewal Business Rpt  001
WTRPTLTNOTLate Notices and Report - Est           WTRPTLTNOT003  Late Notice Report - Est  001
WTRPTAGG  Aging Report                            WTRPTAGG  002  Aging Report              001
WTRPTAGLOSAgent Loss Ratio (PPR Report)           WTRPTAGLOS002  Agent Loss Ratio(PPR Rpt) 001
WTRPTAGACTAgent Activity Report                   WTRPTAGACT002  Agent Activity Report     001
W8190R    Carrier Policy # Report                 W8190R    000                            001
WTIDCTPMT NCCI IDC Transactional                  WTIDCTPMT 000  NCCI IDC Transactional    001
WTIDCQPMT NCCI IDC Quarterly                      WTIDCQPMT 000  NCCI IDC Quarterly        001
DTTFRRCSBMDarts Remark Transfer                   DTTFRRCSBM004                            001
W7230R    Audit Requests                          W7230R    000                            001
WTRPTCERTPPrint Certificates of Insurance         WTRPTCERTP000                            001
** fv1 alt fv2  (additional field values (keyed to pgm ID)
W7942R    I
W7290R1   I
W7995R    I
W8370R    IY
W7480R    E YY  Y
W3017R    I
W7084R    I
W7935R1   I
W8146R    I
W8165R    I
W9010C    I
W7480RE   I YY  Y
W8007R    I
W7324R    I
W7935R    I
W8005R    I  Y
W7300R    I
PNC114    I
W7488R    I YY  Y
W0066R1   I
W8026R    I  Y
W7417R1B  E  YYN *ON
W7350R02B E  YYN
W7350R02  E  YYN
SCEXTR    E
S0009R    E
W7380R    E
SYAOCVT   I
WTCOMCHK  E
W7946C    E
1099RFILE E                YYY
1099RIRS  E                YYY
WTRPTARDTLE                                                                  ** 03/21/16
WTRPTRMBDDE
WTRPTNRWB E
WTRPTLTNOTE
WTRPTAGG  E                                                                  ** 10/21/16
WTRPTAGLOSI                                                                  ** 11/08/16
WTRPTAGACTE                                                                  ** 01/09/17
W8190R    E                                                                  ** 04/04/17
WTIDCTPMT E                   Y                                              ** 08/19/19
WTIDCQPMT E                   Y                                              ** 08/19/19
DTTFRRCSBME                                                                  ** 01/16/20
W7230R    E                                                                  ** 04/04/17
WTRPTCERTPE                                                                  ** 11/20/24
