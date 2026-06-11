     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7197R - MEMBER EXPERIENCE REPORT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM PRINTS A REPORT OF ALL CLAIMS THAT FALL WITHIN   *
     H* THE SPECIFIED ACCIDENT DATE RANGE.  THIS REPORT MAY BE RUN    *
     H* FOR ONE OR ALL EMPLOYERS/DIV, REGIONS, COUNTIES, AGENTS,      *
     H* OR ZIP CODES.  IN ADDITION, JUST OPEN OR CLOSED CASES MAY     *
     H* BE SELECTED.  EACH REPORT MAY BE SEQUENCED BY EMPLOYER NAME,  *
     H* EMPLOYER NUMBER, REGION, COUNTY, AGENT, ZIP CODE OR CATEGORY  *
     H* ONE. THE CLAIM INFORMATION IS SHOWN FOR EACH EMPLOYER AS      *
     H* WELL AS YEAR
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7196C                                                      *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:33     CREATED 05/17/98      PROJECT: WC701     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/22/22 C:33  Support: 102300
     H*                Always update WFE2P with claim specfic data
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 05/13/14 C:283 Support WA046 85331                            *
     H*                Print the "Report for the Period" dates        *
     H*                correctly as per Accident Date Range or Policy *
     H*                Date Range is entered.                         *
     H*                                                               *
     H* 03/11/14 C:282 PROJECT 129WSC Priss 28                        *
     H*                Aligning the 'Heading for Legal Reserve'       *
     H*                with the other reserves.                       *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 07/06/11  C:062  SUPPORT: 77125                               *
     H*                  Populate E2CIDS                              *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Load extension specific description to work    *
     H*                file fields.                                   *
     H*                                                               *
     H*  12/14/05  C:168  PROJECT: 129UIB                             *
     H*                   Added the NCCI code and description to file *
     H*                   WEF2P. Code comes from WMCMP.CMNCCI and     *
     H*                   description from system table - WTGTP       *
     H*                                                               *
     H*  06/21/05 C:58  PROJECT: WC1318
     H*                - Add parameter'Print SS#' Suppress SS# if N.
     H*
     H*  03/21/05 C:132 PROJECT: WC1221                               *
     H*                - Shift output on some print lines over four   *
     H*                  positions, changes installed by C: 015 in    *
     H*                  code on 03/11/05.
     H*
     H*  02/25/05 C:132 PROJECT: WC1221       (UPDATED 03/21/05)      *
     H*                - Add new fields for Recoveries - Current      *
     H*                  period and To-Date fields                    *
     H*                - Calculate new recovery amounts for both      *
     H*                  current period and To-Date.                  *
     H*                - Update new fields in output file             *
     H*                                                               *
     H*  10/14/04  C:15   Support: WC129/44113                        *
     H*                   When run for all groups, XXFND was not set  *
     H*                   immediately, causing the first record for   *
     H*                   each group to have inaccurate data.         *
     H*                   Correct code.                               *
     H*                                                               *
     H*  01/14/04  C:15   Support: WC071/40758                        *
     H*                   Corrected the premium selection calc.       *
     H*                                                               *
     H*  11/14/03  C:132  PROJECT 121WAS                              *
     H*                   Chg to print employer totals if no claims   *
     H*                                                               *
     H*  10/16/03  C:132  PROJECT 121WAS                              *
     H*                   Load new fields to workfile, receive parms  *
     H*                   from W7196C and list on Routing page, list  *
     H*                   final totals at end.                        *
     H*                                                               *
     H* 09/18/03 C:15  PROJECT WC577                                  *
     H*                Added code to load the group name.             *
     H*                                                               *
     H* 02/20/03 C:132 PROJECT WC577                                  *
     H*                Add code to get producer name from producer    *
     H*                number in  WFE2P file input by pgm W7196R.     *
     H*                Update WFE2P with producer name and print on   *
     H*                report.                                        *
     H*                                                               *
     H* 11/22/02 C:142 PROJECT WC1100                                 *
     H*                ADDED PROCESSING BY ALL FUNDS IF FUND ZERO.    *
     H*                                                               *
     H* 05/08/02 C:61  PROJECT WC577                                  *
     H*                Add policy effective date range to output.     *
     H*                                                               *
     H*  03/20/02  C:15  Support: WC091/31538                         *
     H*                  Fixed duplex printing (never worked).        *
     H*                                                               *
     H*  03/29/01  C:15  Support: WC096/26845                         *
     H*                  Added code to break when the sort value      *
     H*                  changes.                                     *
     H*                                                               *
     H*  01/13/00  C:15  Support: WC096/21222                         *
     H*                  When a workfile record is not printed on the *
     H*                  report, certain totals are not being loaded  *
     H*                  with correct data.  I corrected the logic.   *
     H*                                                               *
     H*  12/07/99 C:33 PROJECT: WC577     Y2K#: 20                    *
     H*                LOAD EMPLOYER RELATED DESCRIPTIONS CORRECTLY.  *
     H*                                                               *
     H*  11/24/99 C:33 PROJECT: WC960                                 *
     H*                MODIFY TO RUN FOR ONE OR ALL DIVISIONS         *
     H*                                                               *
     H*  11/18/98 C:33 PROJECT: WC701                                 *
     H*                DO NOT ADD TOTALS IF E2PRT IS NOT 'Y'          *
     H*                                                               *
     H*  10/19/98  C:33  SUPPORT: PER JJ                              *
     H*                  PRINT AREA CODE ON PHONE #                   *
     H*                                                               *
     H*  10/16/98  C:33  SEQ#: WA071/14188                            *
     H*                  DO NOT PRINT DETAIL HEADING WHEN PRINTING    *
     H*                  EMPLOYER TOTALS ON THE NEXT PAGE             *
     H*                                                               *
     H*  06/09/98  C:33  PROJECT: WC701                               *
     H*                 -CHANGE 'POLICY YEAR' VERIBAGE FOR TOTALS TO  *
     H*                  'TOTALS FOR POLICY YEAR'                     *
     H*                 -ADD CALC FOR EMPLOYER TOTAL, ALL YEARS       *
     H*****************************************************************
     FWFE2L     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWACNL     IF   E           K DISK
     F                                     RENAME(PCN01:PCN02)
     FWMCML1    IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FQSYSPRT   O    F  120        PRINTER OFLIND(*INOF)
     F                                     USROPN

     D*
     D SRT             S              1    DIM(7) CTDATA PERRCD(1) ASCEND
     D DES             S             25    DIM(7) ALT(SRT)
     D DESC            S              1    DIM(30)
     D NM1             S              1    DIM(80)
     D NM2             S              1    DIM(80)
     D mbrhd1          S             60
     D*
     D CHGNUM          C                   CONST('SA020R')
     D MBR             C                   CONST('MEMBER EXPERIENCE RE-
     D                                     PORT FOR THE PERIOD:')
     D THRU            C                   CONST('THROUGH')
     D PREM            C                   CONST('PREMIUM RANGE:')
     D MSG1            C                   CONST('(** REPRESENTS AUDIT-
     D                                     ED OR ESTIMATED ANNU-
     D                                     AL PREMIUM.)')
     D MSG2            C                   CONST('(REPRESENTS AUDITED -
     D                                     OR ESTIMATED ANNUAL -
     D                                     PREMIUM.)')
     D*
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  X8DATE                 1      8  0
     D  XXMM                   1      2
     D  XXDD                   3      4
     D  XXCCYY                 5      8
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
     D/copy sprString

     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E2AGT#
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    E2PROD
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E2EMP#
     C                   KFLD                    E2DIV
     C*
     C*  EMPLOYER DETAIL MASTER FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E2EMP#
     C                   KFLD                    E2DIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E2EMP#
     C                   KFLD                    E2DIV
     C*
     C*  REGION MASTER FILE
     C*
     C     RGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E2RGN#
     C*
     C*  EMPLOYEE TRACKING FILE
     C*
     C     ETKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E2EMP#
     C                   KFLD                    E2DIV
     C                   KFLD                    E2SS#
     C*
     C*  LOCATION/ENTITY FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    E2CTG1
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    E2FYR
     C                   KFLD                    E2CASE
     C*
     C*  SYSTEM TABLE FILE
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID            4
     C                   KFLD                    XXKEY2           10
     C*
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *LIKE         DEFINE    AGFEIN        XXFEIN
     C     *LIKE         DEFINE    AGMOD         XXMOD
     C     *LIKE         DEFINE    E2CO#         XXCO#
     C     *LIKE         DEFINE    E2FND         XXFND
     C     *LIKE         DEFINE    E2FYR         XXFYR
     C     *LIKE         DEFINE    EMDT          X7FRDT
     C     *LIKE         DEFINE    EMDT          X7TODT
     C     *LIKE         DEFINE    EMDT          X7RNDT
     C     *LIKE         DEFINE    E2EMP#        XXEMP#
     C     *LIKE         DEFINE    E2DIV         XXDIV
     C     *LIKE         DEFINE    E2RGN#        XXRGN#
     C     *LIKE         DEFINE    E2CNTY        XXCNTY
     C     *LIKE         DEFINE    E2AGT#        XXAGT#
     C     *LIKE         DEFINE    E2ZIP         XXZIP
     C     *LIKE         DEFINE    E2CAD1        XXCAD1
     C     *LIKE         DEFINE    E2CAD2        XXCAD2
     C     *LIKE         DEFINE    E2CCTY        XXCCTY
     C     *LIKE         DEFINE    E2CST         XXCST
     C     *LIKE         DEFINE    E2CZIP        XXCZIP
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    CNKEY         XXKEY
     C     *LIKE         DEFINE    NADESC        XXDESC
     C     *LIKE         DEFINE    FNNAME        XXFNNM
     C     *LIKE         DEFINE    E2OPCL        XXOPCL
     C     *LIKE         DEFINE    E2CTG1        XXCTG1
     C     *LIKE         DEFINE    ELFYR         XXXYR
     C     *LIKE         DEFINE    EMNAME        XXEMNM
     C     *LIKE         DEFINE    EMMAD1        XXEAD1
     C     *LIKE         DEFINE    EMMAD2        XXEAD2
     C     *LIKE         DEFINE    EMEMP#        #@EMP#
     C     *LIKE         DEFINE    EMDIV         #@DIV
     C     *LIKE         DEFINE    EMNAME        #@NAME
     C     *LIKE         DEFINE    E2ACDT        X8CNDT                         CANCEL DT TO PRINT
     C     *LIKE         DEFINE    E2ACDT        X8FRDT
     C     *LIKE         DEFINE    E2ACDT        X8TODT
     C     *LIKE         DEFINE    E2ACDT        X8RNDT
     C     *LIKE         DEFINE    E2ACDT        ##DATE
     C     *LIKE         DEFINE    COAREA        XXAARA
     C     *LIKE         DEFINE    COPHN#        XXAPH#
     C     *LIKE         DEFINE    XXEDN         X2EDN
     C     *LIKE         DEFINE    L1TOTC        L1OPEN
     C     *LIKE         DEFINE    L1TOTC        L1CLOS
     C     *LIKE         DEFINE    L1TOTC        L2TOTC
     C     *LIKE         DEFINE    L1TOTC        L2OPEN
     C     *LIKE         DEFINE    L1TOTC        L2CLOS
     C     *LIKE         DEFINE    L1TOTC        L3TOTC
     C     *LIKE         DEFINE    L1TOTC        L3OPEN
     C     *LIKE         DEFINE    L1TOTC        L3CLOS
     C     *LIKE         DEFINE    EMDT          X7PFDT
     C     *LIKE         DEFINE    EMDT          X7PTDT
     C     *LIKE         DEFINE    E2ACDT        X8PFDT
     C     *LIKE         DEFINE    E2ACDT        X8PTDT
     C     *LIKE         DEFINE    L1TOTC        LRTOTC
     C     *LIKE         DEFINE    L1TOTC        LROPEN
     C     *LIKE         DEFINE    L1TOTC        LRCLOS
     C     *LIKE         DEFINE    L1AMT         LRAMT
     C                   Z-ADD     *ZERO         LRACMP           11 2
     C                   Z-ADD     *ZERO         LRAMED           11 2
     C                   Z-ADD     *ZERO         LRALEG           11 2
     C                   Z-ADD     *ZERO         LRAOTH           11 2
     C                   Z-ADD     *ZERO         LRATOT           11 2
     C*
     C                   Z-ADD     *ZERO         LRPCMP           11 2
     C                   Z-ADD     *ZERO         LRPMED           11 2
     C                   Z-ADD     *ZERO         LRPLEG           11 2
     C                   Z-ADD     *ZERO         LRPOTH           11 2
     C                   Z-ADD     *ZERO         LRPTOT           11 2
     C*
     C                   Z-ADD     *ZERO         LRRCMP           11 2
     C                   Z-ADD     *ZERO         LRRMED           11 2
     C                   Z-ADD     *ZERO         LRRLEG           11 2
     C                   Z-ADD     *ZERO         LRROTH           11 2
     C                   Z-ADD     *ZERO         LRRTOT           11 2
     C*
     C                   Z-ADD     *ZERO         LRICMP           11 2
     C                   Z-ADD     *ZERO         LRIMED           11 2
     C                   Z-ADD     *ZERO         LRILEG           11 2
     C                   Z-ADD     *ZERO         LRIOTH           11 2
     C                   Z-ADD     *ZERO         LRITOT           11 2
      *
     C                   Z-ADD     *ZERO         LRGCMP           11 2
     C                   Z-ADD     *ZERO         LRGMED           11 2
     C                   Z-ADD     *ZERO         LRGLEG           11 2
     C                   Z-ADD     *ZERO         LRGOTH           11 2
     C                   Z-ADD     *ZERO         LRGTOT           11 2
     C*
     C                   Z-ADD     *ZERO         LRGPCO           11 2
     C                   Z-ADD     *ZERO         LRGPME           11 2
     C                   Z-ADD     *ZERO         LRGPLE           11 2
     C                   Z-ADD     *ZERO         LRGPEX           11 2
     C                   Z-ADD     *ZERO         LRGPOT           11 2
     C*
     C                   Z-ADD     *ZERO         LRRCCO           11 2
     C                   Z-ADD     *ZERO         LRRCME           11 2
     C                   Z-ADD     *ZERO         LRRCLE           11 2
     C                   Z-ADD     *ZERO         LRRCEX           11 2
     C                   Z-ADD     *ZERO         LRRCOT           11 2
     C*
     C                   Z-ADD     *ZERO         LRRTCO           11 2
     C                   Z-ADD     *ZERO         LRRTME           11 2
     C                   Z-ADD     *ZERO         LRRTLE           11 2
     C                   Z-ADD     *ZERO         LRRTEX           11 2
     C                   Z-ADD     *ZERO         LRTROT           11 2
      *
     C     *LIKE         DEFINE    E2SORT        XXSORT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@AFDT            8
     C                   PARM                    @@ATDT            8
     C                   PARM                    @@RNDT            8
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@RGN#            3
     C                   PARM                    @@CNTY            2
     C                   PARM                    @@AGT#            5
     C                   PARM                    @@ZIP             5
     C                   PARM                    @@SELO            1
     C                   PARM                    @@SELC            1
     C                   PARM                    @@MNTH            3
     C                   PARM                    @@SEQ             1
     C                   PARM                    @@ACFL            1            ACTIVE ACCTS ONLY FL
     C                   PARM                    @@FPRM           11
     C                   PARM                    @@TPRM           11
     C                   PARM                    @@QUE            10
     C                   PARM                    @@DPLX            1
     C                   PARM                    @@TYPO            1
     C                   PARM                    @@FPDT            8
     C                   PARM                    @@TPDT            8
     C                   PARM                    @@SELN            1
     C                   PARM                    QQPRSS            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     CNTRPL        PLIST
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*_______________________________________________________________
     C*
      /Free
       RtvCltID(CltId);
      /End-Free
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@AFDT        X8FRDT
     C*
     C                   MOVE      @@FPDT        X8PFDT
     C                   MOVE      @@TPDT        X8PTDT
     C*
     C                   Z-ADD     X8FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FRDT
     C*
     C                   MOVE      @@ATDT        X8TODT
     C*
     C                   Z-ADD     X8TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TODT
     C*
     C                   MOVE      @@RNDT        X8RNDT
     C*
     C                   Z-ADD     X8RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7RNDT
     C*
     C                   MOVE      @@EMP#        XXEMP#
     C                   MOVE      @@DIV         XXDIV
     C                   MOVE      @@RGN#        XXRGN#
     C                   MOVE      @@CNTY        XXCNTY
     C                   MOVE      @@AGT#        XXAGT#
     C                   MOVE      @@ZIP         XXZIP
     C                   MOVE      @@MNTH        XXMNTH            3 0
     C                   MOVE      @@FPRM        XXFPRM           11 0
     C                   MOVE      @@TPRM        XXTPRM           11 0
     C*
     C                   Z-ADD     X8PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7PFDT
     C*
     C                   Z-ADD     X8PTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7PTDT
     C*_______________________________________________________________
     C*
     C                   Z-ADD     *ZERO         XXATOT           11 2
     C                   Z-ADD     *ZERO         XXPTOT           11 2
     C                   Z-ADD     *ZERO         XXRTOT           11 2
     C                   Z-ADD     *ZERO         XXITOT           11 2
     C                   Z-ADD     *ZERO         E2ICMP           11 2
     C                   Z-ADD     *ZERO         E2IMED           11 2
     C                   Z-ADD     *ZERO         E2ILEG           11 2
     C                   Z-ADD     *ZERO         E2IOTH           11 2
     C                   Z-ADD     *ZERO         E2ITOT           11 2
     C                   Z-ADD     *ZERO         XXGTOT           11 2
     C                   Z-ADD     *ZERO         XXGPOT           11 2
     C                   Z-ADD     *ZERO         XXRCOT           11 2
     C                   Z-ADD     *ZERO         XXTROT           11 2
     C*
     C                   MOVE      *BLANK        ACTIVE            1            EMP ACTIVE FLAG
     C                   MOVE      *ZERO         XXCNT             5 0          DETAIL LINE PRINTED
     C                   Z-ADD     1             XXPAGE            3 0
     C*
     C*  SET INDICATOR IF THIS IS A CATEGORY ONE SORT
     C*
     C     @@SEQ         IFEQ      '1'
     C                   MOVE      *ON           *IN82
     C                   ELSE
     C                   MOVE      *OFF          *IN82
     C                   ENDIF
     C*
     C*  SET INDICATORS FOR PRINTING OF SELECTION CRITERIA ON
     C*  THE ROUTING PAGE
     C*
     C     @@TYPO        IFNE      'F'
     C                   EXSR      ROUTPG
     C                   ENDIF                                                  @@TYPO<>F
     C*
     C                   Z-ADD     1             X                 3 0
     C     @@SEQ         LOOKUP    SRT(X)                                 60
     C                   MOVEL(P)  DES(X)        XXSEQD           25
     C*
     C                   TIME                    DSDTTM
     C*_______________________________________________________________
     C*
     C*  GET USER PARAMETER RECORD
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C*  GET COMPANY RECORD
     C*
     C     XXCO#         CHAIN     PCO01                              60
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C*  GET GROUP MASTER RECORD
     C*
     C     @@FND         IFEQ      '000'
     C                   READ      PE201                                  40
     C                   MOVE      E2FND         XXFND
     C                   ENDIF
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        XXFNNM
     C                   ELSE
     C                   MOVEL     *BLANKS       XXFNNM
     C                   MOVEL     *BLANKS       FNANAM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     FNANAM        IFNE      *BLANK
     C*
     C                   MOVEL(P)  FNANAM        XCNAME           60
     C                   MOVEL(P)  FNAAD1        XXCAD1
     C                   MOVEL     FNAZIP        XXZIP5
     C                   MOVE      FNAZIP        XXZIP4
     C     FNACTY        CAT(P)    ',':0         XXCCSZ           40
     C                   CAT       FNAST:1       XXCCSZ
     C                   CAT       XXZIP5:1      XXCCSZ
     C*
     C     XXZIP4        IFNE      *ZERO
     C                   CAT       '-':0         XXCCSZ
     C                   CAT       XXZIP4:0      XXCCSZ
     C                   ENDIF                                                  XXZIP4<>*ZERO
     C*
     C                   MOVEL(P)  FNAAD1        XXCAD1
     C                   MOVEL(P)  FNAAD2        XXCAD2
     C                   MOVEL(P)  FNACTY        XXCCTY
     C                   MOVEL(P)  FNAST         XXCST
     C                   Z-ADD     FNAZIP        XXCZIP
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CONAME        XCNAME
     C                   MOVEL(P)  COADR1        XXCAD1
     C                   MOVEL     COZIP         XXZIP5
     C                   MOVE      COZIP         XXZIP4
     C     COCTY         CAT(P)    ',':0         XXCCSZ
     C                   CAT       COST:1        XXCCSZ
     C                   CAT       XXZIP5:1      XXCCSZ
     C*
     C     XXZIP4        IFNE      *ZERO
     C                   CAT       '-':0         XXCCSZ
     C                   CAT       XXZIP4:0      XXCCSZ
     C                   ENDIF                                                  XXZIP4<>*ZERO
     C*
     C                   MOVEL(P)  COADR1        XXCAD1
     C                   MOVEL(P)  COADR2        XXCAD2
     C                   MOVEL(P)  COCTY         XXCCTY
     C                   MOVEL(P)  COST          XXCST
     C                   Z-ADD     COZIP         XXCZIP
     C*
     C                   ENDIF                                                  FNANAM<>*BLANK
     C*
     C                   Z-ADD     COAREA        XXAARA
     C                   Z-ADD     COPHN#        XXAPH#
     C*
     C     @@TYPO        IFNE      'F'
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     XCNAME        ##DATA
     C*
     C                   CALL      'SRCENTER'    CNTRPL                 99
     C*
     C                   MOVEL     ##DATA        XCNAM2           60
     C*
     C                   MOVE      '0'           XXPRT
     C*
     C     ORKY          CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
      /Free
       charLGRH = *Blanks;
       *IN79 = *Off;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                charLGRH = CXlgrh;
             Else;
                charLGRH = 'LEGAL';
             EndIf;
             charLGRH = RightJstfy(charLGRH:10);
             *IN79 = *On;
          Endif;
       EndIf;
      /End-Free
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'    CNTRPL                 99
     C*
     C                   MOVEL     ##DATA        ORTITL
     C*
     C                   ENDIF                                                  @@TYPO<>F
     C*_______________________________________________________________
     C*
     C*  INITIAL READ OF WORKFILE
     C*
     C     @@FND         IFNE      '000'
     C                   READ      PE201                                  40
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *INOF
     C                   Z-ADD     E2EMP#        XXEMP#
     C                   Z-ADD     E2DIV         XXDIV
     C                   MOVEL(P)  E2CTG1        XXCTG1
     C                   Z-ADD     E2FYR         XXFYR
     C                   EXSR      SREFDT
     C                   MOVE      E2SORT        XXSORT
     C*
     C                   EXSR      SREMP
     C*
     C                   MOVE      *ON           *IN11
      /EJECT
     C*
     C*  MAIN LOOP BEGINS
     C*
     C     *IN40         DOWEQ     *OFF
     C*
     C*  EMPLOYER/DIVISION BREAK
     C*
     C     E2EMP#        IFNE      XXEMP#
     C     E2DIV         ORNE      XXDIV
     C     E2SORT        ORNE      XXSORT
     C*
     C                   EXSR      SRCTG1
     C*
     C                   EXSR      SRYR
     C*
     C                   EXSR      EMPBRK
     C*
     C                   EXSR      SREMP
     C*
     C                   Z-ADD     *ZERO         PAGE
     C                   MOVE      *ZERO         XXREM             5 2
     C     XXPAGE        DIV       2             XX52              5 0
     C                   MVR                     XXREM
     C*
     C     XXREM         IFEQ      *ZERO
     C     @@TYPO        ANDNE     'F'
     C                   ADD       1             XXPAGE
     C                   ENDIF                                                  XXREM=*ZERO/@@TYPO<>
     C*
     C     *IN50         IFEQ      *ON
     C     *IN51         OREQ      *ON
     C     *IN52         OREQ      *ON
     C                   MOVEA     '000'         *IN(50)
     C                   MOVEA     '100'         *IN(11)
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *IN50=*ON/*IN51=*ON
     C                   MOVE      E2SORT        XXSORT
     C*
     C                   ELSE
     C*
     C*  YEAR BREAK WITHIN EMPLOYER/DIVISION
     C*
     C     E2FYR         IFNE      XXFYR
     C*
     C                   EXSR      SRCTG1
     C*
     C                   EXSR      SRYR
     C*
     C     *IN50         IFEQ      *ON
     C     *IN51         OREQ      *ON
     C                   MOVEA     '00'          *IN(50)
     C                   MOVEA     '010'         *IN(11)
     C                   ENDIF                                                  *IN50=*ON/*IN51=*ON
     C*
     C                   ELSE
     C*
     C*  CATEGORY 1 BREAK
     C*
     C     E2CTG1        IFNE      XXCTG1
     C     *IN82         ANDEQ     *ON
     C*
     C                   EXSR      SRCTG1
     C*
     C     *IN50         IFEQ      *ON
     C                   MOVE      *OFF          *IN50
     C                   MOVEA     '001'         *IN(11)
     C                   ENDIF                                                  *IN50=*ON
     C*
     C                   ENDIF                                                  E2CTG1<>XXCTG1
     C*
     C                   ENDIF                                                  E2FYR<>XXFYR
     C*
     C                   ENDIF                                                  E2EMP#<>XXEMP#
     C*
     C*  GET ADDTL ENTITY/LOCATION RECORD
     C*
     C     ALKY          CHAIN     PAL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   ALNAME
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVEL     ALNAME        XXNAME           40
     C*
     C                   CALL      'WA005R'
     C                   PARM      XXEMP#        #@EMP#
     C                   PARM      XXDIV         #@DIV
     C                   PARM      XXEMNM        #@NAME
     C                   PARM                    #@EDN
     C*
     C                   MOVEL(P)  #@EDN         XXEDN            50
     C*
     C*  DON'T COUNT INACTIVE EMPLOYER IF RUN FOR ACTIVE ACCOUNTS ONLY .
     C*  ACTIVE FLAG IS DETERMINED BY SREMP
     C*
     C                   MOVE      'N'           SKIP              1
     C*
     C     @@ACFL        IFEQ      'Y'
     C     ACTIVE        ANDEQ     'N'
     C     XXPROC        OREQ      'N'
     C                   MOVE      'Y'           SKIP
     C                   ENDIF                                                  @@ACFL=Y
     C*
     C     SKIP          IFEQ      'N'
     C     E2PRT         ANDEQ     'Y'
     C                   SETON                                            20
     C                   ELSE
     C                   SETOFF                                           20
     C                   ENDIF
     C*
     C   20              ADD       E2ACMP        L1ACMP
     C   20              ADD       E2AMED        L1AMED
     C   20              ADD       E2ALEG        L1ALEG
     C   20              ADD       E2AOTH        L1AOTH
     C     E2ACMP        ADD       E2AMED        XXATOT
     C                   ADD       E2ALEG        XXATOT
     C                   ADD       E2AOTH        XXATOT
     C   20              ADD       XXATOT        L1ATOT
     C*
     C   20              ADD       E2PCMP        L1PCMP
     C   20              ADD       E2PMED        L1PMED
     C   20              ADD       E2PLEG        L1PLEG
     C   20              ADD       E2POTH        L1POTH
     C     E2PCMP        ADD       E2PMED        XXPTOT
     C                   ADD       E2PLEG        XXPTOT
     C                   ADD       E2POTH        XXPTOT
     C   20              ADD       XXPTOT        L1PTOT
     C*
     C     E2GCMP        ADD       E2GMED        XXGTOT
     C                   ADD       E2GLEG        XXGTOT
     C                   ADD       E2GEXP        XXGTOT
     C   20              ADD       XXGTOT        L1GTOT
     C   20              ADD       E2GCMP        L1GCMP
     C   20              ADD       E2GMED        L1GMED
     C   20              ADD       E2GLEG        L1GLEG
     C   20              ADD       E2GEXP        L1GOTH
     C     E2GPCO        ADD       E2GPME        XXGPOT
     C                   ADD       E2GPLE        XXGPOT
     C                   ADD       E2GPEX        XXGPOT
     C   20              ADD       XXGPOT        L1GPOT
     C   20              ADD       E2GPCO        L1GPCO
     C   20              ADD       E2GPME        L1GPME
     C   20              ADD       E2GPLE        L1GPLE
     C   20              ADD       E2GPEX        L1GPEX
     C     E2RCCO        ADD       E2RCME        XXRCOT
     C                   ADD       E2RCLE        XXRCOT
     C                   ADD       E2RCEX        XXRCOT
     C   20              ADD       XXRCOT        L1RCOT
     C   20              ADD       E2RCCO        L1RCCO
     C   20              ADD       E2RCME        L1RCME
     C   20              ADD       E2RCLE        L1RCLE
     C   20              ADD       E2RCEX        L1RCEX
     C     E2RTCO        ADD       E2RTME        XXTROT
     C                   ADD       E2RTLE        XXTROT
     C                   ADD       E2RTEX        XXTROT
     C   20              ADD       XXTROT        L1TROT
     C   20              ADD       E2RTCO        L1RTCO
     C   20              ADD       E2RTME        L1RTME
     C   20              ADD       E2RTLE        L1RTLE
     C   20              ADD       E2RTEX        L1RTEX
     C*
     C                   SUB       E2PCMP        E2RCMP
     C                   SUB       E2PMED        E2RMED
     C                   SUB       E2PLEG        E2RLEG
     C                   SUB       E2POTH        E2ROTH
     C*
     C   20              ADD       E2RCMP        L1RCMP
     C   20              ADD       E2RMED        L1RMED
     C   20              ADD       E2RLEG        L1RLEG
     C   20              ADD       E2ROTH        L1ROTH
     C     E2RCMP        ADD       E2RMED        XXRTOT
     C                   ADD       E2RLEG        XXRTOT
     C                   ADD       E2ROTH        XXRTOT
     C   20              ADD       XXRTOT        L1RTOT
     C*
     C     E2PCMP        ADD       E2RCMP        E2ICMP
     C     E2PMED        ADD       E2RMED        E2IMED
     C     E2PLEG        ADD       E2RLEG        E2ILEG
     C     E2POTH        ADD       E2ROTH        E2IOTH
     C     XXPTOT        ADD       XXRTOT        E2ITOT
     C     E2ICMP        ADD       E2IMED        XXITOT
     C                   ADD       E2ILEG        XXITOT
     C                   ADD       E2IOTH        XXITOT
     C*
     C***  Begin Add  ***  06/22/22  ************************
       Exsr GetClaimData;
     C***  End   Add  ***  06/22/22  ************************
     C*
     C     *IN20         IFEQ      *ON
     C                   ADD       E2ICMP        L1ICMP
     C                   ADD       E2IMED        L1IMED
     C                   ADD       E2ILEG        L1ILEG
     C                   ADD       E2IOTH        L1IOTH
     C                   ADD       XXITOT        L1ITOT
     C*
     C*  ADD TO CLAIM COUNTS
     C*
      * If Open/CLose flag is blank, don't add to totals
     C     E2OPCL        IFNE      ' '
     C*
     C                   ADD       1             L1TOTC            7 0
     C*
     C     E2OPCL        IFEQ      'C'
     C                   ADD       1             L1CLOS
     C                   ELSE
     C                   ADD       1             L1OPEN
     C                   ENDIF                                                  E2OPCL=C
     C                   ENDIF                                                  E2OPCL = ' '
      *
     C*  PRINT CLAIM DETAIL
     C*
     C                   EXSR      SRPRT
     C*
     C                   ELSE
     C***  Begin Del  ***  06/22/22  ************************
     C*****CMKY*         CHAIN     PCM01                              60
     C***  End   Del  ***  06/22/22  ************************
      *
     C                   ENDIF                                                  SKIP=N/E2PRT=Y
     C*
     C                   EXSR      UPDTE2
     C*
     C                   READ      PE201                                  40
     C*
     C                   ENDDO                                                  *IN40=*OFF
     C*
     C                   EXSR      SRCTG1
     C*
     C                   EXSR      SRYR
     C*
     C                   EXSR      EMPBRK
     C*
     C                   EXSR      SREMP
     C*_______________________________________________________________
     C*
     C     @@TYPO        IFNE      'F'
     C*
     C*  PRINT MESSAGE IF NO MEMBER WAS PRINTED
     C*
     C     XXCNT         IFEQ      *ZERO
     C                   EXCEPT    HEAD
     C                   EXCEPT    NORECS
      * If have printed member, print totals at end of report.
     C                   ELSE
     C                   EXCEPT    HEAD
     C*
     C     LRAMT         IFNE      *ZERO
     C     LRITOT        MULT      100           XXWK92
     C     XXWK92        DIV(H)    LRAMT         LRLOSS            7 2          GRAND LOSS RATO
     C                   ELSE
     C                   Z-ADD     *ZERO         LRLOSS
     C                   ENDIF                                                  L3AMT<>*ZERO
     C*
     C                   EXCEPT    TOTLR
      *
     C                   ENDIF                                                  XXCNT=*ZERO
     C*
      /Free
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
     C                   CLOSE     QSYSPRT
     C*
     C                   ENDIF                                                  @@TYPO<>F
     C*
     C     TERM          TAG
     C*
     C                   MOVE      *ON           *INLR
     C*_______________________________________________________________
     C*
      /EJECT
     C*===============================================================
     C*  EMPBRK - SUBROUTINE TO LOAD & PRINT EMPLOYER TOTALS, ALL YEARS
     C*===============================================================
     C*
     C     EMPBRK        BEGSR
     C*
     C     L3AMT         IFNE      *ZERO
     C     L3ITOT        MULT      100           XXWK92
     C     XXWK92        DIV(H)    L3AMT         L3LOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L3LOSS
     C                   ENDIF                                                  L3AMT<>*ZERO
     C*
     C     L3AMT         DIV       1000          THPREM
     C*
     C     THPREM        IFNE      *ZERO
     C     L3TOTC        DIV(H)    THPREM        L3FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L3FREQ
     C                   ENDIF                                                  THPREM<>*ZERO
     C*
     C     *IN50         IFEQ      *ON
     C     L3TOTC        ANDNE     *ZERO
     C     @@TYPO        ANDNE     'F'
     C*
     C     @@SEQ         IFNE      'G'
     C     @@SEQ         ANDNE     '1'
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTHD
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF=*ON
     C*
     C                   EXCEPT    TOTE
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTHD
     C                   EXCEPT    TOTHD2
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF=*ON
     C*
     C* PRINT TOTAL EARNED AND CALCULATE OVERALL LOSS RATIO
     C                   ADD       L3AMT         LRAMT
     C                   EXCEPT    TOTE2
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN50=*ON
     C*
     C     *IN50         IFEQ      *ON
     C     L3TOTC        ANDEQ     *ZERO
     C     L3AMT         ANDGT     *ZERO
     C     @@TYPO        ANDNE     'F'
     C                   ADD       L3AMT         LRAMT
     C                   EXCEPT    TOTE2
     C*
     C                   ENDIF                                                  *IN50=*ON
     C*
     C                   Z-ADD     *ZERO         L3AMT            11 2
     C                   Z-ADD     *ZERO         L3ACMP           11 2
     C                   Z-ADD     *ZERO         L3AMED           11 2
     C                   Z-ADD     *ZERO         L3ALEG           11 2
     C                   Z-ADD     *ZERO         L3AOTH           11 2
     C                   Z-ADD     *ZERO         L3ATOT           11 2
     C*
     C                   Z-ADD     *ZERO         L3PCMP           11 2
     C                   Z-ADD     *ZERO         L3PMED           11 2
     C                   Z-ADD     *ZERO         L3PLEG           11 2
     C                   Z-ADD     *ZERO         L3POTH           11 2
     C                   Z-ADD     *ZERO         L3PTOT           11 2
     C*
     C                   Z-ADD     *ZERO         L3RCMP           11 2
     C                   Z-ADD     *ZERO         L3RMED           11 2
     C                   Z-ADD     *ZERO         L3RLEG           11 2
     C                   Z-ADD     *ZERO         L3ROTH           11 2
     C                   Z-ADD     *ZERO         L3RTOT           11 2
     C*
     C                   Z-ADD     *ZERO         L3ICMP           11 2
     C                   Z-ADD     *ZERO         L3IMED           11 2
     C                   Z-ADD     *ZERO         L3ILEG           11 2
     C                   Z-ADD     *ZERO         L3IOTH           11 2
     C                   Z-ADD     *ZERO         L3ITOT           11 2
     C                   Z-ADD     *ZERO         L3AMT            11 2
     C*
     C                   Z-ADD     *ZERO         L3OPEN
     C                   Z-ADD     *ZERO         L3TOTC
     C                   Z-ADD     *ZERO         L3CLOS
      *
     C                   Z-ADD     *ZERO         L3GCMP           11 2
     C                   Z-ADD     *ZERO         L3GMED           11 2
     C                   Z-ADD     *ZERO         L3GLEG           11 2
     C                   Z-ADD     *ZERO         L3GOTH           11 2
     C                   Z-ADD     *ZERO         L3GTOT           11 2
     C*
     C                   Z-ADD     *ZERO         L3GPCO           11 2
     C                   Z-ADD     *ZERO         L3GPME           11 2
     C                   Z-ADD     *ZERO         L3GPLE           11 2
     C                   Z-ADD     *ZERO         L3GPEX           11 2
     C                   Z-ADD     *ZERO         L3GPOT           11 2
     C*
     C                   Z-ADD     *ZERO         L3RCCO           11 2
     C                   Z-ADD     *ZERO         L3RCME           11 2
     C                   Z-ADD     *ZERO         L3RCLE           11 2
     C                   Z-ADD     *ZERO         L3RCEX           11 2
     C                   Z-ADD     *ZERO         L3RCOT           11 2
     C*
     C                   Z-ADD     *ZERO         L3RTCO           11 2
     C                   Z-ADD     *ZERO         L3RTME           11 2
     C                   Z-ADD     *ZERO         L3RTLE           11 2
     C                   Z-ADD     *ZERO         L3RTEX           11 2
     C                   Z-ADD     *ZERO         L3TROT           11 2
     C*
     C                   MOVE      E2FND         XXFND
     C*
     C                   ENDSR                                                  EMPBRK
     C*
       //***  Begin Add  ***  06/22/22  ************************
      /Free
       //============================================================
       // Get claim specific data
       //============================================================
       Begsr GetClaimData;
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'E2SS#'       QQNAME           25
     C     E2SS#         PARM      E2SS#         QQVAL           100

        //  Get the nature, pare and cause codes from the claim
        Clear CMnatr;
        Clear NAdesc;
        Clear CMpart;
        Clear PAdesc;
        Clear CMcaus;
        Clear CAdesc;
        Chain cmKy pcm01;
        // Get the Nature Description
        xxKey = CMnatr;
        Chain tbKy pna01;
        // Get the Part Description
        xxKey = CMpart;
        Chain tbKy ppa01;
        // Get the Cause Description
        xxKey = CMcaus;
        Chain tbKy pca01;
       Endsr;
      /End-free
       //***  End   Add  ***  06/22/22  ************************
      /EJECT
     C*===============================================================
     C*  SET INDICATOR FOR ROUTING PAGE
     C*===============================================================
     C*
     C     ROUTPG        BEGSR
     C*
     C     @@QUE         COMP      *BLANKS                            090908
     C*
     C     XXEMP#        COMP      *ZERO                              7070       XXEMP#<>*ZERO
     C*
     C     @@DIV         COMP      *BLANK                             7878       @@DIV<>*BLANK
     C*
     C     XXRGN#        COMP      *ZERO                              7171       XXRGN#<>*ZERO
     C*
     C     XXAGT#        COMP      *ZERO                              7272       XXAGT#<>*ZERO
     C*
     C     XXZIP         COMP      *ZERO                              7373       XXZIP<>*ZERO
     C*
     C     @@SELO        COMP      'Y'                                    74     @@SELO=Y
     C*
     C     @@SELC        COMP      'Y'                                    75     @@SELC=Y
     C*
     C     XXMNTH        COMP      *ZERO                              7676       XXMNTH<>*ZERO
     C*
     C                   SELECT
     C*
     C     XXCNTY        WHENEQ    '*S'
     C*
     C                   Z-ADD     *ZERO         T                 3 0
     C*
     C     *LOVAL        SETLL     PCN02
     C                   READ      PCN02                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   ADD       1             T
     C*
     C                   SELECT                                                 PLACE COMMA
     C     T             WHENLE    26
     C     T             ANDNE     1
     C                   CAT       ',':0         XXSEL1
     C     T             WHENLE    51
     C     T             ANDNE     1
     C                   CAT       ',':0         XXSEL2
     C     T             WHENLE    76
     C     T             ANDNE     1
     C                   CAT       ',':0         XXSEL3
     C     T             WHENLE    99
     C     T             ANDNE     1
     C                   CAT       ',':0         XXSEL4
     C                   ENDSL
     C*
     C     2             SUBST(P)  CNKEY:3       CNCNTY            2
     C*
     C                   SELECT
     C     T             WHENLE    25                                           LOAD NEXT CNTY
     C                   CAT       CNCNTY:0      XXSEL1           75
     C     T             WHENLE    50
     C                   CAT       CNCNTY:0      XXSEL2           75
     C     T             WHENLE    75
     C                   CAT       CNCNTY:0      XXSEL3           75
     C     T             WHENLE    100
     C                   CAT       CNCNTY:0      XXSEL4           75
     C                   ENDSL
     C*
     C                   READ      PCN02                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     XXCNTY        WHENEQ    *BLANKS
     C                   MOVEL(P)  '*ALL'        XXSEL1
     C*
     C                   OTHER
     C                   MOVEL(P)  XXCNTY        XXSEL1
     C*
     C                   ENDSL
     C*
     C     XXSEL2        COMP      *BLANK                                 92
     C     XXSEL3        COMP      *BLANK                                 93
     C     XXSEL4        COMP      *BLANK                                 94
     C*_______________________________________________________________
     C*
     C                   CLEAR                   NM1
     C                   CLEAR                   NM2
     C                   Z-ADD     1             Y                 3 0
     C*
     C                   MOVEL(P)  XXFPRM        XXNUM
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C     ' '           CHECKR    XXNUM         XXLEN             3 0
     C                   SUB       1             XXLEN
     C                   MOVEA     XXNUM         NM1
     C*
     C     XXZERO        DO        XXLEN         X                 3 0
     C                   MOVEA     NM1(X)        NM2(Y)
     C                   ADD       1             Y
     C                   ENDDO                                                  XXZERO->X->XXLEN
     C*
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   MOVEA(P)  NM2           XXNUM
     C*
     C     XXNUM         IFEQ      *BLANKS
     C                   MOVEL(P)  '0'           X1PRNG           35
     C                   ELSE
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         X1PRNG
     C                   ENDIF                                                  XXNUM=*BLANKS
     C*
     C                   CAT       '-':1         X1PRNG
     C*
     C                   CLEAR                   NM1
     C                   CLEAR                   NM2
     C                   Z-ADD     1             Y
     C*
     C                   MOVEL(P)  XXTPRM        XXNUM
     C     '0'           CHECK     XXNUM         XXZERO
     C     ' '           CHECKR    XXNUM         XXLEN
     C                   SUB       1             XXLEN
     C                   MOVEA     XXNUM         NM1
     C*
     C     XXZERO        DO        XXLEN         X
     C                   MOVEA     NM1(X)        NM2(Y)
     C                   ADD       1             Y
     C                   ENDDO                                                  XXZERO->X->XXLEN
     C*
     C                   MOVEA(P)  NM2           XXNUM
     C*
     C     XXNUM         IFEQ      *BLANKS
     C                   CAT       '0':1         X1PRNG
     C                   ELSE
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       X1PRNG
     C                   ENDIF                                                  XXNUM=*BLANKS
     C*
     C     PREM          CAT(P)    X1PRNG:1      X2PRNG          100
     C                   CAT       MSG2:1        X2PRNG
     C*
     C     PREM          CAT(P)    X1PRNG:1      X3PRNG          100
     C*
     C                   CAT       '**':1        X1PRNG
     C*_______________________________________________________________
     C*
      /Free
       If x8PFDT <> *Zeros;
          x8DATE = *Zeros;
          x8DATE = x8PFDT;

          mbrhd1 = mbr + ' ' + xxmm + '/' + xxdd + '/' + xxccyy + ' ' +
                   thru;
          x8DATE = *Zeros;
          x8DATE = x8PTDT;
          mbrhd = mbrhd1 + ' ' + xxmm + '/' + xxdd + '/' + xxccyy;
       Else;
      /End-Free
     C                   Z-ADD     X8FRDT        X8DATE
     C*
     C     MBR           CAT(P)    XXMM:1        MBRHD           100
     C                   CAT       '/':0         MBRHD
     C                   CAT       XXDD:0        MBRHD
     C                   CAT       '/':0         MBRHD
     C                   CAT       XXCCYY:0      MBRHD
     C                   CAT       THRU:1        MBRHD
     C*
     C                   Z-ADD     X8TODT        X8DATE
     C*
     C                   CAT       XXMM:1        MBRHD
     C                   CAT       '/':0         MBRHD
     C                   CAT       XXDD:0        MBRHD
     C                   CAT       '/':0         MBRHD
     C                   CAT       XXCCYY:0      MBRHD
      /Free
       Endif;
      /End-Free
     C*
     C                   OPEN      QSYSPRT
     C*
     C                   ENDSR                                                  ROUTPG
     C*
      /EJECT
     C*===============================================================
     C*  REMOVE CHARACTERS FROM END OF TABLE DESCRIPTIONS
     C*===============================================================
     C*
     C     SMALL         BEGSR
     C*
     C                   Z-ADD     *ZEROS        CHANGD            3 0
     C*
     C     X             DO        SHRINK
     C*
     C     31            SUB       X             Y
     C*
     C     DESC(Y)       IFNE      *BLANK
     C                   MOVEA     ' '           DESC(Y)
     C                   ADD       1             CHANGD
     C                   ENDIF                                                  DESC,Y<>*BLANK
     C*
     C                   ENDDO                                                  X->SHRINK
     C*
     C                   ENDSR                                                  SMALL
     C*
      /EJECT
     C*===============================================================
     C*  FORMAT NATURE/PART/CAUSE DESCRIPTIONS TO FIT ON REPORT
     C*===============================================================
     C*
     C     SMDESC        BEGSR
     C*
     C     XXLEN         SUB       58            TOLONG            3 0
     C     TOLONG        DIV       3             SHRINK            3 0
     C*
     C                   MOVEA(P)  NADESC        DESC
     C                   EXSR      SMALL
     C                   MOVEA(P)  DESC          NADESC
     C*
     C                   SUB       CHANGD        TOLONG
     C     TOLONG        DIV       2             SHRINK
     C*
     C                   MOVEA(P)  PADESC        DESC
     C                   EXSR      SMALL
     C                   MOVEA(P)  DESC          PADESC
     C*
     C     TOLONG        SUB       CHANGD        SHRINK
     C*
     C                   MOVEA(P)  CADESC        DESC
     C                   EXSR      SMALL
     C                   MOVEA(P)  DESC          CADESC
     C*
     C                   ENDSR                                                  SMDESC
     C*
      /EJECT
     C*===============================================================
     C*  CATEGORY ONE SUBROUTINE
     C*===============================================================
     C*
     C     SRCTG1        BEGSR
     C*
     C     *IN51         IFEQ      *ON
     C     L1TOTC        ANDNE     *ZERO
     C     *IN82         ANDEQ     *ON
     C     @@TYPO        ANDNE     'F'
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTHD
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF=*ON
     C*
     C                   EXCEPT    TOT2
     C*
     C                   ENDIF                                                  *IN51=*ON
     C*
     C                   ADD       L1OPEN        L2OPEN
     C                   ADD       L1CLOS        L2CLOS
     C                   ADD       L1TOTC        L2TOTC
     C*
     C                   ADD       L1OPEN        L3OPEN
     C                   ADD       L1CLOS        L3CLOS
     C                   ADD       L1TOTC        L3TOTC
     C*
     C                   ADD       L1OPEN        LROPEN
     C                   ADD       L1CLOS        LRCLOS
     C                   ADD       L1TOTC        LRTOTC
     C*
     C                   Z-ADD     *ZERO         L1OPEN
     C                   Z-ADD     *ZERO         L1CLOS
     C                   Z-ADD     *ZERO         L1TOTC
     C*
     C                   MOVE      E2CTG1        XXCTG1
     C*
     C                   ENDSR                                                  SRCTG1
     C*
      /EJECT
     C*===============================================================
     C*   EMPLOYER/DIV SUBROUTINE
     C*===============================================================
     C*
     C     SREMP         BEGSR
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   MOVE      *BLANKS       XXEMNM
     C                   MOVE      *BLANKS       XXEAD1
     C                   MOVE      *BLANKS       XXEAD2
     C                   MOVE      *BLANKS       XXECSZ           40
     C*
     C                   ELSE
     C*
     C*  CHECK TO SEE IF EMPLOYER IS ACTIVE. FIRST FIND THE MOST RECENT
     C*  EFFECTIVE DATE.
     C*
     C                   Z-ADD     1             Y
     C*
     C     *ZERO         LOOKUP    ED(Y)                                  60     ED = EFF DATES
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Y POINTS TO THE MOST CURRENT EFFECTIVE DATE. GET CANCEL DATE
     C*
     C                   Z-ADD     *ZERO         X8CNDT
     C                   MOVE      *OFF          *IN95                          DON'T PRINT CANC DT
     C                   MOVE      'Y'           ACTIVE                         ASSUME EMP IS ACTIVE
     C*
     C     Y             IFEQ      *ZERO
     C*
     C                   MOVE      'N'           ACTIVE                         NO EFF DT; INACTIVE
     C*
     C                   ELSE
     C*
     C     CD(Y)         IFNE      *ZERO                                        CD = CANC DATES
     C     CD(Y)         ANDLT     X7RNDT                                       CANC DT < RUN DT
     C*
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X8CNDT
     C*
     C                   MOVE      *ON           *IN95                          PRINT CANC DT
     C                   MOVE      'N'           ACTIVE
     C*
     C                   ENDIF                                                  *ZERO<>CD,Y<X7RNDT
     C*
     C                   ENDIF                                                  Y=*ZERO
     C*
     C*  LOAD EMPLOYER NAME & ADDRESS
     C*
     C                   MOVEL(P)  EMNAME        XXEMNM
     C*
     C     EMMAD1        IFEQ      *BLANK
     C     EMPAD1        ANDNE     *BLANK
     C*
     C                   MOVEL(P)  EMPAD1        XXEAD1
     C                   MOVEL(P)  EMPAD2        XXEAD2
     C     EMPCTY        CAT(P)    ',':0         XXECSZ
     C                   CAT       EMPST:1       XXECSZ
     C                   MOVEL     EMPZIP        XXZIP5            5
     C                   MOVE      EMPZIP        XXZIP4            4
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  EMMAD1        XXEAD1
     C                   MOVEL(P)  EMMAD2        XXEAD2
     C     EMMCTY        CAT(P)    ',':0         XXECSZ
     C                   CAT       EMMST:1       XXECSZ
     C                   MOVEL     EMMZIP        XXZIP5
     C                   MOVE      EMMZIP        XXZIP4
     C*
     C                   ENDIF                                                  EMMAD1=*BLANKS
     C*
     C                   CAT       XXZIP5:1      XXECSZ
     C*
     C     XXZIP4        IFNE      *ZERO
     C                   CAT       '-':0         XXECSZ
     C                   CAT       XXZIP4:0      XXECSZ
     C                   ENDIF                                                  XXZIP4<>*ZERO
     C*
     C     XXEAD2        IFEQ      *BLANK
     C                   MOVE      *ON           *IN81
     C                   ENDIF                                                  XXEADR2=*BLANK
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  GET REGION DESCRIPTION
     C*
     C     RGKY          CHAIN     PRE01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   REDESC
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVEL(P)  REDESC        XXRGDS           25
     C*
     C*  GET COUNTY DESCRIPTION
     C*
     C     E2EST         CAT(P)    E2CNTY        XXKEY
     C*
     C     TBKY          CHAIN     PCN01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   CNDESC
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVEL(P)  CNDESC        XXCNDS           25
     C*
     C*  SEARCH EMPLOYER DETAIL WITHIN YEAR RANGE FOR A RECORD
     C*  THAT QUALIFIES TO THE PREMIUM RANGE SPECIFIED ON PROMPT
     C*
     C                   MOVE      'N'           XXPROC            1
     C                   Z-ADD     *ZERO         XXXYR
     C*
     C     ELKY1         SETGT     PEL01
     C     ELKY1         READPE    PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     X7RNDT        IFGE      ELPFDT
     C     X7RNDT        ANDLE     ELPTDT
     C                   MOVE      ELFYR         XXXYR
     C                   ENDIF                                                  X7RNDT>=ELPFDT
     C*
     C                   EXSR      SRPREM
     C*
     C     XXPROC        IFEQ      'Y'
     C*
     C     XXXYR         IFEQ      *ZERO
     C                   MOVE      ELFYR         XXXYR
     C                   ENDIF                                                  XXXYR=*ZERO
     C*
     C                   LEAVE
     C*
     C                   ENDIF                                                  XXPROC=Y
     C*
     C     ELKY1         READPE    PEL01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  POSITION TO CORRECT EMPLOYER DETAIL FOR PROCESSING
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C*  GET AGENT ASSIGNMENT RECORD
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   AAFEIN
     C                   CLEAR                   AAMOD
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C*
     C*  GET AGENT NAME RECORD
     C*
     C     AGKY          CHAIN     PAG01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   AGNAME
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   MOVEL(P)  AGNAME        XXAGNM           25
     C*
     C*  GET PRODUCER NAME RECORD
     C*
     C     APKY          CHAIN     PAP01                              60
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   APLSN
     C                   CLEAR                   APFSN
     C*
     C                   ELSE
     C*
     C     APFSN         IFGT      *BLANKS
     C     APFSN         CAT(P)    APLSN:1       XXPDNM           25
     C     APFSN         CAT(P)    APLSN:1       #XPDNM           40
     C                   ELSE
     C                   MOVEL(P)  APLSN         XXPDNM
     C                   MOVEL(P)  APLSN         #XPDNM
     C                   ENDIF                                                  APFSN = *BLNK
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   Z-ADD     E2EMP#        XXEMP#
     C                   Z-ADD     E2DIV         XXDIV
     C*
     C                   ENDSR                                                  SREMP
     C*
      /EJECT
     C*===============================================================
     C*  SRPREM - THIS SUBROUTINE DETERMINES WHETHER THE EMPLOYER'S
     C*           PREMIUM MEETS THE SELECTED PREMIUM CRITERIA.
     C*===============================================================
     C*
     C     SRPREM        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXPREM           11 2
     C*
     C*  IF THE EMPLOYER IS ACTIVE, WE WILL USE THE ESTIMATED PREMIUM
     C*  FROM THE CURRENT YEAR.  IF THE EMPLOYER IS CANCELLED, WE
     C*  WILL CHECK THE PREMIUM FROM ALL YEARS WHICH ARE WITHIN
     C*  THE ACCIDENT DATE RANGE.
     C*
     C     ACTIVE        IFEQ      'Y'                                           ACTIVE EMP
     C*
     C     ELPFDT        IFLE      X7TODT
     C     ELPFDT        ORGE      X7PFDT
     C     ELPFDT        ANDLT     X7PTDT
     C*
     C     ELFAF         IFEQ      'A'
     C     ELAAPR        SUB       ELPRTD        XXPREM
     C                   ELSE
     C     ELANPR        SUB       ELPRTD        XXPREM
     C                   ENDIF                                                  ELFAF=A
     C*
     C                   ENDIF                                                  ELPFDT<=X7TODT
     C*
     C                   ELSE                                                   CANCELLED EMP
     C*
     C     ELPFDT        IFGE      X7FRDT
     C     ELPFDT        ANDLE     X7TODT
     C     ELPTDT        ORGE      X7FRDT
     C     ELPTDT        ANDLE     X7TODT
     C     ELPFDT        ORGE      X7PFDT
     C     ELPFDT        ANDLE     X7PTDT
     C     ELPTDT        ORGE      X7PFDT
     C     ELPTDT        ANDLE     X7PTDT
     C*
     C     ELFAF         IFEQ      'A'
     C     ELAAPR        SUB       ELPRTD        XXPREM
     C                   ELSE
     C     ELANPR        SUB       ELPRTD        XXPREM
     C                   ENDIF                                                  ELFAF=A
     C*
     C                   ENDIF                                                  ELPFDT>=X7FRDT
     C*
     C                   ENDIF                                                  ACTIVE=Y
     C*
     C     XXPREM        IFGE      XXFPRM
     C     XXPREM        ANDLE     XXTPRM
     C                   MOVE      'Y'           XXPROC
     C                   ENDIF                                                  XXPREM>=XXFPRM
     C*
     C                   ENDSR                                                  SRPREM
     C*
      /EJECT
     C*===============================================================
     C*  PRINT CLAIM DETAIL SUBROUTINE
     C*===============================================================
     C*
     C     SRPRT         BEGSR
     C*
     C                   MOVEL     E2CLMN        XXCLNM           20
     C                   MOVEL     E2ACDS        XXACDS           20
     C                   MOVEL     E2CLDS        XXCLDS           15
     C                   MOVEL     E2SPEC        XXSPEC            1
     C                   MOVEL     E2EXT         XXEXT             5
     C*
       //***  Begin Del  ***  06/22/22  ************************
     C**********         CALL      'SPIMASK'
     C**********         PARM      '*MASK'       QQMODE           10
     C**********         PARM      'SSN'         QQTYPE           25
     C**********         PARM      'E2SS#'       QQNAME           25
     C*****E2SS#         PARM      E2SS#         QQVAL           100
       //***  End   Del  ***  06/22/22  ************************
     C     3             SUBST     E2SS#:1       SS3               3
     C     2             SUBST     E2SS#:4       SS2               2
     C     4             SUBST     E2SS#:6       SS4               4
       //***  Begin Del  ***  06/22/22  ************************
     C**********         CALL      'SPIMASK'
     C**********         PARM      '*UNMASK'     QQMODE           10
     C**********         PARM      'SSN'         QQTYPE           25
     C**********         PARM      'E2SS#'       QQNAME           25
     C*****E2SS#         PARM      E2SS#         QQVAL           100
       //***  End   Del  ***  06/22/22  ************************
     C*
     C     E2OPCL        IFEQ      'C'
     C                   MOVEL     'C'           XXOPCL
     C                   ELSE
     C                   MOVEL     'O'           XXOPCL
     C                   ENDIF                                                  E2OPCL=C
     C*
     C*  GET THE DAY OF THE WEEK OF THE ACCIDENT
     C*
     C                   CALL      'SRDAY8'
     C                   PARM      E2ACDT        ##DATE
     C                   PARM      *BLANK        ##SDAY            4
     C                   PARM      *BLANK        ##LDAY            9
     C                   PARM      *BLANK        ##ERR             1
     C*
     C                   MOVEL(P)  ##LDAY        XXADAY            9
     C*
       //***  Begin Del  ***  06/22/22  ************************
     C***GET*THE NATURE, PART AND CAUSE CODES FROM THE CLAIM RECORD.
     C**********
     C*****CMKY*         CHAIN     PCM01                              60
     C**********
     C******IN60         IFEQ      *OFF
       //***  End   Del  ***  06/22/22  ************************
     C*
     C*  GET THE NATURE DESCRIPTION AND LOAD INTO ARRAY
     C*
     C                   Z-ADD     1             X
       //***  Begin Del  ***  06/22/22  ************************
     C**********         MOVEL(P)  CMNATR        XXKEY
     C**********
     C**********         CLEAR                   NADESC
     C*****TBKY*         CHAIN     PNA01                              60
       //***  End   Del  ***  06/22/22  ************************
     C     ' '           CHECKR    NADESC        XNLEN             3 0
     C                   Z-ADD     XNLEN         XXLEN
     C*
     C*  GET THE PART DESCRIPTION AND LOAD INTO ARRAY
     C*
       //***  Begin Del  ***  06/22/22  ************************
     C**********         MOVEL(P)  CMPART        XXKEY
     C**********
     C**********         CLEAR                   PADESC
     C*****TBKY*         CHAIN     PPA01                              60
       //***  End   Del  ***  06/22/22  ************************
     C     ' '           CHECKR    PADESC        XPLEN             3 0
     C                   ADD       XPLEN         XXLEN
     C*
     C*  GET THE CAUSE DESCRIPTION AND LOAD INTO ARRAY
     C*
       //***  Begin Del  ***  06/22/22  ************************
     C**********         MOVEL(P)  CMCAUS        XXKEY
     C**********
     C**********         CLEAR                   CADESC
     C*****TBKY*         CHAIN     PCA01                              60
       //***  End   Del  ***  06/22/22  ************************
     C     ' '           CHECKR    CADESC        XCLEN             3 0
     C                   ADD       XCLEN         XXLEN
     C*
     C     XXLEN         IFGT      60
     C                   EXSR      SMDESC
     C                   ENDIF                                                  XXLEN>58
     C*
     C     NADESC        CAT(P)    '/':0         XXNPC            60
     C                   CAT       PADESC:0      XXNPC
     C                   CAT       '/':0         XXNPC
     C                   CAT       CADESC:0      XXNPC
     C*
       //***  Begin Del  ***  06/22/22  ************************
     C**********         ENDIF                                                  *IN60=*OFF
       //***  End   Del  ***  06/22/22  ************************
     C*
     C     @@TYPO        IFNE      'F'
     C*
     C     *INOF         IFEQ      *ON
     C*
     C                   CALL      'WA005R'
     C                   PARM      E2EMP#        #@EMP#
     C                   PARM      E2DIV         #@DIV
     C                   PARM      XXEMNM        #@NAME
     C                   PARM                    #@EDN            80
     C*
     C                   MOVEL(P)  #@EDN         X2EDN
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *INOF
     C*
     C                   ELSE
     C*
     C     *IN11         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *IN11
     C*
     C                   ELSE
     C*
     C     *IN12         IFEQ      *ON
     C*
     C     *IN82         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTHD
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C*
     C                   ELSE
     C*
     C                   EXCEPT    FYRHD
     C*
     C                   ENDIF                                                  *IN82=*ON
     C*
     C                   MOVE      *OFF          *IN12
     C*
     C                   ELSE
     C*
     C     *IN13         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTHD
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *IN13
     C                   ENDIF                                                  *IN13=*ON
     C*
     C                   ENDIF                                                  *IN12=*ON
     C*
     C                   ENDIF                                                  *IN11=*ON
     C*
     C                   ENDIF                                                  *INOF=*ON
     C*
      * IF HAVE CASE NUMBER, PRINT DETAIL
     C     E2CASE        IFGT      *ZEROS
     C                   EXCEPT    DETAIL
     C                   ENDIF
     C*
     C                   ENDIF                                                  @@TYPO<>F
     C*
     C                   ADD       1             XXCNT                          COUNT PRINTS
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN51
     C                   MOVE      *ON           *IN52
     C*
     C                   ENDSR                                                  SRPRT
     C*
      /EJECT
     C*===============================================================
     C*  YEAR SUBROUTINE
     C*===============================================================
     C*
     C     SRYR          BEGSR
     C*
     C                   MOVE      XXCO#         SRCO#
     C                   MOVE      XXFND         SRFND
     C                   MOVE      XXFYR         SRFYR
     C                   MOVE      XXEMP#        SREMP#
     C                   MOVE      XXDIV         SRDIV
     C                   MOVE      *BLANK        SRERR             1
     C*
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#             3
     C                   PARM                    SRFND             3
     C                   PARM                    SRFYR             3
     C                   PARM                    SREMP#            9
     C                   PARM                    SRDIV             5
     C                   PARM      @@RNDT        SRRNDT            8
     C                   PARM      *BLANK        SRAMT            11
     C                   PARM      *BLANK        SRAMT2           11
     C                   PARM      '1'           SRTERM            1
     C*
     C                   MOVE      SRAMT         L1AMT            11 2
     C*
     C     L1AMT         IFNE      *ZERO
     C     L1ITOT        MULT      100           XXWK92           15 2
     C     XXWK92        DIV(H)    L1AMT         L1LOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L1LOSS
     C                   ENDIF                                                  L1AMT<>*ZERO
     C*
     C     L1AMT         DIV       1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L2TOTC        DIV(H)    THPREM        L1FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L1FREQ
     C                   ENDIF                                                  THPREM<>*ZERO
     C*
     C     *IN50         IFEQ      *ON
     C     L2TOTC        ANDNE     *ZERO
     C     @@TYPO        ANDNE     'F'
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTHD
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF=*ON
     C*
     C                   EXCEPT    TOT
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTHD
     C                   EXCEPT    DETHD
     C                   ADD       1             XXPAGE
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF=*ON
     C*
     C                   EXCEPT    TOT3
     C*
     C                   ENDIF                                                  *IN50=*ON
     C*
     C*  GET AGENT ASSIGNMENT RECORD
     C*
     C                   CLEAR                   AAFEIN
     C                   CLEAR                   AAMOD
     C     AAKY          CHAIN     PAA01                              60
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C*
     C*  GET AGENT NAME RECORD
     C*
     C                   CLEAR                   AGNAME
     C     AGKY          CHAIN     PAG01                              60
     C                   MOVEL(P)  AGNAME        XXAGNM           25
     C*
     C*  GET PRODUCER NAME RECORD
     C*
     C     APKY          CHAIN     PAP01                              60
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   XXPDNM
     C*
     C                   ELSE
     C*
     C     APFSN         IFGT      *BLANKS
     C     APFSN         CAT(P)    APLSN:1       XXPDNM           25
     C     APFSN         CAT(P)    APLSN:1       #XPDNM           40
     C                   ELSE
     C                   MOVEL(P)  APLSN         XXPDNM
     C                   MOVEL(P)  APLSN         #XPDNM
     C                   ENDIF                                                  APFSN = *BLNK
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ADD       L1ACMP        L3ACMP
     C                   ADD       L1AMED        L3AMED
     C                   ADD       L1ALEG        L3ALEG
     C                   ADD       L1AOTH        L3AOTH
     C                   ADD       L1ATOT        L3ATOT
     C*
     C                   ADD       L1PCMP        L3PCMP
     C                   ADD       L1PMED        L3PMED
     C                   ADD       L1PLEG        L3PLEG
     C                   ADD       L1POTH        L3POTH
     C                   ADD       L1PTOT        L3PTOT
     C*
     C                   ADD       L1RCMP        L3RCMP
     C                   ADD       L1RMED        L3RMED
     C                   ADD       L1RLEG        L3RLEG
     C                   ADD       L1ROTH        L3ROTH
     C                   ADD       L1RTOT        L3RTOT
     C*
     C                   ADD       L1ICMP        L3ICMP
     C                   ADD       L1IMED        L3IMED
     C                   ADD       L1ILEG        L3ILEG
     C                   ADD       L1IOTH        L3IOTH
     C                   ADD       L1ITOT        L3ITOT
     C                   ADD       L1AMT         L3AMT
      *
     C                   ADD       L1GCMP        L3GCMP
     C                   ADD       L1GMED        L3GMED
     C                   ADD       L1GLEG        L3GLEG
     C                   ADD       L1GOTH        L3GOTH
     C                   ADD       L1GTOT        L3GTOT
     C*
     C                   ADD       L1GCMP        LRGCMP
     C                   ADD       L1GMED        LRGMED
     C                   ADD       L1GLEG        LRGLEG
     C                   ADD       L1GOTH        LRGOTH
     C                   ADD       L1GTOT        LRGTOT
     C*
     C                   ADD       L1GPCO        L3GPCO
     C                   ADD       L1GPME        L3GPME
     C                   ADD       L1GPLE        L3GPLE
     C                   ADD       L1GPEX        L3GPEX
     C                   ADD       L1GPOT        L3GPOT
     C*
     C                   ADD       L1GPCO        LRGPCO
     C                   ADD       L1GPME        LRGPME
     C                   ADD       L1GPLE        LRGPLE
     C                   ADD       L1GPEX        LRGPEX
     C                   ADD       L1GPOT        LRGPOT
     C*
     C                   ADD       L1RCCO        L3RCCO
     C                   ADD       L1RCME        L3RCME
     C                   ADD       L1RCLE        L3RCLE
     C                   ADD       L1RCEX        L3RCEX
     C                   ADD       L1RCOT        L3RCOT
     C*
     C                   ADD       L1RCCO        LRRCCO
     C                   ADD       L1RCME        LRRCME
     C                   ADD       L1RCLE        LRRCLE
     C                   ADD       L1RCEX        LRRCEX
     C                   ADD       L1RCOT        LRRCOT
     C*
     C                   ADD       L1RTCO        L3RTCO
     C                   ADD       L1RTME        L3RTME
     C                   ADD       L1RTLE        L3RTLE
     C                   ADD       L1RTEX        L3RTEX
     C                   ADD       L1TROT        L3TROT
     C*
     C                   ADD       L1RTCO        LRRTCO
     C                   ADD       L1RTME        LRRTME
     C                   ADD       L1RTLE        LRRTLE
     C                   ADD       L1RTEX        LRRTEX
     C                   ADD       L1TROT        LRTROT
      *
     C                   ADD       L1ACMP        LRACMP
     C                   ADD       L1AMED        LRAMED
     C                   ADD       L1ALEG        LRALEG
     C                   ADD       L1AOTH        LRAOTH
     C                   ADD       L1ATOT        LRATOT
     C*
     C                   ADD       L1PCMP        LRPCMP
     C                   ADD       L1PMED        LRPMED
     C                   ADD       L1PLEG        LRPLEG
     C                   ADD       L1POTH        LRPOTH
     C                   ADD       L1PTOT        LRPTOT
     C*
     C                   ADD       L1RCMP        LRRCMP
     C                   ADD       L1RMED        LRRMED
     C                   ADD       L1RLEG        LRRLEG
     C                   ADD       L1ROTH        LRROTH
     C                   ADD       L1RTOT        LRRTOT
     C*
     C                   ADD       L1ICMP        LRICMP
     C                   ADD       L1IMED        LRIMED
     C                   ADD       L1ILEG        LRILEG
     C                   ADD       L1IOTH        LRIOTH
     C                   ADD       L1ITOT        LRITOT
     C*
     C                   Z-ADD     *ZERO         L1ACMP           11 2
     C                   Z-ADD     *ZERO         L1AMED           11 2
     C                   Z-ADD     *ZERO         L1ALEG           11 2
     C                   Z-ADD     *ZERO         L1AOTH           11 2
     C                   Z-ADD     *ZERO         L1ATOT           11 2
     C*
     C                   Z-ADD     *ZERO         L1PCMP           11 2
     C                   Z-ADD     *ZERO         L1PMED           11 2
     C                   Z-ADD     *ZERO         L1PLEG           11 2
     C                   Z-ADD     *ZERO         L1POTH           11 2
     C                   Z-ADD     *ZERO         L1PTOT           11 2
     C*
     C                   Z-ADD     *ZERO         L1RCMP           11 2
     C                   Z-ADD     *ZERO         L1RMED           11 2
     C                   Z-ADD     *ZERO         L1RLEG           11 2
     C                   Z-ADD     *ZERO         L1ROTH           11 2
     C                   Z-ADD     *ZERO         L1RTOT           11 2
     C*
     C                   Z-ADD     *ZERO         L1ICMP           11 2
     C                   Z-ADD     *ZERO         L1IMED           11 2
     C                   Z-ADD     *ZERO         L1ILEG           11 2
     C                   Z-ADD     *ZERO         L1IOTH           11 2
     C                   Z-ADD     *ZERO         L1ITOT           11 2
     C                   Z-ADD     *ZERO         L1AMT            11 2
     C*
     C                   Z-ADD     *ZERO         L2OPEN
     C                   Z-ADD     *ZERO         L2TOTC
     C                   Z-ADD     *ZERO         L2CLOS
      *
     C                   Z-ADD     *ZERO         L1GCMP           11 2
     C                   Z-ADD     *ZERO         L1GMED           11 2
     C                   Z-ADD     *ZERO         L1GLEG           11 2
     C                   Z-ADD     *ZERO         L1GOTH           11 2
     C                   Z-ADD     *ZERO         L1GTOT           11 2
     C*
     C                   Z-ADD     *ZERO         L1GPCO           11 2
     C                   Z-ADD     *ZERO         L1GPME           11 2
     C                   Z-ADD     *ZERO         L1GPLE           11 2
     C                   Z-ADD     *ZERO         L1GPEX           11 2
     C                   Z-ADD     *ZERO         L1GPOT           11 2
     C*
     C                   Z-ADD     *ZERO         L1RCCO           11 2
     C                   Z-ADD     *ZERO         L1RCME           11 2
     C                   Z-ADD     *ZERO         L1RCLE           11 2
     C                   Z-ADD     *ZERO         L1RCEX           11 2
     C                   Z-ADD     *ZERO         L1RCOT           11 2
     C*
     C                   Z-ADD     *ZERO         L1RTCO           11 2
     C                   Z-ADD     *ZERO         L1RTME           11 2
     C                   Z-ADD     *ZERO         L1RTLE           11 2
     C                   Z-ADD     *ZERO         L1RTEX           11 2
     C                   Z-ADD     *ZERO         L1TROT           11 2
     C*
     C                   Z-ADD     E2FYR         XXFYR
     C                   EXSR      SREFDT
     C*
     C                   ENDSR                                                  SRYR
     C*
      /EJECT
     C*===============================================================
     C*  UPDATE WDE2P FILE
     C*===============================================================
     C*
     C     UPDTE2        BEGSR
     C*
     C                   MOVEL(P)  XCNAME        E2CNAM
     C                   MOVEL(P)  XXCAD1        E2CAD1
     C                   MOVEL(P)  XXCAD2        E2CAD2
     C                   MOVEL(P)  XXCCTY        E2CCTY
     C                   MOVEL(P)  XXCST         E2CST
     C                   MOVEL(P)  XXCZIP        E2CZIP
     C                   Z-ADD     XXAARA        E2CARA
     C                   Z-ADD     XXAPH#        E2CPHN
     C*
     C                   MOVEL(P)  AGNAME        E2ANAM
     C                   Z-ADD     AGFEIN        E2AGFE
     C                   MOVEL(P)  REDESC        E2RGDS
     C                   MOVEL(P)  CNDESC        E2CNDS
     C                   MOVEL(P)  #XPDNM        E2PRNM
     C                   MOVEL(P)  *BLANKS       E2NCCI
     C                   MOVEL(P)  *BLANKS       E2NCDS
     C*
     C     E2CASE        IFNE      0
     C                   MOVEL(P)  CMNATR        E2NATR
     C                   MOVEL(P)  NADESC        E2NADS
     C                   MOVEL(P)  CMPART        E2PART
     C                   MOVEL(P)  PADESC        E2PADS
     C                   MOVEL(P)  CMCAUS        E2CAUS
     C                   MOVEL(P)  CADESC        E2CADS
     C*
     C                   Z-ADD     E2ICMP        E2TCMP
     C                   Z-ADD     E2IMED        E2TMED
     C                   Z-ADD     E2ILEG        E2TLEG
     C                   Z-ADD     E2IOTH        E2TOTH
     C                   Z-ADD     XXATOT        E2TPIP
     C                   Z-ADD     XXPTOT        E2TPTD
     C                   Z-ADD     XXRTOT        E2TRES
     C                   Z-ADD     XXITOT        E2TINC
     C*
     C*  GET THE NCCI AND ITS DESCRIPTION
      *
     C                   MOVEL(P)  CMNCCI        E2NCCI
      *
      * First check the juridition state xxNC...
     C                   MOVEL(P)  CMJRST        XXTBID
     C                   MOVE      'NC'          XXTBID
     C                   MOVEL(P)  CMNCCI        XXKEY2
     C     GTKY          CHAIN     WTGTL                              60
     C     *IN60         IFEQ      *ON
      *
      *... no entry found, use the default of '  NC' to get all
     C                   MOVEL(P)  '  NC'        XXTBID
     C     GTKY          CHAIN     WTGTL                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        E2NCDS
     C                   ELSE
     C                   MOVEL     *BLANKS       E2NCDS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      E2FND         XXFND
     C*
     C*  GET GROUP MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        E2FNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       FNANAM
     C                   ENDIF
     C*
     C                   Z-ADD     XXITOT        E2TINC
     C                   MOVEL(P)  ALNAME        E2CIDS
     C                   UPDATE    PE201
     C*
     C                   ENDSR                                                  UPDTE2
     C* ==============================================================
     C*    *INZSR  Initialization Subroutine
     C* ==============================================================
     C     *INZSR        BEGSR
     C*
     C*    Determine printing of SS#
     C     QQPRSS        IFEQ      'Y'
     C                   MOVE      *ON           *IN84
     C                   ELSE                                                   X. PRSS = Y
     C                   MOVE      *OFF          *IN84
     C                   ENDIF                                                  e. PRSS = Y
     C                   ENDSR
     C* ==============================================================
      /EJECT
      *
      *==================================================================*
      *  SREFDT - Get Emp Pol Eff Date range                             *
      *==================================================================*
     CSR   SREFDT        BEGSR
      *
     C                   Z-ADD     *ZERO         XXPFDT            8 0
     C                   Z-ADD     *ZERO         XXPTDT            8 0
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT
     C                   ENDIF
      *
     C                   ENDSR                                                  SREFDT
     C*_______________________________________________________________
      /EJECT
     O*
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E    08      ROUTE          1
     O                       ORDIS2              60
     O          E    08      ROUTE          1
     O                       ORDIS3              60
     O          E    08      ROUTE          1
     O                       ORDIS4              60
     O          E            ROUTE       1  2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE       2  2
     O                                           26 'COMPANY . . . . . . :'
     O                       XXCO#         Z   +002
     O          E            ROUTE          2
     O                                           26 'GROUP . . . . . . . :'
     O                       XXFND         Z   +002
      *
     O          E            ROUTE          2
     O                                           26 'POLICY DATE RANGE . :'
     O                       X8PFDT            +002 '  /  /    '
     O                                         +001 '-'
     O                       X8PTDT            +001 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'ACCIDENT DATE RANGE :'
     O                       X8FRDT            +002 '  /  /    '
     O                                         +001 '-'
     O                       X8TODT            +001 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'AS OF DATE  . . . . :'
     O                       X8RNDT            +002 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'PREMIUM RANGE . . . :'
     O                       X1PRNG            +002
     O          E            ROUTE          2
     O                                           26 'EMPLOYER/DIV  . . . :'
     O               70      XXEMP#        Z   +002
     O               70 78   XXDIV         Z   +001
     O               70N78                       38 '/'
     O               70N78                     +000 '*ALL'
     O              N70                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'ACTIVE ACCOUNTS ONLY:'
     O                       @@ACFL            +002
     O          E            ROUTE          2
     O                                           26 'REGION  . . . . . . :'
     O               71      XXRGN#        Z   +002
     O              N71                          32 '*ALL'
     O          E            ROUTE          1
     O                                           26 'COUNTY  . . . . . . :'
     O                       XXSEL1             103
     O          E   N92      ROUTE          1
     O                       XXSEL2             103
     O          E   N93      ROUTE          1
     O                       XXSEL3             103
     O          E   N94      ROUTE          1
     O                       XXSEL4             103
     O          E            ROUTE       1  2
     O                                           26 'AGENT . . . . . . . :'
     O               72      XXAGT#        Z   +002
     O              N72                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'ZIP CODE  . . . . . :'
     O               73      XXZIP             +002
     O              N73                          32 '*ALL'
     O          E    74      ROUTE          2
     O                                           26 'SELECT OPEN CASES   :'
     O                       @@SELO            +002
     O          E    75      ROUTE          2
     O                                           26 'SELECT CLOSED CASES :'
     O                       @@SELC            +002
     O               76                          40 '(CLOSED WITHIN THE LAST'
     O               76      XXMNTH        Z   +001
     O               76                        +001 'MONTHS)'
      *
     O          E            ROUTE          2
     O                                           26 'EMPLOYERS W/NO CLMS :'
     O                       @@SELN            +002
     O          E            ROUTE          2
     O                                           26 'PRINT SS NUMBER     :'
     O                       QQPRSS            +002
     O          E            ROUTE          2
     O                                           26 'DUPLEX PRINTING . . :'
     O                       @@DPLX            +002
     O          E            ROUTE          2
     O                                           26 'TYPE OF OUTPUT  . . :'
     O                       @@TYPO            +002
     O          E            ROUTE          2
     O                                           24 'SORT SEQUENCE IS BY'
     O                       XXSEQD            +001
     O          E            ROUTE          2
     O                       MSG1                57
     O          E            ROUTE          1 69
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*_______________________________________________________________
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAM2              90
     O                       DSDATE        Y    120
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          114 'PAGE:'
     O                       PAGE          Z    120
     O          E            HEAD           1
     O                                           10 'COMPANY:'
     O                       XCNAME              77
     O                                          109 'AS OF DATE:'
     O                       X8RNDT             120 '  /  /    '
     O          E            HEAD           1
     O                       XXCAD1              57
     O          E            HEAD           1
     O                       XXCCSZ              57
     O          E            HEAD           2
     O                                           25 'PHONE #:'
     O                                         +001 '('
     O                       XXAARA            +000 '   )'
     O                       XXAPH#            +001 '   -    '
     O          E            HEAD           2
     O                                            8 'GROUP:'
     O                       XXFND         Z     14
     O                       XXFNNM              77
     O          E            HEAD           1
     O                       MBRHD              107
     O*_______________________________________________________________
     O*
     O          E            HEAD1          1
     O                                           12 'SORT:'
     O                       XXSEQD            +002
     O          E            HEAD1          1
     O                       X2PRNG             107
     O          E            HEAD1       1  1
     O                                           11 'EMPLOYER:'
     O                       XXEMNM              57
     O                                           79 'EMPLOYER #:'
     O                       E2EMP#        Z     91
     O                       E2DIV         Z     97
     O          E            HEAD1          1
     O                       XXEAD1              57
     O                                           79 'AGENT . . :'
     O                       E2AGT#        Z     91
     O                       XXAGNM             117
     O          E            HEAD1          1
     O               81      XXECSZ              57
     O              N81      XXEAD2              57
     O                                           79 'COUNTY    :'
     O                       E2CNTY              91
     O                       XXCNDS             117
     O          E            HEAD1          1
     O              N81      XXECSZ              57
     O                                           79 'REGION    :'
     O                       E2RGN#        Z     91
     O                       XXRGDS             117
     O          E            HEAD1          1
     O                                           79 'PRODUCER  :'
     O                       E2PROD        Z     91
     O                       XXPDNM             117
     O          E            HEAD1          1
     O               95                          20 'CANCELLATION DATE:'
     O               95      X8CNDT        Y     32
     O               82                          79 'LOCATION:'
     O               82      XXNAME             120
     O*_______________________________________________________________
     O*
     O          E            HEAD2       1  2
     O                       X2PRNG             117
     O          E            HEAD2          1
     O                                            9 'EMPLOYER:'
     O                       X2EDN             +002
     O                                           84 'AGENT:'
     O                       E2AGT#        Z     91
     O                       XXAGNM             117
     O          E            HEAD2          1
     O               95                          20 'CANCELLATION DATE:'
     O               95      X8CNDT        Y     32
     O                                           84 'PRODUCER:'
     O                       E2PROD        Z     91
     O                       XXPDNM             117
     O          E    82      HEAD2          1
     O                                            9 'LOCATION:'
     O                       E2CTG1              20
     O                       XXNAME              61
     O*_______________________________________________________________
     O*
      *
     O          E            DETHD       1  1
     O                                           13 'POLICY YEAR:'
     O                       XXFYR         Z   +002
     O                                         +003 'EFFECTIVE:'
     O                       XXPFDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       XXPTDT            +001 '  /  /    '
      *
     O*_______________________________________________________________
     O*
     O          E            DETAIL      1  0
     O                                           10 'YR-CLAIM#'
     O                                           23 'CLAIMANT'
     O                                           47 'SOC SEC #'
     O                                           59 'LOST TIME'
     O                                           68 'STATUS'
     O                                           85 'DESC OF INJURY'
     O          E            DETAIL         1
     O                                           12 '___________'
     O                                           35 '____________________'
     O                                           48 '___________'
     O                                           59 '_________'
     O                                           68 '______'
     O                                           91 '____________________'
     O          E            DETAIL         1
     O                       E2FYR                4
     O                                            5 '-'
     O                       E2CASE              12
     O                       XXCLNM              35
     O               84      SS3                 40
     O               84                          41 '-'
     O               84      SS2                 43
     O               84                          44 '-'
     O               84      SS4                 48
     O*
     O                       E2LSTM              55
     O                       XXOPCL              65
     O                       XXACDS              91
     O          E            DETAIL         0
     O                                           17 'DT OF INJURY'
     O                                           33 'DAY OF INJURY'
     O                                           47 'CLOSED DATE'
     O                                           68 'NATURE/PART/CAUSE'
     O          E            DETAIL         1
     O                                           17 '____________'
     O                                           33 '_____________'
     O                                           47 '___________'
     O                                           68 '____________________'
     O                                           90 '____________________'
     O                                          110 '____________________'
     O          E            DETAIL         1
     O                       E2ACDT              16 '  /  /    '
     O                       XXADAY              29
     O                       E2CLDT              46 '  /  /    '
     O                       XXNPC              112
     O          E            DETAIL         0
     O              N82                          15 'LOCATION  '
     O                                           71 'CLASS CODE'
     O          E            DETAIL         1
     O                                           29 '________________________'
     O                                           53 '________________________'
     O                                           58 '_____'
     O                                           83 '______________________'
     O          E            DETAIL         1
     O              N82      E2CTG1              15
     O              N82      XXNAME              58
     O                       E2CLAS        Z     66
     O                       E2SPEC              68
     O                       E2EXT               74
     O                       XXCLDS              91
      *
      *
     O          E            DETAIL         0
     O                                           56 'COMPENSATION'
     O                                           70 'MEDICAL'
     O              N79                          85 'LEGAL'
     O               79      charLGRH            85
     O                                          101 'OTHER'
     O                                          117 'TOTAL'
     O          E            DETAIL         1
     O                                           56 '____________'
     O                                           72 '____________'
     O                                           88 '____________'
     O                                          104 '____________'
     O                                          120 '____________'
     O          E            DETAIL         1
     O                                           35 'GROSS PAID THIS PERIOD:'
     O                       E2GPCO        J     56
     O                       E2GPME        J     72
     O                       E2GPLE        J     88
     O                       E2GPEX        J    104
     O                       XXGPOT        J    120
     O          E            DETAIL         1
     O                                           35 'RECOVERED THIS PERIOD :'
     O                       E2RCCO        J     56
     O                       E2RCME        J     72
     O                       E2RCLE        J     88
     O                       E2RCEX        J    104
     O                       XXRCOT        J    120
     O          E            DETAIL         1
     O                                           35 'NET PAID THIS PERIOD  :'
     O                       E2ACMP        J     56
     O                       E2AMED        J     72
     O                       E2ALEG        J     88
     O                       E2AOTH        J    104
     O                       XXATOT        J    120
     O          E            DETAIL         1
     O                                           35 'GROSS PAID TO DATE  . :'
     O                       E2GCMP        J     56
     O                       E2GMED        J     72
     O                       E2GLEG        J     88
     O                       E2GEXP        J    104
     O                       XXGTOT        J    120
     O          E            DETAIL         1
     O                                           35 'RECOVERED TO DATE . . :'
     O                       E2RTCO        J     56
     O                       E2RTME        J     72
     O                       E2RTLE        J     88
     O                       E2RTEX        J    104
     O                       XXTROT        J    120
     O          E            DETAIL         1
     O                                           35 'NET PAID TO DATE  . . :'
     O                       E2PCMP        J     56
     O                       E2PMED        J     72
     O                       E2PLEG        J     88
     O                       E2POTH        J    104
     O                       XXPTOT        J    120
     O          E            DETAIL         1
     O                                           35 'OUTSTANDING RESERVES  :'
     O                       E2RCMP        J     56
     O                       E2RMED        J     72
     O                       E2RLEG        J     88
     O                       E2ROTH        J    104
     O                       XXRTOT        J    120
     O          E            DETAIL         1
     O                                           35 'TOTAL INCURRED  . . . :'
     O                       E2ICMP        J     56
     O                       E2IMED        J     72
     O                       E2ILEG        J     88
     O                       E2IOTH        J    104
     O                       XXITOT        J    120
     O*_______________________________________________________________
     O*
     O          E            FYRHD          1
     O                                           13 'POLICY YEAR:'
     O                       XXFYR         Z   +002
     O                                           84 'AGENT:'
     O                       E2AGT#        Z     91
     O                       XXAGNM             117
     O*_______________________________________________________________
     O          E            FYRHD          2
     O                                           84 'PRODUCER:'
     O                       E2PROD        Z     91
     O                       XXAGNM             117
     O*
     O          E            TOTHD          2
     O                       X3PRNG             117
     O          E            TOTHD       1  2
     O                                           09 'EMPLOYER:'
     O                       XXEDN             +002
     O               82                          68 'LOCATION:'
     O               82      E2CTG1              79
     O               82      XXNAME             120
     O*_______________________________________________________________
     O*
     O          E            TOT         1  0
     O                                           24 'TOTALS FOR POLICY YEAR:'
     O                       XXFYR         Z   +002
     O                                           56 'COMPENSATION'
     O                                           70 'MEDICAL'
     O              N79                          85 'LEGAL'
     O               79      charLGRH            85
     O                                          101 'OTHER'
     O                                          117 'TOTAL'
     O          E            TOT            1
     O                                           56 '____________'
     O                                           72 '____________'
     O                                           88 '____________'
     O                                          104 '____________'
     O                                          120 '____________'
     O          E            TOT            1
     O                                           35 'GROSS PAID THIS PERIOD:'
     O                       L1GPCO        J     56
     O                       L1GPME        J     72
     O                       L1GPLE        J     88
     O                       L1GPEX        J    104
     O                       L1GPOT        J    120
     O          E            TOT            1
     O                                           35 'RECOVERED THIS PERIOD :'
     O                       L1RCCO        J     56
     O                       L1RCME        J     72
     O                       L1RCLE        J     88
     O                       L1RCEX        J    104
     O                       L1RCOT        J    120
      *
     O          E            TOT            1
     O                                           35 'NET PAID THIS PERIOD  :'
     O                       L1ACMP        J     56
     O                       L1AMED        J     72
     O                       L1ALEG        J     88
     O                       L1AOTH        J    104
     O                       L1ATOT        J    120
     O          E            TOT            1
     O                                           35 'GROSS PAID TO DATE  . :'
     O                       L1GCMP        J     56
     O                       L1GMED        J     72
     O                       L1GLEG        J     88
     O                       L1GOTH        J    104
     O                       L1GTOT        J    120
     O          E            TOT            1
     O                                           35 'RECOVERED TO DATE . . :'
     O                       L1RTCO        J     56
     O                       L1RTME        J     72
     O                       L1RTLE        J     88
     O                       L1RTEX        J    104
     O                       L1TROT        J    120
     O          E            TOT            1
     O                                           35 'NET PAID TO DATE  . . :'
     O                       L1PCMP        J     56
     O                       L1PMED        J     72
     O                       L1PLEG        J     88
     O                       L1POTH        J    104
     O                       L1PTOT        J    120
     O          E            TOT            1
     O                                           35 'OUTSTANDING RESERVES  :'
     O                       L1RCMP        J     56
     O                       L1RMED        J     72
     O                       L1RLEG        J     88
     O                       L1ROTH        J    104
     O                       L1RTOT        J    120
     O          E            TOT            2
     O                                           35 'TOTAL INCURRED  . . . :'
     O                       L1ICMP        J     56
     O                       L1IMED        J     72
     O                       L1ILEG        J     88
     O                       L1IOTH        J    104
     O                       L1ITOT        J    120
     O*_______________________________________________________________
     O*
     O          E            TOT2        1  3
     O                                           13 'LOCATION:  '
     O                       XXCTG1              24
     O                                           39 'OPEN CLAIMS:'
     O                       L1OPEN        Z     55
     O                                           75 'CLOSED CLAIMS:'
     O                       L1CLOS        Z     85
     O                                          103 'TOTAL CLAIMS:'
     O                       L1TOTC        Z    116
     O*_______________________________________________________________
     O*
     O          E            TOT3           1
     O                                           37 'EARNED PREMIUM:'
     O                       L1AMT         J     53
     O                                           67 'LOSS RATIO:'
     O                       L1LOSS        J     81
     O                                          101 'FREQUENCY RATIO:'
     O                       L1FREQ        4    111
     O          E            TOT3           3
     O                                           34 'OPEN CLAIMS:'
     O                       L2OPEN        Z     50
     O                                           70 'CLOSED CLAIMS:'
     O                       L2CLOS        Z     80
     O                                           98 'TOTAL CLAIMS:'
     O                       L2TOTC        Z    111
     O*_______________________________________________________________
     O*
     O          E            TOTE        1  0
     O                                           22 'EMPLOYER TOTALS, ALL'
     O                                         +001 'YEARS:'
     O                                           56 'COMPENSATION'
     O                                           70 'MEDICAL'
     O              N79                          85 'LEGAL'
     O               79      charLGRH            85
     O                                          101 'OTHER'
     O                                          117 'TOTAL'
     O          E            TOTE           1
     O                                           56 '____________'
     O                                           72 '____________'
     O                                           88 '____________'
     O                                          104 '____________'
     O                                          120 '____________'
     O          E            TOTE           1
     O                                           35 'GROSS PAID THIS PERIOD:'
     O                       L3GPCO        J     56
     O                       L3GPME        J     72
     O                       L3GPLE        J     88
     O                       L3GPEX        J    104
     O                       L3GPOT        J    120
     O          E            TOTE           1
     O                                           35 'RECOVERED THIS PERIOD :'
     O                       L3RCCO        J     56
     O                       L3RCME        J     72
     O                       L3RCLE        J     88
     O                       L3RCEX        J    104
     O                       L3RCOT        J    120
     O          E            TOTE           1
     O                                           35 'NET PAID THIS PERIOD  :'
     O                       L3ACMP        J     56
     O                       L3AMED        J     72
     O                       L3ALEG        J     88
     O                       L3AOTH        J    104
     O                       L3ATOT        J    120
     O          E            TOTE           1
     O                                           35 'GROSS PAID TO DATE  . :'
     O                       L3GCMP        J     56
     O                       L3GMED        J     72
     O                       L3GLEG        J     88
     O                       L3GOTH        J    104
     O                       L3GTOT        J    120
     O          E            TOTE           1
     O                                           35 'RECOVERED TO DATE . . :'
     O                       L3RTCO        J     56
     O                       L3RTME        J     72
     O                       L3RTLE        J     88
     O                       L3RTEX        J    104
     O                       L3TROT        J    120
     O          E            TOTE           1
     O                                           35 'NET PAID TO DATE  . . :'
     O                       L3PCMP        J     56
     O                       L3PMED        J     72
     O                       L3PLEG        J     88
     O                       L3POTH        J    104
     O                       L3PTOT        J    120
     O          E            TOTE           1
     O                                           35 'OUTSTANDING RESERVES  :'
     O                       L3RCMP        J     56
     O                       L3RMED        J     72
     O                       L3RLEG        J     88
     O                       L3ROTH        J    104
     O                       L3RTOT        J    120
     O          E            TOTE           2
     O                                           35 'TOTAL INCURRED  . . . :'
     O                       L3ICMP        J     56
     O                       L3IMED        J     72
     O                       L3ILEG        J     88
     O                       L3IOTH        J    104
     O                       L3ITOT        J    120
     O          E            TOTHD2      1  1
     O                                           22 'EMPLOYER TOTALS, ALL'
     O                                         +001 'YEARS:'
     O          E            TOTE2          1
     O                                           37 'EARNED PREMIUM:'
     O                       L3AMT         J     53
     O                                           67 'LOSS RATIO:'
     O                       L3LOSS        J     81
     O                                          101 'FREQUENCY RATIO:'
     O                       L3FREQ        4    111
     O          E            TOTE2          3
     O                                           34 'OPEN CLAIMS:'
     O                       L3OPEN        Z     50
     O                                           70 'CLOSED CLAIMS:'
     O                       L3CLOS        Z     80
     O                                           98 'TOTAL CLAIMS:'
     O                       L3TOTC        Z    111
     O*_______________________________________________________________
     O*
     O          E            NORECS      2  1
     O                                           23 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTION CRITERIA ***'
     O*_______________________________________________________________
     O*
     O*_______________________________________________________________
     O*
     O*_______________________________________________________________
     O*
     O          E            TOTLR       2  2
     O                                           22 'FINAL TOTALS, ALL EM'
     O                                              'PLOYERS:'
     O          E            TOTLR          0
     O                                           56 'COMPENSATION'
     O                                           70 'MEDICAL'
     O              N79                          85 'LEGAL'
     O               79      charLGRH            85
     O                                          101 'OTHER'
     O                                          117 'TOTAL'
     O          E            TOTLR          1
     O                                           56 '____________'
     O                                           72 '____________'
     O                                           88 '____________'
     O                                          104 '____________'
     O                                          120 '____________'
     O          E            TOTLR          1
     O                                           35 'GROSS PAID THIS PERIOD:'
     O                       LRGPCO        J     56
     O                       LRGPME        J     72
     O                       LRGPLE        J     88
     O                       LRGPEX        J    104
     O                       LRGPOT        J    120
     O          E            TOTLR          1
     O                                           35 'RECOVERED THIS PERIOD :'
     O                       LRRCCO        J     56
     O                       LRRCME        J     72
     O                       LRRCLE        J     88
     O                       LRRCEX        J    104
     O                       LRRCOT        J    120
     O          E            TOTLR          1
     O                                           35 'NET PAID THIS PERIOD  :'
     O                       LRACMP        J     56
     O                       LRAMED        J     72
     O                       LRALEG        J     88
     O                       LRAOTH        J    104
     O                       LRATOT        J    120
     O          E            TOTLR          1
     O                                           35 'GROSS PAID TO DATE  . :'
     O                       LRGCMP        J     56
     O                       LRGMED        J     72
     O                       LRGLEG        J     88
     O                       LRGOTH        J    104
     O                       LRGTOT        J    120
     O          E            TOTLR          1
     O                                           35 'RECOVERED TO DATE . . :'
     O                       LRRTCO        J     56
     O                       LRRTME        J     72
     O                       LRRTLE        J     88
     O                       LRRTEX        J    104
     O                       LRTROT        J    120
     O          E            TOTLR          1
     O                                           35 'NET PAID TO DATE  . . :'
     O                       LRPCMP        J     56
     O                       LRPMED        J     72
     O                       LRPLEG        J     88
     O                       LRPOTH        J    104
     O                       LRPTOT        J    120
     O          E            TOTLR          1
     O                                           35 'OUTSTANDING RESERVES  :'
     O                       LRRCMP        J     56
     O                       LRRMED        J     72
     O                       LRRLEG        J     88
     O                       LRROTH        J    104
     O                       LRRTOT        J    120
     O          E            TOTLR          2
     O                                           35 'TOTAL INCURRED  . . . :'
     O                       LRICMP        J     56
     O                       LRIMED        J     72
     O                       LRILEG        J     88
     O                       LRIOTH        J    104
     O                       LRITOT        J    120
     O          E            TOTLR          2
     O                                           37 'EARNED PREMIUM:'
     O                       LRAMT         J     53
     O                                           67 'LOSS RATIO:'
     O                       LRLOSS        J     81
     O          E            TOTLR          2
     O                                           34 'OPEN CLAIMS:'
     O                       LROPEN        Z     50
     O                                           70 'CLOSED CLAIMS:'
     O                       LRCLOS        Z     80
     O                                           98 'TOTAL CLAIMS:'
     O                       LRTOTC        Z    111
**  SORT CODES & DESCRIPTIONS
AEMPLOYER NAME
CCOUNTY
GAGENT
NEMPLOYER NUMBER
RREGION
ZZIP CODE
1CATEGORY ONE
