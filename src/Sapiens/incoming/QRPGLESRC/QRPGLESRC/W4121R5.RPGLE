     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4121R5 - REPORT - Experience by payroll class report
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: W4121C5 - OVERRIDE                               *
     H*                                                               *
     H* CALLS TO:    WA002R - AGT INFO                                *
     H*              SRCVTDAT                                         *
     H*                                                               *
     H* CREATED: 03/16/00  C:61  PROJECT  WC741                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name
     H*                                                               *
     H* 04/20/09 C:190 Project: WC1473                                *
     H*                Load the new class code extension field        *
      *
     H* 05/23/05 C:33  Project: WC1252                                *
     H*                Load 'Anniversary Rating' fields.              *
      *
      * 03/10/05 C:61  PROJECT WC741 WA093 44220
      *              - Add Policy From Date to printed output and
      *                reformat output as necessary.
      *              - Write Policy From Date to output file WFEXP.
      *              - Changes not commented.
      *
      * 09/24/04 C:61  SUPPORT WA122 439596979899
      *                Only produce printed output when requested.
     H*                                                               *
     H* 07/29/04 C:61  PROJECT WC741                                  *
     H*              - Add processing by "Group range".               *
     H*              - Add report break and totals by Group.          *
     H*              - Changes not commented.                         *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  08       Batch mode                                          *
     H*  09       Demand mode                                         *
     H*  18       No records meet selection criteria                  *
     H*  65       Chain to Employer Detail file                       *
     H*  71       Print exp mod eff date 1                            *
     H*  72       Print exp mod eff date 2                            *
     H*  73       Print exp mod eff date 3                            *
     H*  74       Control print of payroll class on Routing page.     *
     H*  75       Print Policy FROM Date                              *
     H*  76 - 93  Print Employer Experience Mod totals                *
     H*  97       L2 Break processing                                 *
     H*  98       Last record processing                              *
     H*  99       Read main file                                      *
     H*                                                               *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * User Parameter
     FSMUPL     IF   E           K DISK
      *
      * Fund
     FWMFNL     IF   E           K DISK
      *
      * Employer
     FWMEML     IF   E           K DISK
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
      *
      * Rate Set Code
     FWMRSL     IF   E           K DISK
      *
      * Region Master
     FWMREL     IF   E           K DISK
      *
      * Payroll Classification
     FWMPRL     IF   E           K DISK
      *
      * Employer Payroll
     FWMEPL7E   IF   E           K DISK
      *
      * Work File from W4122R5
     FWXPYL     IF   E           K DISK
      *
      * Printed Output
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
      *
      * File Output
     FWFEXP     O    E             DISK
      *
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
      *
      *================================================================
      * CONSTANTS
      *================================================================
      *
     D XTITL           C                   CONST('Experience by Payrol-
     D                                     l Class Report')
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *================================================================
      *   KEY LISTS
      *================================================================
      *
      *   Output Routing
      *

     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *   User Parameter
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *   Company
      *
     C     COKY          KLIST
     C                   KFLD                    KKCO#
      *
      *   Fund
      *
     C     FNKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PYFND
     C                   KFLD                    PYEMP#
     C                   KFLD                    PYDIV
      *
      *  Employer Detail
      *
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PYFND
     C                   KFLD                    PYEMP#
     C                   KFLD                    PYDIV
     C                   KFLD                    PYFYR
      *
      *  Payroll Class
      *
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PYCLAS
     C                   KFLD                    KKRNDT
      *
     C     PRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PYCLAS
      *
      *  KEY LIST FOR REGION MASTER FILE
      *
     C     REKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMRGN#
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    ELRTCD
      *
      *  Employer Payroll
      *
     C     EPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    PYFND
     C                   KFLD                    PYFYR
     C                   KFLD                    PYEMP#
     C                   KFLD                    PYDIV
      *
      *
      *
      *================================================================
      *   *LIKE Define Variables
      *================================================================
      *
     C     *LIKE         DEFINE    CONAME        XXCNAM
     C     *LIKE         DEFINE    PYCLAS        X1CLAS
      *
     C     *LIKE         DEFINE    PYCO#         SVCO#
     C     *LIKE         DEFINE    PYFND         SVFND
     C     *LIKE         DEFINE    FNNAME        SVFNAM
     C     *LIKE         DEFINE    PYCLAS        SVCLAS
     C     *LIKE         DEFINE    PYSPEC        SVSPEC
      *
     C     *LIKE         DEFINE    PYCO#         KKCO#
     C     *LIKE         DEFINE    PYFND         KKFFND
     C     *LIKE         DEFINE    PYFND         KKTFND
      *
     C     *LIKE         DEFINE    PREFDT        KKRNDT
      *
     C     *LIKE         DEFINE    PYTNPR        MNL1TN
     C     *LIKE         DEFINE    PYPAMT        MNL1PA
     C     *LIKE         DEFINE    ELEXM1        MNL1E1
     C     *LIKE         DEFINE    ELEXM2        MNL1E2
     C     *LIKE         DEFINE    ELEXM3        MNL1E3
     C     *LIKE         DEFINE    PYEPRM        MNL1EP
      *
     C     *LIKE         DEFINE    PYTNPR        MNL2TN
     C     *LIKE         DEFINE    PYPAMT        MNL2PA
     C     *LIKE         DEFINE    ELEXM1        MNL2E1
     C     *LIKE         DEFINE    ELEXM2        MNL2E2
     C     *LIKE         DEFINE    ELEXM3        MNL2E3
     C     *LIKE         DEFINE    PYEPRM        MNL2EP
      *
     C     *LIKE         DEFINE    PYPAMT        MNLRPA
     C     *LIKE         DEFINE    ELEXM1        MNLRE1
     C     *LIKE         DEFINE    ELEXM2        MNLRE2
     C     *LIKE         DEFINE    ELEXM3        MNLRE3
     C     *LIKE         DEFINE    PYEPRM        MNLREP
      *
     C     *LIKE         DEFINE    PYTNPR        MXL1TN
     C     *LIKE         DEFINE    PYPAMT        MXL1PA
     C     *LIKE         DEFINE    ELEXM1        MXL1E1
     C     *LIKE         DEFINE    ELEXM2        MXL1E2
     C     *LIKE         DEFINE    ELEXM3        MXL1E3
     C     *LIKE         DEFINE    PYEPRM        MXL1EP
      *
     C     *LIKE         DEFINE    PYTNPR        MXL2TN
     C     *LIKE         DEFINE    PYPAMT        MXL2PA
     C     *LIKE         DEFINE    ELEXM1        MXL2E1
     C     *LIKE         DEFINE    ELEXM2        MXL2E2
     C     *LIKE         DEFINE    ELEXM3        MXL2E3
     C     *LIKE         DEFINE    PYEPRM        MXL2EP
      *
     C     *LIKE         DEFINE    PYPAMT        MXLRPA
     C     *LIKE         DEFINE    ELEXM1        MXLRE1
     C     *LIKE         DEFINE    ELEXM2        MXLRE2
     C     *LIKE         DEFINE    ELEXM3        MXLRE3
     C     *LIKE         DEFINE    PYEPRM        MXLREP
      *
     C     *LIKE         DEFINE    PYTNPR        AVL1TN
     C     *LIKE         DEFINE    PYPAMT        AVL1PA
     C     *LIKE         DEFINE    ELEXM1        AVL1E1
     C     *LIKE         DEFINE    ELEXM2        AVL1E2
     C     *LIKE         DEFINE    ELEXM3        AVL1E3
     C     *LIKE         DEFINE    PYEPRM        AVL1EP
      *
     C     *LIKE         DEFINE    PYTNPR        AVL2TN
     C     *LIKE         DEFINE    PYPAMT        AVL2PA
     C     *LIKE         DEFINE    ELEXM1        AVL2E1
     C     *LIKE         DEFINE    ELEXM2        AVL2E2
     C     *LIKE         DEFINE    ELEXM3        AVL2E3
     C     *LIKE         DEFINE    PYEPRM        AVL2EP
      *
     C     *LIKE         DEFINE    PYPAMT        AVLRPA
     C     *LIKE         DEFINE    ELEXM1        AVLRE1
     C     *LIKE         DEFINE    ELEXM2        AVLRE2
     C     *LIKE         DEFINE    ELEXM3        AVLRE3
     C     *LIKE         DEFINE    PYEPRM        AVLREP
      *
     C     *LIKE         DEFINE    PYTNPR        L1TNPR
     C     *LIKE         DEFINE    PYPAMT        L1PAMT
     C     *LIKE         DEFINE    PYEPRM        L1EPRM
     C     *LIKE         DEFINE    PYCLMS        L1CLMS
     C     *LIKE         DEFINE    PYTOTI        L1TOTI
      *
     C     *LIKE         DEFINE    PYTNPR        L2TNPR
     C     *LIKE         DEFINE    PYPAMT        L2PAMT
     C     *LIKE         DEFINE    PYEPRM        L2EPRM
     C     *LIKE         DEFINE    PYCLMS        L2CLMS
     C     *LIKE         DEFINE    PYTOTI        L2TOTI
      *
     C     *LIKE         DEFINE    PYPAMT        LRPAMT
     C     *LIKE         DEFINE    PYEPRM        LREPRM
     C     *LIKE         DEFINE    PYCLMS        LRCLMS
     C     *LIKE         DEFINE    PYTOTI        LRTOTI
     C     *LIKE         DEFINE    PYEXT         SVEXT
      *
      *
      *================================================================
      *   PARAMETER LISTS INCLUDING ENTRY PARMS
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FFND            3
     C                   PARM                    @@TFND            3
     C                   PARM                    @@CLAS            5
     C                   PARM                    @@RNDT            8
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RPT             1
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@SRPR            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *================================================================
      *
      *   Initialize
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            198
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
      *  PGMSTR executes first
      *
     C                   EXSR      PGMSTR
      *
      *  Print Routing Page
      *
     C                   EXSR      SRRTE
      *
      *  Read 1st record
      *
     C                   EXSR      READ1
      *
      *  Initialize break criteria
      *
     C                   EXSR      IZL1BK
     C                   EXSR      IZL2BK
      *
     C     *IN99         DOWEQ     '0'
      *
      *  If subset criteria met
      *
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
      *
     C     MEETSS        IFEQ      *ON
      *
      *  Test for break in report
      *
     C                   MOVE      *ON           MEETL1            1
     C                   MOVE      *ON           MEETL2            1
     C                   EXSR      L1TST
     C                   EXSR      L2TST
      *
      *  L1 Break
      *
     C     MEETL1        IFEQ      *OFF
     C     MEETL2        OREQ      *OFF
      *
     C     L1CNT         IFNE      *ZERO
     C                   EXSR      L1BK
     C                   ENDIF                                                  L1CNT
      *
     C                   EXSR      IZL1BK
     C                   ENDIF                                                  MEETL1
      *
      *  L2 Break
      *
     C     MEETL2        IFEQ      *OFF
      *
     C     L2CNT         IFNE      *ZERO
     C                   EXSR      L2BK
     C                   ENDIF                                                  L2CNT
      *
     C                   EXSR      IZL2BK
     C                   ENDIF                                                  MEETL2
      *
      *
      *  Write detail record
      *
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
      *
      *  Accumulate Totals
      *
     C                   ADD       1             L1CNT             9 0
     C                   ADD       1             L2CNT             9 0
     C                   ADD       1             LRCNT             9 0
     C                   EXSR      ACCTOT
      *
     C                   ENDIF                                                  MEETSS
      *
      *  Read next record
      *
     C                   EXSR      READN
      *
     C                   ENDDO                                                  WRK RECORD
      *
      *  Last Record processing
      *
     C     MEETPR        IFEQ      *ON
     C     LRCNT         OREQ      *ZERO
     C                   SETON                                        OF98
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF
      *
     C                   ELSE
      *
     C     L1CNT         IFNE      *ZERO
     C                   EXSR      L1BK
     C                   ENDIF
      *
     C     L2CNT         IFNE      *ZERO
     C                   EXSR      L2BK
     C                   ENDIF
      *
     C                   EXSR      LRBK
     C                   ENDIF                                                  MEETPR
      *
      *  End of Processing
      *
     C     END           TAG
      /Free
       kkprt = '0';
       Chain Orky Por01;
       If %found and (@@rpt = 'B' or @@rpt = 'P');
          Except Route;
       Endif;
      /End-free
     C                   MOVE      *ON           *INLR
      *
      *****************************************************************
      * ACCTOT - Accumulate Totals
      *****************************************************************
     CSR   ACCTOT        BEGSR
      *
      *  Accumulate L1 Totals - (most detailed level)
      *
     C                   ADD       PYTNPR        L1TNPR
      *
     C     PYTNPR        IFLT      MNL1TN
     C                   Z-ADD     PYTNPR        MNL1TN
     C                   ENDIF
      *
     C     PYTNPR        IFGT      MXL1TN
     C                   Z-ADD     PYTNPR        MXL1TN
     C                   ENDIF
      *
     C                   ADD       PYPAMT        L1PAMT
      *
     C     PYPAMT        IFLT      MNL1PA
     C                   Z-ADD     PYPAMT        MNL1PA
     C                   ENDIF
      *
     C     PYPAMT        IFGT      MXL1PA
     C                   Z-ADD     PYPAMT        MXL1PA
     C                   ENDIF
      *
      *====
      *
     C                   ADD       X1EXM1        L1EXM1           15 4
      *
     C     X1EXM1        IFLT      MNL1E1
     C     X1EXM1        ANDNE     *ZERO
     C                   Z-ADD     X1EXM1        MNL1E1
     C                   ENDIF
      *
     C     X1EXM1        IFGT      MXL1E1
     C                   Z-ADD     X1EXM1        MXL1E1
     C                   ENDIF
      *
     C     X1EXM1        IFNE      *ZERO
     C                   ADD       1             L1CTE1            9 0          CNT L1 EXP MOD
     C                   ENDIF
      *====
      *
     C                   ADD       X1EXM2        L1EXM2           15 4
      *
     C     X1EXM2        IFLT      MNL1E2
     C     X1EXM2        ANDNE     *ZERO
     C                   Z-ADD     X1EXM2        MNL1E2
     C                   ENDIF
      *
     C     X1EXM2        IFGT      MXL1E2
     C                   Z-ADD     X1EXM2        MXL1E2
     C                   ENDIF
      *
     C     X1EXM2        IFNE      *ZERO
     C                   ADD       1             L1CTE2            9 0          CNT L1 EXP MOD
     C                   ENDIF
      *====
      *
     C                   ADD       X1EXM3        L1EXM3           15 4
      *
     C     X1EXM3        IFLT      MNL1E3
     C     X1EXM3        ANDNE     *ZERO
     C                   Z-ADD     X1EXM3        MNL1E3
     C                   ENDIF
      *
     C     X1EXM3        IFGT      MXL1E3
     C                   Z-ADD     X1EXM3        MXL1E3
     C                   ENDIF
      *
     C     X1EXM3        IFNE      *ZERO
     C                   ADD       1             L1CTE3            9 0          CNT L1 EXP MOD
     C                   ENDIF
      *
      *====
     C                   ADD       PYEPRM        L1EPRM
      *
     C     PYEPRM        IFLT      MNL1EP
     C                   Z-ADD     PYEPRM        MNL1EP
     C                   ENDIF
      *
     C     PYEPRM        IFGT      MXL1EP
     C                   Z-ADD     PYEPRM        MXL1EP
     C                   ENDIF
      *
     C                   ADD       PYCLMS        L1CLMS
     C                   ADD       PYTOTI        L1TOTI
      *
      *  Accumulate L2 Totals - (most detailed level)
      *
     C                   ADD       PYTNPR        L2TNPR
      *
     C     PYTNPR        IFLT      MNL2TN
     C                   Z-ADD     PYTNPR        MNL2TN
     C                   ENDIF
      *
     C     PYTNPR        IFGT      MXL2TN
     C                   Z-ADD     PYTNPR        MXL2TN
     C                   ENDIF
      *
     C                   ADD       PYPAMT        L2PAMT
      *
     C     PYPAMT        IFLT      MNL2PA
     C                   Z-ADD     PYPAMT        MNL2PA
     C                   ENDIF
      *
     C     PYPAMT        IFGT      MXL2PA
     C                   Z-ADD     PYPAMT        MXL2PA
     C                   ENDIF
      *====
      *
     C                   ADD       X1EXM1        L2EXM1           15 4
      *
     C     X1EXM1        IFLT      MNL2E1
     C     X1EXM1        ANDNE     *ZERO
     C                   Z-ADD     X1EXM1        MNL2E1
     C                   ENDIF
      *
     C     X1EXM1        IFGT      MXL2E1
     C                   Z-ADD     X1EXM1        MXL2E1
     C                   ENDIF
      *
     C     X1EXM1        IFNE      *ZERO
     C                   ADD       1             L2CTE1            9 0          CNT L2 EXP MOD
     C                   ENDIF
      *
      *====
      *
     C                   ADD       X1EXM2        L2EXM2           15 4
      *
     C     X1EXM2        IFLT      MNL2E2
     C     X1EXM2        ANDNE     *ZERO
     C                   Z-ADD     X1EXM2        MNL2E2
     C                   ENDIF
      *
     C     X1EXM2        IFGT      MXL2E2
     C                   Z-ADD     X1EXM2        MXL2E2
     C                   ENDIF
      *
     C     X1EXM2        IFNE      *ZERO
     C                   ADD       1             L2CTE2            9 0          CNT L2 EXP MOD
     C                   ENDIF
      *
      *====
      *
     C                   ADD       X1EXM3        L2EXM3           15 4
      *
     C     X1EXM3        IFLT      MNL2E3
     C     X1EXM3        ANDNE     *ZERO
     C                   Z-ADD     X1EXM3        MNL2E3
     C                   ENDIF
      *
     C     X1EXM3        IFGT      MXL2E3
     C                   Z-ADD     X1EXM3        MXL2E3
     C                   ENDIF
      *
     C     X1EXM3        IFNE      *ZERO
     C                   ADD       1             L2CTE3            9 0          CNT L2 EXP MOD
     C                   ENDIF
      *
     C                   ADD       PYEPRM        L2EPRM
      *
     C     PYEPRM        IFLT      MNL2EP
     C                   Z-ADD     PYEPRM        MNL2EP
     C                   ENDIF
      *
     C     PYEPRM        IFGT      MXL2EP
     C                   Z-ADD     PYEPRM        MXL2EP
     C                   ENDIF
      *
     C                   ADD       PYCLMS        L2CLMS
     C                   ADD       PYTOTI        L2TOTI
      *
      *  Accumulate LR Totals - REPORT
      *
     C                   ADD       PYPAMT        LRPAMT
      *
     C     PYPAMT        IFLT      MNLRPA
     C                   Z-ADD     PYPAMT        MNLRPA
     C                   ENDIF
      *
     C     PYPAMT        IFGT      MXLRPA
     C                   Z-ADD     PYPAMT        MXLRPA
     C                   ENDIF
      *====
     C                   ADD       X1EXM1        LREXM1           15 4
      *
     C     X1EXM1        IFLT      MNLRE1
     C     X1EXM1        ANDNE     *ZERO
     C                   Z-ADD     X1EXM1        MNLRE1
     C                   ENDIF
      *
     C     X1EXM1        IFGT      MXLRE1
     C                   Z-ADD     X1EXM1        MXLRE1
     C                   ENDIF
      *
     C     X1EXM1        IFNE      *ZERO
     C                   ADD       1             LRCTE1            9 0          CNT L1 EXP MOD
     C                   ENDIF
      *====
     C                   ADD       X1EXM2        LREXM2           15 4
      *
     C     X1EXM2        IFLT      MNLRE2
     C     X1EXM2        ANDNE     *ZERO
     C                   Z-ADD     X1EXM2        MNLRE2
     C                   ENDIF
      *
     C     X1EXM2        IFGT      MXLRE2
     C                   Z-ADD     X1EXM2        MXLRE2
     C                   ENDIF
      *
     C     X1EXM2        IFNE      *ZERO
     C                   ADD       1             LRCTE2            9 0          CNT L1 EXP MOD
     C                   ENDIF
      *====
     C                   ADD       X1EXM3        LREXM3           15 4
      *
     C     X1EXM3        IFLT      MNLRE3
     C     X1EXM3        ANDNE     *ZERO
     C                   Z-ADD     X1EXM3        MNLRE3
     C                   ENDIF
      *
     C     X1EXM3        IFGT      MXLRE3
     C                   Z-ADD     X1EXM3        MXLRE3
     C                   ENDIF
      *
     C     X1EXM3        IFNE      *ZERO
     C                   ADD       1             LRCTE3            9 0          CNT L1 EXP MOD
     C                   ENDIF
      *
     C                   ADD       PYEPRM        LREPRM
      *
     C     PYEPRM        IFLT      MNLREP
     C                   Z-ADD     PYEPRM        MNLREP
     C                   ENDIF
      *
     C     PYEPRM        IFGT      MXLREP
     C                   Z-ADD     PYEPRM        MXLREP
     C                   ENDIF
      *
     C                   ADD       PYCLMS        LRCLMS
     C                   ADD       PYTOTI        LRTOTI
      *
     C                   ENDSR                                                  ACCTOT
      *
      *
      *
      *================================================================
      * IZL1BK - Initialize Level 1 break criteria
      *================================================================
     CSR   IZL1BK        BEGSR
      *
     C                   Z-ADD     PYCLAS        SVCLAS
     C                   MOVE      PYSPEC        SVSPEC
     C                   MOVE      PYEXT         SVEXT
      *
      *  Get Class Description
      *
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C  N60              MOVEL(P)  PRDESC        XXDESC           30
     C   60              MOVEL(P)  *BLANKS       XXDESC           30
      *
     C                   Z-ADD     *ZERO         L1CTE1
     C                   Z-ADD     *ZERO         L1CTE2
     C                   Z-ADD     *ZERO         L1CTE3
     C                   Z-ADD     *ZERO         L1EXM1
     C                   Z-ADD     *ZERO         L1EXM2
     C                   Z-ADD     *ZERO         L1EXM3
      *
     C                   Z-ADD     *HIVAL        MNL1TN
     C                   Z-ADD     *HIVAL        MNL1PA
     C                   Z-ADD     *HIVAL        MNL1E1
     C                   Z-ADD     *HIVAL        MNL1E2
     C                   Z-ADD     *HIVAL        MNL1E3
     C                   Z-ADD     *HIVAL        MNL1EP
      *
     C                   ENDSR                                                  IZL1BK
      *
      *
      *
      *****************************************************************
      * IZL2BK - Initialize Level 2 break criteria
      *****************************************************************
     CSR   IZL2BK        BEGSR
      *
     C                   Z-ADD     PYCO#         SVCO#
     C                   Z-ADD     PYFND         SVFND
      *
      * Get Group Name and Alternate Company Name
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *OFF
     C  N60              MOVEL(P)  FNNAME        SVFNAM
     C                   MOVE      *OFF          *IN39
      *
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        XCNAME           60
      *
     C                   MOVEL(P)  XCNAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XCNAME
      *
     C                   MOVE      *ON           *IN39
     C                   ENDIF
      *
     C                   ELSE
      *
     C   60              MOVEL(P)  *BLANKS       SVFNAM
     C                   ENDIF                                                  *IN60
      *
     C                   Z-ADD     *ZERO         L2CTE1
     C                   Z-ADD     *ZERO         L2CTE2
     C                   Z-ADD     *ZERO         L2CTE3
     C                   Z-ADD     *ZERO         L2EXM1
     C                   Z-ADD     *ZERO         L2EXM2
     C                   Z-ADD     *ZERO         L2EXM3
      *
     C                   Z-ADD     *HIVAL        MNL2TN
     C                   Z-ADD     *HIVAL        MNL2PA
     C                   Z-ADD     *HIVAL        MNL2E1
     C                   Z-ADD     *HIVAL        MNL2E2
     C                   Z-ADD     *HIVAL        MNL2E3
     C                   Z-ADD     *HIVAL        MNL2EP
      *
     C                   ENDSR                                                  IZL2BK
      *
      *
      *
      *================================================================
      * L1BK  -  Level ONE Break action
      *================================================================
     CSR   L1BK          BEGSR
      *
     C     MNL1E1        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNL1E1
     C                   ENDIF
      *
     C     MNL1E2        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNL1E2
     C                   ENDIF
      *
     C     MNL1E3        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNL1E3
     C                   ENDIF
      *
      * Calculate L1 averages
      *
     C     L1CNT         IFNE      *ZERO
     C     L1TNPR        DIV       L1CNT         AVL1TN
     C     L1PAMT        DIV       L1CNT         AVL1PA
     C     L1EPRM        DIV       L1CNT         AVL1EP
     C                   ELSE
     C                   Z-ADD     0             AVL1TN
     C                   Z-ADD     0             AVL1PA
     C                   Z-ADD     0             AVL1EP
     C                   ENDIF
      *
     C     L1CTE1        IFNE      *ZERO
     C     L1EXM1        DIV       L1CTE1        AVL1E1
     C                   ELSE
     C                   Z-ADD     0             AVL1E1
     C                   ENDIF
      *
     C     L1CTE2        IFNE      *ZERO
     C     L1EXM2        DIV       L1CTE2        AVL1E2
     C                   ELSE
     C                   Z-ADD     0             AVL1E2
     C                   ENDIF
      *
     C     L1CTE3        IFNE      *ZERO
     C     L1EXM3        DIV       L1CTE3        AVL1E3
     C                   ELSE
     C                   Z-ADD     0             AVL1E3
     C                   ENDIF
      *
      * Calculate L1 Loss and Freq Ratios
      *
     C                   Z-ADD     L1CLMS        CLMS              5 0
     C                   Z-ADD     L1EPRM        PREM             11 2
     C                   Z-ADD     L1TOTI        TOTINC           11 2
     C                   EXSR      SUBLF
     C                   Z-ADD     LOSS          L1LOSS            7 2
     C                   Z-ADD     FREQ          L1FREQ            5 2
      *
     C     AVL1E2        COMP      *ZERO                                  76
     C     AVL1E3        COMP      *ZERO                                  77
     C     MNL1E2        COMP      *ZERO                                  78
     C     MNL1E3        COMP      *ZERO                                  79
     C     MXL1E2        COMP      *ZERO                                  80
     C     MXL1E3        COMP      *ZERO                                  81
      *
      *  Output L1 totals
      *
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C                   EXCEPT    L1TOT
     C                   ENDIF
     C                   SETON                                        OF
      *
     C                   ENDSR                                                  L1BK
      *
      *****************************************************************
      * L2BK  -  Level TWO Break Action
      *****************************************************************
     CSR   L2BK          BEGSR
      *
     C     MNL2E1        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNL2E1
     C                   ENDIF
      *
     C     MNL2E2        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNL2E2
     C                   ENDIF
      *
     C     MNL2E3        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNL2E3
     C                   ENDIF
      *
      * Calculate L2 averages
      *
     C     L2CNT         IFNE      *ZERO
     C     L2TNPR        DIV       L2CNT         AVL2TN
     C     L2PAMT        DIV       L2CNT         AVL2PA
     C     L2EPRM        DIV       L2CNT         AVL2EP
     C                   ELSE
     C                   Z-ADD     0             AVL2TN
     C                   Z-ADD     0             AVL2PA
     C                   Z-ADD     0             AVL2EP
     C                   ENDIF
      *
     C     L2CTE1        IFNE      *ZERO
     C     L2EXM1        DIV       L2CTE1        AVL2E1
     C                   ELSE
     C                   Z-ADD     0             AVL2E1
     C                   ENDIF
      *
     C     L2CTE2        IFNE      *ZERO
     C     L2EXM2        DIV       L2CTE2        AVL2E2
     C                   ELSE
     C                   Z-ADD     0             AVL2E2
     C                   ENDIF
      *
     C     L2CTE3        IFNE      *ZERO
     C     L2EXM3        DIV       L2CTE3        AVL2E3
     C                   ELSE
     C                   Z-ADD     0             AVL2E3
     C                   ENDIF
      *
      * Calculate L2 Loss and Freq Ratios
      *
     C                   Z-ADD     L2CLMS        CLMS              5 0
     C                   Z-ADD     L2EPRM        PREM             11 2
     C                   Z-ADD     L2TOTI        TOTINC           11 2
     C                   EXSR      SUBLF
     C                   Z-ADD     LOSS          L2LOSS            7 2
     C                   Z-ADD     FREQ          L2FREQ            5 2
      *
     C     AVL2E2        COMP      *ZERO                                  82
     C     AVL2E3        COMP      *ZERO                                  83
     C     MNL2E2        COMP      *ZERO                                  84
     C     MNL2E3        COMP      *ZERO                                  85
     C     MXL2E2        COMP      *ZERO                                  86
     C     MXL2E3        COMP      *ZERO                                  87
      *
      *  Output L2 totals
      *
     C                   SETON                                        97
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C                   EXCEPT    HEAD
     C                   EXCEPT    L2TOT
     C                   ENDIF
     C                   SETON                                        OF
     C                   SETOFF                                       97
      *
     C                   ENDSR                                                  L2BK
      *
      *
      *
      *================================================================
      * L1TST - Level ONE Break TEST criteria
      *================================================================
     CSR   L1TST         BEGSR
      *
      *  MEETL1 = *OFF if Level 1 Break processing to proceed
      *
     C     PYCO#         IFNE      SVCO#
     C     PYFND         ORNE      SVFND
     C     PYCLAS        ORNE      SVCLAS
     C     PYSPEC        ORNE      SVSPEC
     C     PYEXT         ORNE      SVEXT
     C                   MOVE      *OFF          MEETL1
     C                   ENDIF
      *
     C                   ENDSR                                                  L1TST
      *
      *
      *
      *****************************************************************
      * L2TST - Level TWO Break TEST criteria
      *****************************************************************
     CSR   L2TST         BEGSR
      *
      *  MEETL2 = *OFF if Level 2 Break processing to proceed
      *
     C     PYCO#         IFNE      SVCO#
     C     PYFND         ORNE      SVFND
     C                   MOVE      *OFF          MEETL2
     C                   ENDIF
      *
     C                   ENDSR                                                  L2TST
      *
      *================================================================
      * LRBK - Last Record Processing
      *================================================================
     CSR   LRBK          BEGSR
      *
     C     MNLRE1        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNLRE1
     C                   ENDIF
      *
     C     MNLRE2        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNLRE2
     C                   ENDIF
      *
     C     MNLRE3        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNLRE3
     C                   ENDIF
      *
      * Calculate LR averages
      *
     C     LRCNT         IFNE      *ZERO
     C     LRPAMT        DIV       LRCNT         AVLRPA
     C     LREPRM        DIV       LRCNT         AVLREP
     C                   ELSE
     C                   Z-ADD     0             AVLRPA
     C                   Z-ADD     0             AVLREP
     C                   ENDIF
      *
     C     LRCTE1        IFNE      *ZERO
     C     LREXM1        DIV       LRCTE1        AVLRE1
     C                   ELSE
     C                   Z-ADD     0             AVLRE1
     C                   ENDIF
      *
     C     LRCTE2        IFNE      *ZERO
     C     LREXM2        DIV       LRCTE2        AVLRE2
     C                   ELSE
     C                   Z-ADD     0             AVLRE2
     C                   ENDIF
      *
     C     LRCTE3        IFNE      *ZERO
     C     LREXM3        DIV       LRCTE3        AVLRE3
     C                   ELSE
     C                   Z-ADD     0             AVLRE3
     C                   ENDIF
      *
     C     AVLRE2        COMP      *ZERO                                  88
     C     AVLRE3        COMP      *ZERO                                  89
     C     MNLRE2        COMP      *ZERO                                  90
     C     MNLRE3        COMP      *ZERO                                  91
     C     MXLRE2        COMP      *ZERO                                  92
     C     MXLRE3        COMP      *ZERO                                  93
      *
      * Output LR totals
      *
     C                   SETON                                        98
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C                   EXCEPT    HEAD
     C                   EXCEPT    LRTOT
     C                   ENDIF
      *
     C                   ENDSR                                                  LRBK
      *
      *
      *
      *================================================================
      * PGMSTR - Set-up and Initialize
      *================================================================
     CSR   PGMSTR        BEGSR
      *
      *
     C     @@MODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
      *
     C                   ELSE
      *
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      @@CLAS        X1CLAS
     C                   MOVE      @@RNDT        X1RNDT            8 0
     C                   MOVE      @@FRDT        X1FRDT            8 0
     C                   MOVE      @@TODT        X1TODT            8 0
      *
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKRNDT
      *
     C     X1CLAS        COMP      *ZERO                                  74
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FFND        KKFFND
     C                   MOVE      @@TFND        KKTFND
      *
     C                   Z-ADD     *HIVAL        MNL1TN
     C                   Z-ADD     *HIVAL        MNL1PA
     C                   Z-ADD     *HIVAL        MNL1E1
     C                   Z-ADD     *HIVAL        MNL1E2
     C                   Z-ADD     *HIVAL        MNL1E3
     C                   Z-ADD     *HIVAL        MNL1EP
      *
     C                   Z-ADD     *HIVAL        MNL2TN
     C                   Z-ADD     *HIVAL        MNL2PA
     C                   Z-ADD     *HIVAL        MNL2E1
     C                   Z-ADD     *HIVAL        MNL2E2
     C                   Z-ADD     *HIVAL        MNL2E3
     C                   Z-ADD     *HIVAL        MNL2EP
      *
     C                   Z-ADD     *HIVAL        MNLRPA
     C                   Z-ADD     *HIVAL        MNLRE1
     C                   Z-ADD     *HIVAL        MNLRE2
     C                   Z-ADD     *HIVAL        MNLRE3
     C                   Z-ADD     *HIVAL        MNLREP
      *
     C                   SETON                                        OF
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ1 - Read first record from file
      *================================================================
     CSR   READ1         BEGSR
      *
     C                   READ      PPY01                                  99
      *
     C                   ENDSR                                                  READ1
      *
      *
      *
      *================================================================
      * READN - Read next record from file
      *================================================================
     CSR   READN         BEGSR
      *
     C                   READ      PPY01                                  99
      *
     C                   ENDSR                                                  READN
      *
      *
      *
      *================================================================
      * SRDET - Write Detail Record
      *================================================================
     CSR   SRDET         BEGSR
      *
      * Get Employer name
      *
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMNAME        XXENAM           60
     C  N60              MOVEL(P)  EMNAME        ENAME            30
     C   60              MOVEL(P)  *BLANKS       XXENAM
     C   60              MOVEL(P)  *BLANKS       ENAME
      *
      * Get experience modifier info
      *
     C     ELKY          CHAIN     PEL01                              65
      *
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     ELEXM1        X1EXM1            5 4
     C                   Z-ADD     ELEXM2        X1EXM2            5 4
     C                   Z-ADD     ELEXM3        X1EXM3            5 4
      *
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD1            6 0
      *
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD2            6 0
      *
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD3            6 0
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPFD             6 0
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         X1EXM1            5 4
     C                   Z-ADD     *ZERO         X1EXM2            5 4
     C                   Z-ADD     *ZERO         X1EXM3            5 4
     C                   Z-ADD     0             XXEMD1
     C                   Z-ADD     0             XXEMD2
     C                   Z-ADD     0             XXEMD3
     C                   Z-ADD     0             XXPFD
     C                   ENDIF                                                  ELKY
      *
     C     XXEMD1        COMP      *ZERO                                  71
     C     XXEMD2        COMP      *ZERO                                  72
     C     XXEMD3        COMP      *ZERO                                  73
     C     XXPFD         COMP      *ZERO                                  75
      *
      * Loss / Freq ratio - Detail Level
      *
     C                   Z-ADD     PYCLMS        CLMS              5 0
     C                   Z-ADD     PYEPRM        PREM             11 2
     C                   Z-ADD     PYTOTI        TOTINC           11 2
     C                   EXSR      SUBLF
     C                   Z-ADD     LOSS          XXLOSS            7 2
     C                   Z-ADD     FREQ          XXFREQ            5 2
      *
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'F'
     C                   EXSR      WRITER
     C                   ENDIF
      *
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C   OF              EXCEPT    HEAD
     C                   ENDIF
     C   OF              SETOFF                                       OF
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C                   EXCEPT    DETAIL
     C                   ENDIF
      *
      * Check if other payroll classes exist for this employer
      *
     C     EPKY          CHAIN     PEP01                              60
     C     EPKY          READE     PEP01                                  60
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C   60              EXCEPT    SPEC
     C                   ENDIF
      *
     C                   ENDSR                                                  SRDET
      *
      *================================================================
      * SRRTE - Print routing page
      *================================================================
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *   Output Routing - detail report 0
      *
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      *  User Name if batch mode and no Distribution specified in routing
      *
     C   09              MOVEL(P)  UPUSNM        ORDIS1
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
     C     @@RPT         IFEQ      'B'
     C     @@RPT         OREQ      'P'
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL         ORTITL
      *
     C                   ENDIF
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXCNAM
      *
     C                   ENDSR                                                  SRRTE
      *
      *
      *
      *==================================================================*
      *   Loss and Frequency Ratio Calcs   ** Subroutine **   S U B L F  *
      *==================================================================*
      *
     CSR   SUBLF         BEGSR
      *
      *    CALCULATE LOSS RATIO
      *
     C     PREM          IFNE      *ZERO
     C     TOTINC        DIV(H)    PREM          LOSS              7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         LOSS
     C                   END
     C     LOSS          MULT      100           LOSS
      *
      *    CALCULATE FREQUENCY RATIO
      *
     C     PREM          DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     CLMS          DIV(H)    THPREM        FREQ              5 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
     C                   END
      *
     C                   ENDSR                                                  SUBLF
      *
      *
      *
      *================================================================
      * SUBSET - Check if member of records processed subset
      *================================================================
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if record not to be processed
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
      *================================================================
      * WRITER - WRITE FXR5P
      *================================================================
     CSR   WRITER        BEGSR
      *
     C                   CLEAR                   PEX01
      *
     C                   Z-ADD     PYCO#         EXCO#
     C                   MOVEL(P)  CONAME        EXCNAM
     C                   Z-ADD     PYFND         EXGRP
     C                   MOVEL(P)  FNNAME        EXGNAM
     C                   Z-ADD     PYFYR         EXFYR
     C                   Z-ADD     X1RNDT        EXASOF
     C                   Z-ADD     PYCLAS        EXCLAS
     C                   MOVE      PYSPEC        EXSPEC
     C                   MOVE      PYEXT         EXEXT
     C                   MOVEL(P)  PRDESC        EXCDES
     C                   Z-ADD     PYARAT        EXARAT
     C                   Z-ADD     PYANRT        EXANRT
     C                   Z-ADD     PYANDT        EXANDT
     C                   Z-ADD     PYEMP#        EXEMP#
     C                   Z-ADD     PYDIV         EXDIV
     C                   MOVEL(P)  XXENAM        EXENAM
     C                   Z-ADD     PYPAMT        EXPYRL
     C                   Z-ADD     PYMPRM        EXMAN
     C                   Z-ADD     PYTNPR        EXNORM
      *
     C     *IN65         IFEQ      *OFF
      *
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        EXWRIT
     C                   ELSE
     C                   Z-ADD     ELANPR        EXWRIT
     C                   ENDIF
      *
     C                   Z-ADD     ELEXM1        EXEXM1
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXEXD1
      *
     C                   Z-ADD     ELEXM2        EXEXM2
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXEXD2
      *
     C                   Z-ADD     ELEXM3        EXEXM3
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXEXD3
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXPFD
      *
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL     RSJRST        EXRST
     C   60              MOVE      *BLANKS       EXRST
      *
     C                   ENDIF                                                  *IN60 ELKY
      *
      *
     C                   Z-ADD     PYEPRM        EXEARN
     C                   Z-ADD     PYCLMS        EX#CLM
     C                   Z-ADD     PYTOTI        EXTOTI
      *
     C                   Z-ADD     XXLOSS        EXLOSS
     C                   Z-ADD     XXFREQ        EXFREQ
      *
     C                   Z-ADD     EMRGN#        EXRGN#
      *
     C     REKY          CHAIN     PRE01                              42
     C     *IN42         IFEQ      *OFF
     C                   MOVEL(P)  REDESC        EXRDES
     C                   ENDIF
      *
     C                   CALL      'WA002R'
     C                   PARM      PYCO#         ##CO#             3 0
     C                   PARM      PYFND         ##FND             3 0
     C                   PARM      PYEMP#        ##EMP#            9 0
     C                   PARM      PYDIV         ##DIV             5 0
     C                   PARM      PYFYR         ##FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    ##AGT#            5 0
     C***********        PARM                    ##ANAM           60
     C                   PARM      *zeros        ##AGT#            5 0
     C                   PARM      *blanks       ##ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C                   PARM                    ##AREA            3 0
     C                   PARM                    ##PHON            7 0
      *
     C                   Z-ADD     ##AGT#        EXAGT#
     C                   MOVEL     ##ANAM        EXANAM
      *
     C                   WRITE     PEX01
     C                   FEOD      WFEXP
      *
     C                   ENDSR                                                  WRITER
      *
      *
      *
      *================================================================
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
     O          E            ROUTE          1
     O                       ORDIS2              56
     O          E            ROUTE          1
     O                       ORDIS3              56
     O          E            ROUTE          1
     O                       ORDIS4              56
      *
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      *
     O          E            ROUTE       2  2
     O                                         +006 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                         +010 'COMPANY  . . . . . . . .'
     O                                         +000 ' . :'
     O                       KKCO#         Z   +001
      *
     O          E            ROUTE          2
     O                                         +010 'GROUP RANGE  . . . . . .'
     O                                         +000 ' . :'
     O                       KKFFND        3   +001
     O                                         +001 '-'
     O                       KKTFND        3   +001
      *
     O          E            ROUTE          2
     O                                         +010 'POLICY EFFECTIVE DATE RA'
     O                                         +000 'NGE:'
     O                       X1FRDT            +001 '  /  /    '
     O                                         +001 'to'
     O                       X1TODT            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                         +010 'PAYROLL CLASS  . . . . .'
     O                                         +000 ' . :'
     O              N74      X1CLAS        Z   +001
     O               74                          43 '*ALL'
      *
     O          E            ROUTE          2
     O                                         +010 'RUN DATE   . . . . . . .'
     O                                         +000 ' . :'
     O                       X1RNDT            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                         +010 'USE S/R PAYROLL  . . . .'
     O                                         +000 ' . :'
     O                       @@SRPR            +001
      *
     O          E            ROUTE          2
     O                                         +010 'TYPE OF OUTPUT . . . . .'
     O                                         +000 ' . :'
     O                       @@RPT             +001
     O                                         +010 '(P=Printed, F=File,'
     O                                         +001 'B=Both)'
      *
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O              N39      XXCNAM             119
     O               39      XCNAME             129
     O                       DSDATE        Y    198
      *
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          187 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E            HEAD           2
     O                                           96 'AS OF'
     O                       X1RNDT        Y   +001
      *
     O          E   N98      HEAD           2
     O                                            6 'GROUP:'
     O                       SVFND         Z   +001
     O                       SVFNAM            +002
      *
     O          E   N98N97   HEAD           2
     O                                           14 'PAYROLL CLASS:'
     O                       SVCLAS        Z   +001
     O                       SVSPEC            +001
     O                       SVEXT             +001
     O                       XXDESC            +001
      *
     O          E            HEAD           1
     O              N98                          63 'TOTAL'
     O              N98N97                       76 'POLICY'
     O                                         +024 'EXP'
     O                                         +017 'EARNED'
     O                                         +005 'TOTAL'
     O                                         +008 'INCURRED'
     O              N98                        +008 'LOSS'
     O              N98                        +005 'FREQ'
      *
     O          E            HEAD        0  0
     O                       LINE              +000
      *
     O          E            HEAD           2
     O              N98N97                     +001 'EMPLOYER'
     O              N98N97                     +003 'DIV'
     O              N98N97                     +001 'NAME'
     O              N98N97                     +028 'PYR'
     O              N98                        +002 'NORMAL PREMIUM'
     O              N98N97                     +002 'EFFECTIVE'
     O                                         +005 'PAYROLL AMT'
     O                                         +006 'MOD'
     O              N98N97                     +001 'EFF DATE'
     O                                         +007 ' PREMIUM '
     O                                         +002 'CLAIMS'
     O                                         +012 'LOSS'
     O              N98                        +007 'RATIO'
     O              N98                        +004 'RATIO'
      *
      *
     O          E            DETAIL         1
     O                       PYEMP#        Z   +000
     O                       PYDIV         Z   +001
     O                       ENAME             +001
     O                       PYFYR         Z   +002
     O                       PYTNPR        J   +002
     O              N75      XXPFD             +002 '  /  /  '
     O                       PYPAMT        J   +002
     O                       X1EXM1        4   +002
     O              N71      XXEMD1            +001 '  /  /  '
     O                       PYEPRM        J   +002
     O                       PYCLMS        Z   +002
     O                       PYTOTI        J   +002
     O                       XXLOSS        J   +002
     O                       XXFREQ        J   +002
      *
     O          E   N72      DETAIL         1
     O                       X1EXM2        4   +097
     O                       XXEMD2            +001 '  /  /  '
      *
     O          E   N73      DETAIL         1
     O                       X1EXM3        4   +097
     O                       XXEMD3            +001 '  /  /  '
      *
      *
     O          E            L1TOT       1  1
     O                                         +032 'JOB CLASS TOTAL:'
     O                       L1TNPR        JB  +005
     O                       L1PAMT        JB  +012
     O                       L1EPRM        JB  +019
     O                       L1CLMS        ZB  +002
     O                       L1TOTI        JB  +002
     O                       L1LOSS        JB  +002
     O                       L1FREQ        JB  +002
      *
     O          E            L1TOT          1
     O                                         +043 'AVG'
     O                       AVL1TN        JB  +007
     O                       AVL1PA        JB  +012
     O                       AVL1E1        3B  +002
     O                       AVL1EP        JB  +011
      *
     O          E   N76      L1TOT          1
     O                       AVL1E2        3B  +097
      *
     O          E   N77      L1TOT          1
     O                       AVL1E3        3B  +097
      *
     O          E            L1TOT          1
     O                                         +043 'MIN'
     O                       MNL1TN        JB  +007
     O                       MNL1PA        JB  +012
     O                       MNL1E1        3B  +002
     O                       MNL1EP        JB  +011
      *
     O          E   N78      L1TOT          1
     O                       MNL1E2        3B  +097
      *
     O          E   N79      L1TOT          1
     O                       MNL1E3        3B  +097
      *
     O          E            L1TOT          1
     O                                         +043 'MAX'
     O                       MXL1TN        JB  +007
     O                       MXL1PA        JB  +012
     O                       MXL1E1        3B  +002
     O                       MXL1EP        JB  +011
      *
     O          E   N80      L1TOT          1
     O                       MXL1E2        3B  +097
      *
     O          E   N81      L1TOT          1
     O                       MXL1E3        3B  +097
      *
     O          E            L1TOT          2
     O                                           51 'COUNT:'
     O                       L1CNT         ZB  +007
      *
      *
     O          E            L2TOT       1  1
     O                                         +032 'GROUP TOTAL:    '
     O                       L2TNPR        JB  +005
     O                       L2PAMT        JB  +012
     O                       L2EPRM        JB  +019
     O                       L2CLMS        ZB  +002
     O                       L2TOTI        JB  +002
     O                       L2LOSS        JB  +002
     O                       L2FREQ        JB  +002
      *
     O          E            L2TOT          1
     O                                         +043 'AVG'
     O                       AVL2TN        JB  +007
     O                       AVL2PA        JB  +012
     O                       AVL2E1        3B  +002
     O                       AVL2EP        JB  +011
      *
     O          E   N82      L2TOT          1
     O                       AVL2E2        3B  +097
      *
     O          E   N83      L2TOT          1
     O                       AVL2E3        3B  +097
      *
     O          E            L2TOT          1
     O                                         +043 'MIN'
     O                       MNL2TN        JB  +007
     O                       MNL2PA        JB  +012
     O                       MNL2E1        3B  +002
     O                       MNL2EP        JB  +011
      *
     O          E   N84      L2TOT          1
     O                       MNL2E2        3B  +097
      *
     O          E   N85      L2TOT          1
     O                       MNL2E3        3B  +097
      *
     O          E            L2TOT          1
     O                                         +043 'MAX'
     O                       MXL2TN        JB  +007
     O                       MXL2PA        JB  +012
     O                       MXL2E1        3B  +002
     O                       MXL2EP        JB  +011
      *
     O          E   N86      L2TOT          1
     O                       MXL2E2        3B  +097
      *
     O          E   N87      L2TOT          1
     O                       MXL2E3        3B  +097
      *
     O          E            L2TOT          2
     O                                           51 'COUNT:'
     O                       L2CNT         ZB  +007
      *
      *
     O          E            LRTOT       1  1
     O                                         +032 'REPORT TOTALS:  '
     O                       LRPAMT        JB  +032
     O                       LREPRM        JB  +019
     O                       LRCLMS        ZB  +002
     O                       LRTOTI        JB  +002
      *
     O          E            LRTOT          1
     O                                         +043 'AVG'
     O                       AVLRPA        JB  +034
     O                       AVLRE1        3B  +002
     O                       AVLREP        JB  +011
      *
     O          E   N88      LRTOT          1
     O                       AVLRE2        3B  +097
      *
     O          E   N89      LRTOT          1
     O                       AVLRE3        3B  +097
      *
     O          E            LRTOT          1
     O                                         +043 'MIN'
     O                       MNLRPA        JB  +034
     O                       MNLRE1        3B  +002
     O                       MNLREP        JB  +011
      *
     O          E   N90      LRTOT          1
     O                       MNLRE2        3B  +097
      *
     O          E   N91      LRTOT          1
     O                       MNLRE3        3B  +097
      *
     O          E            LRTOT          1
     O                                         +043 'MAX'
     O                       MXLRPA        JB  +034
     O                       MXLRE1        3B  +002
     O                       MXLREP        JB  +011
      *
     O          E   N92      LRTOT          1
     O                       MXLRE2        3B  +097
      *
     O          E   N93      LRTOT          1
     O                       MXLRE3        3B  +097
      *
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
     O          E            SPEC           1
     O                                         +016 'NO OTHER CLASSES EXIST'
     O                                         +001 'FOR EMPLOYER'
