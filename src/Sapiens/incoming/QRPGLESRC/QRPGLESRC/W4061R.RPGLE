     Hcopyright('(c) 2024 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/24/24 C:344 Support 114953                                 *
     H*                Retrieve report title at pgm initialization    *
     H*                prior to the Header Exception.                 *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 05/23/11 C:136 SUPPORT: WA099 73548                           *
     H*                ADJUST A/R BALANCE CALC'S FOR NEW DEPOSIT      *
     H*                REFUND AND VOID CODES.                         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 05/17/00  C:118 PROJECT: WC577                                *
     H*                 IF GRP ENTERED THAT HAS NO A/R RECORDS, GOES  *
     H*                 INTO INFINITE LOOP.                           *
     H*                                                               *
     H* 04/13/99  C:90  PROJECT: 96WAD (IN BASE: 07/19/99 C:33 WC899) *
     H*                 DO NOT PROCESS ACCOUNT CURRENT POLICIES.      *
     H*                                                               *
     H* 07/10/98  C:35  CHANGED TO NOT INCLUDE TRANSACTIONS POSTED    *
     H*                 AFTER THE RUN DATE ON AUDITED ACCOUNTS, SINCE *
     H*                 AUDITS NOW DELETE FUTURE TRANSACTIONS.        *
     H*                                                               *
     H* 10/27/97  C:86  VERSION 3 PROJECT: WC577                      *
     H*                 REVISED TO PRINT THE ALTERNATE FUND NAME IN   *
     H*                 PLACE OF COMPANY NAME, IF ONE EXISTS, AND     *
     H*                 CENTER NAME.                                  *
     H*                                                               *
     H*****************************************************************
     H*
     H/TITLE W4061R - A/R 4 Year Recap Report
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Routing Master File
     FSMORL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Employer A/R File
     FWMEAL7    IF   E           K DISK
     F*
     F* Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F* Employer Detail File
     FWDELL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F*
     F* Output File
     FQSYSPRT   O    F  202        PRINTER OFLIND(*INOA)

     D*
     D* Arrays for Employers' Effective & Cancel Dates & Fund Years
     D YR              S              3  0 DIM(4)                               Fund Years
     D*
     D* Employers' Effective Dates
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        Effect Date
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D* Employers' Cancel Dates
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        Cancel Date
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D* System Date & Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  X1RNDT                 1      8  0
     D  X1MMDD                 1      4  0
     D  X1YR                   7      8  0
     D                 DS
     D  X2RNDT                 1      6  0
     D  X2MMDD                 1      4  0
     D  X2YR                   5      6  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Routing Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Employer Accounts Receivable File
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C*                                                    JJJ 2-20-89
     C*  AGENT HISTORY FILE                                |
     C*                                                    |
     C     AHKEY         KLIST                                                  |
     C                   KFLD                    XXCO#                          |
     C                   KFLD                    XXFND                          |
     C                   KFLD                    EAEMP#                         |
     C                   KFLD                    EADIV                          |
     C                   KFLD                    EAFYR             3 0          |
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR1            3
     C                   PARM                    QQFYR2            3
     C                   PARM                    QQFYR3            3
     C                   PARM                    QQFYR4            3
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQTRFE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C* Set-Up and Initialize
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        Demand Mode
     C                   ELSE
     C                   SETON                                        08        Batch Mode
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#             3 0          Company #
     C                   MOVE      QQFND         XXFND             3 0          Fund #
     C                   MOVE      QQFYR1        XXFYR1            3 0          Fund year 1
     C                   MOVE      QQFYR2        XXFYR2            3 0          Fund year 2
     C                   MOVE      QQFYR3        XXFYR3            3 0          Fund year 3
     C                   MOVE      QQFYR4        XXFYR4            3 0          Fund year 4
     C                   MOVE      QQRNDT        X1RNDT                         Run date-output
     C                   MOVEL     X1MMDD        X2MMDD
     C                   MOVEL     X1YR          X2YR
     C                   MOVE      QQTRFE        XXTRFE            1            Fee trans.
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT            7 0
     C                   Z-ADD     0             X1TTL            11 2          Total Fyr 1
     C                   Z-ADD     0             X2TTL            11 2          Total Fyr 2
     C                   Z-ADD     0             X3TTL            11 2          Total Fyr 3
     C                   Z-ADD     0             X4TTL            11 2          Total Fyr 4
     C                   Z-ADD     0             X5TTL            11 2          Total Othr Fyrs
     C                   Z-ADD     0             XXDBTL           11 2          Total Dep Bill
     C                   Z-ADD     0             XXDPTL           11 2          Total Dep Paid
     C*
     C* Move Fund Years to the Array
     C*
     C                   Z-ADD     XXFYR1        YR(1)
     C                   Z-ADD     XXFYR2        YR(2)
     C                   Z-ADD     XXFYR3        YR(3)
     C                   Z-ADD     XXFYR4        YR(4)
     C*
     C* Get the Company Master Record
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C* Get the User Parameter Master Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Get the Routing Master Record
     C*
     C                   MOVE      '1'           *INOA
     C*
     C* Get the Fund Master Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXEMP#            5 0
     C                   Z-ADD     0             XXDIV             3 0
     C                   Z-ADD     0             XXFYR             3 0
     C                   EXSR      ELCLR
       //*** Beg Add *** 01/24/2024 *** PHW114953 *************************
       // Retrieve Report Title from System Output Routing Maint. File for
       // report heading.
       Chain (Q1pgm:'0') por01;
       If not %found(SMORL);
          Clear ORtitl;
       Endif;
       //*** End Add *** 01/24/2024 *** PHW114953 *************************
     C*
     C* Begin Reading the A/R File
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C*
     C                   MOVE      '1'           *IN62
     C     *IN62         DOWEQ     '1'
     C     *IN60         ANDEQ     *OFF
     C     EMKEY         CHAIN     PEM01                              62
     C   62EAKEY         READE     PEA01                                  60
     C                   END
     C*
     C                   MOVE      *BLANKS       ENAME            35            JJJ 2-20-89
     C                   MOVEL     EMNAME        ENAME                          |
     C                   Z-ADD     0             NOREC             4 0
     C                   Z-ADD     EAEMP#        XXEMP#
     C                   Z-ADD     EADIV         XXDIV
     C                   Z-ADD     EAFYR         XXFYR
     C  N60              EXSR      GETEMP
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for New Fund Year.  Add Current Amount to Appropiate
     C* Bucket.
     C*
     C     EAFYR         IFNE      XXFYR
     C     EAEMP#        ORNE      XXEMP#
     C     EADIV         ORNE      XXDIV
     C*
     C     XXAMT         IFNE      0                                            Employer has
     C                   MOVE      '1'           *IN51                          A/R <> 0.
     C                   END
     C*
     C* Find matching fund year
     C*
     C                   MOVE      '0'           *IN61
     C                   Z-ADD     1             Z                 3 0
     C                   Z-ADD     0             X                 3 0
     C     Z             DOWLE     4
     C     XXFYR         IFEQ      YR(Z)
     C                   Z-ADD     Z             X
     C                   MOVE      '1'           *IN61
     C                   Z-ADD     5             Z
     C                   ELSE
     C                   ADD       1             Z
     C                   END
     C                   END
     C*
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     ELEMP#        @@EMP#
     C                   Z-ADD     ELDIV         @@DIV
     C                   Z-ADD     XXFYR         @@FYR
     C                   Z-ADD     X1RNDT        @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C***   Begin Chg  ***  01/10/18  *************************************
     C***********        PARM                    @@AGT#            5 0
     C***********        PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***   End   Chg  ***  01/10/18  *************************************
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
      *
     C     @@PLAN        IFNE      'A'
     C     *IN61         IFEQ      '1'
     C     X             IFEQ      1
     C                   Z-ADD     XXAMT         X1AMT
     C                   ADD       XXAMT         X1TTL
     C                   ELSE
     C     X             IFEQ      2
     C                   Z-ADD     XXAMT         X2AMT
     C                   ADD       XXAMT         X2TTL
     C                   ELSE
     C     X             IFEQ      3
     C                   Z-ADD     XXAMT         X3AMT
     C                   ADD       XXAMT         X3TTL
     C                   ELSE
     C     X             IFEQ      4
     C                   Z-ADD     XXAMT         X4AMT
     C                   ADD       XXAMT         X4TTL
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   ELSE
     C*
     C                   ADD       XXAMT         X5AMT
     C                   ADD       XXAMT         X5TTL
     C*
     C                   END
     C                   ENDIF                                                  @@PLAN NE A
     C*
     C                   Z-ADD     0             XXAMT
     C     EAFYR         IFNE      XXFYR
     C                   EXSR      GETEMP
     C                   END
     C                   Z-ADD     EAFYR         XXFYR                          JJJ 10-19-88
     C*
     C* Check for Different Employer
     C*
     C     EAEMP#        IFNE      XXEMP#
     C     EADIV         ORNE      XXDIV
     C*
     C     *IN51         IFEQ      '1'                                          There is
     C     *INOA         IFEQ      '1'                                          detail to be
     C                   EXCEPT    HEAD                                         printed.
     C                   MOVE      '0'           *INOA
     C                   END
     C                   ADD       1             NOREC
     C     @@PLAN        IFNE      'A'
     C                   EXCEPT    DETL
     C                   ADD       XXDPBL        XXDBTL
     C                   ADD       XXDPPD        XXDPTL
     C                   ENDIF                                                  @@PLAN NE A
     C                   END
     C*
     C                   EXSR      ELCLR
     C     EMKEY         CHAIN     PEM01                              62
     C     *IN62         IFEQ      '1'                                          Employer
     C                   GOTO      NXTAR                                        does not
     C                   END                                                    exist.
     C                   MOVE      *BLANKS       ENAME                          JJJ 2-20-89
     C                   MOVEL     EMNAME        ENAME                          |
     C                   Z-ADD     EAEMP#        XXEMP#
     C                   Z-ADD     EADIV         XXDIV
     C                   Z-ADD     EAFYR         XXFYR
     C                   EXSR      GETEMP
     C*
     C                   END
     C*
     C                   END
     C*
     C* Accumulate Employer A/R
     C*
     C     EAPODT        IFLE      XXDAT
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C                   SELECT
     C     TTCTYP        WHENEQ    'P'
     C                   SELECT
     C     EAARTY        WHENEQ    'P'
     C     EAARTY        OREQ      'T'
     C                   SUB       EAAMT         XXAMT
     C                   OTHER
     C                   ADD       EAAMT         XXAMT
     C                   ENDSL
     C     TTCTYP        WHENEQ    'D'
     C                   SELECT
     C     EAARTY        WHENEQ    'H'
     C                   ADD       EAAMT         XXDPBL
     C     EAARTY        WHENEQ    'I'
     C     EAARTY        OREQ      'T'
     C                   ADD       EAAMT         XXDPPD
     C     EAARTY        WHENEQ    'V'
     C     EAARTY        OREQ      'R'
     C                   SUB       EAAMT         XXDPPD
     C                   ENDSL
     C     XXTRFE        WHENEQ    'Y'
     C                   SELECT
     C     EAARTY        WHENEQ    'Q'
     C                   SUB       EAAMT         XXAMT
     C     EAARTY        WHENEQ    'F'
     C                   ADD       EAAMT         XXAMT
     C                   ENDSL
     C                   ENDSL
     C                   ENDIF                                                  EAPODT < XXDAT
     C*
     C* Get Next A/R Record
     C*
     C     NXTAR         TAG
     C     EAKEY         READE     PEA01                                  60
     C     *IN60         IFEQ      '1'
     C     NOREC         ANDEQ     0
     C                   EXCEPT    DETL2
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN50
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOTL
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*          GETEMP  Subroutine To Get Employer Information       *
     C*****************************************************************
     C*
     C     GETEMP        BEGSR
     C*
     C                   Z-ADD     0             XXDAT
     C*
     C* Get the Employer Detail Record
     C*
     C     ELKEY         CHAIN     PEL01                              61
     C     *IN61         IFEQ      '0'
     C*
     C* If employer has been auditted include all A/R
     C*
     C                   Z-ADD     XXRNDT        XXDAT             7 0
     C                   ENDIF                                                  *IN61 IFEQ *OFF
     C*
     C*  Use Cancelled Date if <> 0 and Employer isn't Auditted
     C*
     C                   Z-ADD     0             XXCXDT            6 0
     C                   MOVE      '0'           *IN54
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
     C*
     C     ED(X)         IFGT      XXRNDT                                       JJJ 10-20-88
     C                   SUB       1             X                              |
     C                   END                                                    |
     C*                                                    |
     C     X             IFNE      0                                            |
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    CD(Y)                                  55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C     Y             IFNE      0
     C     CD(Y)         ANDGE     ED(X)
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCXDT
     C                   MOVE      '1'           *IN54
     C                   END
     C                   END
     C*
     C     EAEMP#        IFEQ      XXEMP#                                       JJJ 2-24-89
     C     EADIV         ANDEQ     XXDIV                                        *
     C*                                                    *
     C     AHKEY         SETGT     PAH01                                        |
     C                   READP     PAH01                                  50    |
     C     *IN50         DOWEQ     '0'                                          |
     C     AHSTAT        ANDEQ     'I'                                          |
     C                   READP     PAH01                                  50    |
     C                   END                                                    |
     C*                                                    |
     C     *IN50         IFEQ      '1'                                          |
     C     XXCO#         ORNE      AHCO#                                        |
     C     XXFND         ORNE      AHFND                                        |
     C     EAEMP#        ORNE      AHEMP#                                       |
     C     EADIV         ORNE      AHDIV                                        |
     C     EAFYR         ORNE      AHFYR                                        |
     C                   Z-ADD     0             AHAGT#                         |
     C                   END                                                    |
     C*
     C                   END                                                    JJJ 2-24-89
     C                   ENDSR
     C*
     C*****************************************************************
     C*          ELCLR - Subroutine To Clear Detail Fields            *
     C*****************************************************************
     C*
     C     ELCLR         BEGSR
     C*
     C                   Z-ADD     0             XXDAT             7 0
     C                   Z-ADD     0             XXAMT            11 2
     C                   Z-ADD     0             X1AMT            11 2
     C                   Z-ADD     0             X2AMT            11 2
     C                   Z-ADD     0             X3AMT            11 2
     C                   Z-ADD     0             X4AMT            11 2
     C                   Z-ADD     0             X5AMT            11 2
     C                   Z-ADD     0             XXDPBL           11 2
     C                   Z-ADD     0             XXDPPD           11 2
     C                   Z-ADD     0             XXCXDT            6 0
     C                   MOVE      '0'           *IN51
     C                   MOVE      '0'           *IN54
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2 35
     O                                         +  6 'POLICY YR 1:'
     O                       XXFYR1        Z   +  1
     O          E            ROUTE          2
     O                                         +  6 'POLICY YR 2:'
     O                       XXFYR2        Z   +  1
     O          E            ROUTE          2
     O                                         +  6 'POLICY YR 3:'
     O                       XXFYR3        Z   +  1
     O          E            ROUTE          2
     O                                         +  6 'POLICY YR 4:'
     O                       XXFYR4        Z   +  1
     O          E            ROUTE          2
     O                                         +  6 'CURRENT BALANCE DATE:'
     O                       X2RNDT        Y   +  1
     O          E            ROUTE          2
     O                                         +  6 'INCLUDE FEE TRANSACTIONS'
     O                                         +  1 '(Y/N):'
     O                       XXTRFE            +  1
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O* Report Heading
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             131
     O                       DSDATE        Y    196
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             131
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           1
     O                                          101 'RUN DATE:'
     O                       X2RNDT        Y   +  1
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       FNFND         Z   +  1
     O                       FNNAME            +  2
     O          E            HEAD           2
     O              N50                             'EMPLOYER    DIV'
     O              N50                        +  1 'EMPLOYER NAME'
     O              N50                          58 'AGENT#'
     O                       XXFYR1        Z     74
     O                       XXFYR2        Z     93
     O                       XXFYR3        Z    112
     O                       XXFYR4        Z    131
     O                                          150 'OTHER YEARS'
     O                                          169 'DEP. BILLED'
     O                                          187 'DEP. PAID'
     O              N50                         198 'CANCEL DT'
     O*
     O* Detail Line
     O*
     O          E            DETL2          1
     O                                           34 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O*
     O* Detail Line
     O*
     O          E            DETL           1
     O                       EMEMP#        Z     09
     O                       EMDIV         Z     15
     O                       ENAME               51
     O                       AHAGT#        Z     58
     O                       X1AMT         B     76
     O                       X2AMT         B     95
     O                       X3AMT         B    114
     O                       X4AMT         B    133
     O                       X5AMT         B    152
     O                       XXDPBL        B    171
     O                       XXDPPD        B    189
     O               54      XXCXDT        Y    198
     O*
     O* Total Line
     O*
     O          E            TOTL        2  2
     O                                           52 'TOTALS:'
     O                       X1TTL         B     76
     O                       X2TTL         B     95
     O                       X3TTL         B    114
     O                       X4TTL         B    133
     O                       X5TTL         B    152
     O                       XXDBTL        B    171
     O                       XXDPTL        B    189
     O*
