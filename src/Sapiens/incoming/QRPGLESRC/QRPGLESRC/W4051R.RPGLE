     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4051R - Debit / Credit Report
     H*****************************************************************
     H* REVISIONS:                                                    *
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
     H* 08/15/05 C:09  PROJECT 129WLZ                                 *
     H*                Add 2 new fields to reflect audit status and   *
     H*                agent rate number and percentage,              *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 12/15/99 C:61  PROJECT WC577                                  *
     H*              - Print cancellation date on report.             *
     H*              - Write output file WFDCP.                       *
     H*                                                               *
     H*  04/13/99  C:90  PROJECT: 96WAD (IN BASE: 07/19/99 C:33 WC899)*
     H*            DO NOT PROCESS ACCOUNT CURRENT POLICIES.           *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
     H*****************************************************************
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
     F* Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F* Employer Detail File
     FWDELL1    IF   E           K DISK
     F*
     F* Employer A/R File
     FWMEAL8    IF   E           K DISK
     F*
     FWTABL     IF   E           K DISK
     F* Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
     FWFDCP     O  A E             DISK
      *
      * Effective and Cancel Date Arrays
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D* System Date & Time
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  *LIKE DEFINES
      *

     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQRNDT        X1RNDT            8 0
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT            7 0
     C*
     C* Set-Up and Initialize
     C*
     C                   MOVE      *ALL'_'       LINE            132
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        Demand Mode
     C                   ELSE
     C                   SETON                                        08        Batch Mode
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*          XXFYR     ADD  1900      XXYEAR  40
     C                   Z-ADD     0             XXTLDB           11 2          Total Debit Amt
     C                   Z-ADD     0             XXTLCR           11 2          Total Credit Am
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
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Key List for the Employer Accounts Receivable File
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
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
     C*
     C* Get the User Parameter Master Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Get the Routing Master Record
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* Print the Distribution / Routing Information
     C*
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
     C                   END
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
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
     C* Begin Reading the Employer Master Records
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             XXAMTD           11 2
     C                   Z-ADD     0             XXCRED           11 2
     C                   Z-ADD     0             XXDEBT           11 2
     C                   MOVE      '0'           *IN50
     C                   MOVE      '0'           *IN51
     C                   Z-ADD     0             XXDAT
     C*
     C* Get the Employer Detail Record
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
      *
      *  Get Employer Effective/Cancellation date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     ED(X)         XXEFDT
     C                   Z-ADD     CD(X)         XXCXDT
      *
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT            8 0
      *
     C     SRERR         COMP      *ON                                    71
      *
     C                   Z-ADD     XXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1CXDT            8 0
      *
     C     SRERR         COMP      *ON                                    72
     C*
     C* If employer has been auditted include all A/R
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     99999999      XXDAT             8 0
     C                   ELSE
     C                   Z-ADD     XXRNDT        XXDAT
     C                   END
     C*
     C*  Use Cancelled Date if <> 0 and Employer isn't Auditted
     C*
     C*          XXDAT     IFNE 999999                     AMM 09-29-89
     C*                    Z-ADD1         X       30       |
     C*          *ZERO     LOKUPED,X                     60|
     C*          *IN60     IFEQ '0'                        |
     C*                    Z-ADD5         X                |
     C*                    ELSE                            |
     C*                    SUB  1         X                |
     C*                    END                             |
     C*                                                    |
     C*                    Z-ADD1         Y       30       |
     C*          *ZERO     LOKUPCD,Y                     60|
     C*          *IN60     IFEQ '0'                        |
     C*                    Z-ADD5         Y                |
     C*                    ELSE                            |
     C*                    SUB  1         Y                |
     C*                    END                             |
     C*                                                    |
     C*          Y         IFEQ 0                          |
     C*          CD,Y      ORLT ED,X                       |
     C*                    Z-ADDXXRNDT    XXDAT            |
     C*                    ELSE                            |
     C*                    Z-ADDCD,Y      XXDAT            |
     C*                    END                             |
     C*                                                    |
     C*                    END                             AMM 09-29-89
     C*
     C* Begin Reading Employer's Accounts Receivable Records
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
      *
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     EAPODT        IFLE      XXDAT
     C     TTCTYP        ANDEQ     'P'
     C                   SELECT
     C     EAARTY        WHENEQ    'P'
     C     EAARTY        OREQ      'T'
     C                   SUB       EAAMT         XXAMTD
     C                   OTHER
     C                   ADD       EAAMT         XXAMTD
     C                   ENDSL
     C                   ENDIF                                                  EAPODT < XXDAT
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   END
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
     C                   PARM                    @@NWRT            5 2          New Rate
     C                   PARM                    @@RNRT            5 2          Renew Rate
     C*
     C* Print the Detail Line If XXAMTD <> 0
     C*
     C     XXAMTD        IFNE      0
     C     @@PLAN        ANDNE     'A'
     C*
     C* If XXAMTD > 0 then add to Debit Column
     C*
     C     XXAMTD        IFGT      0
     C                   Z-ADD     XXAMTD        XXDEBT
     C                   ADD       XXAMTD        XXTLDB
     C                   MOVE      '1'           *IN51
     C                   END
     C*
     C* If XXAMTD < 0 then add to Credit Column
     C*
     C     XXAMTD        IFLT      0
     C                   MULT      -1            XXAMTD
     C                   Z-ADD     XXAMTD        XXCRED
     C                   ADD       XXAMTD        XXTLCR
     C                   MOVE      '1'           *IN50
     C                   END
      *
      *  Write record to output file
      *
     C                   EXSR      SROUT
      *
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C                   EXCEPT    DETL2
     C*
     C*
     C                   EXCEPT    DETL
     C*
     C                   END
     C*
     C                   END
     C*
     C     ELKEY         READE     PEL01                                  60
     C                   END
     C*
     C* Print Total Line
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C                   EXCEPT    TOTL
     C*
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
     C                   MOVE      '1'           *INLR
     O*
      *************************************************************
      * SROUT - Write record to output file WFDCP
      *************************************************************
     C     SROUT         BEGSR
      *
     C                   Z-ADD     ELCO#         DCCO#
     C                   MOVEL(P)  CONAME        DCCONM
     C                   Z-ADD     ELFND         DCFND
     C                   MOVEL(P)  FNNAME        DCFNNM
     C                   MOVEL(P)  FNANAM        DCFANM
     C                   Z-ADD     ELEMP#        DCEMP#
     C                   Z-ADD     ELDIV         DCDIV
     C                   MOVEL(P)  EMNAME        DCEMNM
     C                   Z-ADD     ELFYR         DCFYR
     C                   MOVE      ELFAF         DCFAF
      *
     C                   Z-ADD     XXDEBT        DCDAMT
     C                   Z-ADD     XXCRED        DCCAMT
      *
     C                   Z-ADD     X1RNDT        DCRNDT
     C                   Z-ADD     X1EFDT        DCEFDT
     C                   Z-ADD     X1CXDT        DCCXDT
      *
     C                   Z-ADD     @@AGT#        DCAGT#
     C                   MOVEL(P)  @@ANAM        DCANAM
     C                   Z-ADD     @@PROD        DCPROD
     C                   MOVEL(P)  @@PNAM        DCPNAM
     C                   MOVEL     @@NWRT        DCNWRT
      *
     C                   MOVEL     Q1USER        DCIUSR
     C                   MOVEL     Q1USER        DCUSER
     C                   MOVEL     Q1PGM         DCIPGM
     C                   MOVEL     Q1PGM         DCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCIDT
     C                   MOVEL     SRDTO         DCDT
     C                   MOVEL     DSTIME        DCITM
     C                   MOVEL     DSTIME        DCTIME
      *
     C                   WRITE     PDC01
      *
     C                   ENDSR                                                  SROUT
      *
      *
     O*
     O* Output Specifications
     O*
     O* Routing Page
     O*
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
     O          E            ROUTE          2
     O                                         +  6 'POLICY YEAR:'
     O                       XXFYR         Z   +  1
     O          E            ROUTE          2
     O                                         +  6 'CURRENT BALANCE DATE:'
     O                       X1RNDT            +  1 '  /  /    '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *
      * Report Heading
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME              96
     O                       DSDATE        Y    132
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
     O          E            HEAD           2
     O                                         + 37 'POLICY YEAR:'
     O                       XXFYR         Z   +  1
     O                                         + 10 'CURRENT BALANCE DATE:'
     O                       X1RNDT        Y   +  1
      *
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       FNFND         Z   +  1
     O                       FNNAME            +  2
      *
     O          E            HEAD        0  0
     O                       LINE              +  0
      *
     O          E            HEAD           2
     O                                           09 'EMPLOYER'
     O                                           15 'DIV'
     O                                           29 'EMPLOYER NAME'
     O                                           71 'DEBIT AMT'
     O                                           89 'CREDIT AMT'
     O                                         +003 'CANCEL DATE'
     O*
     O* Detail Line
     O*
     O          E   N50N51   DETL2          1
     O                                           34 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O*
     O* Detail Line
     O*
     O          E    50      DETL           1
     O                       EMEMP#        Z     09
     O                       EMDIV         Z     15
     O                       EMNAME              56
     O                       XXCRED        J     90
     O              N72      X1CXDT             102 '  /  /    '
      *
     O          E    51      DETL           1
     O                       EMEMP#        Z     09
     O                       EMDIV         Z     15
     O                       EMNAME              56
     O                       XXDEBT        J     72
     O              N72      X1CXDT             102 '  /  /    '
     O*
     O* Total Line
     O*
     O          E            TOTL        2  1
     O                                           51 'TOTAL:'
     O                       XXTLDB        J     72
     O                       XXTLCR        J     90
