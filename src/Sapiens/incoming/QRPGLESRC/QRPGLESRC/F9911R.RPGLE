     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F9911R - REPORT - CM nurse unbilled time
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM: F9911C - OVERRIDE - CM nurse unbilled time       *
     H*                                                               *
     H* CALLS TO:    NONE                                             *
     H*                                                               *
     H* CREATED: 09/27/99  C:61  PROJECT  MC176                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* XX/XX/XX C:XX  PROJECT XXXXX                                  *
     H*                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
     H*                                                               *
     H* 01/28/10  C:249     Project: SY215                            *
     H*           C:189     Retrieve brand Name                       *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  08       Batch mode                                          *
     H*  09       Demand mode                                         *
     H*  18       No records meet selection criteria                  *
     H*  19       No printed output                                   *
     H*  20       Print Activity date                                 *
     H*  71 - 74  Control print of detail information                 *
     H*  95       Read main file                                      *
     H*  96       Print Header Information                            *
     H*  97       Print Group Information                             *
     H*  98       Last record processing                              *
     H*  99       Read main file                                      *
     H*                                                               *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * User Parameter
     FSMUPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * Case master - alpha
     FFMCMLM    IF   E           K DISK
      *
      * Case master - numeric
     FFMCMLN    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
      *
      * Time / materials detail charge
     FNDTML7    IF   E           K DISK
      *
      * Subscriber
     FFMSML     IF   E           K DISK
      *
      * Group
     FFMFNL     IF   E           K DISK
      *
      * Employer
     FFMEML     IF   E           K DISK
      *
      * Medical service code table
     FFTMCL     IF   E           K DISK
      *
      * file output - CM nurse unbilled time report
     FFXR5P     O    E             DISK
      *
      * printed output - CM nurse unbilled time report
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
      *
      *****************************************************************
      * CONSTANTS
      *****************************************************************
      *
     D XTITL           C                   CONST('CM Nurse Unbilled Ti-
     D                                     me Report')
      *
      *
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *****************************************************************
      *   KEY LISTS
      *****************************************************************
      *
      *   Output Routing
      *

     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *   Company
      *
     C     COKY          KLIST
     C                   KFLD                    @@CO#
      *
      *   Subscriber
      *
     C     SMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
      *
      *   Fund
      *
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *   User Parameter
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
     C     UPKY1         KLIST
     C                   KFLD                    CMCMNU
      *
      *  Time / materials detail charge
      *
     C     TMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
      *  Medical service code table
      *
     C     MCKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    K1KEY
      *
      *
      *
      *****************************************************************
      *   *LIKE Define Variables
      *****************************************************************
     C     *LIKE         DEFINE    CMSUB#        SVSUB#
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMCMNU        SVCMNU
     C     *LIKE         DEFINE    CMLSN         SVLSN
     C     *LIKE         DEFINE    CMFSN         SVFSN
     C     *LIKE         DEFINE    CMFYR         SVFYR
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    TMEOB#        SVEOB#
     C     *LIKE         DEFINE    TMMSCD        SVMSCD
     C     *LIKE         DEFINE    TMMSEQ        SVMSEQ
      *
     C     *LIKE         DEFINE    CMACDT        XXDATE
     C     *LIKE         DEFINE    MCKEY         K1KEY
      *
      *
      *
      *****************************************************************
      *   PARAMETER LISTS INCLUDING ENTRY PARMS
      *****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SYS             1
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@CMNU            3
     C                   PARM                    @@STAF            3
     C                   PARM                    @@FRDT            7 0
     C                   PARM                    @@TODT            7 0
     C                   PARM                    @@SORT            1
     C                   PARM                    @@TYPE            1
     C                   PARM                    @@MODE            1
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
     C                   EXSR      IZL3BK
     C                   EXSR      IZL4BK
     C                   EXSR      IZL5BK
     C                   EXSR      IZL6BK
     C                   EXSR      IZL7BK
     C                   EXSR      IZL8BK
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
     C                   MOVE      *ON           MEETL3            1
     C                   MOVE      *ON           MEETL4            1
     C                   MOVE      *ON           MEETL5            1
     C                   MOVE      *ON           MEETL6            1
     C                   MOVE      *ON           MEETL7            1
     C                   MOVE      *ON           MEETL8            1
     C                   EXSR      L1TST
     C                   EXSR      L2TST
     C                   EXSR      L3TST
     C                   EXSR      L4TST
     C                   EXSR      L5TST
     C                   EXSR      L6TST
     C                   EXSR      L7TST
     C                   EXSR      L8TST
      *
      *  L1 Break
      *
     C     MEETL1        IFEQ      *OFF
     C     MEETL2        OREQ      *OFF
     C     MEETL3        OREQ      *OFF
     C     MEETL4        OREQ      *OFF
     C     MEETL5        OREQ      *OFF
     C     MEETL6        OREQ      *OFF
     C     MEETL7        OREQ      *OFF
     C     MEETL8        OREQ      *OFF
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
     C     MEETL3        OREQ      *OFF
     C     MEETL4        OREQ      *OFF
     C     MEETL5        OREQ      *OFF
     C     MEETL6        OREQ      *OFF
     C     MEETL7        OREQ      *OFF
     C     MEETL8        OREQ      *OFF
      *
     C     L2CNT         IFNE      *ZERO
     C                   EXSR      L2BK
     C                   ENDIF                                                  L2CNT
      *
     C                   EXSR      IZL2BK
     C                   ENDIF                                                  MEETL2
      *
      *  L3 Break
      *
     C     MEETL3        IFEQ      *OFF
     C     MEETL4        OREQ      *OFF
     C     MEETL5        OREQ      *OFF
     C     MEETL6        OREQ      *OFF
     C     MEETL7        OREQ      *OFF
     C     MEETL8        OREQ      *OFF
      *
     C     L3CNT         IFNE      *ZERO
     C                   EXSR      L3BK
     C                   ENDIF                                                  L3CNT
      *
     C                   EXSR      IZL3BK
     C                   ENDIF                                                  MEETL3
      *
      *  L4 Break
      *
     C     MEETL4        IFEQ      *OFF
     C     MEETL5        OREQ      *OFF
     C     MEETL6        OREQ      *OFF
     C     MEETL7        OREQ      *OFF
     C     MEETL8        OREQ      *OFF
      *
     C     L4CNT         IFNE      *ZERO
     C                   EXSR      L4BK
     C                   ENDIF                                                  L4CNT
      *
     C                   EXSR      IZL4BK
     C                   ENDIF                                                  MEETL4
      *
      *  L5 Break
      *
     C     MEETL5        IFEQ      *OFF
     C     MEETL6        OREQ      *OFF
     C     MEETL7        OREQ      *OFF
     C     MEETL8        OREQ      *OFF
      *
     C     L5CNT         IFNE      *ZERO
     C                   EXSR      L5BK
     C                   ENDIF                                                  L5CNT
      *
     C                   EXSR      IZL5BK
     C                   ENDIF                                                  MEETL5
      *
      *  L6 Break
      *
     C     MEETL6        IFEQ      *OFF
     C     MEETL7        OREQ      *OFF
     C     MEETL8        OREQ      *OFF
      *
     C     L6CNT         IFNE      *ZERO
     C                   EXSR      L6BK
     C                   ENDIF                                                  L6CNT
      *
     C                   EXSR      IZL6BK
     C                   ENDIF                                                  MEETL6
      *
      *  L7 Break
      *
     C     MEETL7        IFEQ      *OFF
     C     MEETL8        OREQ      *OFF
      *
     C     L7CNT         IFNE      *ZERO
     C                   EXSR      L7BK
     C                   ENDIF                                                  L7CNT
      *
     C                   EXSR      IZL7BK
     C                   ENDIF                                                  MEETL7
      *
      *  L8 Break
      *
     C     MEETL8        IFEQ      *OFF
      *
     C     L8CNT         IFNE      *ZERO
     C                   EXSR      L8BK
     C                   ENDIF                                                  L8CNT
      *
     C                   EXSR      IZL8BK
     C                   ENDIF                                                  MEETL8
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
     C                   ADD       1             L3CNT             9 0
     C                   ADD       1             L4CNT             9 0
     C                   ADD       1             L5CNT             9 0
     C                   ADD       1             L6CNT             9 0
     C                   ADD       1             L7CNT             9 0
     C                   ADD       1             L8CNT             9 0
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
     C                   SETON                                        OA96
     C  N19
     CAN OA              EXCEPT    HEAD
     C  N19              EXCEPT    NOREC
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
     C     L3CNT         IFNE      *ZERO
     C                   EXSR      L3BK
     C                   ENDIF
      *
     C     L4CNT         IFNE      *ZERO
     C                   EXSR      L4BK
     C                   ENDIF
      *
     C     L5CNT         IFNE      *ZERO
     C                   EXSR      L5BK
     C                   ENDIF
      *
     C     L6CNT         IFNE      *ZERO
     C                   EXSR      L6BK
     C                   ENDIF
      *
     C     L7CNT         IFNE      *ZERO
     C                   EXSR      L7BK
     C                   ENDIF
      *
     C     L8CNT         IFNE      *ZERO
     C                   EXSR      L8BK
     C                   ENDIF
      *
     C                   EXSR      LRBK
      *
     C                   ENDIF                                                  MEETPR
      *
      *  End of Processing
      *
     C     END           TAG
     C                   MOVE      *ON           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkprt = '0';
       Chain Orky Por01;
       If %found and @@type <> 'F';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
      *
      *
      *****************************************************************
      * ACCTOT - Accumulate Totals
      *****************************************************************
     CSR   ACCTOT        BEGSR
      *
      *  Accumulate L1 Totals - (most detailed level)
      *
      *
      *  Accumulate L2 Totals - (next most detailed level)
      *
      *
      *  Accumulate L3 Totals - (next most detailed level)
      *
      *
      *  Accumulate L4 Totals - (next most detailed level)
      *
      *
      *  Accumulate L5 Totals - (next most detailed level)
      *
     C                   ADD       TMHRS         L5THRS           11 2
     C                   ADD       XXTAMT        L5TAMT           11 2
      *
      *  Accumulate L6 Totals - (next most detailed level)
      *
      *
      *  Accumulate L7 Totals - (next most detailed level)
      *
      *
      *  Accumulate L8 Totals - (next most detailed level)
      *
      *
      *  Accumulate LR Totals - REPORT
      *
     C                   ADD       TMHRS         LRTHRS           11 2
     C                   ADD       XXTAMT        LRTAMT           11 2
      *
     C                   ENDSR                                                  ACCTOT
      *
      *
      *
      *****************************************************************
      * IZL1BK - Initialize Level 1 break criteria
      *****************************************************************
     CSR   IZL1BK        BEGSR
      *
     C                   Z-ADD     TMMSEQ        SVMSEQ
     C                   SETOFF                                       74
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
     C                   MOVEL(P)  TMMSCD        SVMSCD
     C                   SETOFF                                       73
      *
     C                   ENDSR                                                  IZL2BK
      *
      *
      *
      *****************************************************************
      * IZL3BK - Initialize Level 3 break criteria
      *****************************************************************
     CSR   IZL3BK        BEGSR
      *
     C                   Z-ADD     TMEOB#        SVEOB#
     C                   SETOFF                                       72
      *
     C                   ENDSR                                                  IZL3BK
      *
      *
      *
      *****************************************************************
      * IZL4BK - Initialize Level 4 break criteria
      *****************************************************************
     CSR   IZL4BK        BEGSR
      *
     C                   MOVEL(P)  CMLSN         SVLSN
     C                   MOVEL(P)  CMFSN         SVFSN
      *
     C                   Z-ADD     CMFYR         SVFYR
     C                   Z-ADD     CMCASE        SVCASE
      *
     C                   SETOFF                                       71
      *
     C                   ENDSR                                                  IZL4BK
      *
      *
      *
      *****************************************************************
      * IZL5BK - Initialize Level 5 break criteria
      *****************************************************************
     CSR   IZL5BK        BEGSR
      *
     C                   MOVEL(P)  CMCMNU        SVCMNU
      *
     C                   ENDSR                                                  IZL5BK
      *
      *
      *
      *****************************************************************
      * IZL6BK - Initialize Level 6 break criteria
      *****************************************************************
     CSR   IZL6BK        BEGSR
      *
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         SVDIV
      *
     C                   ENDSR                                                  IZL6BK
      *
      *
      *
      *****************************************************************
      * IZL7BK - Initialize Level 7 break criteria
      *****************************************************************
     CSR   IZL7BK        BEGSR
      *
     C                   Z-ADD     CMFND         SVFND
      *
     C                   ENDSR                                                  IZL7BK
      *
      *
      *
      *****************************************************************
      * IZL8BK - Initialize Level 8 break criteria
      *****************************************************************
     CSR   IZL8BK        BEGSR
      *
     C                   Z-ADD     CMSUB#        SVSUB#
      *
     C                   ENDSR                                                  IZL8BK
      *
      *
      *
      *****************************************************************
      * L1BK  -  Level ONE Break action
      *****************************************************************
     CSR   L1BK          BEGSR
      *
      *  Output L1 totals
      *
      *
     C                   ENDSR                                                  L1BK
      *
      *
      *
      *****************************************************************
      * L2BK  -  Level TWO Break Action
      *****************************************************************
     CSR   L2BK          BEGSR
      *
      *
     C                   ENDSR                                                  L2BK
      *
      *
      *
      *****************************************************************
      * L3BK  - Level THREE Break Action
      *****************************************************************
     CSR   L3BK          BEGSR
      *
      *
     C                   ENDSR                                                  L3BK
      *
      *
      *
      *****************************************************************
      * L4BK  -  Level FOUR Break Action
      *****************************************************************
     CSR   L4BK          BEGSR
      *
      *
     C                   ENDSR                                                  L4BK
      *
      *
      *
      *****************************************************************
      * L5BK  -  Level FIVE Break Action
      *****************************************************************
     CSR   L5BK          BEGSR
      *
     C  N19              EXCEPT    L5TOT
     C                   SETON                                        OA
      *
     C                   ENDSR                                                  L5BK
      *
      *
      *
      *****************************************************************
      * L6BK  -  Level SIX Break Action
      *****************************************************************
     CSR   L6BK          BEGSR
      *
      *
     C                   ENDSR                                                  L6BK
      *
      *
      *
      *****************************************************************
      * L7BK  -  Level SEVEN Break Action
      *****************************************************************
     CSR   L7BK          BEGSR
      *
      *
     C                   ENDSR                                                  L7BK
      *
      *
      *
      *****************************************************************
      * L8BK  -  Level EIGHT Break Action
      *****************************************************************
     CSR   L8BK          BEGSR
      *
      *
     C                   ENDSR                                                  L8BK
      *
      *
      *
      *****************************************************************
      * L1TST - Level ONE Break TEST criteria
      *****************************************************************
     CSR   L1TST         BEGSR
      *
      *  MEETL1 = *OFF if Level 1 Break processing to proceed
      *
     C     TMMSEQ        IFNE      SVMSEQ
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
     C     TMMSCD        IFNE      SVMSCD
     C                   MOVE      *OFF          MEETL2
     C                   ENDIF
      *
     C                   ENDSR                                                  L2TST
      *
      *
      *
      *****************************************************************
      * L3TST - Level THREE Break TEST criteria
      *****************************************************************
     CSR   L3TST         BEGSR
      *
      *  MEETL3 = *OFF if Level 3 Break processing to proceed
      *
     C     TMEOB#        IFNE      SVEOB#
     C                   MOVE      *OFF          MEETL3
     C                   ENDIF
      *
     C                   ENDSR                                                  L3TST
      *
      *
      *
      *****************************************************************
      * L4TST - Level FOUR Break TEST criteria
      *****************************************************************
     CSR   L4TST         BEGSR
      *
      *  MEETL4 = *OFF if Level 4 Break processing to proceed
      *
      *
     C     @@SORT        IFEQ      'A'
      *
     C     CMLSN         IFNE      SVLSN
     C     CMFSN         ORNE      SVFSN
     C                   MOVE      *OFF          MEETL4
     C                   ENDIF
      *
     C                   ELSE
      *
     C     CMFYR         IFNE      SVFYR
     C     CMCASE        ORNE      SVCASE
     C                   MOVE      *OFF          MEETL4
     C                   ENDIF
      *
     C                   ENDIF                                                  @@SORT
      *
     C                   ENDSR                                                  L4TST
      *
      *
      *
      *****************************************************************
      * L5TST - Level FIVE Break TEST criteria
      *****************************************************************
     CSR   L5TST         BEGSR
      *
      *  MEETL5 = *OFF if Level 5 Break processing to proceed
      *
     C     CMCMNU        IFNE      SVCMNU
     C                   MOVE      *OFF          MEETL5
     C                   ENDIF
      *
     C                   ENDSR                                                  L5TST
      *
      *
      *
      *****************************************************************
      * L6TST - Level SIX Break TEST criteria
      *****************************************************************
     CSR   L6TST         BEGSR
      *
      *  MEETL6 = *OFF if Level 6 Break processing to proceed
      *
     C     CMEMP#        IFNE      SVEMP#
     C     CMDIV         ORNE      SVDIV
     C                   MOVE      *OFF          MEETL6
     C                   ENDIF
      *
     C                   ENDSR                                                  L6TST
      *
      *
      *
      *****************************************************************
      * L7TST -  Level SEVEN Break TEST criteria
      *****************************************************************
     CSR   L7TST         BEGSR
      *
      *  MEETL7 = *OFF if Level 7 Break processing to proceed
      *
     C     CMFND         IFNE      SVFND
     C                   MOVE      *OFF          MEETL7
     C                   ENDIF
      *
     C                   ENDSR                                                  L7TST
      *
      *
      *
      *****************************************************************
      * L8TST -  Level EIGHT Break TEST criteria
      *****************************************************************
     CSR   L8TST         BEGSR
      *
      *  MEETL8 = *OFF if Level 8 Break processing to proceed
      *
     C     CMSUB#        IFNE      SVSUB#
     C                   MOVE      *OFF          MEETL8
     C                   ENDIF
      *
     C                   ENDSR                                                  L8TST
      *
      *
      *
      *****************************************************************
      * LRBK - Last Record Processing
      *****************************************************************
     CSR   LRBK          BEGSR
      *
     C                   SETON                                        98
     C  N19              EXCEPT    LRTOT
      *
     C                   ENDSR                                                  LRBK
      *
      *
      *
      *****************************************************************
      * PGMSTR - Set-up and Initialize
      *****************************************************************
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
     C                   Z-ADD     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
      *
     C                   Z-ADD     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C     @@TYPE        COMP      'F'                                    19
      *
     C                   Z-ADD     *ZERO         L5TAMT           11 2
     C                   Z-ADD     *ZERO         LRTAMT           11 2
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *****************************************************************
      * READ1 - Read first record from file
      *****************************************************************
     CSR   READ1         BEGSR
      *
     C     *LOVAL        SETLL     PCM01
     C     *LOVAL        SETLL     PCM02
      *
     C     *IN95         DOWEQ     *OFF
      *
     C     @@SORT        IFEQ      'A'
     C                   READ      PCM01                                  95
      *
     C                   ELSE
      *
     C                   READ      PCM02                                  95
     C                   ENDIF
      *
     C   95              SETON                                        99
     C   95              LEAVE
     C     TMKY          CHAIN     PTM01                              99
     C  N99              LEAVE
     C                   ENDDO                                                  *IN95
      *
     C                   ENDSR                                                  READ1
      *
      *
      *
      *****************************************************************
      * READN - Read next record from file
      *****************************************************************
     CSR   READN         BEGSR
      *
     C  N99TMKY          READE     PTM01                                  99
      *
     C     *IN99         IFEQ      *ON
     C                   SETOFF                                       95
      *
     C     *IN95         DOWEQ     *OFF
      *
     C     @@SORT        IFEQ      'A'
     C                   READ      PCM01                                  95
      *
     C                   ELSE
      *
     C                   READ      PCM02                                  95
     C                   ENDIF
      *
     C   95              SETON                                        99
     C   95              LEAVE
     C     TMKY          CHAIN     PTM01                              99
     C  N99              LEAVE
     C                   ENDDO                                                  *IN95 = *OFF
      *
     C                   ENDIF                                                  *IN99 = *ON
      *
     C                   ENDSR                                                  READN
      *
      *
      *
      *****************************************************************
      * SRDET - Write Detail Record
      *****************************************************************
     CSR   SRDET         BEGSR
      *
      * Get subscriber name
      *
     C     SMKY          CHAIN     PSM01                              60
     C  N60              MOVEL(P)  SMNAME        XXSNAM           60
     C   60              MOVEL(P)  *BLANKS       XXSNAM
      *
      * Get fund name
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        XXFNAM           60
     C   60              MOVEL(P)  *BLANKS       XXFNAM
      *
      *
      * Get Employer name
      *
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMNAME        XXENAM           60
     C   60              MOVEL(P)  *BLANKS       XXENAM
      *
      * Get CM nurse name
      *
     C     UPKY1         CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPUSNM        XXNNAM           60
     C   60              MOVEL(P)  *BLANKS       XXNNAM
      *
      * Claimant Name
      *
     C     CMFSN         IFNE      *BLANKS
     C     CMLSN         CAT(P)    ',':0         XXCLNM           40
     C                   CAT(P)    CMFSN:1       XXCLNM
      *
     C                   ELSE
      *
     C                   MOVEL(P)  CMLSN         XXCLNM
     C                   ENDIF
      *
      * Activity date
      *
     C                   Z-ADD     *ZERO         XXSVDT
      *
     C                   Z-ADD     TMSVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSVDT            8 0
      *
     C     XXSVDT        COMP      *ZERO                                  20
      *
      * Total amount
      *
     C     TMHRS         MULT      TMHRRT        XXTMP1           15 5
     C     TMMILE        MULT      TMMIRT        XXTMP2           15 5
     C                   Z-ADD     XXTMP1        XXTAMT           11 2
     C                   ADD       XXTMP2        XXTAMT
     C                   ADD       TMMSCG        XXTAMT
      *
     C     @@TYPE        IFEQ      'B'
     C     @@TYPE        OREQ      'F'
     C                   EXSR      WRITER
     C                   ENDIF
      *
     C  N19
     CAN OA              EXCEPT    HEAD
     C  N19              EXCEPT    DETAIL
      *
     C                   SETON                                        717273
     C                   SETON                                        74
      *
     C                   ENDSR                                                  SRDET
      *
      *
      *
      *****************************************************************
      * SRRTE - Print routing page
      *****************************************************************
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *
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
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********  N19              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
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
     C                   MOVEL(P)  ##DATA        CONAME
      *
     C                   ENDSR                                                  SRRTE
      *
      *
      *
      *****************************************************************
      * SUBSET - Check if member of records processed subset
      *****************************************************************
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if record not to be processed
      *
      * Select STAF CODE if entered
      *
     C     @@STAF        IFNE      *BLANKS
     C     @@STAF        ANDNE     TMSTAF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Invoice number must be zero
      *
     C     TMINV#        IFNE      *ZERO
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Service Date must be within range
      *
     C     TMSVDT        IFLT      @@FRDT
     C     TMSVDT        ORGT      @@TODT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * System ID must match if entered.
      *
     C     @@SYS         IFNE      *BLANK
     C     TMSYS         ANDNE     @@SYS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
      *****************************************************************
      * WRITER - WRITE FXR5P
      *****************************************************************
     CSR   WRITER        BEGSR
      *
     C                   CLEAR                   PR501
      *
     C                   Z-ADD     CMCO#         R5CO#
     C                   MOVEL(P)  CONAME        R5CONM
      *
      *
     C                   Z-ADD     CMSUB#        R5SUB#
     C     SMKY          CHAIN     PSM01                              60
     C  N60              MOVEL(P)  SMNAME        R5SBNM
     C   60              MOVEL(P)  *BLANKS       R5SBNM
      *
      *
     C                   Z-ADD     CMFND         R5FND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        R5FNNM
     C   60              MOVEL(P)  *BLANKS       R5FNNM
      *
      *
     C                   Z-ADD     CMEMP#        R5EMP#
     C                   Z-ADD     CMDIV         R5DIV
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMNAME        R5EMNM
     C   60              MOVEL(P)  *BLANKS       R5EMNM
      *
      *
     C                   Z-ADD     CMFYR         R5FYR
     C                   Z-ADD     CMCASE        R5CASE
      *
     C     CMFSN         IFNE      *BLANKS
     C     CMLSN         CAT(P)    ',':0         R5CLNM
     C                   CAT(P)    CMFSN:1       R5CLNM
      *
     C                   ELSE
      *
     C                   MOVEL(P)  CMLSN         R5CLNM
     C                   ENDIF
      *
     C                   MOVE      CMSS#         R5SS#
     C                   Z-ADD     CMACDT        R5ACDT
     C                   MOVEL(P)  CMCMNU        R5CMNU
      *
     C                   MOVE      TMSYS         R5SYS
      *
     C                   Z-ADD     TMEOB#        R5EOB#
      *
     C                   MOVEL(P)  TMMSCD        R5SVCD
      *
     C                   MOVEL(P)  TMMSCD        K1KEY
     C     MCKY          CHAIN     PMC01                              60
     C  N60              MOVEL(P)  MCDESC        R5SVDS
     C   60              MOVEL(P)  *BLANKS       R5SVDS
      *
     C                   MOVEL(P)  TMSTAF        R5STAF
     C                   Z-ADD     TMSEQ#        R5SEQ#
      *
     C                   Z-ADD     TMSVDT        R5SVDT
     C                   MOVEL(P)  TMACT         R5ACT
     C                   MOVEL(P)  TMPNFL        R5PNFL
     C                   Z-ADD     TMHRS         R5HRS
     C                   Z-ADD     TMHRRT        R5HRRT
     C                   Z-ADD     TMMILE        R5MILE
     C                   Z-ADD     TMMIRT        R5MIRT
     C                   Z-ADD     TMMSCG        R5MSCG
      *
     C                   Z-ADD     XXTAMT        R5TAMT
      *
     C                   MOVEL(P)  'A'           R5STAT
     C                   MOVEL(P)  Q1USER        R5USER
     C                   MOVEL(P)  Q1PGM         R5PGM
     C                   MOVEL(P)  Q1USER        R5IUSR
     C                   MOVEL(P)  Q1PGM         R5IPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSTIME        R5TIME
     C                   Z-ADD     DSTIME        R5ITIM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R5DT
     C                   MOVEL     SRDTO         R5IDT
      *
     C                   WRITE     PR501
      *
     C                   FEOD      FXR5P
      *
     C                   ENDSR                                                  WRITER
      *
      *
      *
      *****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'

     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              56
      *
     O**********E            ROUTE          1
     O**********             ORDIS2              56
      *
     O**********E            ROUTE          1
     O**********             ORDIS3              56
      *
     O**********E            ROUTE          1
     O**********             ORDIS4              56
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
      *
     O          E            ROUTE          1
     O                       ORDIS2              56
      *
     O          E            ROUTE          1
     O                       ORDIS3              56
      *
     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
      *
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      *
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      *
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                           26 'COMPANY . . . . :'
     O                       @@CO#         Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'SYSTEM  . . . . :'
     O                       @@SYS             +001
      *
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER  . . :'
     O                       @@SUB#        Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'GROUP . . . . . :'
     O                       @@FND         Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'EMPLOYER / DIV  :'
     O                       @@EMP#        Z   +001
     O                       @@DIV         Z   +001
      *
     O          E            ROUTE          2
     O                                           26 'CM NURSE  . . . :'
     O                       @@CMNU            +001
      *
     O          E            ROUTE          2
     O                                           26 'STAFF CODE  . . :'
     O                       @@STAF            +001
      *
     O          E            ROUTE          2
     O                                           26 'DATE RANGE  . . :'
     O                       XXFRDT            +001 '  /  /    '
     O                                         +001 'to'
     O                       XXTODT            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                           26 'SORT ORDER  . . :'
     O                       @@SORT            +001
      *
     O          E            ROUTE          2
     O                                           26 'TYPE OF OUTPUT  :'
     O                       @@TYPE            +001
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                25 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      *
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
      *
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          191 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E            HEAD           2
     O                                           91 'Date Range:'
     O                       XXFRDT            +001 '0  /  /    '
     O                                         +001 'to'
     O                       XXTODT            +001 '0  /  /    '
      *
     O          E            HEAD           2
     O                                           17 'Report Selection:'
     O                                         +004 'Sub:'
     O                       @@SUB#        4   +001
     O                                         +004 'Group:'
     O                       @@FND         4   +001
     O                                         +004 'Emp:'
     O                       @@EMP#        4   +001
     O                                         +000 '-'
     O                       @@DIV             +000 '    0'
     O                                         +004 'CM nurse:'
     O                       @@CMNU            +001
     O                                         +004 'Staff code:'
     O                       @@STAF            +001
     O                                         +004 'Sort:'
     O                       @@SORT            +001
     O                                         +004 'Type of Output:'
     O                       @@TYPE            +001
      *
     O          E   N96      HEAD           1
     O                                         +000 'Subscriber:'
     O                       CMSUB#        Z   +001
     O                       XXSNAM            +013
      *
     O          E   N96      HEAD           1
     O                                         +000 'Group     :'
     O                       CMFND         Z   +001
     O                       XXFNAM            +015
      *
     O          E   N96      HEAD           1
     O                                         +000 'Employer  :'
     O                       CMEMP#        Z   +001
     O                       CMDIV         Z   +001
     O                       XXENAM            +003
      *
     O          E   N96      HEAD           1
     O                                         +000 'CM nurse  :'
     O                       CMCMNU            +001
     O                       XXNNAM            +015
      *
     O          E   N96      HEAD           1
     O                                         +066 'Svc'
     O                                         +004 'Svc'
     O                                         +003 'Staff'
     O                                         +007 'Activity'
     O                                         +002 'Act'
     O                                         +017 'Hourly'
     O                                         +008 'Mileage'
     O                                         +003 'Misc'
     O                                         +012 'Total'
      *
     O          E            HEAD        0  0
     O                       LINE              +000
      *
     O          E   N96      HEAD           2
     O                                         +000 'Claim #'
     O                                         +006 'Claimant'
     O                                         +034 'EOB#/SVC#'
     O                                         +002 'Code'
     O                                         +003 'Seq#'
     O                                         +002 'Code'
     O                                         +002 'Sys'
     O                                         +005 'Date'
     O                                         +004 'Code'
     O                                         +002 'P/N'
     O                                         +002 'Hours'
     O                                         +005 'Rate'
     O                                         +003 'Miles'
     O                                         +002 'Rate'
     O                                         +004 'Charges'
     O                                         +010 'Amount'
      *
      *
     O          E            DETAIL         1
     O              N71      CMFYR         X   +000
     O              N71                        +000 '-'
     O              N71      CMCASE        X   +000
     O              N71      XXCLNM            +002
     O              N72      TMEOB#        4   +002
     O              N73      TMMSCD            +002
     O              N74      TMMSEQ        3   +002
     O                       TMSTAF            +003
     O                       TMSYS             +003
     O              N20      XXSVDT            +003 '  /  /    '
     O                       TMACT             +002
     O                       TMPNFL            +003
     O                       TMHRS         4   +004
     O                       TMHRRT        3   +002
     O                       TMMILE        4   +002
     O                       TMMIRT        3   +002
     O                       TMMSCG        J   +002
     O                       XXTAMT        J   +002
      *
      *
     O          E            L5TOT       1  1
     O                                         +079 'CM Nurse:'
     O                                         +  3 'Total hours:'
     O                       L5THRS        JB  +001
     O                                         + 12 'Total unbilled charges:'
     O                       L5TAMT        JB  +001
      *
     O          E            LRTOT       1  1
     O                                         +081 'Report:'
     O                                         +  3 'Total hours:'
     O                       LRTHRS        JB  +001
     O                                         + 12 'Total unbilled charges:'
     O                       LRTAMT        JB  +001
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
