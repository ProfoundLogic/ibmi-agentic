     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F9312R - CM SERVICES SECONDARY REPORT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     LISTS PERSONS WHO ARE RESPONSIBLE FOR SECONDARY CASE LEVEL*
     H*                                                               *
     H*  CALLED FROM: F9311C - OVERRIDE - CM SERVICES REPORT          *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*  CREATED: 11/03/99        PROGRAMMER: 62       PROJECT: MC175 *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  FILES:                                                       *
     H*  SMCOL    PCO01: CO                                   Company *
     H*  SMORL    POR01: CO/PGM                              Override *
     H*  SMUPL    PUP01: USER                               User Parm *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       Read main File                                      *
     H*  98       Last record processing                              *
     H*  97       Print Group Information                             *
     H*  20       TO FILE                                             *
     H*  19       TO REPORT                                           *
     H*  18       IF THERE IS NO HITS                                 *
     H*  17       Is the case status 'C'                              *
     H*  16       What Case Master File to Use                        *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*****************************************************************
     F*
     F* System Company
     FSMCOL     IF   E           K DISK
     F*
     F* System Output Routing
     FSMORL     IF   E           K DISK
     F*
     F* User Parameter
     FSMUPL     IF   E           K DISK
     F*
     F* User Parameter
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*
     F* CASE MASTER - ALPHA
     FFXR6L2    IF   E           K DISK
     F*
     F* CASE MASTER - NUMERIC
     FFXR6L1    IF   E           K DISK
     F                                     RENAME(PR601:PR602)
     FFMCML1    IF   E           K DISK
     F* T/M ACTIVITY FILE
     FNDTML1    IF   E           K DISK
     F* CASE SERVICE FILE
     FFAMSL     IF   E           K DISK
     FFTMCL     IF   E           K DISK
     F* SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* GROUP FILE
     FFMFNL     IF   E           K DISK
     F* EMPLOYER FILE
     FFMEML     IF   E           K DISK
     F* CASE HISTORY REPORT FILE
     FFXR4P     O    E             DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D XTITL           C                   CONST('CM Secondary Servi-
     D                                     ces Report')
     D*
     D*****************************************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*   KEY LISTS
     C*****************************************************************
     C*
     C*  CLAIM Master
     C*
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    R6SUB#
     C                   KFLD                    R6FND
     C                   KFLD                    R6FYR
     C                   KFLD                    R6CASE
     C*
     C*   Company
     C*
     C     COKY          KLIST
     C                   KFLD                    @@CO#
     C*
     C*  Employer Master
     C*
     C     EMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    R6SUB#
     C                   KFLD                    R6FND
     C                   KFLD                    R6EMP#
     C                   KFLD                    R6DIV
     C*
     C*   Fund
     C*
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    R6SUB#
     C                   KFLD                    R6FND
     C*
     C*  CASE SERVICE FILE
     C*
     C     MSKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    R6SUB#
     C                   KFLD                    R6FND
     C                   KFLD                    R6FYR
     C                   KFLD                    R6CASE
     C                   KFLD                    R6SEQ#
     C                   KFLD                    R6MSCD
     C*
     C*  SERVICE CODE TABLE FILE
     C*
     C     MCKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    KKKEY
     C*
     C*  Output Routing
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C*   Subscriber
     C*
     C     SMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    R6SUB#
     C*
     C*  T/M ACTIVITY FILE
     C*
     C     TMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    R6SUB#
     C                   KFLD                    R6FND
     C                   KFLD                    R6FYR
     C                   KFLD                    R6CASE
     C*
     C*   User Parameter
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    KKSTAF
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    R6SUB#        SVSUB#
     C     *LIKE         DEFINE    R6FND         SVFND
     C     *LIKE         DEFINE    R6EMP#        SVEMP#
     C     *LIKE         DEFINE    R6DIV         SVDIV
     C     *LIKE         DEFINE    R6STAF        SVSTAF
     C     *LIKE         DEFINE    R6CASE        SVCASE
     C     *LIKE         DEFINE    R6FYR         SVFYR
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    CMDT          XXBIDT
     C     *LIKE         DEFINE    MCKEY         KKKEY
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@STAF            3
     C                   PARM                    @@CSLD            1
     C                   PARM                    @@SRT1            2
     C                   PARM                    @@SRT2            1
     C                   PARM                    @@TYPE            1
     C                   PARM                    @@MODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C*   Initialize
     C*****************************************************************
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           MEETPR            1            PRINTED
     C*
     C*  AGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*   Print Routing Page
     C*
     C   19              EXSR      SRRTE
     C*
     C*   What File to Read
     C*
     C                   EXSR      RDCOND
     C*
     C*  Read 1st record
     C*
     C  N16              EXSR      READ1
     C   16              EXSR      READ2
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        18
     C*
     C                   ELSE
     C*
     C     SMKY          CHAIN     PSM01                              95
     C*
     C     FNKY          CHAIN     PFN01                              95
     C*
     C  N95              MOVEL(P)  FNNAME        XXFNNM           40
     C   95              MOVEL     *BLANKS       XXFNNM
     C*
     C     EMKY          CHAIN     PEM01                              95
     C*
     C*  Initialize break criteria
     C*
     C                   EXSR      IZHEAD
     C                   EXSR      IZL2BK
     C                   EXSR      IZL1BK
     C*
     C                   SETON                                        22
     C*
     C   19              EXCEPT    HEAD
     C   19              EXCEPT    HEAD2
     C*
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C*  If subset criteria met
     C*
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
     C*
     C     MEETSS        IFEQ      *ON
     C*
     C*  Test for break in report
     C*
     C                   MOVE      *ON           MEETL1            1
     C                   EXSR      L1TST
     C                   MOVE      *ON           MEETL2            1
     C                   EXSR      L2TST
     C*
     C                   EXSR      HDBK
     C*
     C*  L2 Break
     C*
     C     MEETL2        IFEQ      *OFF
     C*
     C     L2CNT         IFNE      *ZERO
     C*
     C                   SETON                                        22
     C                   EXSR      BREAK
     C                   EXSR      L2BK
     C                   SETOFF                                       18
     C*
     C                   Z-ADD     *ZEROS        TTUNBD            6 2
     C                   Z-ADD     *ZEROS        TTBLLD            6 2
     C                   Z-ADD     *ZEROS        TTINVD            9 2
     C*
     C   19              EXCEPT    HEAD
     C   19              EXCEPT    HEAD2
     C*
     C                   ENDIF                                                  L2CNT
     C*
     C                   EXSR      IZL2BK
     C                   Z-ADD     *ZEROS        L2CNT
     C*
     C                   ENDIF                                                  MEETL1
     C*
     C                   EXSR      IZHEAD
     C*
     C*  Write detail record
     C*
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
     C                   EXSR      IZL1BK
     C*
     C*  Accumulate Totals
     C*
     C                   ADD       1             L2CNT             5 0
     C                   ADD       1             LRCNT             5 0
     C                   EXSR      ACCTOT
     C*
     C                   ENDIF                                                  MEETSS
     C*
     C                   SETOFF                                       22
     C                   EXSR      IZL1BK
     C*
     C*  Read next record
     C*
     C  N16              EXSR      READN1
     C   16              EXSR      READN2
     C*
     C                   ENDDO                                                  WRK RECORD
     C*
     C*  Last Record processing
     C*
     C     MEETPR        IFEQ      *ON
     C     LRCNT         OREQ      *ZERO
     C*
     C                   SETON                                        OA98
     C   19              EXCEPT    HEAD
     C   19              EXCEPT    NOREC
     C*
     C                   ELSE
     C*
     C     L2CNT         IFNE      *ZERO
     C*
     C                   EXSR      L2BK
     C*
     C                   ENDIF
     C*
     C     L1CNT         IFNE      *ZERO
     C*
     C                   EXSR      L1BK
     C*
     C                   ENDIF
     C*
     C*                    EXCPTHEAD
     C                   EXSR      LRBK
     C*
     C                   ENDIF                                                  MEETPR
     C*
     C*  End of Processing
     C*
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
         kkPrt = '0';
         Chain orKy POR01;
         If %found and (@@TYPE = 'B' or @@TYPE = 'R');
            Except Route;
         Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C* ACCTOT - Accumulate Totals
     C*****************************************************************
     CSR   ACCTOT        BEGSR
     C*
     C                   ENDSR                                                  ACCTOT
     C*****************************************************************
     C* HDBK  - SEE IF HEADER SHOULD APPEAR
     C*****************************************************************
     CSR   HDBK          BEGSR
     C*
     C*  SETON 18 IF IT SHOULD APPEAR
     C*
     C     R6SUB#        IFNE      SVSUB#
     C     R6FND         ORNE      SVFND
     C     R6EMP#        ORNE      SVEMP#
     C     R6DIV         ORNE      SVDIV
     C*
     C                   SETOFF                                       18
     C*
     C                   ELSE
     C*
     C                   SETON                                        18
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  L2BK
     C*****************************************************************
     C* IZHEAD - Initialize Level FOR HEADER INFORMATION
     C*****************************************************************
     CSR   IZHEAD        BEGSR
     C*
     C*  CLAIM NURSE BREAK
     C*
     C                   Z-ADD     R6SUB#        SVSUB#
     C                   Z-ADD     R6FND         SVFND
     C                   Z-ADD     R6EMP#        SVEMP#
     C                   Z-ADD     R6DIV         SVDIV
     C*
     C                   ENDSR                                                  IZL1BK
     C*****************************************************************
     C* IZL1BK - Initialize Level 1 break criteria
     C*****************************************************************
     CSR   IZL1BK        BEGSR
     C*
     C*  CLAIM NURSE BREAK
     C*
     C                   Z-ADD     R6FYR         SVFYR
     C                   Z-ADD     R6CASE        SVCASE
     C*
     C                   ENDSR                                                  IZL1BK
     C*****************************************************************
     C* IZL2BK - Initialize Level 2 break criteria
     C*****************************************************************
     CSR   IZL2BK        BEGSR
     C*
     C*  CASE BREAK
     C*
     C                   MOVEL     R6STAF        SVSTAF
     C*
     C                   ENDSR                                                  IZL2BK
     C*****************************************************************
     C* L1BK  - First Level Break action
     C*****************************************************************
     CSR   L1BK          BEGSR
     C*
     C*  Output L1 totals
     C*
     C*  PAGE BREAK AFTER
     C*
     C*  19                EXCPTL1TOT
     C*
     C*
     C*                    SETON                     OA
     C*
     C                   ENDSR                                                  L1BK
     C*****************************************************************
     C* L2BK  - Second Level Break Action
     C*****************************************************************
     CSR   L2BK          BEGSR
     C*
     C*  Output L2 totals
     C*
     C*  PAGE BREAK AFTER
     C*
     C   19              EXCEPT    L2TOT
     C*
     C*                    SETON                     OA
     C*
     C                   ENDSR                                                  L2BK
     C*****************************************************************
     C* L1TST - First Level Break TEST criteria
     C*****************************************************************
     CSR   L1TST         BEGSR
     C*
     C*  MEETL1 = *OFF if Level 1 Break processing to proceed
     C*
     C*
     C*  CLAIM NURSE CHANGE
     C*
     C     R6FYR         IFNE      SVFYR
     C     R6CASE        ORNE      SVCASE
     C*
     C                   SETON                                        22
     C*
     C*                    ELSE
     C*
     C*                    SETON                     22
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  L1TST
     C*****************************************************************
     C* L2TST - Second Level Break TEST criteria
     C*****************************************************************
     CSR   L2TST         BEGSR
     C*
     C*  MEETL2 = *OFF if Level 2 Break processing to proceed
     C*
     C*  CASE CHANGE
     C*
     C     R6STAF        IFNE      SVSTAF
     C*
     C                   MOVE      *OFF          MEETL2
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  L2TST
     C*****************************************************************
     C* LRBK - Last Record Processing
     C*****************************************************************
     CSR   LRBK          BEGSR
     C*
     C                   SETON                                        98
     C   19              EXCEPT    LRTOT
     C*
     C                   ENDSR                                                  LRBK
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C     @@SUB#        COMP      *ZEROS                                 31
     C     @@FND         COMP      *ZEROS                                 32
     C     @@EMP#        COMP      *ZEROS                                 33
     C     @@STAF        COMP      *BLANKS                                34
     C*
     C                   Z-ADD     *ZEROS        GTUNBD            6 2
     C                   Z-ADD     *ZEROS        GTBLLD            6 2
     C                   Z-ADD     *ZEROS        GTINVD            9 2
     C*
     C     @@MODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C*
     C                   ELSE
     C*
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C                   SELECT
     C*
     C     @@TYPE        WHENEQ    'B'
     C*
     C                   SETON                                        1920
     C*
     C     @@TYPE        WHENEQ    'R'
     C*
     C                   SETON                                        19
     C*
     C     @@TYPE        WHENEQ    'F'
     C*
     C                   SETON                                        20
     C*
     C                   ENDSL
     C*
     C     @@STAF        IFEQ      *BLANKS
     C*
     C                   SETON                                        21
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       21
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* RDCOND - CONDITION OF WHICH LOGICAL TO READ
     C*****************************************************************
     CSR   RDCOND        BEGSR
     C*
     C     @@SRT2        IFEQ      'A'
     C*
     C                   SETOFF                                       16
     C*
     C                   ELSE
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  RDCOND
     C*****************************************************************
     C* READ1 - Read first record from file
     C*****************************************************************
     CSR   READ1         BEGSR
     C*
     C     *LOVAL        SETLL     PR601
     C                   READ      PR601                                  99
     C*
     C                   ENDSR                                                  READ1
     C*****************************************************************
     C* READ2 - Read first record from file
     C*****************************************************************
     CSR   READ2         BEGSR
     C*
     C     *LOVAL        SETLL     PR602
     C                   READ      PR602                                  99
     C*
     C                   ENDSR                                                  READ2
     C*****************************************************************
     C* READN1 - Read next record from file
     C*****************************************************************
     CSR   READN1        BEGSR
     C*
     C                   READ      PR601                                  99
     C*
     C                   ENDSR                                                  READN1
     C*****************************************************************
     C* READN2 - Read next record from file
     C*****************************************************************
     CSR   READN2        BEGSR
     C*
     C                   READ      PR602                                  99
     C*
     C                   ENDSR                                                  READN2
     C*****************************************************************
     C* SRDET - Write Detail Record
     C*****************************************************************
     CSR   SRDET         BEGSR
     C*
     C* CLAIMANT NAME
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     CMLSN         CAT(P)    ',':0         XXCLNM           24
     C                   CAT       CMFSN:1       XXCLNM
     C*
     C* OPEN DATE
     C*
     C                   CLEAR                   PMS01
     C*
     C     MSKY          CHAIN     PMS01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       1             L1CNT             5 0
     C                   MOVEL(P)  MSMSCD        KKKEY
     C*
     C                   Z-ADD     MSBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBGDT            8 0
     C*
     C     MCKY          CHAIN     PMC01                              99
     C*
     C                   MOVEL(P)  MCDESC        XXDESC           17
     C*
     C                   Z-ADD     *ZEROS        XXUNBD            6 2
     C                   Z-ADD     *ZEROS        XXBLLD            6 2
     C                   Z-ADD     *ZEROS        XXINVD            9 2
     C                   Z-ADD     *ZEROS        XXBIDT
     C*
     C     TMKY          CHAIN     PTM01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     TMMSCD        IFEQ      MSMSCD
     C*
     C     TMINV#        IFEQ      *ZEROS
     C*
     C                   ADD       TMHRS         XXUNBD
     C*
     C                   ELSE
     C*
     C     TMHRS         MULT      TMHRRT        XXHOUR            9 2
     C     TMMILE        MULT      TMMIRT        XXMILE            6 2
     C     XXHOUR        ADD       XXMILE        X1INVD            9 2
     C                   ADD       TMMSCG        X1INVD
     C*
     C                   ADD       X1INVD        XXINVD
     C*
     C                   ADD       TMHRS         XXBLLD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     TMKY          READE     PTM01                                  99
     C*
     C                   ENDDO
     C*
     C                   ADD       XXUNBD        TTUNBD            6 2
     C                   ADD       XXBLLD        TTBLLD            6 2
     C                   ADD       XXINVD        TTINVD            9 2
     C*
     C*  Detail record print
     C*
     C     *INOA         IFEQ      *ON
     C*
     C   19              EXCEPT    HEAD
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C   19              EXCEPT    DETAIL
     C   20              EXSR      WRITER
     C*
     C                   ADD       XXUNBD        GTUNBD
     C                   ADD       XXBLLD        GTBLLD
     C                   ADD       XXINVD        GTINVD
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRDET
     C*****************************************************************
     C* SRRTE - Print routing page
     C*****************************************************************
     CSR   SRRTE         BEGSR
     C*
     C*   GET COMPANY, USER, INFO.
     C*
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
     C*
     C*   Output Routing - detail report 0
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
     C*
     C*  User Name if no Distribution specified in routing
     C*
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********19              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  XTITL         ORTITL
     C*
     C                   ENDIF
     C*
     C*  Center Title in Field
     C*
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
     C*
     C* Center Company name in display field
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C                   ENDSR                                                  SRRTE
     C*****************************************************************
     C* SUBSET - Check if member of records processed subset
     C*****************************************************************
     CSR   SUBSET        BEGSR
     C*
     C*  MEETSS = *OFF, if not to be processed
     C*
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* BREAK - CHECK IF AT END OF PAGE AND BREAK IF SO
     C*****************************************************************
     CSR   BREAK         BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  BREAK
     C*****************************************************************
     C* WRITER - WRITE FXR2P
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PR401
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   Z-ADD     CMCO#         R4CO#
     C                   MOVEL(P)  CONAME        R4CONM
     C                   Z-ADD     CMSUB#        R4SUB#
     C                   MOVEL(P)  SMNAME        R4SBNM
     C                   Z-ADD     CMFND         R4FND
     C                   MOVEL(P)  FNNAME        R4FNNM
     C                   Z-ADD     CMEMP#        R4EMP#
     C                   Z-ADD     CMDIV         R4DIV
     C                   MOVEL(P)  EMNAME        R4EMNM
     C                   Z-ADD     CMFYR         R4FYR
     C                   Z-ADD     CMCASE        R4CASE
     C*
     C     CMFSN         IFNE      *BLANKS
     C*
     C     CMLSN         CAT(P)    ',':0         R4CLNM
     C                   CAT(P)    CMFSN:1       R4CLNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CMLSN         R4CLNM
     C*
     C                   ENDIF
     C*
     C                   MOVEL     CMSS#         R4SS#
     C                   Z-ADD     CMACDT        R4ACDT
     C*
     C                   MOVEL(P)  MSSTAF        R4STAF
     C                   MOVEL(P)  MSSTAF        KKSTAF
     C*
     C     UPKY1         CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        R4STNM
     C   99              MOVEL(P)  *BLANKS       R4STNM
     C*
     C                   MOVEL(P)  CMCMNU        R4CMNU
     C                   MOVEL(P)  CMCMNU        KKSTAF
     C*
     C     UPKY1         CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        R4NUNM
     C   99              MOVEL(P)  *BLANKS       R4NUNM
     C*
     C                   MOVEL(P)  CMCMSU        R4CMSU
     C                   MOVEL(P)  CMCMSU        KKSTAF
     C*
     C     UPKY1         CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        R4SUNM
     C   99              MOVEL(P)  *BLANKS       R4SUNM
     C*
     C*
     C                   MOVEL(P)  MSMSCD        R4SVCD
     C                   Z-ADD     MSSEQ#        R4SEQ#
     C                   MOVEL(P)  MCDESC        R4SVDS
     C                   Z-ADD     MSBGDT        R4BGDT
     C                   Z-ADD     XXUNBD        R4UTIM
     C                   Z-ADD     XXBLLD        R4BTIM
     C                   Z-ADD     XXINVD        R4TAMT
     C*
     C                   MOVEL(P)  'A'           R4STAT
     C                   MOVEL(P)  Q1USER        R4USER
     C                   MOVEL(P)  Q1PGM         R4PGM
     C                   MOVEL(P)  Q1USER        R4IUSR
     C                   MOVEL(P)  Q1PGM         R4IPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        R4TIME
     C                   Z-ADD     DSTIME        R4ITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R4DT
     C                   MOVEL     SRDTO         R4IDT
     C*
     C                   WRITE     PR401
     C*
     C                   FEOD      FXR4P
     C*
     C                   ENDSR                                                  WRITER
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS2              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS3              56
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS4              56
     O**********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS2              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS3              56
     O**********
     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE          2
     O                                           27 'COMPANY  . . . . :'
     O                       @@CO#         Z   +001
     O          E            ROUTE          2
     O                                           27 'SUBSCRIBER . . . :'
     O              N31      @@SUB#        Z   +001
     O               31                          32 '*ALL'
     O          E            ROUTE          2
     O                                           27 'GROUP  . . . . . :'
     O              N32      @@FND         Z   +001
     O               32                          32 '*ALL'
     O*
     O          E            ROUTE          2
     O                                           27 'EMPLOYER/DIVISION:'
     O              N33      @@EMP#            +001 '       0 '
     O              N33                        +001 '-'
     O              N33      @@DIV             +001 '   0 '
     O               33                          32 '*ALL'
     O          E            ROUTE          2
     O                                           27 'STAFF CODE . . . :'
     O              N34      @@STAF            +001
     O               34                          32 '*ALL'
     O          E            ROUTE          2
     O                                           27 'CASE LOAD  . . . :'
     O                       @@CSLD            +001
     O          E            ROUTE          2
     O                                           27 'PRIMARY SORT . . :'
     O                       @@SRT1            +001
     O          E            ROUTE          2
     O                                           27 'SECONDARY SORT   :'
     O                       @@SRT2            +001
     O          E            ROUTE          2
     O                                           27 'TYPE OF OUTPUT   :'
     O                       @@TYPE            +001
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                25 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              80
     O                       DSDATE        Y    132
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            HEAD           2
     O                                           17 'Report Selection:'
     O                                          022 'Sub:'
     O              N31      @@SUB#        4    028
     O               31                         028 '*ALL'
     O                                          035 'Group:'
     O              N32      @@FND         4    040
     O               32                         040 '*ALL'
     O                                          045 'Emp:'
     O              N33      @@EMP#        4    055
     O               33                         055 '*ALL'
     O                                          056 '-'
     O              N33      @@DIV              061 '   0 '
     O               33                         061 '*ALL'
     O                                          073 'Staff Code:'
     O              N34      @@STAF             078
     O               34                         078 '*ALL'
     O                                         +001 'Case Load:'
     O                       @@CSLD            +001
     O                                         +001 'Sort 1:'
     O                       @@SRT1            +001
     O                                         +001 'Sort 2:'
     O                       @@SRT2            +001
     O                                         +001 'Type of Output:'
     O                       @@TYPE            +001
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Subscriber  :'
     O                       R6SUB#            +001 '   0 '
     O                       SMNAME              73
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Group       :'
     O                       R6FND             +003 ' 0 '
     O                       XXFNNM              73
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Employer    :'
     O                       R6EMP#            +001 '       0 '
     O                                         +000 ' '
     O                       R6DIV             +000 '   0 '
     O                       EMNAME              73
     O*
     O          E   N18      HEAD2          2
     O                                           13 'Staff . . . :'
     O                       R6STAF            +001
     O*
     O          E   N18      HEAD2          1
     O                                           39 'CM'
     O                                         +030 'Begin'
     O                                         +007 'Unbilled'
     O                                         +003 'Billed'
     O                                         +006 'Total'
     O*
     O          E   N18      HEAD2          1
     O                                           05 'Case#'
     O                                         +007 'Patient Name'
     O                                         +013 'Nrs'
     O                                         +002 'Supv'
     O                                         +001 'Open'
     O                                         +001 'Services'
     O                                         +009 'Date'
     O                                         +010 'Time'
     O                                         +006 'Time'
     O                                         +005 'Invoiced'
     O*
     O          E   N22      DETAIL         1
     O               22      CMFYR               03
     O               22                        +000 '-'
     O               22      CMCASE            +000
     O               22      XXCLNM            +001
     O               22      CMCMNU            +001
     O               22      CMCMSU            +002
     O                       MSMSCD            +002
     O                       XXDESC            +001
     O                       XXBGDT            +001 '  /  /    '
     O                       XXUNBD            +002 '  0 .  '
     O                       XXBLLD            +002 '  0 .  '
     O                       XXINVD            +002 '     0 .  '
     O*
     O          E    22      DETAIL      1  1
     O                       CMFYR               03
     O                                         +000 '-'
     O                       CMCASE            +000
     O                       XXCLNM            +001
     O                       CMCMNU            +001
     O                       CMCMSU            +002
     O                       MSMSCD            +002
     O                       XXDESC            +001
     O                       XXBGDT            +001 '  /  /    '
     O                       XXUNBD            +002 '  0 .  '
     O                       XXBLLD            +002 '  0 .  '
     O                       XXINVD            +002 '     0 .  '
     O*
     O          E            L2TOT       1  2
     O                                           55 'Staff Totals:'
     O                                         +002 'Open Services:'
     O                       L2CNT         4   +002
     O                       TTUNBD            +004 '  0 .  '
     O                       TTBLLD            +002 '  0 .  '
     O                       TTINVD            +002 '     0 .  '
     O*
     O          E            LRTOT          2
     O                                           55 'Report Totals:'
     O                                         +002 'Open Services:'
     O                       L1CNT         4   +002
     O                       GTUNBD            +004 '  0 .  '
     O                       GTBLLD            +002 '  0 .  '
     O                       GTINVD            +002 '     0 .  '
     O*
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
