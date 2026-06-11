     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/15/20 **************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/15/20 **************************************
     H*****************************************************************
     H/TITLE F9906R - CASE MANAGEMENT SERVICES REPORT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     LISTS CASE MANAGEMENT STATUS INFORMATION BASED UPON       *
     H*     VARIOUS SELECTION CRITERIA.                               *
     H*                                                               *
     H*  CALLED FROM: F9906C-OVERRIDE - CASE MANAGEMENT STATUS REPORT *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*  CREATED: 09/28/99        PROGRAMMER: 90       PROJECT: MC174 *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/15/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 08/05/13  C:277  Project: MC329 Phase 2                       *
     H*                  Get Description from ICD10 if ICD10 is active*
     H*                                                               *
     H* 01/28/10  C:249     Project: SY215                            *
     H*           C:189     Retrieve brand Name                       *
     H*                                                               *
     H* 11/07/07   C:180    SUPPORT 60228 - Fix printing of case info.
     H*                     Same info for a case should not repeat
     H*                     multiple times. Instead, info for same case
     H*                     should be different each time printed.
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
     H*  95       Read main File                                      *
     H*  98       Last record processing                              *
     H*  97       Print Group Information                             *
     H*  20       TO FILE                                             *
     H*  19       TO REPORT                                           *
     H*  18       IF THERE IS NO HITS                                 *
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
     F* WORKFILE - ALPHA
     FFXR8L2    IF   E           K DISK
     F*
     F* WORKFILE - NUMERIC
     FFXR8L1    IF   E           K DISK
     F                                     RENAME(PR801:PR802)
     F* CASE MASTER
     FFMCML1    IF   E           K DISK
     F* CASE DEMOGRAPHICS
     FFMCDL     IF   E           K DISK
     F* FEE SCHEDULE DIAGNOSIS CODE FILE
     FFMDIL     IF   E           K DISK
     F* SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* GROUP FILE
     FFMFNL     IF   E           K DISK
     F* EMPLOYER FILE
     FFMEML     IF   E           K DISK
     F* CASE HISTORY REPORT FILE
     FFXR1P     O    E             DISK
     F* MEDICAL SERVICE CODE FILE
     FFTMCL     IF   E           K DISK
     F* MEDICAL SERVICE TRANSACTION FILE
     FFTCLL     IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*** Beg Add *** 08/05/13 ****************************************
     Ffmicd10l1 if   e           k disk
     Ffamsl     if   e           k disk
     F*** End Add *** 08/05/13 ****************************************
      ***** Beg Add *** 06/15/20 *************************************
      /copy sPrMask
      ***** End Add *** 06/15/20 *************************************

     D CD              S              4    DIM(99) ASCEND
     D CS              S              5  0 DIM(99)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  XXSS#                  1      9
     D  XXS1                   1      3
     D  XXS2                   4      5
     D  XXS3                   5      9
     D*
     D                 DS
     D  X1SS#                  1     11
     D  X1S1                   1      3
     D  HYP1                   4      4    INZ('-')
     D  X1S2                   5      6
     D  HYP2                   7      7    INZ('-')
     D  X1S3                   8     11
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D XTITL           C                   CONST('Case Management Serv-
     D                                     ices Report')
     D*
     D*
     D*****************************************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Beg Add *** 08/05/13 ****************************************
     D kkicd10         s                   like(iccode)
     D*** End Add *** 08/05/13 ****************************************

     C*****************************************************************
     C*   KEY LISTS
     C*****************************************************************
     C*
     C*   Company
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Case History
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  Case Master
     C*
     C     CMKY          KLIST
     C                   KFLD                    R8CO#
     C                   KFLD                    R8SUB#
     C                   KFLD                    R8FND
     C                   KFLD                    R8FYR
     C                   KFLD                    R8CASE
     C*
     C*  ICD9 TABLE FILE
     C*
      *
     C     DIKY          KLIST
     C                   KFLD                    R8ICD9
     C                   KFLD                    R8LOST
     C*
     C*  Employer Master
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     CLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKKEY
     C*
     C*   Subscriber
     C*
     C     SMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C*
     C*   Fund
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C*
     C*   Output Routing
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C*   User Parameter
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    CMCMNU
     C*
     C*  Service Code Table File
     C*
     C     MCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKKEY
     C*
     C*  MEDICAL SERVICES TRANSACTION FILE
     C*
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE 10 MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    R8lost
     C*
     C*  Claim Med Svc Trans File
     C*
     C     famslkey      KLIST
     C                   KFLD                    R8CO#
     C                   KFLD                    R8SUB#
     C                   KFLD                    R8FND
     C                   KFLD                    R8FYR
     C                   KFLD                    R8CASE
     C*** End Add *** 08/05/13 ***************************************
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    CMSUB#        SVSUB#
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMACDT        XXDATE
     C     *LIKE         DEFINE    CLKEY         KKKEY
     C     *LIKE         DEFINE    R8SEQ#        XXSEQ#
     C     *LIKE         DEFINE    EMNAME        XXEMNM
     C     *LIKE         DEFINE    SMNAME        XXSBNM
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@SVCD            4
     C                   PARM                    @@STAT            1
     C                   PARM                    @@FRDT            7
     C                   PARM                    @@TODT            7
     C                   PARM                    @@SORT            1
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
     C*  PGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*  Print Routing Page
     C*
     C   19              EXSR      SRRTE
     C*
     C*  Read 1st record
     C*
     C                   EXSR      READ1
     C*
     C     *IN95         IFEQ      *ON
     C*
     C                   SETON                                        18
     C*
     C                   ELSE
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C   99              MOVEL(P)  *BLANKS       XXSBNM
     C  N99              MOVEL(P)  SMNAME        XXSBNM
     C*
     C*  Initialize break criteria
     C*
     C                   EXSR      IZL1BK
     C                   EXSR      IZL2BK
     C                   EXSR      IZL3BK
     C*
     C                   MOVE      *ON           *INOA
     C*
     C                   ENDIF
     C*
     C     *IN95         DOWEQ     '0'
     C*
     C*  Test for break in report
     C*
     C                   MOVE      *ON           MEETL1            1
     C                   MOVE      *ON           MEETL2            1
     C                   MOVE      *ON           MEETL3            1
     C                   EXSR      L1TST
     C                   EXSR      L2TST
     C                   EXSR      L3TST
     C*
     C*  L1 Break
     C*
     C     MEETL1        IFEQ      *OFF
     C     MEETL2        OREQ      *OFF
     C     MEETL3        OREQ      *OFF
     C     L1CNT         IFNE      *ZERO
     C                   EXSR      L1BK
     C                   ENDIF                                                  L1CNT
     C                   EXSR      IZL1BK
     C                   ENDIF                                                  MEETL1
     C*
     C*  L2 Break
     C*
     C     MEETL2        IFEQ      *OFF
     C     MEETL3        OREQ      *OFF
     C     L2CNT         IFNE      *ZERO
     C                   EXSR      L2BK
     C                   ENDIF                                                  L2CNT
     C                   EXSR      IZL2BK
     C                   ENDIF                                                  MEETL1
     C*
     C*  L3 Break
     C*
     C     MEETL3        IFEQ      *OFF
     C     L3CNT         IFNE      *ZERO
     C                   EXSR      L3BK
     C                   ENDIF                                                  L2CNT
     C                   EXSR      IZL3BK
     C                   ENDIF                                                  MEETL1
     C*
     C*  Write detail record
     C*
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
     C*
     C*  Accumulate Totals
     C*
     C                   ADD       1             L1CNT             9 0
     C                   ADD       1             L2CNT             9 0
     C                   ADD       1             L3CNT             9 0
     C                   ADD       1             LRCNT             9 0
      *
     C                   Z-ADD     1             X                 2 0
     C     XXSVCD        LOOKUP    CD(X)                                  99
     C     *IN99         IFEQ      *ON
     C                   EXSR      ACCTOT
     C                   ELSE
     C     *BLANKS       LOOKUP    CD(X)                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVE      XXSVCD        CD(X)
     C                   EXSR      ACCTOT
     C                   ENDIF                                                  *BLANKS
     C                   ENDIF                                                  *XXSVCD
     C*
     C                   ADD       CS(X)         TOTCAS            6 0
     C*
     C*  Read next record
     C*
     C                   EXSR      RNXT1
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
     C     L1CNT         IFNE      *ZERO
     C*
     C                   EXSR      L1BK
     C*
     C                   ENDIF
     C*
     C     L2CNT         IFNE      *ZERO
     C*
     C                   EXSR      L2BK
     C*
     C                   ENDIF
     C*
     C     L3CNT         IFNE      *ZERO
     C*
     C                   EXSR      L3BK
     C*
     C                   ENDIF
     C*
     C                   EXSR      LRBK
     C*
     C                   ENDIF                                                  MEETPR
     C*
     C*  End of Processing
     C*
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkprt = '0';
       Chain orky POR01;
       If %found and (@@type = 'B'or @@type = 'R');
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
     C*
     C*  Accumulate L1 Totals - SERVICE CODE
     C*
     C                   ADD       1             CS(X)
     C                   ADD       1             CSTOT             5 0
     C                   ADD       XXWKSO        TOTWKS            7 0
     C*
     C                   ENDSR                                                  ACCTOT
     C*****************************************************************
     C* IZL1BK - Initialize Level 1 break criteria
     C*****************************************************************
     CSR   IZL1BK        BEGSR
     C*
     C                   MOVEL(P)  R8MSCD        KKKEY
     C                   MOVEL(P)  R8MSCD        SVSVCD            4
     C     MCKY          CHAIN     PMC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  MCKEY         XXSVCD            4
     C                   MOVEL(P)  MCDESC        XXSVDS           40
     C                   ELSE
     C                   MOVEL(P)  R8MSCD        XXSVCD
     C                   MOVEL(P)  *BLANKS       XXSVDS
     C                   ENDIF                                                  *IN60 = OFF
     C*
     C                   ENDSR                                                  IZL1BK
     C*****************************************************************
     C* IZL2BK - Initialize Level 2 break criteria
     C*****************************************************************
     CSR   IZL2BK        BEGSR
     C*
     C*  EMPLOYER/DIV
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C   99              MOVEL(P)  *BLANKS       XXEMNM
     C  N99              MOVEL(P)  EMNAME        XXEMNM
     C*
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         SVDIV
     C*
     C                   ENDSR                                                  IZL2BK
     C*****************************************************************
     C* IZL3BK - Initialize Level 3 break criteria
     C*****************************************************************
     CSR   IZL3BK        BEGSR
     C*
     C*  Group break
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SMNAME        XXSBNM
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       XXSBNM
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXFNNM
     C                   ENDIF
     C*
     C                   Z-ADD     CMSUB#        SVSUB#
     C                   Z-ADD     CMFND         SVFND
     C*
     C                   ENDSR                                                  IZL3BK
     C*****************************************************************
     C* L1BK  - First Level Break action
     C*****************************************************************
     CSR   L1BK          BEGSR
     C*
     C*  PAGE BREAK AFTER
     C*
     C                   SETON                                        OA
     C*
     C                   ENDSR                                                  L1BK
     C*****************************************************************
     C* L2BK  - Second Level Break Action
     C*****************************************************************
     CSR   L2BK          BEGSR
     C*
     C*  Output L2 totals
     C*
     C                   Z-ADD     *ZEROS        L2CNT
     C*
     C*  PAGE BREAK AFTER
     C*
     C                   SETON                                        OA
     C*
     C                   ENDSR                                                  L2BK
     C*****************************************************************
     C* L3BK  - Second Level Break Action
     C*****************************************************************
     CSR   L3BK          BEGSR
     C*
     C*  Output L3 totals
     C*
     C                   Z-ADD     *ZEROS        L3CNT
     C*
     C*  PAGE BREAK AFTER
     C*
     C                   SETON                                        OA
     C*
     C                   ENDSR                                                  L3BK
     C*****************************************************************
     C* L1TST - First Level Break TEST criteria
     C*****************************************************************
     CSR   L1TST         BEGSR
     C*
     C*  MEETL1 = *OFF if Level 1 Break processing to proceed
     C*
     C*  SERVICE CODE CHANGE
     C*
     C     R8MSCD        IFNE      SVSVCD
     C                   MOVE      *OFF          MEETL1
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
     C*  EMPLOYER CHANGE
     C*
     C     CMEMP#        IFNE      SVEMP#
     C     CMDIV         ORNE      SVDIV
     C                   MOVE      *OFF          MEETL2
     C                   ENDIF
     C*
     C                   ENDSR                                                  L2TST
     C*****************************************************************
     C* L3TST - Third Level Break TEST criteria
     C*****************************************************************
     CSR   L3TST         BEGSR
     C*
     C*  MEETL3 = *OFF if Level 3 Break processing to proceed
     C*
     C*
     C*  Transaction Date change
     C*
     C     CMSUB#        IFNE      SVSUB#
     C     CMFND         ORNE      SVFND
     C                   MOVE      *OFF          MEETL3
     C                   ENDIF
     C*
     C                   ENDSR                                                  L3TST
     C*****************************************************************
     C* LRBK - Last Record Processing
     C*****************************************************************
     CSR   LRBK          BEGSR
     C*
     C* CALCULATE REPORT AVERAGES
     C*
     C                   SETON                                        98
      *
     C     CSTOT         IFNE      *ZEROS
     C     TOTWKS        DIV       CSTOT         AVGWKS            7 0
     C                   ENDIF
      *
     C   19              EXCEPT    LRTOTH
     C                   Z-ADD     1             X                 2 0
     C     CD(X)         DOUEQ     *BLANKS
     C                   MOVE      *BLANKS       XXSVDS
     C                   MOVEL     CD(X)         KKKEY
     C     MCKY          CHAIN     PMC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     MCDESC        XXSVDS
     C                   ENDIF
      *
     C     CSTOT         IFNE      *ZEROS
     C     CS(X)         DIV       CSTOT         CSPCNT            5 4
     C                   ENDIF
      *
     C     CSPCNT        MULT      100           XXPCNT            5 2
     C   19              EXCEPT    LRTOT
     C                   ADD       1             X
     C                   ENDDO
     C   19              EXCEPT    LRTOTT
      *
     C*
     C                   ENDSR                                                  LRBK
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C     @@MODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      @@FRDT        YYFRDT            7 0
     C                   MOVE      @@TODT        YYTODT            7 0
     C*
     C                   Z-ADD     YYFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     YYTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVEL(P)  @@SVCD        XXSVCD
     C*
     C                   Z-ADD     1             X
     C*
     C                   SELECT
     C     @@TYPE        WHENEQ    'B'
     C                   SETON                                        1920
     C     @@TYPE        WHENEQ    'R'
     C                   SETON                                        19
     C     @@TYPE        WHENEQ    'F'
     C                   SETON                                        20
     C                   ENDSL
     C*
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   MOVE      @@SUB#        XXSUB#            5 0
     C                   MOVE      @@FND         XXFND             3 0
     C                   MOVE      @@EMP#        XXEMP#            9 0
     C                   MOVE      @@DIV         XXDIV             5 0
      *
     C*
     C     XXSUB#        IFEQ      *ZEROS
     C                   SETON                                        21
     C                   ENDIF
     C*
     C     XXFND         IFEQ      *ZEROS
     C                   SETON                                        22
     C                   ENDIF
     C*
     C     XXEMP#        IFEQ      *ZEROS
     C                   SETON                                        23
     C                   ENDIF
     C*
     C     @@SVCD        IFEQ      *BLANK
     C                   SETON                                        24
     C                   ENDIF
     C*
     C     @@STAT        IFEQ      *BLANK
     C                   SETON                                        25
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* READ1 - Read first record from file
     C*****************************************************************
     CSR   READ1         BEGSR
     C*
     C     @@SORT        IFEQ      'A'
     C     *LOVAL        SETLL     PR801
     C                   READ      PR801                                  95
     C                   ELSE
     C     *LOVAL        SETLL     PR802
     C                   READ      PR802                                  95
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   ENDSR                                                  READ2
     C*****************************************************************
     C* RNXT1  - Read next record from file
     C*****************************************************************
     CSR   RNXT1         BEGSR
     C*
     C     @@SORT        IFEQ      'A'
     C                   READ      PR801                                  95
     C                   ELSE
     C                   READ      PR802                                  95
     C                   ENDIF
     C*
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   ENDSR                                                  RNXT2
     C*****************************************************************
     C* SRDET - Write Detail Record
     C*****************************************************************
     CSR   SRDET         BEGSR
     C*
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* CLIENT NAME
     C*
     C     CDFSN         IFEQ      *BLANKS
     C                   MOVEL     CDLSN         XXCLNM           21
     C                   ELSE
     C     CDLSN         CAT(P)    ',':0         XXCLNM
     C                   CAT       CDFSN:1       XXCLNM
     C                   ENDIF
     C*
     C* SOC. SEC. #
     C*
     C                   MOVEL     CDSS#         XXSS#
     C                   MOVEL     XXS1          X1S1
     C                   MOVEL     XXS2          X1S2
     C                   MOVEL     XXS3          X1S3
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXCLNM
     C                   MOVEL     *BLANKS       X1SS#
     C*
     C                   ENDIF
     C*
     C* ACCIDENT DATE
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C* GET ICD9 DESCRIPTION
     C*
     C*** Beg Add *** 08/05/13 *****************************************
      /Free
       Chain famslkey pms01;
       If MSicdInd = 'Y';
         kkICd10 = MSicd1001;
         Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
               xxDesc = ICdsc1;
            Else;
               xxDesc = *blanks;
            Endif;
         Else;
      /End-Free
     C*** End Add *** 08/05/13 *****************************************
     C     DIKY          CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        XXDESC           20
     C                   ELSE
     C                   MOVEL     *BLANKS       XXDESC
     C                   ENDIF
     C*** Beg Add *** 08/05/13 *****************************************
      /Free
         Endif;
      /End-Free
     C*** End Add *** 08/05/13 *****************************************
     C*
     C* CLAIM NURSE STAFF CODE
     C*
     C                   MOVEL(P)  R8STAF        XXSTAF            3
     C*
     C                   Z-ADD     *ZEROS        XXBGDT
     C                   Z-ADD     *ZEROS        XXENDT
     C*
     C                   Z-ADD     R8BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBGDT            8 0
     C*
     C*
      *
     C     R8ENDT        IFNE      *ZEROS
     C                   Z-ADD     R8ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXENDT            8 0
     C*
     C                   ENDIF
     C*
     C* # OF WKS OP
     C*
     C     R8ENDT        IFNE      *ZEROS
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      XXBGDT        SRFRDC            8 0
     C                   PARM      XXENDT        SRTODC            8 0
     C                   PARM      *ZEROS        SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
     C                   PARM      *BLANKS       SRTERM            1
     C*
     C                   ELSE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      XXBGDT        SRFRDC            8 0
     C                   PARM      DSDATE        SRTODC            8 0
     C                   PARM      *ZEROS        SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
     C                   PARM      *BLANKS       SRTERM            1
     C*
     C                   ENDIF
     C*
     C     SRDAYS        DIV       7             XXWKSO            5 0
     C*
     C*  Detail record print
     C*
     C     *INOA         IFEQ      *ON
     C*
     C   19              EXCEPT    HEAD
     C   19              EXCEPT    HEAD2
     C                   SETOFF                                       OA
     C*
     C                   ENDIF
     C*
      ***** Beg Add *** 06/15/20 **************************************
      /free
       X1SS# = MaskData('SSN':'X1SS#':X1SS#) ;
      /End-free
      ***** End Add *** 06/15/20 **************************************
     C   19              EXCEPT    DETAIL
      ***** Beg Add *** 06/15/20 **************************************
      /free
       X1SS# = UnMaskData('SSN':'X1SS#':X1SS#) ;
      /End-free
      ***** End Add *** 06/15/20 **************************************
     C   20              EXSR      WRITER
     C*
     C                   ENDSR                                                  SRDET
     C*****************************************************************
     C* SRRTE - Print routing page
     C*****************************************************************
     CSR   SRRTE         BEGSR
     C*
     C*   GET COMPANY, USER, INFO.
     C*
     C     COKY          CHAIN     PCO01                              99
     C     UPKY          CHAIN     PUP01                              99
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *
     C*
     C*   Output Routing - detail report 0
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              99
     C*
     C     *IN99         IFEQ      '0'
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
     C* WRITER - WRITE FXR1P
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PR101
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   Z-ADD     CMCO#         R1CO#
     C                   MOVEL(P)  CONAME        R1CONM
     C                   Z-ADD     CMSUB#        R1SUB#
     C                   MOVEL(P)  SMNAME        R1SBNM
     C                   Z-ADD     CMFND         R1FND
     C                   MOVEL(P)  FNNAME        R1FNNM
     C                   Z-ADD     CMEMP#        R1EMP#
     C                   Z-ADD     CMDIV         R1DIV
     C                   MOVEL(P)  EMNAME        R1EMNM
     C                   MOVEL(P)  R8MSCD        R1SVCD
     C                   MOVEL(P)  XXSVDS        R1SVDS
     C                   Z-ADD     CMFYR         R1FYR
     C                   Z-ADD     CMCASE        R1CASE
     C*
     C     CMFSN         IFNE      *BLANKS
     C*
     C     CMLSN         CAT(P)    ',':0         R1CLNM
     C                   CAT(P)    CMFSN:1       R1CLNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CMLSN         R1CLNM
     C*
     C                   ENDIF
     C*
     C                   MOVEL     CDSS#         R1SS#
     C                   Z-ADD     CMACDT        R1ACDT
     C                   MOVEL(P)  R8ICD9        R1ICD9
     C                   MOVEL(P)  R8LOST        R1LOST
     C                   MOVEL(P)  DIDSC1        R1ICDD
     C                   MOVEL(P)  R8STAF        R1STAF
     C*
     C     UPKY1         CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        R1USNM
     C   99              MOVEL(P)  *BLANKS       R1USNM
     C*
     C                   Z-ADD     R8BGDT        R1BGDT
     C                   Z-ADD     XXWKSO        R1#WKS
     C*
     C                   Z-ADD     R8ENDT        R1ENDT
     C                   MOVEL(P)  R8MSST        R1CLCD
     C                   MOVEL(P)  R8MSST        KKKEY
     C*
     C     CLKY          CHAIN     PCL01                              99
     C*
     C  N99              MOVEL(P)  CLDESC        R1CLDS
     C   99              MOVEL     *BLANKS       R1CLDS
     C*
     C                   MOVEL(P)  'A'           R1STAT
     C                   MOVEL(P)  Q1USER        R1USER
     C                   MOVEL(P)  Q1PGM         R1PGM
     C                   MOVEL(P)  Q1USER        R1IUSR
     C                   MOVEL(P)  Q1PGM         R1IPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        R1TIME
     C                   Z-ADD     DSTIME        R1ITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R1DT
     C                   MOVEL     SRDTO         R1IDT
     C*** Beg Add *** 08/05/13 *****************************************
      /Free
       If MSicdInd = 'Y';
          R1ICD10 = MSicd1001;
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 *****************************************
     C*
     C                   WRITE     PR101
     C*
     C                   FEOD      FXR1P
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
     O
     O          E            ROUTE          1
     O                       ORDIS2              56
     O
     O          E            ROUTE          1
     O                       ORDIS3              56
     O
     O          E            ROUTE          1
     O                       ORDIS4              56
     O
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O*
      ***** Beg Del ***** 08/26/14 ************************************
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
     O                                           26 'SUBSCRIBER. . . :'
     O              N21      XXSUB#        Z   +001
     O               21                          31 '*ALL'
     O          E            ROUTE          2
     O                                           26 'GROUP . . . . . :'
     O              N22      XXFND         Z   +001
     O               22                          31 '*ALL'
     O          E            ROUTE          2
     O                                           26 'EMPLOYER / DIV  :'
     O              N23      XXEMP#        Z   +001
     O              N23                        +001 '/'
     O              N23      XXDIV         Z   +001
     O               23                          31 '*ALL'
     O          E            ROUTE          2
     O                                           26 'SERVICE CODE    :'
     O              N24      @@SVCD            +001
     O               24                          31 '*ALL'
     O          E            ROUTE          2
     O                                           26 'SERVICE STATUS  :'
     O              N25      @@STAT            +001
     O               25                          31 '*ALL'
     O          E            ROUTE          2
     O                                           26 'DATE RANGE  . . :'
     O                       XXFRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       XXTODT            +001 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'SORT ORDER  . . :'
     O                       @@SORT            +001
     O          E            ROUTE          2
     O                                           26 'TYPE OF OUTPUT  :'
     O                       @@TYPE            +001
     O*
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              80
     O                       DSDATE        Y    132
     O*
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            HEAD           2
     O                                           52 'Date Range:'
     O                       XXFRDT            +001 '0  /  /    '
     O                                         +001 'to'
     O                       XXTODT            +000 '0  /  /    '
     O*
     O          E            HEAD           2
     O                                           17 'Report Selection:'
     O                                         +001 'Sub:'
     O                       XXSUB#        4   +001
     O                                         +001 'Group:'
     O                       XXFND         4   +001
     O                                         +001 'Emp:'
     O                       XXEMP#        4   +001
     O                                         +000 '-'
     O                       XXDIV             +000 '    0'
     O                                         +001 'Svc Code:'
     O                       @@SVCD            +000
     O                                         +001 'Svc Stat:'
     O                       @@STAT            +001
     O                                         +001 'Sort:'
     O                       @@SORT            +001
     O                                         +001 'Type of Output:'
     O                       @@TYPE            +001
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Subscriber  :'
     O                       CMSUB#            +001 '   0 '
     O                       XXSBNM              73
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Group       :'
     O                       CMFND             +003 ' 0 '
     O                       XXFNNM              73
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Employer    :'
     O                       CMEMP#            +001 '       0 '
     O                                         +000 ' '
     O                       CMDIV             +000 '   0 '
     O                       XXEMNM              73
     O          E   N18      HEAD2          2
     O                                           13 'Service Code:'
     O                       XXSVCD            +003
     O                       XXSVDS              73
     O*
     O          E            HEAD2          1
     O                                           54 'Injury'
     O                                           82 'Staff'
     O                                           90 'Begin'
     O                                          106 '# of'
     O                                          130 '** Closing **'
     O*
     O          E            HEAD2          2
     O                                           05 'Case#'
     O                                           24 'Patient Name'
     O                                           44 'Soc Sec #'
     O                                           53 'Date'
     O                                           67 'Diagnosis'
     O                                           81 'Code'
     O                                           90 'Date'
     O                                          108 'Wks Op'
     O                                          121 'Date'
     O                                          130 'Code'
     O*
     O          E            DETAIL         1
     O                       CMFYR               03
     O                                         +000 '-'
     O                       CMCASE            +000
     O                       XXCLNM            +001
     O                       X1SS#             +001
     O                       XXACDT            +001 '  /  /    '
     O                       XXDESC            +001
     O                       XXSTAF            +001
     O                       XXBGDT            +001 '  /  /    '
     O                       XXWKSO        4    106
     O                       XXENDT             123 '  /  /    '
     O                       R8MSST             131
     O*
     O*
     O          E            LRTOTH      3  1
     O                                           14 'Report Totals:'
     O          E            LRTOTH         1
     O                                           15 'Avg Wks Open:'
     O                       AVGWKS        Z   +002
     O          E            LRTOTH         1
     O                                           06 'Code'
     O                                           20 'Description'
     O                                           61 '# Cases'
     O                                           76 '% of Total'
     O          E            LRTOT          1
     O                       CD(X)               06
     O                       XXSVDS              55
     O                       CS(X)         Z     61
     O                       XXPCNT        4     76
     O                                         +000 '%'
     O          E            LRTOTT         1
     O                                           54 'All Codes'
     O                       CSTOT         Z     61
     O*
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
