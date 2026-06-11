     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H/TITLE F9903R - CASE STATUS REPORT
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     LISTS CASE STATUS INFORMATION BASED UPON VARIOUS SELECTION*
     H*     CRITERIA.                                                 *
     H*                                                               *
     H*  CALLED FROM: F9903C - OVERRIDE - CASE STATUS REPORT          *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*  CREATED: 09/09/99        PROGRAMMER: 62       PROJECT: MC173 *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
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
     FFMCMLH    IF   E           K DISK
     F*
     F* CASE MASTER - NUMERIC
     FFMCMLJ    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F* CASE HISTORY
     FFMCHL2    IF   E           K DISK
     F* CASE DEMOGRAPHICS
     FFMCDL     IF   E           K DISK
     F* EOMB HEADER
     FFAEAL1    IF   E           K DISK
     F* FEE SCHEDULE DIAGNOSIS CODE FILE
     FFADAL2    IF   E           K DISK
     F* FEE SCHEDULE DIAGNOSIS CODE FILE
     FFMDIL     IF   E           K DISK
     F* SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* GROUP FILE
     FFMFNL     IF   E           K DISK
     F* EMPLOYER FILE
     FFMEML     IF   E           K DISK
     F* GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
     F* CASE HISTORY REPORT FILE
     FFXR2P     O    E             DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*** Beg Add *** 08/05/13 ****************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ****************************************

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D XTITL           C                   CONST('Case Status Report')
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
     C                   KFLD                    @@CO#
     C*
     C*  Case History
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  Case History
     C*
     C     CHKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     CHKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKEFDT
     C*
     C*  CASE PROCEDURE FILE
     C*
     C     DAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKPRIM
     C*
     C*  ICD9 TABLE FILE
     C*
     C     DIKY          KLIST
     C                   KFLD                    DAICD9
     C                   KFLD                    DALOST
     C*
     C*  EOMB File
     C*
     C     EAKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Employer Master
     C*
     C     EMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*   Subscriber
     C*
     C     SMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    CMSUB#
     C*
     C*   Fund
     C*
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
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
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    DAlost
     C*** End Add *** 08/05/13 ***************************************
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMACDT        XXDATE
     C     *LIKE         DEFINE    EABIDT        HDBIDT
     C     *LIKE         DEFINE    CHEFDT        KKEFDT
     C     *LIKE         DEFINE    DAPRIM        KKPRIM
     C     *LIKE         DEFINE    EABAMT        XXBAMT
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@STAT            1
     C                   PARM                    @@FRDT            7 0
     C                   PARM                    @@TODT            7 0
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
     C   19              EXCEPT    HEAD
     C*
     C                   ELSE
     C*
     C     SMKY          CHAIN     PSM01                              95
     C*
     C*  Initialize break criteria
     C*
     C                   EXSR      IZL1BK
     C                   EXSR      IZL2BK
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
     C*  L1 Break
     C*
     C     MEETL1        IFEQ      *OFF
     C*
     C     L1CNT         IFNE      *ZERO
     C*
     C                   EXSR      L2BK
     C                   EXSR      L1BK
     C*
     C   19              EXCEPT    HEAD
     C   19              EXCEPT    HEAD2
     C*
     C                   ENDIF                                                  L1CNT
     C*
     C                   EXSR      IZL1BK
     C                   EXSR      IZL2BK
     C*
     C                   ENDIF                                                  MEETL1
     C*
     C*  L2 Break
     C*
     C     MEETL2        IFEQ      *OFF
     C*
     C     L2CNT         IFNE      *ZERO
     C*
     C                   EXSR      L2BK
     C                   EXSR      L1BK
     C*
     C   19              EXCEPT    HEAD
     C   19              EXCEPT    HEAD2
     C*
     C                   ENDIF                                                  L2CNT
     C*
     C                   EXSR      IZL2BK
     C*
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
     C                   ADD       1             LRCNT             9 0
     C                   EXSR      ACCTOT
     C*
     C                   ENDIF                                                  MEETSS
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
     C                   SETON                                        OA9818
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
     C                   EXCEPT    HEAD
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
     C*  Accumulate L1 Totals - GROUP
     C*
     C                   ADD       1             GTCASE            3 0
     C                   ADD       XXWKSD        GTWKSD            7 0
     C                   ADD       XXWKSO        GTWKSO            7 0
     C                   ADD       XXAGE         GTOTAG            5 0
     C                   ADD       XXTBAM        GTOTB            13 2
     C*
     C*  Accumulate L2 Totals - EMPLOYER
     C*
     C                   ADD       1             ETCASE            3 0
     C                   ADD       XXWKSD        ETWKSD            7 0
     C                   ADD       XXWKSO        ETWKSO            7 0
     C                   ADD       XXAGE         ETOTAG            5 0
     C                   ADD       XXTBAM        ETOTB            13 2
     C*
     C*  Accumulate LR Totals - REPORT
     C*
     C                   ADD       1             RTCASE            3 0
     C                   ADD       XXWKSD        RTWKSD            7 0
     C                   ADD       XXWKSO        RTWKSO            7 0
     C                   ADD       XXAGE         RTOTAG            5 0
     C                   ADD       XXTBAM        RTOTB            13 2
     C*
     C                   ENDSR                                                  ACCTOT
     C*****************************************************************
     C* IZL1BK - Initialize Level 1 break criteria
     C*****************************************************************
     CSR   IZL1BK        BEGSR
     C*
     C*  Group break
     C*
     C     FNKY          CHAIN     PFN01                              95
     C*
     C     *IN95         IFEQ      *OFF
     C*
     C                   MOVEL(P)  FNNAME        XXFNNM           40
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXFNNM
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMFND         SVFND
     C*
     C                   ENDSR                                                  IZL1BK
     C*****************************************************************
     C* IZL2BK - Initialize Level 2 break criteria
     C*****************************************************************
     CSR   IZL2BK        BEGSR
     C*
     C*  EMPLOYER/DIV
     C*
     C     EMKY          CHAIN     PEM01                              95
     C*
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         SVDIV
     C*
     C                   ENDSR                                                  IZL2BK
     C*****************************************************************
     C* L1BK  - First Level Break action
     C*****************************************************************
     CSR   L1BK          BEGSR
     C*
     C*  Output L1 totals
     C*
     C*  CALCULATE GROUP AVERAGES
     C*
     C     GTWKSD        DIV       GTCASE        GAWKSD            5 0
     C     GTWKSO        DIV       GTCASE        GAWKSO            5 0
     C     GTOTAG        DIV       GTCASE        GAVGAG            3 0
     C     GTOTB         DIV       GTCASE        GAVGB            11 2
     C*
     C*  Group Totals
     C*
     C   19              EXCEPT    L1TOT
     C*
     C                   Z-ADD     *ZEROS        GTCASE
     C                   Z-ADD     *ZEROS        GTWKSD
     C                   Z-ADD     *ZEROS        GTWKSO
     C                   Z-ADD     *ZEROS        GTOTAG
     C                   Z-ADD     *ZEROS        GTOTB
     C                   Z-ADD     *ZEROS        L1CNT
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
     C*  CALCULATE EMPLOYER AVERAGES
     C*
     C     ETWKSD        DIV       ETCASE        EAWKSD            5 0
     C     ETWKSO        DIV       ETCASE        EAWKSO            5 0
     C     ETOTAG        DIV       ETCASE        EAVGAG            3 0
     C     ETOTB         DIV       ETCASE        EAVGB            11 2
     C*
     C*  EMPLOYER TOTALS
     C*
     C   19              EXCEPT    L2TOT
     C*
     C                   Z-ADD     *ZEROS        ETCASE
     C                   Z-ADD     *ZEROS        ETWKSD
     C                   Z-ADD     *ZEROS        ETWKSO
     C                   Z-ADD     *ZEROS        ETOTAG
     C                   Z-ADD     *ZEROS        ETOTB
     C                   Z-ADD     *ZEROS        L2CNT
     C*
     C*  PAGE BREAK AFTER
     C*
     C                   SETON                                        OA
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
     C*  Transaction Date change
     C*
     C     CMFND         IFNE      SVFND
     C*
     C                   MOVE      *OFF          MEETL1
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
     C*  EMPLOYER CHANGE
     C*
     C     CMEMP#        IFNE      SVEMP#
     C     CMDIV         ORNE      SVDIV
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
     C* CALCULATE REPORT AVERAGES
     C*
     C     RTWKSD        DIV       RTCASE        RAWKSD            5 0
     C     RTWKSO        DIV       RTCASE        RAWKSO            5 0
     C     RTOTAG        DIV       RTCASE        RAVGAG            3 0
     C     RTOTB         DIV       RTCASE        RAVGB            11 2
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
     C     @@STAT        COMP      *BLANKS                                34
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
     C                   Z-ADD     @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVEL     'Y'           KKPRIM
     C*
     C     @@STAT        IFEQ      'C'
     C     @@STAT        OREQ      ' '
     C*
     C                   SETON                                        17
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       17
     C*
     C                   ENDIF
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
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* RDCOND - CONDITION OF WHICH LOGICAL TO READ
     C*****************************************************************
     CSR   RDCOND        BEGSR
     C*
     C     @@SORT        IFEQ      'A'
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
     C     *LOVAL        SETLL     PCM01
     C                   READ      PCM01                                  99
     C*
     C                   ENDSR                                                  READ1
     C*****************************************************************
     C* READ2 - Read first record from file
     C*****************************************************************
     CSR   READ2         BEGSR
     C*
     C     *LOVAL        SETLL     PCM02
     C                   READ      PCM02                                  99
     C*
     C                   ENDSR                                                  READ2
     C*****************************************************************
     C* READN1 - Read next record from file
     C*****************************************************************
     CSR   READN1        BEGSR
     C*
     C                   READ      PCM01                                  99
     C*
     C                   ENDSR                                                  READN1
     C*****************************************************************
     C* READN2 - Read next record from file
     C*****************************************************************
     CSR   READN2        BEGSR
     C*
     C                   READ      PCM02                                  99
     C*
     C                   ENDSR                                                  READN2
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
     C*
     C                   MOVEL     CDLSN         XXCLNM           21
     C*
     C                   ELSE
     C*
     C     CDLSN         CAT(P)    ',':0         XXCLNM
     C                   CAT       CDFSN:1       XXCLNM
     C*
     C                   ENDIF
     C*
     C* CALCULATE AGE
     C*
     C     CDDTOB        IFNE      *ZEROS
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DTOB
     C*
     C                   CALL      'SRAGE'
     C                   PARM                    ##DTOB            8
     C                   PARM      *BLANKS       ##AGE             3
     C*
     C                   MOVE      ##AGE         XXAGE             3 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        XXAGE
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        XXAGE
     C                   MOVEL     *BLANKS       XXCLNM
     C*
     C                   ENDIF
     C*
     C* ACCIDENT DATE
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C* REFERRAL DATE
     C*
     C                   Z-ADD     CMRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRFDT            8 0
     C*
     C* GET ICD9 DESCRIPTION
     C*
     C     DAKY          CHAIN     PDA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*** Beg Add *** 08/05/13 *****************************************
      /Free
       If DAicdInd = 'Y';
          kkICd10 = DAicd1001;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                xxDesc = ICdsc1;
             Else;
                xxDesc = *blanks;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 *****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  DIDSC1        XXDESC           19
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXDESC
     C*
     C                   ENDIF
     C*
     C*** Beg Add *** 08/05/13 *****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 *****************************************
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       DIICD9
     C                   MOVEL     *BLANKS       DILOST
     C                   MOVEL     *BLANKS       DIDSC1
     C                   MOVEL     *BLANKS       XXDESC
     C*
     C                   ENDIF
     C*
     C* CLAIM NURSE STAFF CODE
     C*
     C                   MOVEL(P)  CMCMNU        XXSTAF            3
     C*
     C                   Z-ADD     *ZEROS        HDBIDT
     C                   Z-ADD     *ZEROS        XXTBAM
     C*
     C                   Z-ADD     *ZEROS        HDBIDT
     C                   Z-ADD     *ZEROS        XXTBAM
     C                   Z-ADD     *ZEROS        XXBIDT
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     EASYS         IFEQ      'M'
     C*
     C* LAST BILL DATE
     C*
     C     EABIDT        IFGT      HDBIDT
     C*
     C                   Z-ADD     EABIDT        HDBIDT
     C*
     C                   ENDIF
     C*
     C* TOTAL AMT BILLED
     C*
     C     EABINV        IFNE      *ZEROS
     C*
     C                   ADD       EABAMT        XXTBAM           13 2
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY          READE     PEA01                                  99
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     HDBIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBIDT            8 0
     C*
     C* # OF WKS FROM ACDT TO RNDT
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRFRDC
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC            8 0
     C                   PARM      XXRFDT        SRTODC            8 0
     C                   PARM      *ZEROS        SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
     C                   PARM      *BLANKS       SRTERM            1
     C*
     C     SRDAYS        DIV       7             XXWKSD            7 0
     C*
     C* # OF WKS OP
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRTODC
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      XXRFDT        SRFRDC            8 0
     C                   PARM                    SRTODC            8 0
     C                   PARM      *ZEROS        SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
     C                   PARM      *BLANKS       SRTERM            1
     C*
     C                   ELSE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      XXRFDT        SRFRDC            8 0
     C                   PARM      DSDATE        SRTODC            8 0
     C                   PARM      *ZEROS        SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
     C                   PARM      *BLANKS       SRTERM            1
     C*
     C                   ENDIF
     C*
     C     SRDAYS        DIV       7             XXWKSO            5 0
     C*
     C* CLOSING DATE AND CODE IF @@STAT = 'C'.
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCLDT            8 0
     C*
     C                   MOVEL     CHCLCD        XXCLCD            3
     C                   Z-ADD     *ZEROS        XXOPDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXOPDT            8 0
     C*
     C                   MOVEL     *BLANKS       XXCLCD
     C                   Z-ADD     *ZEROS        XXCLDT
     C*
     C                   ENDIF
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
     C                   SELECT
     C*
     C* IF @@STAT EQ 'O' THEN LOOK FOR EFDT FOR THAT PERIOD WHEN OPEN
     C*
     C     @@STAT        WHENEQ    'O'
     C*
     C                   Z-ADD     *HIVAL        KKEFDT
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     CHEFDT        IFGE      @@FRDT
     C     CHEFDT        ANDLE     @@TODT
     C*
     C     CHOCST        IFEQ      'O'
     C     CHOCST        OREQ      'R'
     C*
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CHKY1         READPE    PCH01                                  99
     C*
     C                   ENDDO
     C*
     C   99              MOVEL     *OFF          MEETSS            1
     C*
     C* IF @@STAT EQ 'C' THEN LOOK FOR EFDT FOR THAT PERIOD WHEN CLOSED
     C*
     C     @@STAT        WHENEQ    'C'
     C*
     C                   Z-ADD     *HIVAL        KKEFDT
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     CHEFDT        IFGE      @@FRDT
     C     CHEFDT        ANDLE     @@TODT
     C*
     C     CHOCST        IFEQ      'C'
     C*
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CHKY1         READPE    PCH01                                  99
     C*
     C                   ENDDO
     C*
     C   99              MOVEL     *OFF          MEETSS            1
     C*
     C     @@STAT        WHENEQ    ' '
     C*
     C                   Z-ADD     *HIVAL        KKEFDT
     C*
     C     CHKY2         SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     CHEFDT        IFGE      @@FRDT
     C     CHEFDT        ANDLE     @@TODT
     C*
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     CHKY1         READPE    PCH01                                  99
     C*
     C                   ENDDO
     C*
     C   99              MOVEL     *OFF          MEETSS            1
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE FXR2P
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PR201
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   Z-ADD     CMCO#         R2CO#
     C                   MOVEL(P)  CONAME        R2CONM
     C                   Z-ADD     CMSUB#        R2SUB#
     C                   MOVEL(P)  SMNAME        R2SBNM
     C                   Z-ADD     CMFND         R2FND
     C                   MOVEL(P)  FNNAME        R2FNNM
     C                   Z-ADD     CMEMP#        R2EMP#
     C                   Z-ADD     CMDIV         R2DIV
     C                   MOVEL(P)  EMNAME        R2EMNM
     C                   Z-ADD     CMFYR         R2FYR
     C                   Z-ADD     CMCASE        R2CASE
     C*
     C     CMFSN         IFNE      *BLANKS
     C*
     C     CMLSN         CAT(P)    ',':0         R2CLNM
     C                   CAT(P)    CMFSN:1       R2CLNM
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  CMLSN         R2CLNM
     C*
     C                   ENDIF
     C*
     C                   MOVEL     CDSS#         R2SS#
     C                   Z-ADD     XXACDT        R2ACDT
     C                   Z-ADD     XXAGE         R2AGE
     C                   Z-ADD     XXRFDT        R2RFDT
     C                   MOVEL(P)  DIICD9        R2ICD9
     C                   MOVEL(P)  DILOST        R2LOST
     C                   MOVEL(P)  DIDSC1        R2ICDD
     C                   MOVEL(P)  CMCMNU        R2CMNU
     C*
     C     UPKY1         CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        R2CMNN
     C   99              MOVEL(P)  *BLANKS       R2CMNN
     C*
     C                   Z-ADD     XXBIDT        R2LBDT
     C                   Z-ADD     XXTBAM        R2TBAM
     C                   Z-ADD     XXOPDT        R2OPDT
     C                   Z-ADD     XXWKSO        R2#WKS
     C                   Z-ADD     XXCLDT        R2CLDT
     C                   MOVEL(P)  XXCLCD        R2CLCD
     C*
     C                   MOVEL(P)  XXCLCD        KKKEY
     C                   MOVEL(P)  'CLCD'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTKEY         R2CLDS
     C   99              MOVEL     *BLANKS       R2CLDS
     C*
     C                   MOVEL(P)  'A'           R2STAT
     C                   MOVEL(P)  Q1USER        R2USER
     C                   MOVEL(P)  Q1PGM         R2PGM
     C                   MOVEL(P)  Q1USER        R2IUSR
     C                   MOVEL(P)  Q1PGM         R2IPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        R2TIME
     C                   Z-ADD     DSTIME        R2ITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         R2DT
     C                   MOVEL     SRDTO         R2IDT
     C*** Beg Add *** 08/05/13 *****************************************
      /Free
       If DAicdInd = 'Y';
          R2ICD10 = DAicd1001;
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 *****************************************
     C*
     C                   WRITE     PR201
     C*
     C                   FEOD      FXR2P
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
     O                                           27 'CASE STATUS  . . :'
     O              N34      @@STAT            +001
     O               34                          32 '*ALL'
     O          E            ROUTE          2
     O                                           27 'DATE RANGE . . . :'
     O                       XXFRDT            +000 '0  /  /    '
     O                                         +001 '-'
     O                       XXTODT            +000 '0  /  /    '
     O          E            ROUTE          2
     O                                           27 'SORT . . . . . . :'
     O                       @@SORT            +001
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
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            HEAD           2
     O                                           55 'Date Range:'
     O                       XXFRDT            +001 '0  /  /    '
     O                                         +001 'to'
     O                       XXTODT            +001 '0  /  /    '
     O*
     O          E            HEAD           2
     O                                           17 'Report Selection:'
     O                                         +002 'Sub:'
     O                       @@SUB#        4   +001
     O                                         +002 'Group:'
     O                       @@FND         4   +001
     O                                         +002 'Emp:'
     O                       @@EMP#        4   +001
     O                                         +001 '-'
     O                       @@DIV             +001 '   0 '
     O                                         +002 'Case Stat:'
     O                       @@STAT            +001
     O                                         +002 'Sort:'
     O                       @@SORT            +001
     O                                         +001 'Type of Output:'
     O                       @@TYPE            +001
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Subscriber  :'
     O                       CMSUB#            +001 '   0 '
     O                       SMNAME              73
     O*
     O          E   N18      HEAD2          1
     O                                           13 'Group       :'
     O                       CMFND             +003 ' 0 '
     O                       XXFNNM              73
     O*
     O          E   N18      HEAD2          2
     O                                           13 'Employer    :'
     O                       CMEMP#            +001 '       0 '
     O                                         +000 ' '
     O                       CMDIV             +000 '   0 '
     O                       EMNAME              73
     O*
     O          E            HEAD2          1
     O                                           45 'Injury'
     O                                           57 'Referral'
     O                                           86 'Staff'
     O                                           96 'Last Bill'
     O                                          108 'Total Amt'
     O                                          115 '# of'
     O               17                         132 '** Closing **'
     O*
     O          E            HEAD2          2
     O                                           05 'Case#'
     O                                           24 'Patient Name'
     O                                           37 'Age'
     O                                           44 'Date'
     O                                           55 'Date'
     O                                           77 'Primary Diagnosis'
     O                                           85 'Code'
     O                                           93 'Date'
     O                                          107 'Billed'
     O                                          117 'Wks Op'
     O               17                         124 'Date'
     O               17                         132 'Cod'
     O*
     O          E            DETAIL         1
     O                       CMFYR         4     03
     O                                         +000 '-'
     O                       CMCASE            +000
     O                       XXCLNM            +001
     O                       XXAGE         4   +001
     O                       XXACDT            +001 '  /  /    '
     O                       XXRFDT            +001 '  /  /    '
     O                       XXDESC            +001
     O                       XXSTAF            +001
     O                       XXBIDT            +001 '  /  /    '
     O                       XXTBAM        4   +001
     O                       XXWKSO        P   +001
     O               17      XXCLDT            +001 '  /  /    '
     O               17      XXCLCD            +002
     O*
     O          E            L1TOT       1  1
     O                                           13 'Group Totals:'
     O          E            L1TOT          1
     O                                           07 'Total'
     O                                           19 'Avg Wks'
     O                                           29 'Avg Wks'
     O                                           35 'Avg'
     O                                           47 'Total'
     O                                           63 'Avg'
     O          E            L1TOT          1
     O                                           07 'Cases'
     O                                           19 'DOI-DOR'
     O                                           27 'Open'
     O                                           35 'Age'
     O                                           48 'Billing'
     O                                           65 'Billing'
     O          E            L1TOT          1
     O                       GTCASE        4     07
     O                       GAWKSD        4     17
     O                       GAWKSO        4     28
     O                       GAVGAG        4     35
     O                       GTOTB         4     52
     O                       GAVGB         4     68
     O*
     O          E            L2TOT       3  1
     O                                           16 'Employer Totals:'
     O          E            L2TOT          1
     O                                           07 'Total'
     O                                           19 'Avg Wks'
     O                                           29 'Avg Wks'
     O                                           35 'Avg'
     O                                           47 'Total'
     O                                           63 'Avg'
     O          E            L2TOT          1
     O                                           07 'Cases'
     O                                           19 'DOI-DOR'
     O                                           27 'Open'
     O                                           35 'Age'
     O                                           48 'Billing'
     O                                           65 'Billing'
     O          E            L2TOT          1
     O                       ETCASE        4     07
     O                       EAWKSD        4     17
     O                       EAWKSO        4     28
     O                       EAVGAG        4     35
     O                       ETOTB         4     52
     O                       EAVGB         4     68
     O*
     O          E            LRTOT       3  1
     O                                           14 'Report Totals:'
     O          E            LRTOT          1
     O                                           07 'Total'
     O                                           19 'Avg Wks'
     O                                           29 'Avg Wks'
     O                                           35 'Avg'
     O                                           47 'Total'
     O                                           63 'Avg'
     O          E            LRTOT          1
     O                                           07 'Cases'
     O                                           19 'DOI-DOR'
     O                                           27 'Open'
     O                                           35 'Age'
     O                                           48 'Billing'
     O                                           65 'Billing'
     O          E            LRTOT          1
     O                       RTCASE        4     07
     O                       RAWKSD        4     17
     O                       RAWKSO        4     28
     O                       RAVGAG        4     35
     O                       RTOTB         4     52
     O                       RAVGB         4     68
     O*
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
