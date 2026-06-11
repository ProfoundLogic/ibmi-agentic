     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*=========================================================================
     H*       StoneRiver is a trademark of StoneRiver, Inc.
     H*=========================================================================
     H*
     H*-------------------------------------------------------------------------
     H/TITLE CMPRSVBAL - Reserve History Totals vs Claim Total Incurred
     H**************************************************************************
     H* Description:  This pgm prints a list of claims which have a   *
     H*   discrepancy between the Reserve History file and the claim's*
     H*   total incurred.                                             *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     F*-------------------------------------------------------------------------
     F*       F   I   L   E   S
     F*-------------------------------------------------------------------------
     FWMRHL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D*-------------------------------------------------------------------------
     D*       A   R   R   A   Y   S
     D*-------------------------------------------------------------------------
     D*     Model: Reserves rearranged for this run.
     D MT              S              2    DIM(10)                              Reserve codes
     D HP              S              9  2 DIM(10)                              Hold Paid amounts-te
     D HR              S              9  2 DIM(10)                              Hold Remaining tempr
     D*
     D*     Claim Master File
     D IN              S              9  2 DIM(10)                              Incurred - total
     D DF              S              9  2 DIM(10)                              Difference - claim
     D*
     D*     Reserve History File
     D AH              S              9  2 DIM(10)                              Accum Hist-SU
     D*
     D*     Totals
     D TD              S              9  2 DIM(10)                              Difference - total
     D TP              S              9  2 DIM(10)                              Incurred - total
     D*
     D*-------------------------------------------------------------------------
     D*     Conversion file arrays
     D FI              S              7  0 DIM(10)                              Nbr input
     D FU              S              7  0 DIM(10)                              Nbr updated
     D FW              S              7  0 DIM(10)                              Nbr written
     D FE              S              7  0 DIM(10)                              Errors
     D FN              S             15    DIM(10) CTDATA PERRCD(1)             File names
     D*-------------------------------------------------------------------------
     D*
     D*-------------------------------------------------------------------------
     D*       D   A   T   A
     D*-------------------------------------------------------------------------
     D*
     D*   System Date and Time
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*   Reserve Codes in Claim Master File
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              Reserve codes
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D*
     D*   Reserve Codes in Reserve History File
     D                 DS
     D  RC                     1     20
     D                                     DIM(10)                              Reserve codes
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D*
     D*   Remaining Reserve Totals in Claim Master
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              Remaining Reserve
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D*
     D*   Payment Totals in Claim Master
     D                 DS
     D  PR                     1     90  2
     D                                     DIM(10)                              Total Paid on Claim
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D*
     D*   Total Incurred Changes in Reserve History file
     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)                              Total Incurred Chang
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*
     C*  Key list for the Company Master file
     C*
     C     COKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C*  Key list for the Reserve History file
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*     Key list for the Fund Master file
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C     *LIKE         DEFINE    CMFND         SVFND
     C*
     C                   TIME                    DSDTTM
     C**************************************************************************
     C*  Initialization
     C*        Claim Master
     C                   READ      PCM01                                  50
     C                   MOVEA     RT            MT                              Res types to model
     C*
     C*        Company File
     C  N50COKY          CHAIN     PCO01                              60
     C                   EXCEPT    HEAD
     C                   ADD       1             FI(6)                           Company
     C*
     C*  Start of Main loop ----------------------------------------------------
     C     *IN50         DOWEQ     '0'
     C                   ADD       1             FI(1)                          Claim Master
     C*
     C*        Group Change
     C     SVFND         IFNE      CMFND
     C                   EXSR      GRPTOT                                       Print total
     C                   ENDIF                                                  e- SVFND <>
     C*
     C*    Not Test, process claim
     C     FNTEST        IFNE      'Y'
     C*
     C*    Reserve codes not same as model, do rearrangement
     C                   MOVEA     MT            XXMT             20             Mod types to field
     C                   MOVEA     RT            XXRT             20             Res types to field
     C     XXMT          IFNE      XXRT
     C                   EXSR      REARR
     C                   ENDIF                                                  e- MT <> RT
     C*
     C*    Reserve codes now same as model, load whole array
     C     RR            ADD       PR            IN                             Total Incurred
     C*
     C                   XFOOT     IN            XXINC             9 2          History tot
     C                   EXSR      ACCHIS                                       Accum hist incur
     C                   XFOOT     AH            XXHIST            9 2          History tot
     C     IN            SUB       AH            DF                             Diff by code
     C                   XFOOT     DF            XXDIF             9 2          Diff total
     C*
     C*        Double check incurred
     C                   XFOOT     RR            XXOUTS            9 2          Remaining
     C                   XFOOT     PR            XXPAID            9 2           plus Paid
     C     XXOUTS        ADD       XXPAID        XXTI              9 2          Incurred
     C     XXINC         IFNE      XXTI
     C                   ADD       1             FE(9)                           Xfoot diff
     C                   ENDIF                                                  e- HISINC <>
     C*
     C     XXDIF         IFNE      *ZERO
     C                   ADD       1             FE(4)                           Xfoot diff
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       XXINC         XTINC            11 2               totals
     C                   ADD       XXOUTS        XTOUTS           11 2               totals
     C                   ADD       XXDIF         XTDIF            11 2               totals
     C                   ADD       1             XTCASE            9 0                 for the run
     C                   ENDIF                                                  e- XXDIF <> 0
     C*
     C                   ENDIF                                                  e- FNTEST <> Y
     C*
     C                   READ      PCM01                                  50
     C                   ENDDO                                                  e- IN50 DOW = ZERO
     C*
     C                   EXSR      ENDER
     C                   MOVE      '1'           *INLR
     C*
     C*===================================================================
     C*      GRPTOT Total Prior; Setup New Group
     C*===================================================================
     C     GRPTOT        BEGSR
     C*
     C     FNKY          CHAIN     PFN01                              67
     C     *IN67         IFEQ      *ON
     C                   MOVE      'Y'           FNTEST
     C                   ELSE                                                   X- IN67 = ON
     C                   ADD       1             FI(7)                           Fund Master
     C                   ENDIF                                                  e- IN67 = ON
     C                   Z-ADD     CMFND         SVFND
     C*
     C*   ( Print  )
     C                   ENDSR
     C*
     C*===================================================================
     C*      REARR Rearrange Claim Reserve Codes, Paid, and Remaining
     C*===================================================================
     C     REARR         BEGSR
     C*    Reserve codes not same as model
     C*       Clear work arrays
     C                   Z-ADD     0             HP
     C                   Z-ADD     0             HR
     C*
     C*       Look up each code
     C                   DO        10            A                               Fund Master
     C                   MOVE      *OFF          *IN47
     C                   Z-ADD     1             B                 3 0
     C     RC(A)         LOOKUP    MT(B)                                  47     Find code in model
     C     *IN47         IFEQ      *OFF                                           in claim
     C     '  '          LOOKUP    MT(B)                                  47
     C     *IN47         IFEQ      *ON                                            Got it
     C                   MOVE      RC(A)         MT(B)
     C                   ENDIF                                                  e- IN47 = ON
     C                   ENDIF                                                  X- IN47 = OFF
     C     *IN47         IFEQ      *ON                                            Got it
     C                   ADD       PR(A)         HP(B)
     C                   ADD       RR(A)         HR(B)
     C                   ENDIF                                                  e- IN47 = ON
     C                   ENDDO
     C*
     C                   Z-ADD     HP            PR
     C                   Z-ADD     HR            RR
     C*
     C                   ENDSR
     C*
     C*===================================================================
     C*      Acumulate Claim History Records
     C*===================================================================
     C     ACCHIS        BEGSR
     C*
     C*        Read Claim History -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
     C                   Z-ADD     0             AH
     C     RHKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             FI(2)                          Claim History
     C*
     C                   MOVEA     RT            TESTG            20
     C                   MOVEA     RC            TESTH            20
     C     TESTG         IFEQ      TESTH                                        Same order
     C                   ADD       SU            AH                                   zap
     C                   ELSE
     C*                                                    Reorder
     C*    Different sequence: align individually with Claim
     C                   DO        10            A                 3 0
     C                   Z-ADD     1             B                 3 0
     C                   MOVE      *OFF          *IN47
     C     RC(A)         LOOKUP    RT(B)                                  47     Find hist code
     C     *IN47         IFEQ      *ON                                            in claim
     C                   ADD       SU(A)         AH(B)
     C                   ELSE
     C     '  '          LOOKUP    RT(B)                                  47
     C     *IN47         IFEQ      *ON                                            in claim
     C                   MOVE      RC(A)         RT(B)
     C                   ADD       SU(A)         AH(B)
     C                   ENDIF                                                  e- IN47 = ON
     C                   ENDIF                                                  X- IN47 = OFF
     C                   ENDDO                                                  e- Do 10 A
     C                   ENDIF                                                  e- RT = RC
     C*
     C     RHKY          READE     PRH01                                  60
     C                   ENDDO                                                  e- IN60 DOWEQ OFF
     C*
     C                   ENDSR
     C*
     C*===================================================================
     C*      ENDER End of Job Subroutine
     C*===================================================================
     C     ENDER         BEGSR
     C                   EXCEPT    TOT
     C*
     C*     Print File Totals
     C                   EXCEPT    TOTF
     C                   DO        10            F                 3 0
     C     FI(F)         IFNE      *ZERO
     C     FE(F)         ORNE      *ZERO
     C     FU(F)         ORNE      *ZERO
     C     FW(F)         ORNE      *ZERO
     C                   EXCEPT    DETF
     C                   ENDIF                                                  Fx, <> 0
     C                   ENDDO
     C*
     C                   ENDSR
     C*===================================================================
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           76 'COMPARE WMCMP & WMRHP'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           22 'FND FYR    Case      I'
     O                                           44 'ncurred      History  '
     O                                           66 ' Difference  by Rsv:  '
     O                                           88 '                      '
     O                       MT(1)               71
     O                       MT(2)               84
     O                       MT(3)               97
     O                       MT(4)              110
     O                       MT(5)              123
     O          E            DET            1
     O                       CMFND         4      3
     O                       CMFYR         4      7
     O                       CMCASE        4     15
     O                       XXDIF         K     56
     O                       XXHIST        K     43
     O                       XXINC         K     30
     O                       DF(1)         K     72
     O                       DF(2)         K     85
     O                       DF(3)         K     98
     O                       DF(4)         K    111
     O                       DF(5)         K    124
     O          E            TOT         2
     O                                            7 'TOTALS:'
     O                       XTCASE        4     17
     O                       XTOUTS        J     51
     O*=========================================================================
     O          E            TOTF        2  1
     O                                           22 '*** RECORD  COUNT SUMM'
     O                                           44 'MARY ***              '
     O          E            TOTF        1  2
     O                                           22 '  Description       In'
     O                                           44 'put read        Update'
     O                                           66 'd    Written out      '
     O                                           88 '   Errors             '
     O          E            DETF           1
     O                       FN(F)               15
     O                       FI(F)         Z     30
     O                       FU(F)         Z     45
     O                       FW(F)         Z     60
     O                       FE(F)         Z     75
     O*
** TABLE      v       FN - File names   10 @ 15
WMCML1            1
WMRHL             2
xxxxxxxxx3        3
Out of Balance    4
xxxxxxxxx5        5
SMCOL             6
WMFNL             7
xxxxxxxxx8        8
XFOOT Diff        9
xxxxxxxx10       10
