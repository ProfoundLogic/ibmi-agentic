     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE W7471R - NATURE, PART & CAUSE SUMMARY BY EMPLOYER
     H*================================================================
     H* REVISIONS
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/10/00  C:118  PROJECT: WC987
     H*                  LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H*
     H*================================================================
     F*
     F* * * CLAIMS LOGICAL FILE
     F*
     FWMCML8    IF   E           K DISK
     F*
     F* * * BATCH ROUTING FILE
     F*
     FSMORL     IF   E           K DISK
     F*
     F* * * COMPANY FILE
     F*
     FSMCOL     IF   E           K DISK
     F*
     F* * * FUND FILE
     F*
     FWMFNL     IF   E           K DISK
     F*
     F* * * FUND DETAIL FILE
     F*
     FWDF1L     IF   E           K DISK
     F*
     F* * * CLAIM HISTORY FILE
     F*
     FWMCHL     IF   E           K DISK
     F*
     F* * * EMPLOYER FILE
     F*
     FWMEML     IF   E           K DISK
     F*
     F* * * TABLE FILE
     F*
     FWTABL     IF   E           K DISK
     F*
     F* * * USER PARAMETER FILE
     F*
     FWMUPL     IF   E           K DISK
     F*
     F* * * OUTPUT FILE
     F*
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOA)
     F*

     D*
     D*   CAUSE SUMMARY ARRAYS
     D*
     D CSM             S              2    DIM(999)
     D CDS             S             59    DIM(999)
     D COMB            S             61    DIM(999)
     D*
     D*
     D*
     D*
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)
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
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)
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
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)
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
     D*  DATA STRUCTURE FOR CDS ARRAY
     D*
     D                 DS
     D  XCDS                   1     59
     D  XXCNT                  1      5  0
     D  XXCOMP                 6     14  2
     D  XXMED                 15     23  2
     D  XXOTHR                24     32  2
     D  XXTOTL                33     41  2
     D  XXTINC                42     50  2
     D  XXORES                51     59  2
     D*
     D*  DATA STRUCTURE FOR COMBINATION (CSM & CDS) ARRAY
     D*
     D                 DS
     D  XCOMB                  1     61
     D  XCODE                  1      2
     D  XCNT                   3      7  0
     D  XCOMP                  8     16  2
     D  XMED                  17     25  2
     D  XOTHR                 26     34  2
     D  XTOTL                 35     43  2
     D  XTINC                 44     52  2
     D  XORES                 53     61  2
     D*
     D* DATA STRUTURES FOR WORK FIELD AND FILE FIELD
     D*
     D                 DS
     D  XXEMP                  1     14  0
     D  HEMP                   1      9  0
     D  HDIV                  10     14  0
     D                 DS
     D  XYEMP                  1     14  0
     D  EMEMP#                 1      9  0
     D  EMDIV                 10     14  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D                 DS
     D  DSDATE                 1      6  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR CLAIM FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    F1FYR
     C*
     C*   KEY LIST FOR EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    F1FYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXDATE
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRES            10
     C*
     C     *LIKE         DEFINE    EMDT          XXDATE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C*   ENTRY PARMS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQEMP             9
     C                   PARM                    QQDIV             5
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
     C                   MOVE      QQDTE         X1DATE            8 0
     C                   MOVE      QQEMP         XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQMODE        XXMODE            1
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
MRA  C                   MOVEL     SRDTO         DSDATE
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  GET FUND DESCRIPTION
     C*
     C     XXFND         IFEQ      *ZERO
     C     XXCO#         SETLL     WMFNL
     C     TAG03         TAG
     C                   MOVE      '1'           *IN30
     C     XXCO#         READE     WMFNL                                  60
     C                   MOVEL     FNNAME        XFNAME           60
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF
     C***** END   ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      '1'
     C                   SETON                                        LR
     C                   GOTO      END
     C                   END
     C                   ELSE
     C     FNKEY         CHAIN     WMFNL                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF
     C***** END   ADD **** 04/10/00 ***********************************
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME           60
     C                   END
     C                   END
     C*
     C*  READ FUND DETAIL FILE
     C*
     C     F1KEY         SETLL     PF101
     C     TAG01         TAG
     C*
     C     F1KEY         READE     PF101                                  60
     C     *IN60         IFEQ      '1'
     C     *IN30         IFEQ      '1'
     C                   GOTO      TAG03
     C                   ELSE
     C                   SETON                                        LR
     C                   GOTO      END
     C                   END
     C                   END
     C*
     C     *IN26         IFEQ      '1'
     C*
     C* ADD FUND YEAR TOTALS (L4) TO FINAL GRAND TOTALS (LR)
     C*
     C                   ADD       L4COMP        LRCOMP           11 2
     C                   ADD       L4MED         LRMED            11 2
     C                   ADD       L4OTHR        LROTHR           11 2
     C                   ADD       L4TOTL        LRTOTL           11 2
     C                   ADD       L4TINC        LRTINC           11 2
     C                   ADD       L4ORES        LRORES           11 2
     C                   ADD       L4TCAS        LRTCAS            5 0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    SUMHD
     C                   END
     C*
     C*  PRINT TOTAL FOR FUND YEAR
     C*
     C     L4TCAS        IFNE      0
     C                   EXCEPT    YRTTL
     C                   MOVE      '1'           *IN23
     C                   Z-ADD     0             PAGE
     C                   END
     C*
     C*  INITIALIZE FUND YEAR TOTALS
     C*
     C                   EXSR      YRCLR
     C*
     C                   END
     C*
     C                   MOVE      F1FYR         XXYR              2 0
     C                   MOVEL     F1FYR         XX1               1 0
     C     XX1           IFEQ      *ZERO
     C                   MOVE      '19'          XXCENT            2 0
     C                   END
     C*
     C                   MOVE      *BLANK        HPART
     C                   MOVE      *BLANK        HNATR
     C                   Z-ADD     0             XXEMP
     C*
     C*  READ EMPLOYER FILE
     C*
     C                   MOVE      QQEMP         XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      *BLANKS       XYNAME           38
     C     XXEMP#        IFEQ      *ZERO
     C                   MOVE      '0'           *IN28
     C     EMKEY         SETLL     WMEML                                  68
     C     *IN68         IFNE      '1'
     C                   MOVE      *BLANKS       XYNAME
     C                   GOTO      TAG01
     C                   END
     C     TAG04         TAG
     C*
     C     EMKEY         READE     WMEML                                  69
     C                   GOTO      TAG06
     C                   ELSE
     C     EMKEY1        CHAIN     PEM01                              69
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN26
     C                   END
     C*
     C     TAG06         TAG
     C*
     C     *IN69         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      TAG01
     C                   ELSE
     C                   MOVEL     EMNAME        XYNAME
     C                   END
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         SETLL     WMCML8                                 67
     C     *IN67         IFNE      '1'
     C     XXEMP#        IFEQ      *ZERO
     C                   GOTO      TAG04
     C                   ELSE
     C                   GOTO      TAG01
     C                   END
     C                   END
     C     TAG02         TAG
     C     CMKEY         READE     WMCML8                                 66
     C     *IN66         IFEQ      '1'
     C*
     C*  ADD PART TOTALS (L1) TO NATURE TOTALS (L2)
     C*
     C                   ADD       L1COMP        L2COMP            9 2
     C                   ADD       L1MED         L2MED             9 2
     C                   ADD       L1OTHR        L2OTHR            9 2
     C                   ADD       L1TOTL        L2TOTL            9 2
     C                   ADD       L1TINC        L2TINC            9 2
     C                   ADD       L1ORES        L2ORES            9 2
     C                   ADD       L1TCAS        L2TCAS            5 0
     C*
     C*  CHECK FOR OVERFLOW
     C*
     C     *INOA         IFEQ      '1'
     C     *IN23         OREQ      '1'
     C                   MOVE      '0'           *IN23
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    SUMHD
     C                   END
     C*
     C*  MAKE SURE THERE IS A PART TOTAL TO PRINT BEFORE PRINTING
     C*
     C     L1TCAS        IFNE      0
     C*
     C*  CHECK TO SEE IF EMPLOYER HEADING HAS BEEN PRINTED BEFORE
     C*  PRINT PART CODE DETAIL
     C*
     C     *IN25         IFEQ      '1'
     C                   EXCEPT    EMPTTL
     C                   MOVE      '0'           *IN25
     C                   END
     C*
     C*  CHECK TO SEE IF NATURE CODE HEADING HAS BEEN PRINTED BEFORE
     C*  PRINT PART CODE DETAIL
     C*
     C     *IN24         IFEQ      '1'
     C                   EXCEPT    NATHD
     C                   MOVE      '0'           *IN24
     C                   END
     C*
     C                   EXCEPT    PDETL
     C*
     C                   END
     C*
     C*  INITIALIZE PART CODE TOTALS
     C*
     C                   EXSR      PTCLR
     C*
     C*  ADD NATURE TOTALS (L2) TO EMPLOYER TOTALS (L3)
     C*
     C                   ADD       L2COMP        L3COMP            9 2
     C                   ADD       L2MED         L3MED             9 2
     C                   ADD       L2OTHR        L3OTHR            9 2
     C                   ADD       L2TOTL        L3TOTL            9 2
     C                   ADD       L2TINC        L3TINC            9 2
     C                   ADD       L2ORES        L3ORES            9 2
     C                   ADD       L2TCAS        L3TCAS            5 0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    SUMHD
     C                   END
     C*
     C* MAKE SURE THERE IS A NATURE CODE TOTAL TO PRINT
     C*
     C     L2TCAS        IFNE      0
     C*
     C*  PRINT NATURE CODE TOTAL LINE THEN SET INDICATOR TO PRINT NEXT
     C*  NEW NATURE CODE
     C*
     C                   EXCEPT    NDETL
     C                   MOVE      '1'           *IN24
     C*
     C                   END
     C*
     C*  INITIALIZE NATURE CODE TOTALS
     C*
     C                   EXSR      NTCLR
     C*
     C* ADD EMPLOYER TOTALS (L3) TO FUND YEAR TOTALS (L4)
     C*
     C                   ADD       L3COMP        L4COMP           11 2
     C                   ADD       L3MED         L4MED            11 2
     C                   ADD       L3OTHR        L4OTHR           11 2
     C                   ADD       L3TOTL        L4TOTL           11 2
     C                   ADD       L3TINC        L4TINC           11 2
     C                   ADD       L3ORES        L4ORES           11 2
     C                   ADD       L3TCAS        L4TCAS            5 0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    SUMHD
     C                   END
     C*
     C*  PRINT TOTAL FOR EMPLOYER
     C*
     C     *IN27         IFNE      '1'
     C                   MOVE      '0'           *IN25
     C                   EXCEPT    EMPTTL
     C                   MOVE      '1'           *IN25
     C                   END
     C*
     C*  INITIALIZE EMPLOYER TOTALS
     C*
     C                   EXSR      EMPCLR
     C*
     C     *IN28         IFEQ      '1'
     C                   GOTO      TAG01
     C                   ELSE
     C                   GOTO      TAG04
     C                   END
     C*
     C*
     C                   ELSE
     C*
     C*
     C*
     C     CMNATR        IFNE      HNATR
     C     HNATR         ANDNE     *BLANK
     C                   MOVE      '1'           *INL1
     C                   ADD       L1COMP        L2COMP            9 2
     C                   ADD       L1MED         L2MED             9 2
     C                   ADD       L1OTHR        L2OTHR            9 2
     C                   ADD       L1TOTL        L2TOTL            9 2
     C                   ADD       L1TINC        L2TINC            9 2
     C                   ADD       L1ORES        L2ORES            9 2
     C                   ADD       L1TCAS        L2TCAS            5 0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C     *IN23         IFEQ      '1'
     C     *INOA         OREQ      '1'
     C                   EXCEPT    SUMHD
     C                   MOVE      '0'           *INOA
     C                   MOVE      '0'           *IN23
     C                   END
     C*
     C* MAKE SURE THERE IS A NATURE CODE TOTAL BEFORE PRINTING
     C*
     C     L2TCAS        IFNE      0
     C*
     C     HPART         IFNE      *BLANKS
     C*
     C*  CHECK TO SEE IF EMPLOYER HEADING HAS BEEN PRINTED BEFORE
     C*  PRINT PART CODE DETAIL
     C*
     C     *IN25         IFEQ      '1'
     C                   EXCEPT    EMPTTL
     C                   MOVE      '0'           *IN25
     C                   END
     C*
     C*  CHECK TO SEE IF NATURE CODE HEADING HAS BEEN PRINTED BEFORE
     C*  PRINT PART CODE DETAIL
     C*
     C     *IN24         IFEQ      '1'
     C                   EXCEPT    NATHD
     C                   MOVE      '0'           *IN24
     C                   END
     C                   EXCEPT    PDETL
     C*
     C*  INITIALIZE PART CODE TOTALS
     C*
     C                   EXSR      PTCLR
     C*
     C                   END
     C*
     C                   MOVE      '1'           *INL2
     C                   ADD       L2COMP        L3COMP            9 2
     C                   ADD       L2MED         L3MED             9 2
     C                   ADD       L2OTHR        L3OTHR            9 2
     C                   ADD       L2TOTL        L3TOTL            9 2
     C                   ADD       L2TINC        L3TINC            9 2
     C                   ADD       L2ORES        L3ORES            9 2
     C                   ADD       L2TCAS        L3TCAS            5 0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C     *IN23         IFEQ      '1'
     C     *INOA         OREQ      '1'
     C                   EXCEPT    SUMHD
     C                   MOVE      '0'           *INOA
     C                   MOVE      '0'           *IN23
     C                   END
     C*
     C*  PRINT NATURE CODE TOTAL LINE THEN SET INDICATOR TO PRINT NEXT
     C*  NEW NATURE CODE
     C*
     C                   EXCEPT    NDETL
     C                   MOVE      '1'           *IN24
     C*
     C                   END
     C*
     C*  INITIALIZE NATURE CODE TOTALS
     C*
     C                   EXSR      NTCLR
     C*
     C*
     C* CHECK FOR PAGE OVERFLOW
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    SUMHD
     C                   END
     C*
     C                   GOTO      ENDA
     C                   END
     C*
     C     CMPART        IFNE      HPART
     C     HPART         ANDNE     *BLANK
     C                   MOVE      '1'           *INL1
     C                   ADD       L1COMP        L2COMP            9 2
     C                   ADD       L1MED         L2MED             9 2
     C                   ADD       L1OTHR        L2OTHR            9 2
     C                   ADD       L1TOTL        L2TOTL            9 2
     C                   ADD       L1TINC        L2TINC            9 2
     C                   ADD       L1ORES        L2ORES            9 2
     C                   ADD       L1TCAS        L2TCAS            5 0
     C*
     C*  CHECK FOR PAGE OVERFLOW
     C*
     C     *IN23         IFEQ      '1'
     C     *INOA         OREQ      '1'
     C                   EXCEPT    SUMHD
     C                   MOVE      '0'           *INOA
     C                   MOVE      '0'           *IN23
     C                   END
     C*
     C* MAKE SURE THERE IS A PART CODE TOTAL BEFORE PRINTING
     C*
     C     L1TCAS        IFNE      0
     C*
     C*  CHECK TO SEE IF EMPLOYER HEADING HAS BEEN PRINTED BEFORE
     C*  PRINT PART CODE DETAIL
     C*
     C     *IN25         IFEQ      '1'
     C                   EXCEPT    EMPTTL
     C                   MOVE      '0'           *IN25
     C                   END
     C*
     C*  CHECK TO SEE IF NATURE CODE HEADING HAS BEEN PRINTED BEFORE
     C*  PRINT PART CODE DETAIL
     C*
     C     *IN24         IFEQ      '1'
     C                   EXCEPT    NATHD
     C                   MOVE      '0'           *IN24
     C                   END
     C                   EXCEPT    PDETL
     C*
     C                   END
     C*
     C*  INITIALIZE PART CODE TOTALS
     C*
     C                   EXSR      PTCLR
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    SUMHD
     C                   END
     C                   END
     C*
     C                   END
     C*
     C     ENDA          TAG
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHCO#         ANDEQ     XXCO#
     C     CHFND         ANDEQ     FNFND
     C     CHFYR         ANDEQ     F1FYR
     C     CHCASE        ANDEQ     CMCASE
     C                   ADD       1             L1TCAS            5 0
     C                   ADD       1             PARTL1            5 0
     C                   ADD       1             NATRL1            5 0
     C*
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXRES
     C*
     C*  GET RESERVE TYPE CODE FILE
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'
     C                   MOVE      '1'           *IN20
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   MOVE      '1'           *IN21
     C                   END
     C                   END
     C*
     C                   MOVE      CMPART        HPART             2
     C                   MOVE      CMNATR        HNATR             2
     C*
     C*  CHAIN TO PART CODE AND NATURE CODE FILE TO RETRIEVE DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     CMPART        XXRES
     C     TBKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PADESC        HPRTD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HPRTD
     C                   END
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVEL     CMNATR        XXRES
     C     TBKEY         CHAIN     PNA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     NADESC        HNTRD            30
     C                   ELSE
     C                   MOVE      *BLANKS       HNTRD
     C                   END
     C*
     C   20              ADD       CMA(R)        COMP              9 2
     C   21              ADD       CMA(R)        MED               9 2
     C  N20
     CANN21              ADD       CMA(R)        OTHR              9 2
     C                   ADD       CMR(R)        OUTRES            9 2
     C*
     C                   END
     C*
     C     COMP          ADD       MED           TOTALP            9 2
     C                   ADD       OTHR          TOTALP
     C     TOTALP        ADD       OUTRES        TOTINC            9 2
     C*
     C                   ADD       COMP          L1COMP            9 2
     C                   ADD       MED           L1MED             9 2
     C                   ADD       OTHR          L1OTHR            9 2
     C                   ADD       TOTALP        L1TOTL            9 2
     C                   ADD       TOTINC        L1TINC            9 2
     C                   ADD       OUTRES        L1ORES            9 2
     C*
     C*  EXECUTE SUBROUTINE TO LOAD ARRAYS FOR CAUSE CODE SUMMARY
     C*
     C                   EXSR      CSUM
     C*
     C                   ELSE
     C*
     C     PARTL1        IFEQ      0
     C                   MOVE      *BLANKS       HPART             2
     C                   END
     C     NATRL1        IFEQ      0
     C                   MOVE      *BLANKS       HNATR             2
     C                   END
     C*
     C     L1TCAS        IFEQ      0
     C                   MOVE      '1'           *IN27
     C                   ELSE
     C                   MOVE      '0'           *IN27
     C                   END
     C*
     C                   END
     C*
     C                   GOTO      TAG02
     C*
     C     END           TAG
     C*
     CLR 23              EXCEPT    SUMHD
     CLR                 EXCEPT    FNTTL
     CLR                 EXSR      CDONE
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C                   TIME                    DSDTTM
     C     UPKEY         CHAIN     PUP01                              60
     C* * * INITIALIZE FIELDS
     C                   Z-ADD     *ZERO         X                 1 0
     C                   Z-ADD     *ZERO         Y                 1 0
     C                   Z-ADD     *ZERO         Z                 1 0
     C                   MOVE      *BLANKS       CSM
     C                   MOVE      *BLANKS       CDS
     C                   MOVE      *BLANKS       COMB
     C                   MOVE      *ZEROS        XCDS
     C                   MOVE      *ZEROS        XCOMB
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION  PRINTER FILE 0
     C*
     C                   MOVE      *BLANKS       XXTITL           60
     C                   MOVEL     ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      '1'           *IN11
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN25
     C                   MOVE      '0'           *IN26
     C                   MOVE      '0'           *IN27
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   Load Arrays for Cause Code Summary   Subroutine  ** CSUM    =  *
     C*==================================================================*
     C*
     CSR   CSUM          BEGSR
     C*
     C*  CHECK TO SEE IF CODE IS ALREADY IN ARRAY
     C*
     C                   Z-ADD     1             V                 5 0
     C                   MOVE      '1'           *IN33
     C*
     C     CMCAUS        LOOKUP    CSM(V)                                 33
     C*
     C     *IN33         IFEQ      '0'
     C                   Z-ADD     1             V
     C*
     C*  FIND NEXT AVAILABLE POSITION
     C*
     C     CSM(V)        DOWNE     *BLANKS
     C                   ADD       1             V
     C                   END
     C*
     C*  ADD FIELDS TO THE ARRAY
     C*
     C                   MOVE      CMCAUS        CSM(V)
     C                   MOVE      *ZEROS        XCDS
     C                   Z-ADD     1             XXCNT
     C                   Z-ADD     COMP          XXCOMP
     C                   Z-ADD     MED           XXMED
     C                   Z-ADD     OTHR          XXOTHR
     C                   Z-ADD     TOTALP        XXTOTL
     C                   Z-ADD     TOTINC        XXTINC
     C                   Z-ADD     OUTRES        XXORES
     C*
     C                   MOVEA     XCDS          CDS(V)
     C*
     C*   IF CODE IS IN ARRAY ADD TO EXISTING TOTALS
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        XCDS
     C                   MOVEA     CDS(V)        XCDS
     C                   ADD       1             XXCNT
     C                   ADD       COMP          XXCOMP
     C                   ADD       MED           XXMED
     C                   ADD       OTHR          XXOTHR
     C                   ADD       TOTALP        XXTOTL
     C                   ADD       TOTINC        XXTINC
     C                   ADD       OUTRES        XXORES
     C*
     C                   MOVEA     XCDS          CDS(V)
     C*
     C                   END
     C*
     C* INITIALIZE DETAIL FIELDS
     C*
     C                   Z-ADD     0             COMP
     C                   Z-ADD     0             MED
     C                   Z-ADD     0             OTHR
     C                   Z-ADD     0             TOTALP
     C                   Z-ADD     0             TOTINC
     C                   Z-ADD     0             OUTRES
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   PRINT CAUSE CODE SUMMARY             Subroutine  ** CDONE   =  *
     C*==================================================================*
     C*
     CSR   CDONE         BEGSR
     C*
     C*
     C                   MOVE      '1'           *IN34
     C*
     C* MOVE CODE AND TOTAL ARRAYS TO COMBINATION ARRAY
     C*
     C                   Z-ADD     1             V
     C*
     C     CSM(V)        DOWNE     *BLANKS
     C                   MOVEL     CSM(V)        COMB(V)
     C                   MOVE      CDS(V)        COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C* MOVE COMBINATION ARRAY TO OUTPUT FIELDS AFTER RETRIEVING CAUSE DES
     C*
     C                   EXCEPT    SUMHD
     C*
     C*  SORT THE ARRAY
     C*
     C                   SORTA     COMB
     C*
     C*  FIND THE FIRST RECORD
     C*
     C                   Z-ADD     1             X1                5 0
     C     COMB(X1)      DOWEQ     *BLANKS
     C                   ADD       1             X1
     C                   END
     C*
     C     X1            DOWLE     999
     C                   MOVE      *BLANKS       XCOMB
     C                   MOVEA     COMB(X1)      XCOMB
     C*
     C*  RETRIEVE CAUSE CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXRES
     C                   MOVE      *BLANKS       XXDESC           30
     C*
     C                   MOVE      XCODE         XXCD              2
     C                   MOVEL     XXCD          XXRES
     C     TBKEY         CHAIN     PCA01                              60
     C                   MOVE      CADESC        XXDESC
     C*
     C*  PRINT DETAIL LINE
     C*
     C                   EXCEPT    CASDT
     C                   ADD       1             X1
     C                   END
     C*
     C                   EXCEPT    FNTTL
     C*
     C                   ENDSR
     C*
     C*
     C*==================================================================*
     C*   INITIALIZE PART TOTAL FIELDS         Subroutine  ** PTCLR   =  *
     C*==================================================================*
     C*
     CSR   PTCLR         BEGSR
     C*
     C                   Z-ADD     0             PARTL1
     C                   Z-ADD     0             L1TCAS
     C                   Z-ADD     0             L1COMP
     C                   Z-ADD     0             L1MED
     C                   Z-ADD     0             L1OTHR
     C                   Z-ADD     0             L1TOTL
     C                   Z-ADD     0             L1TINC
     C                   Z-ADD     0             L1ORES
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   INITIALIZE NATURE TOTAL FIELDS       Subroutine  ** NTCLR   =  *
     C*==================================================================*
     C*
     CSR   NTCLR         BEGSR
     C*
     C                   Z-ADD     0             NATRL1
     C                   Z-ADD     0             L2TCAS
     C                   Z-ADD     0             L2COMP
     C                   Z-ADD     0             L2MED
     C                   Z-ADD     0             L2OTHR
     C                   Z-ADD     0             L2TOTL
     C                   Z-ADD     0             L2TINC
     C                   Z-ADD     0             L2ORES
     C*
     C                   ENDSR
     C*
     C*
     C*==================================================================*
     C*   INITIALIZE EMPLOYER TOTALS          Subroutine  ** EMPCLR   =  *
     C*==================================================================*
     C*
     CSR   EMPCLR        BEGSR
     C*
     C                   Z-ADD     0             L3TCAS
     C                   Z-ADD     0             L3COMP
     C                   Z-ADD     0             L3MED
     C                   Z-ADD     0             L3OTHR
     C                   Z-ADD     0             L3TOTL
     C                   Z-ADD     0             L3TINC
     C                   Z-ADD     0             L3ORES
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   INITIALIZE FUND YEAR TOTALS          Subroutine  ** YRCLR   =  *
     C*==================================================================*
     C*
     CSR   YRCLR         BEGSR
     C*
     C                   Z-ADD     0             L4TCAS
     C                   Z-ADD     0             L4COMP
     C                   Z-ADD     0             L4MED
     C                   Z-ADD     0             L4OTHR
     C                   Z-ADD     0             L4TOTL
     C                   Z-ADD     0             L4TINC
     C                   Z-ADD     0             L4ORES
     C*
     C                   ENDSR
     C*
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
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
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
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
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O*   MLS 1/19/88  ADD QUE AND INDICATORS
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O*  HEADING FOR NATURE, PART CAUSE SUMMARY
     O*
     O          E            SUMHD          1 06
     O*
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             115
     O                       DSDATE        Y    196
     O          E            SUMHD          2
     O                       Q1PGM               10
     O                       XXTITL             126
     O                                          190 'PAGE:'
     O                       PAGE1         Z    196
     O          E            SUMHD          1
     O                                           09 'FUND:'
     O                       CMFND         Z     13
     O                       XFNAME              75
     O          E            SUMHD          2
     O              NLRN34                       14 'FUND YEAR:'
     O              NLRN34   XXYR                19
     O              NLRN34   XXCENT              17
     O          E            SUMHD          2
     O               34                          18 'CAUSE CODE SUMMARY'
     O                                          105 'COMPENSATION'
     O                                          123 'MEDICAL'
     O                                          140 'OTHER'
     O                                          157 'TOTAL PAID'
     O                                          175 'TOTAL INCURRED'
     O                                          194 'OUTSTANDING RES'
     O*
     O* NATURE CODE HEADING
     O*
     O          E            NATHD          2
     O                                           17 'NATURE CODE'
     O                       HNATR               21
     O                       HNTRD               54
     O*
     O* PART CODE DETAIL
     O*
     O          E            PDETL          1
     O                                           30 '*  PART CODE'
     O                       HPART               36
     O                       HPRTD               70
     O                       PARTL1        1     82
     O                                           89 'CASES'
     O                       L1COMP        A    107
     O                       L1MED         A    125
     O                       L1OTHR        A    142
     O                       L1TOTL        A    159
     O                       L1TINC        A    176
     O                       L1ORES        A    193
     O                                          194 '*'
     O*
     O* NATURE CODE DETAIL
     O*
     O          E            NDETL       1  2
     O                                            4 '**'
     O                                           17 'NATURE CODE'
     O                       HNATR               21
     O                                           37 'TOTALS'
     O                       NATRL1        1     82
     O                                           89 'CASES'
     O                       L2COMP        A    107
     O                       L2MED         A    125
     O                       L2OTHR        A    142
     O                       L2TOTL        A    159
     O                       L2TINC        A    176
     O                       L2ORES        A    193
     O                                          195 '**'
     O*
     O* EMPLOYER TOTALS
     O*
     O          E            EMPTTL         2
     O               25      EMEMP#        Z     11
     O               25      EMDIV         Z     17
     O               25      XYNAME              57
     O              N25                          13 '***  EMPLOYER'
     O              N25      EMEMP#        Z     22
     O              N25      EMDIV         Z     28
     O              N25                          37 'TOTALS'
     O              N25      L3TCAS        1     82
     O              N25                          89 'CASES'
     O              N25      L3COMP        A    107
     O              N25      L3MED         A    125
     O              N25      L3OTHR        A    142
     O              N25      L3TOTL        A    159
     O              N25      L3TINC        A    176
     O              N25      L3ORES        A    193
     O              N25                         196 '***'
     O          E   N25      EMPTTL         1
     O*
     O* FUND YEAR TOTALS
     O*
     O          E            YRTTL       2  3
     O                                            4 '****'
     O                                           26 'TOTALS FOR FUND YEAR'
     O                       L4TCAS        1     76
     O                                           89 'TOTAL CASES'
     O                       L4COMP        A    107
     O                       L4MED         A    125
     O                       L4OTHR        A    142
     O                       L4TOTL        A    159
     O                       L4TINC        A    176
     O                       L4ORES        A    193
     O*
     O*  FINAL TOTALS
     O*
     O          E    LR      FNTTL       2  2
     O                                           14 'FINAL TOTALS'
     O                       LRTCAS        1     68
     O                                           80 'TOTAL CASES'
     O                       LRCOMP        A    107
     O                       LRMED         A    125
     O                       LROTHR        A    142
     O                       LRTOTL        A    159
     O                       LRTINC        A    176
     O                       LRORES        A    193
     O*
     O*  CAUSE CODE SUMMARY
     O*
     O          E            CASDT          2
     O                                           12 'CAUSE CODE'
     O                       XCODE               16
     O                       XXDESC         B    50
     O                       XCNT          1     82
     O                                           89 'CASES'
     O                       XCOMP         A    107
     O                       XMED          A    125
     O                       XOTHR         A    142
     O                       XTOTL         A    159
     O                       XTINC         A    176
     O                       XORES         A    193
