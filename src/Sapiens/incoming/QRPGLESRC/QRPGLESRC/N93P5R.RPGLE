     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE N93P5R - TYPE P5 BILLING TYPE FORMULA TO CALC BILL AMT
     H*
     H*****************************************************************
     H*  P5 Billing Calculation:                                      *
     H*  1.  With "P" System type, look up Group's Billing code       *
     H*  2.  BDPC01 * MRA = MRA discount amount (EAPCMR)          EA  *
     H*  3.  BDPC02 *(Other 1 + Other 2)= Other discount amount (PCOT)*
     H*  4.  BDPC03 * NDIS = Network discount amount (EAPCNW)         *
     H*  5.  BDAM01 = Flat rate per bill amount (PCFLBL)              *
     H*  6.  BDAM02 = Flat rate for first line (XXLIN1)               *
     H*  7.  BDAM03*(#Lines - 1) = Flat rate for addtl lines (XXLIN2) *
     H*  8.  EAPCMR + EAPCOT + EAPCNW + EAFLBL + EAFLLN = @@BAMT      *
     H*  9.  Update FAEAP with amounts                                *
     H*****************************************************************
     H* Created on 02/19/2002 by C:62 for Project 48MEZ               *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*****************************************************************
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFAEAL1    UF   E           K DISK
     FFDCAL13   IF   E           K DISK
     F*

     D*** FILE ARRAYS
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              FILE SYSTEM CODE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D                 DS
     D  FBR                    1     30
     D                                     DIM(10)                              FILE BILL/RATE CD
     D  FNBR01                 1      3
     D  FNBR02                 4      6
     D  FNBR03                 7      9
     D  FNBR04                10     12
     D  FNBR05                13     15
     D  FNBR06                16     18
     D  FNBR07                19     21
     D  FNBR08                22     24
     D  FNBR09                25     27
     D  FNBR10                28     30
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C* Parameter Lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@EOB#            9
     C                   PARM                    @@BAMT            9
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@RCN             3
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
     C*****************************************************************
     C* Key Lists
     C*****************************************************************
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C*
     C     BDKY2         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKDATE
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EACASE        KKCASE
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    FNDT          KKDATE
     C*****************************************************************
     C* Initialization Section
     C*****************************************************************
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@EOB#        KKEOB#
     C                   MOVE      @@RCN         KKRCN
     C                   Z-ADD     *ZEROS        XXBAMT            9 2
     C                   Z-ADD     *ZEROS        XXLIN1            9 2
     C                   Z-ADD     *ZEROS        XXLIN2            9 2
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE
     C*
     C*** Get Current 'P5' type record for group
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     1             X                 5 0
     C     'P'           LOOKUP    FSY(X)                                 99
     C     *IN99         IFEQ      *ON
     C*
     C* Get Correct Billing Code
     C*
     C                   MOVEL     FBR(X)        KKCODE
     C     BDKY2         SETGT     PBD01
     C     BDREAD        TAG
     C     BDKY1         READPE    PBD01                                  99
     C     *IN99         IFEQ      *OFF
     C     BDSTAT        CABEQ     'I'           BDREAD
     C*
     C* Lets process the bill
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* MRA %
     C*
     C     EAMRA         MULT(H)   BDPC01        EAPCMR
     C*
     C* Other %
     C*
     C     EAOTHR        ADD       EAOTH2        XXOTHR            9 2
     C     XXOTHR        MULT(H)   BDPC02        EAPCOT
     C*
     C* Network %
     C*
     C     EANDIS        MULT(H)   BDPC03        EAPCNW
     C*
     C* Flat Rate Per Bill
     C*
     C                   Z-ADD     BDAM01        EAFLBL
     C*
     C* Find out the number of lines
     C*
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             COUNT
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C* If the line count is greater than zero then calcualte line amt.
     C*
     C     COUNT         IFGT      *ZEROS
     C*
     C* Assign 1st Line Value
     C*
     C                   Z-ADD     BDAM02        XXLIN1
     C*
     C* Assign Additional Line Item Value
     C*
     C                   SUB       1             COUNT
     C     COUNT         MULT(H)   BDAM03        XXLIN2
     C                   ENDIF
     C*
     C     XXLIN1        ADD       XXLIN2        EAFLLN
     C*
     C     EAPCMR        ADD       EAPCOT        XXBAMT
     C                   ADD       EAPCNW        XXBAMT
     C                   ADD       EAFLBL        XXBAMT
     C                   ADD       EAFLLN        XXBAMT
     C                   MOVE      XXBAMT        @@BAMT
     C*
     C* Update FAEAP
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   SETON                                        LR
