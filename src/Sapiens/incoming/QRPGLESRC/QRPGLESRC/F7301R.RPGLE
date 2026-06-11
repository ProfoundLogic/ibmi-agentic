     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F7301R  UPDATE FAEOP, FAE1P AND WRITE WARPP   *
     H* DESCRIPTION:                                           *
     H*   1. IT IS RUN FROM THE FEE SCHED BATCH JOB. IT IS USED*
     H*      FOR AUTO CHECK ISSUANCE TO WRITE A PROVIDER CHECK *
     H*      REQUEST AND A BRS CHECK REQUEST.                  *
     H*                                                        *
     H*                                                        *
     H* SUBROUTINES USED:                                      *
     H*                                                        *
     H*   SRLDRP - SUBROUTINE TO LOAD WARPP CONSTANT FIELDS    *
     H*            AND RETRIEVE SEQ #.                         *
     H*                                                        *
     H*   SRPROV - SUBROUTINE TO WRITE A REQUEST FOR PAYMENT   *
     H*            RECORD FOR THE PROVIDER.                    *
     H*                                                        *
     H*   SRNWS  - SUBROUTINE TO WRITE A REQUEST FOR PAYMENT   *
     H*            RECORD FOR THE PPO NETWORK.                 *
     H*                                                        *
     H*   SRBRS  - SUBROUTINE TO WRITE A REQUEST FOR PAYMENT   *
     H*            RECORD FOR THE BRS  FEE  SCHEDULE ENTRY.    *
     H*                                                        *
     H*                                                        *
     H**********************************************************
     H* INDICATORS:                                            *
     H*  50          MAIN DOWHILE LOOP                         *
     H*  60-63       GENERAL CHAINING                          *
     H**********************************************************
     H* REVISIONS:                                             *
     H*                                                        *
     H* 07/06/98 C:94 PROJ: WC051                              *
     H*          FIXED PROBLEM-RPRQST WAS NOT GETTING UPDATED  *
     H*                                                        *
     H* 05/26/98 C:53 PROJ: MC152                              *
     H*          EOMB CHECK MISMATCH                           *
     H*                                                        *
     H* 02/25/97 C:32 PROJ: 31MAD                              *
     H*          COPIED VER FROM MCMOD3                        *
     H*          REMOVED REFERENCE TO NAMBP SYS TYPE 'M'       *
     H**********************************************************
     F*
     FSMCOL     IF   E           K DISK
     F*
     FFMFNL     IF   E           K DISK
     FFAEOL3    UF   E           K DISK
     FFAE1L     UF   E           K DISK
     F*
     F*
     FWMCML1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWARPL     UF A E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PTR01)
     FFAHBL     UF   E           K DISK
     FWMCDL     IF   E           K DISK
     FFZEOP     O    E             DISK
     F                                     RENAME(PEO01:PEO03)
     FNAPBL1    UF   E           K DISK
     F*
     FWMCIL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FNATRL     IF   E           K DISK
     F**** #  WTPSL   IF  E           K        DISK
     F*

     D*
     D*
     D* DATA STRUCTURE FOR BATCH ID NUMBER
     D                 DS
     D  XXBID#                 1     15
     D  X1AST                  1      2
     D  X1BID                  3      7  0
     D  X1SUFX                 8     11
     D*
     D* FILE KEY INFORMATION
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  E120A1                 1     20
     D  X1NWCD                 1      5
     D  X1TYPE                 6      8
     D  X1PNCD                13     14
     D*
     D                 DS
     D  FNT                    1     10
     D                                     DIM(10)                              NET TYPE
     D  F1NT01                 1      1
     D  F1NT02                 2      2
     D  F1NT03                 3      3
     D  F1NT04                 4      4
     D  F1NT05                 5      5
     D  F1NT06                 6      6
     D  F1NT07                 7      7
     D  F1NT08                 8      8
     D  F1NT09                 9      9
     D  F1NT10                10     10
     D*
     D                 DS
     D  FNF                    1     10
     D                                     DIM(10)                              NETWK FEE RATE
     D  F1NF01                 1      1
     D  F1NF02                 2      2
     D  F1NF03                 3      3
     D  F1NF04                 4      4
     D  F1NF05                 5      5
     D  F1NF06                 6      6
     D  F1NF07                 7      7
     D  F1NF08                 8      8
     D  F1NF09                 9      9
     D  F1NF10                10     10
     D                 DS
     D  FNW                    1     50
     D                                     DIM(10)                              NETWK CODES
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D*
     D*
     D                 DS
     D  FNI                    1     90  0
     D                                     DIM(10)                              NET FEIN
     D  F1NI01                 1      9  0
     D  F1NI02                10     18  0
     D  F1NI03                19     27  0
     D  F1NI04                28     36  0
     D  F1NI05                37     45  0
     D  F1NI06                46     54  0
     D  F1NI07                55     63  0
     D  F1NI08                64     72  0
     D  F1NI09                73     81  0
     D  F1NI10                82     90  0
     D                 DS
     D  FNM                    1     30  0
     D                                     DIM(10)                              NET MOD
     D  F1NM01                 1      3  0
     D  F1NM02                 4      6  0
     D  F1NM03                 7      9  0
     D  F1NM04                10     12  0
     D  F1NM05                13     15  0
     D  F1NM06                16     18  0
     D  F1NM07                19     21  0
     D  F1NM08                22     24  0
     D  F1NM09                25     27  0
     D  F1NM10                28     30  0
     D                 DS
     D  FN$                    1     90  2
     D                                     DIM(10)                              NET FEE AMT
     D  F1N$01                 1      9  2
     D  F1N$02                10     18  2
     D  F1N$03                19     27  2
     D  F1N$04                28     36  2
     D  F1N$05                37     45  2
     D  F1N$06                46     54  2
     D  F1N$07                55     63  2
     D  F1N$08                64     72  2
     D  F1N$09                73     81  2
     D  F1N$10                82     90  2
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*  KEY LISTING FOR PROGRAM FILES
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE FOR W/C X-REF #
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*   KEY LIST FOR THE EOMB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE EOMB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE (WORK COMP)
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXWCF
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C*   KEY LIST FOR THE REQUEST FOR PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXWCF
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXSEQ
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C*   KEY LIST FOR THE TABLE FILES
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR THE HBA TRANSACTION FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*   KEY LIST FOR THE PRE-CERT BILLING TRANS FILE
     C*
     C     PBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* KEY LIST FOR THE MEDICAL SERVICES TRANS FILE
     C*
     C     MBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*
     C*   KEY LIST FOR THE WMCIP FILE (WORK COMP)
     C*
     C     CIKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXWCF
     C                   KFLD                    X#PTYP
     C                   KFLD                    X#SYS             1
     C*
     C*   KEY LIST FOR THE FUND AUX
     C*
     C     F1KEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*   KEY LIST FOR THE TR FILE
     C*
     C     TRKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*****************************************************************
     C*  LIKE DEFINES FOR PROGRAM VARIABLES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    FNWCF#        XXWCF
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    RPSEQ#        XXSEQ
     C     *LIKE         DEFINE    EOAST         XXAST
     C     *LIKE         DEFINE    EONDIS        XXDISC
     C     *LIKE         DEFINE    EOAMT         XXREDC
     C     *LIKE         DEFINE    EOAMT         XEAMT
     C     *LIKE         DEFINE    EONDIS        XENDIS
     C     *LIKE         DEFINE    EOTRDT        XNBCDT
     C     *LIKE         DEFINE    RPAMT         XXAMT
     C*
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      *BLANK        XXPAGE            1            PRINT SUM PAGE
     C                   MOVE      *BLANK        XXQSYS            1            PRINT EDIT
     C                   Z-ADD     0             XXREDC
     C                   Z-ADD     0             XEAMT
     C                   Z-ADD     0             XENDIS
     C                   MOVE      *ZEROS        XPCNT             5 0          PROVIDER COUNT
     C                   MOVE      *ZEROS        XPAMT            11 2          PROVIDER AMT
     C                   MOVE      *ZEROS        XQCNT             5 0          BRS  COUNT
     C                   MOVE      *ZEROS        XQAMT            11 2          BRS  AMT
     C                   MOVE      *ZEROS        XFCNT             5 0          NETWORK COUNT
     C                   MOVE      *ZEROS        XFAMT            11 2          NETWORK AMT
     C                   Z-ADD     0             XXHCNT            5 0          HEADER CNT
     C                   Z-ADD     0             XXDCNT            9 0          DETL CNT
     C                   Z-ADD     0             XXDELT            9 0          DELETED CNT
     C                   MOVE      *ALL'-'       XXLPRT           70
     C*
     C*****************************************************************
     C*  PROGRAM ENTRY PARAMETERS
     C*****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQPRTY            1
     C                   PARM                    QQMSYS            1
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQEOB#        XXEOB#
     C                   MOVE      QQMSYS        MSYS              1
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*****************************************************************
     C*  PROGRAM MAIN LINE
     C*****************************************************************
     C*
     C* USE THE FAEOP FILE FOR PROCESSING
     C*
     C     *LOVAL        SETLL     PEO01
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     E1KEY         CHAIN     PE101                              61
     C*
     C                   MOVE      *BLANKS       XXNWCD                         ADD 02/25/97
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             B                 3 0
     C     5             SUBST     E120A1:B      XXNWCD            5
     C                   END
     C*
     C     F1KEY         CHAIN     PF101                              64
     C*
     C*
     C*
     C*   CHAIN TO GET W/C X-REF FUND NUMBER
     C*
     C     FNKEY         CHAIN     PFN01                              62
     C                   MOVE      *BLANKS       XXWCF                          ADD 02/25/97
     C     *IN62         IFEQ      '0'
     C                   Z-ADD     FNWCF#        XXWCF
     C*
     C*   CHAIN TO GET W/C CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              63
     C     *IN63         IFEQ      '0'
     C*
     C*
     C                   EXSR      SRLDRP
     C                   EXSR      SRPROV
     C                   EXSR      SRBRS
     C     XXNWCD        IFNE      *BLANKS                                      ADD 02/25/97
     C                   EXSR      SRNWS                                        ADD 02/25/97
     C                   ENDIF                                                  ADD 02/25/97
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1DT
     C                   MOVE      Q1PGM         E1PGM
     C     *IN61         IFEQ      '0'
     C                   UPDATE    PE101
     C                   END                                                    *IN61 = '0'
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVE      Q1PGM         EOPGM
     C                   UPDATE    PEO01
     C*
     C                   END                                                    *IN63 = '0'
     C                   END                                                    *IN62 = '0'
     C*
     C                   READ      PEO01                                  60
     C                   END                                                    END *IN60 EQ '0'
     C                   GOTO      TERM
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C*  SRLDRP - SUBROUTINE TO LOAD CONSTANT REQUEST FOR PAYMENT FIELDS
     C*****************************************************************
     C*
     C     SRLDRP        BEGSR
     C*
     C*
     C*  GET NEXT AVAILABLE SEQUENCE NUMBER
     C*
     C                   Z-ADD     99999         XXSEQ
     C     RPKEY         SETGT     PRP01
     C                   READP     PRP01                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             XXSEQ
     C                   ELSE
     C                   EXCEPT    PRP01E
     C     RPFYR         COMP      EOFYR                              6060
     C  N60RPCASE        COMP      EOCASE                             6060
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             XXSEQ
     C                   ELSE
     C     RPSEQ#        ADD       1             XXSEQ
     C                   END
     C                   END
     C*
     C*  LOAD FIELDS
     C*
     C                   MOVE      EOCO#         RPCO#
     C                   MOVE      FNWCF#        RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   MOVE      EOCASE        RPCASE
     C                   Z-ADD     EOTRDT        RPTRDT
     C     CMLTFL        IFEQ      'Y'
     C                   MOVE      'LT'          RPBR7
     C                   ELSE
     C                   MOVE      'NL'          RPBR7
     C                   END
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   MOVE      *ZEROS        RPWKS
     C                   MOVE      *ZEROS        RPDAYS
     C                   MOVE      'P'           RPTYPE
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C                   MOVE      *BLANKS       RPDESC
     C                   MOVE      *BLANKS       RPCKCN
     C*
     C                   MOVE      'P'           RPSTAT
     C                   MOVE      EOUSER        RPUSER
     C*** BEG ADD *** 07/06/98 *************************************
     C                   MOVE      RPUSER        RPRQST
     C*** END ADD *** 07/06/98 *************************************
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVE      *ZEROS        RPBUL1
     C                   MOVE      *ZEROS        RPCHK#
     C                   Z-ADD     EOEOB#        RPEOB#
     C*
     C*  LOAD TRANSACTION CODE AND RESERVE CODE AND TYPE
     C*
     C                   MOVE      EOTRCD        RPTRCD
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPDESC
     C                   END
     C*
     C* LOAD RESERVE TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   END
     C*
     C*  LOAD PAYMENT VALUE #1 CODE AND ADD NUMBER OF DAYS FOR DUE
     C*  DATE
     C*
     C     EOSYS         IFEQ      'H'
     C                   MOVE      'HP'          RPBR2
     C                   ELSE
     C                   MOVE      'MD'          RPBR2
     C                   END
     C     RPRETP        IFEQ      'E'
     C                   MOVE      'EX'          RPBR2
     C                   END
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  SRPROV - SUBROUTINE TO LOAD PROVIDER PAYMENT REQUEST
     C*****************************************************************
     C*
     C     SRPROV        BEGSR
     C*
     C     EOSYS         IFNE      'U'
     C     EOSYS         ANDNE     'M'
     C*
     C*  LOAD PROVIDER FEIN/MOD AND ADDRESS
     C*
     C                   MOVE      EOFEIN        RPFEIN
     C                   MOVE      EOMOD         RPMOD
     C                   MOVEL     EOPNAM        RPPNAM
     C                   MOVEL     EOADR1        RPADR1
     C                   MOVEL     EOADR2        RPADR2
     C                   MOVEL     EOCTY         RPCTY
     C                   MOVE      EOST          RPST
     C                   MOVE      EOZIP         RPZIP
     C*
     C                   MOVEL     'P'           X#PTYP            1
     C                   MOVE      EOSYS         X#SYS
     C                   CLEAR                   PCI01
     C     CIKY          CHAIN     PCI01                              60
     C*
     C     E1FLG3        IFEQ      'X'
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C*
     C*
     C*  LOAD TRANSACTION CODE AND RESERVE CODE AND TYPE
     C*
     C                   MOVE      EOTRCD        RPTRCD
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPDESC
     C                   END
     C*
     C* LOAD RESERVE TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   END
     C*
     C* LOAD REMARK CODES
     C*
     C                   MOVEL     EORMDS        RPRMDS
     C                   MOVE      EORMK1        RPRMK1
     C                   MOVE      EORMK2        RPRMK2
     C                   MOVE      EORMK3        RPRMK3
     C*
     C* LOAD STS CODE AND SORT VALUE
     C*
     C*
     C                   MOVE      *BLANKS       XXKEY
     C*
     C* LOAD SEQUENCE NUMBER AND CHECK TYPE
     C*
     C                   Z-ADD     XXSEQ         RPSEQ#
     C                   MOVE      ' '           RPPGTP
     C*
     C*  CALCULATE PROVIDER AMOUNT
     C*
     C     EOAMT         SUB       EONDIS        XXAMT
     C     XXAMT         SUB       E1AMT3        RPAMT
     C*
     C                   MOVE      EOTRDT        RPTRDT
     C*
     C                   MOVE      *BLANKS       RPBR7
     C*
     C     CMLTFL        IFEQ      'Y'
     C                   MOVE      CIBR7L        RPBR7
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'N'
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   MOVE      EOFRDT        RPFRDT
     C                   MOVE      EOTODT        RPTODT
     C                   MOVEL     'P'           RPTYPE
     C                   MOVE      EOEOB#        RPEOB#
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'N'
     C                   ENDIF
     C*
     C                   MOVEL     'P'           RPSTAT
     C                   MOVE      *ZEROS        RPWKS
     C                   MOVE      *ZEROS        RPDAYS
     C                   MOVE      *BLANKS       RPCKCN
     C                   MOVE      EOUSER        RPUSER
     C*** BEG ADD *** 07/06/98 *************************************
     C                   MOVE      RPUSER        RPRQST
     C*** END ADD *** 07/06/98 *************************************
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   MOVE      *ZEROS        RPBUL1
     C                   MOVE      *ZEROS        RPCHK#
     C*
     C*
     C*  WRITE REQUEST FOR PAYMENT RECORD ONLY IF RPAMT
     C*   GT ZERO
     C*
     C     RPAMT         IFGT      0
     C*
     C                   WRITE     PRP01
     C*
     C*  INCREMENT THE SEQUENCE NUMBER
     C*
     C                   ADD       1             XXSEQ
     C*
     C* INCREASE PROVIDER COUNT AND AMOUNT FOR SUMMARY PAGE
     C*
     C                   ADD       1             XPCNT
     C                   ADD       RPAMT         XPAMT
     C                   END
     C*
     C*** EOMB CHECK MISMATCH TRACKING
     C/COPY SCPYCMF73P
     C                   ENDIF                                                  EOSYS NE U/M
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  SRNWS  - SUBROUTINE TO LOAD NETWORK GROUP PAYMENT REQUEST
     C*****************************************************************
     C*
     C     SRNWS         BEGSR
     C*
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EOSYS         OREQ      'N'
     C*
     C*** LOAD  FEIN/MOD
     C                   Z-ADD     1             B                 3 0
     C     XXNWCD        LOOKUP    FNW(B)                                 60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C     FNF(B)        IFEQ      'I'
     C*** BEGIN CHG *** 02/25/97 ***********************************************
     C**********           MOVEL'N'       X#PTYP  1
     C                   MOVEL     'B'           X#PTYP            1
     C*** END CHG *** 02/25/97 *************************************************
     C                   MOVE      'N'           X#SYS
     C                   CLEAR                   PCI01
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'                                          ADD 02/25/97
     C                   MOVE      CIFEIN        RPFEIN
     C                   MOVE      CIMOD         RPMOD
     C                   ELSE                                                   ADD 02/25/97
     C                   Z-ADD     *ZEROS        RPFEIN                         ADD 02/25/97
     C                   Z-ADD     *ZEROS        RPMOD                          ADD 02/25/97
     C                   ENDIF                                                  ADD 02/25/97
     C*** USE NATRP/TRBAMT
     C     TRKY          CHAIN     PTR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      TRBAMT        RPAMT
     C                   ELSE
     C                   MOVE      *ZEROS        RPAMT
     C                   ENDIF
     C*
     C*
     C                   ELSE                                                   <> 'I'
     C     FNF(B)        IFEQ      'Y'
     C                   MOVE      FNI(B)        RPFEIN
     C                   MOVE      FNM(B)        RPMOD
     C**** USE  E1AMT1
     C                   MOVE      E1AMT1        RPAMT
     C*
     C                   ELSE
     C     FNF(B)        IFEQ      'N'
     C                   MOVE      FNI(B)        RPFEIN
     C                   MOVE      FNM(B)        RPMOD
     C**** #               MOVEL' '       RPNTWK
     C**** CALC BASED ON NT AND N$
     C     FNT(B)        IFEQ      'B'
     C                   MOVE      FN$(B)        RPAMT
     C                   ELSE
     C     FNT(B)        IFEQ      'S'
     C     FN$(B)        MULT      EONDIS        RPAMT
     C                   ELSE
     C                   MOVE      *ZERO         RPAMT
     C                   ENDIF                                                  NT=S
     C                   ENDIF                                                  NT=B
     C***
     C                   MOVE      RPAMT         E1AMT1
     C                   ENDIF                                                  FNF,B = 'N'
     C                   ENDIF                                                  FNF,B = 'Y'
     C                   ENDIF                                                  FNF,B = 'I'
     C                   ELSE
     C                   MOVE      *ZEROS        RPMOD
     C                   MOVE      *ZEROS        RPFEIN
     C                   MOVE      *ZEROS        RPAMT
     C                   ENDIF                                                  FOUND FNW
     C*
     C                   MOVEL     'N'           X#PTYP            1
     C                   MOVE      EOSYS         X#SYS
     C                   CLEAR                   PCI01
     C     CIKY          CHAIN     PCI01                              60
     C*
     C*
     C*   LOAD PROVIDER NAME AND ADDRESS FROM WMPDP FILE
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C*
     C*
     C*
     C*
     C*
     C*  LOAD PAYMENT VALUE CODES
     C*
     C                   MOVE      CIBR2         RPBR2
     C*
     C*  LOAD TRANSACTION CODE AND RESERVE CODE AND TYPE
     C*
     C                   MOVE      CITRCD        RPTRCD
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPDESC
     C                   END
     C*
     C* LOAD RESERVE TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   END
     C*
     C* LOAD REMARK CODES
     C*
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C* LOAD STS CODE AND SORT VALUE
     C*
     C*
     C*
     C                   MOVE      *BLANKS       XXKEY
     C*
     C*
     C* LOAD SEQUENCE NUMBER AND CHECK TYPE
     C*
     C                   Z-ADD     XXSEQ         RPSEQ#
     C                   MOVE      'G'           RPPGTP
     C*
     C                   MOVE      'P'           RPSTAT
     C                   MOVEL     'P'           RPTYPE
     C                   MOVE      EOEOB#        RPEOB#
     C*
     C*
     C                   MOVE      EOTRDT        RPTRDT
     C*
     C                   MOVE      *ZEROS        RPWKS
     C                   MOVE      *ZEROS        RPDAYS
     C                   MOVE      *BLANKS       RPCKCN
     C                   MOVE      EOUSER        RPUSER
     C*** BEG ADD *** 07/06/98 *************************************
     C                   MOVE      RPUSER        RPRQST
     C*** END ADD *** 07/06/98 *************************************
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   MOVE      *ZEROS        RPBUL1
     C                   MOVE      *ZEROS        RPCHK#
     C     CMLTFL        IFEQ      'Y'
     C                   MOVEL     CIBR7L        RPBR7
     C                   ENDIF
     C     CMLTFL        IFEQ      'N'
     C                   MOVEL     CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVEL     CMNCCI        RPNCCI
     C*
     C                   MOVE      EOFRDT        RPFRDT
     C                   MOVE      EOTODT        RPTODT
     C*
     C*
     C*
     C*  WRITE REQUEST FOR PAYMENT RECORD ONLY IF RPAMT
     C*   GT ZERO
     C*
     C     RPAMT         IFGT      0
     C*
     C                   WRITE     PRP01
     C*
     C*  INCREMENT THE SEQUENCE NUMBER
     C*
     C                   ADD       1             XXSEQ
     C*
     C* INCREASE COUNT AND AMOUNT FOR SUMMARY PAGE
     C*
     C                   ADD       1             XFCNT
     C                   ADD       RPAMT         XFAMT
     C*
     C                   END                                                    RPAMT GT 0
     C*** EOMB CHECK MISMATCH TRACKING
     C/COPY SCPYCMF73N
     C*
     C* WRITE RECORD TO EOMB RE-PRINT WORKFILE IF HAS PROCESSED
     C* (I.E. NETWORK DISCOUNT TAKEN)
     C*
     C     EONDIS        IFNE      *ZEROS
     C* CHAIN OUT TO THE W/C DEMOGRAPHIC FILE TO GET THE CLAIMANTS NAME
     C*
     C     CMSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         EOLSN
     C                   MOVEL     CDFSN         EOFSN
     C                   MOVEL     CDMID         EOMID
     C                   MOVEL     CDSS#         EOSS#
     C*
     C*  WRITE TO THE EOMB RE-PRINT WORK FILE
     C*
     C                   WRITE     PEO03
     C*
     C                   END
     C*
     C                   END                                                    END EONDIS
     C*
     C                   END                                                    PDKEY
     C*
     C                   ENDIF                                                  EOSYS VALID
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  SRBRS  - SUBROUTINE TO LOAD BRS  GROUP PAYMENT REQUEST
     C*****************************************************************
     C*
     C     SRBRS         BEGSR
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EOSYS         OREQ      'U'
     C     EOSYS         OREQ      'M'
     C*
     C*
     C                   MOVEL     'B'           X#PTYP            1
     C                   MOVE      EOSYS         X#SYS
     C                   CLEAR                   PCI01
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'                                          ADD 02/25/97
     C                   MOVE      CIFEIN        RPFEIN
     C                   MOVE      CIMOD         RPMOD
     C                   ELSE                                                   ADD 02/25/97
     C                   Z-ADD     *ZEROS        RPFEIN                         ADD 02/25/97
     C                   Z-ADD     *ZEROS        RPMOD                          ADD 02/25/97
     C                   ENDIF                                                  ADD 02/25/97
     C*
     C*
     C*   LOAD PROVIDER NAME AND ADDRESS FROM WMPDP FILE
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C*
     C*  IF THIS IS AN EXPENSE PAYMENT, THE PAYMENT VALUE, TRANSACTION,
     C*  AND RESERVE CODES ARE THE SAME AND DO NOT NEED LOADED HERE.
     C*
     C*
     C*  LOAD PAYMENT VALUE CODES
     C*
     C                   MOVEL     CIBR2         RPBR2
     C*
     C     CMLTFL        IFEQ      'Y'
     C                   MOVEL     CIBR7L        RPBR7
     C                   ENDIF
     C     CMLTFL        IFEQ      'N'
     C                   MOVEL     CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVEL     CMNCCI        RPNCCI
     C*
     C                   MOVE      EOFRDT        RPFRDT
     C                   MOVE      EOTODT        RPTODT
     C*
     C*
     C*  LOAD TRANSACTION CODE AND RESERVE CODE AND TYPE
     C*
     C                   MOVE      CITRCD        RPTRCD
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPDESC
     C                   END
     C*
     C* LOAD RESERVE TYPE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   END
     C*
     C* LOAD REMARK CODES
     C*
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C* LOAD PAYMENT CODE
     C*
     C     EOSYS         IFEQ      'U'
     C     EOSYS         OREQ      'M'
     C                   MOVEL     CITRCD        RPTRCD
     C                   MOVEL     CITRCD        EOTRCD
     C                   ENDIF
     C*
     C*
     C                   MOVE      *BLANKS       XXKEY
     C*
     C* LOAD SEQUENCE NUMBER AND CHECK TYPE
     C*
     C                   Z-ADD     XXSEQ         RPSEQ#
     C                   MOVE      'G'           RPPGTP
     C*
     C*  LOAD BRS  AMOUNT
     C*
     C     EOSYS         IFEQ      'P'
     C                   Z-ADD     EOBAMT        RPAMT
     C                   ELSE                                                   ELSE EOSYS
     C*   GET PRE-CERT AMOUNT
     C*
     C     EOSYS         IFEQ      'U'
     C     PBKEY         CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PPB01E
     C                   Z-ADD     PBBAMT        RPAMT
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   Z-ADD     *ZEROS        RPAMT
     C                   END                                                    END *IN60
     C                   END                                                    END EOSYS=U
     C*
     C     EOSYS         IFEQ      'H'
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PHB01E
     C                   Z-ADD     HB1AAM        RPAMT
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   Z-ADD     *ZEROS        RPAMT
     C                   END                                                    END *IN60
     C                   END                                                    END EOSYS=H
     C                   END                                                    END EOSYS=P
     C*
     C                   MOVE      'P'           RPSTAT
     C                   MOVEL     'P'           RPTYPE
     C                   MOVE      EOEOB#        RPEOB#
     C*
     C*
     C                   MOVE      EOTRDT        RPTRDT
     C*
     C                   MOVE      *ZEROS        RPWKS
     C                   MOVE      *ZEROS        RPDAYS
     C                   MOVE      *BLANKS       RPCKCN
     C                   MOVE      EOUSER        RPUSER
     C*** BEG ADD *** 07/06/98 *************************************
     C                   MOVE      RPUSER        RPRQST
     C*** END ADD *** 07/06/98 *************************************
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   MOVE      *ZEROS        RPBUL1
     C                   MOVE      *ZEROS        RPCHK#
     C*
     C*
     C*
     C*  WRITE REQUEST FOR PAYMENT RECORD  IF RPAMT GT ZERO
     C*
     C     RPAMT         IFGT      0
     C*
     C                   WRITE     PRP01
     C*
     C*  INCREMENT THE SEQUENCE NUMBER
     C*
     C                   ADD       1             XXSEQ
     C*
     C* INCREASE EMRE COUNT AND AMOUNT FOR SUMMARY PAGE
     C*
     C                   ADD       1             XQCNT
     C                   ADD       RPAMT         XQAMT
     C*
     C                   END                                                    RPAMT GT 0
     C*** EOMB CHECK MISMATCH TRACKING
     C/COPY SCPYCMF73B
     C*
     C                   END                                                    PDKEY
     C*
     C                   ENDIF                                                  VALID EOSYS
     C*
     C                   ENDSR
     C*
     OPRP01     E            PRP01E
     OPHB01     E            PHB01E
     OPPB01     E            PPB01E
