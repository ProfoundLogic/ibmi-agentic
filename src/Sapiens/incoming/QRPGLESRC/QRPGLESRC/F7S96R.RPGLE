     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7096R - CASE TRANSFER/DELETION PROGRAM
     H*****************************************************************
     H* DESCRIPTION: CASE TRANSFER/DELETION - WRITE ALL FILES
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     H* *IN98 = *OFF - @@FLAG = 'X' (SINGLE CASE TRANSFER)
     H* *IN98 = *ON  - @@FLAG <> 'X' (DOUBLE CASE TRANSFER)
     H*****************************************************************
     FFMCML1    UF   E           K DISK
     F*
     FFMCHL1    UF   E           K DISK
     F*
     FNMARLA    UF   E           K DISK
     F*
     FNMIML2    UF   E           K DISK
     F*
     FFADAL     UF   E           K DISK
     F*
     FFAMSL8    UF   E           K DISK
     F*
     FFAMSL3    IF   E           K DISK
     F                                     RENAME(PMS01:PMS02)
     F*
     FFAPPL1    UF   E           K DISK
     F*
     FFATXL     UF   E           K DISK
     F*
     FFAWHL     UF   E           K DISK
     F*
     FFATRL     UF   E           K DISK
     F*
     FFATDL     UF   E           K DISK
     F*
     FFATIL     UF   E           K DISK
     F*
     FFAERL     UF   E           K DISK
     F*
     FFALTL     UF   E           K DISK
     F*
     FUMPCL11   UF   E           K DISK
     F*
     FUMCHL1    UF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     F*
     FUDPOL     UF   E           K DISK
     F*
     FNDTML6    UF   E           K DISK
     F*
     FFDOSL2    UF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     IPCH02
     I              CHCO#                       C2CO#
     I              CHSUB#                      C2SUB#
     I              CHFND                       C2FND
     I              CHFYR                       C2FYR
     I              CHCASE                      C2CASE
     I              CHUSER                      C2USER
     I              CHTIME                      C2TIME
     I              CHPGM                       C2PGM
     I              CHDT                        C2DT
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FFYR            3
     C                   PARM                    @@TFYR            3
     C                   PARM                    @@FLAG            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    MSMSCD
     C                   KFLD                    SVSEQ#
     C*
     C     GENKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     GENKY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    KKFND
     C                   KFLD                    K1FYR
     C                   KFLD                    KKCASE
     C*
     C     OSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C                   KFLD                    SVSEQ#
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMPGM         SVFILE
     C     *LIKE         DEFINE    CMPGM         OOFILE
     C     *LIKE         DEFINE    TRTX#         XXTX#
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMCO#         K1CO#
     C     *LIKE         DEFINE    CMSUB#        K1SUB#
     C     *LIKE         DEFINE    CMFND         K1FND
     C     *LIKE         DEFINE    CMFYR         K1FYR
     C     *LIKE         DEFINE    CMCASE        K1CASE
     C     *LIKE         DEFINE    CMSS#         K1SS#
     C     *LIKE         DEFINE    TMMSEQ        KKSEQ#
     C     *LIKE         DEFINE    OSMSCD        KKMSCD
     C     *LIKE         DEFINE    OSSEQ#        KKMSEQ
     C     *LIKE         DEFINE    MSSEQ#        SVSEQ#
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @#WCIF            1            WC INST
     C                   PARM                    @#MCIF            1            MC INST
     C                   PARM                    @#WCIN            1            WC INTERFACE
     C                   PARM                    @#AR              1            MC AR INST
     C                   PARM                    @#NTWK            1            NETWORK INST
     C                   PARM                    @#UR              1            PRECERT INST
     C                   PARM                    @#MED             1            MEDMGMT INST
     C                   PARM                    @#EDI             1            EDI INST
     C                   PARM                    @#WCDK            1            WC DARTS KEY
     C                   PARM                    @#RHAB            1            REHAB INST
     C                   PARM                    @#MSS             1            MS SUPPORT
     C                   PARM                    @#FSCH            1            FEE SCHEDULE
     C                   PARM                    @#PRVB            1            PROVIDER BILLS
     C                   MOVE      *ON           *INOA
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         K1SUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FFYR        KKFYR
     C                   MOVE      @@TFYR        K1FYR
     C                   MOVE      @@CASE        KKCASE
     C*
     C     @@FLAG        IFEQ      'X'
     C                   MOVE      *OFF          *IN98
     C                   ELSE
     C                   MOVE      *ON           *IN98
     C                   ENDIF
     C*
     C                   EXSR      MOVEFI
     C*
     C                   MOVE      *BLANK        QQRTCD
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C                   MOVE      KKFYR         #1FYR
     C                   MOVE      KKCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    @#WCIN            1
     C                   PARM                    @#WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY1           40
TEMP C                   PARM                    @@PGM            10
     C*
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      K1SUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C                   MOVE      K1FYR         #1FYR
     C                   MOVE      KKCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    @#WCIN            1
     C                   PARM                    @#WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY2           40
TEMP C                   PARM                    @@PGM            10
     C*
     C                   CALL      'SC900C'                             99
     C                   PARM      ##KEY1        QFKEY            40
     C                   PARM      ##KEY2        QTKEY            40
     C                   PARM      *BLANKS       QQQUE            10
     C                   PARM      'B'           QQMODE            1
     C                   PARM      ##RTCD        QQRTCD            1
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* MOVEFI - FILES THAT ARE BEING UPDATED
     C*================================================================
     C     MOVEFI        BEGSR
     C*
     C*** FILE FMCMP
     C*
     C*
     C     GENKY         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        CMSUB#
     C                   Z-ADD     K1FYR         CMFYR
     C                   ENDIF
     C*
     C*** FILE FMCHP
     C*
     C     GENKY         SETLL     PCH01
     C     GENKY         READE     PCH01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        CHSUB#
     C                   Z-ADD     K1FYR         CHFYR
     C                   UPDATE    PCH01
     C     GENKY         READE     PCH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NMIMP
     C*
     C     GENKY         SETLL     PIM01
     C     GENKY         READE     PIM01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        IMSUB#
     C                   Z-ADD     K1FYR         IMFYR
     C                   UPDATE    PIM01
     C     GENKY         READE     PIM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NMARP
     C*
     C     GENKY         SETLL     PAR01
     C     GENKY         READE     PAR01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        ARSUB#
     C                   Z-ADD     K1FYR         ARFYR
     C                   UPDATE    PAR01
     C     GENKY         READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FADAP
     C*
     C     GENKY         SETLL     PDA01
     C     GENKY         READE     PDA01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        DASUB#
     C                   Z-ADD     K1FYR         DAFYR
     C                   UPDATE    PDA01
     C     GENKY         READE     PDA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAMSP
     C*
     C     GENKY1        SETGT     PMS02
     C     GENKY1        READPE    PMS02                                  99
     C     *IN99         IFEQ      '0'
     C   98              Z-ADD     MSSEQ#        HLSEQ#
     C                   ELSE
     C                   Z-ADD     0             HLSEQ#            5 0
     C                   ENDIF
     C*
     C     GENKY         CHAIN     PMS01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        MSSUB#
     C                   Z-ADD     K1FYR         MSFYR
     C                   Z-ADD     MSSEQ#        SVSEQ#
     C   98              ADD       1             HLSEQ#
     C   98              Z-ADD     HLSEQ#        MSSEQ#
     C   98              MOVE      HLSEQ#        #@SEQ#            5
     C*
     C                   UPDATE    PMS01
     C**************************************************************
     C*** FILE FDOSP
     C**************************************************************
     C                   MOVEL(P)  MSMSCD        KKMSCD
     C*
     C     OSKY          CHAIN     POS01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        OSSUB#
     C                   Z-ADD     K1FYR         OSFYR
     C                   MOVE      MSMSCD        OSMSCD
     C                   MOVE      MSSEQ#        OSSEQ#
     C                   UPDATE    POS01
     C     OSKY          READE     POS01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*
     C**************************************************************
     C*** FILE NDTMP
     C**************************************************************
     C     TMKY          SETLL     PTM01
     C     TMKY          READE     PTM01                                  99
     C     *IN99         DOWEQ     *OFF
     C   98              Z-ADD     HLSEQ#        TMMSEQ
     C                   UPDATE    PTM01
     C     TMKY          READE     PTM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C     GENKY         READE     PMS01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAPPP
     C*
     C     GENKY         SETLL     PPP01
     C     GENKY         READE     PPP01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        PPSUB#
     C                   Z-ADD     K1FYR         PPFYR
     C                   UPDATE    PPP01
     C     GENKY         READE     PPP01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FATXP
     C*
     C     GENKY         SETLL     PTX01
     C     GENKY         READE     PTX01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        TXSUB#
     C                   Z-ADD     K1FYR         TXFYR
     C                   UPDATE    PTX01
     C     GENKY         READE     PTX01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAWHP
     C*
     C     GENKY         SETLL     PWH01
     C     GENKY         READE     PWH01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        WHSUB#
     C                   Z-ADD     K1FYR         WHFYR
     C                   UPDATE    PWH01
     C     GENKY         READE     PWH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FATRP
     C*
     C     *IN98         IFEQ      *ON
     C     GENKY1        SETGT     PTR01
     C     GENKY1        READPE    PTR01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     TRTX#         XXTX#
     C                   ELSE
     C                   Z-ADD     0             XXTX#
     C                   ENDIF
     C                   ENDIF
     C*
     C     GENKY         SETLL     PTR01
     C     GENKY         READE     PTR01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        TRSUB#
     C                   Z-ADD     K1FYR         TRFYR
     C                   ADD       1             XXTX#
     C   98              Z-ADD     XXTX#         TRTX#
     C*
     C*** FILE FATDP
     C*
     C     GENKY         SETLL     PTD01
     C     GENKY         READE     PTD01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        TDSUB#
     C                   Z-ADD     K1FYR         TDFYR
     C   98              Z-ADD     XXTX#         TDTX#
     C                   UPDATE    PTD01
     C     GENKY         READE     PTD01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FATIP
     C*
     C     GENKY         SETLL     PTI01
     C     GENKY         READE     PTI01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        TISUB#
     C                   Z-ADD     K1FYR         TIFYR
     C   98              Z-ADD     XXTX#         TITX#
     C                   UPDATE    PTI01
     C     GENKY         READE     PTI01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C                   UPDATE    PTR01                                        FATRP
     C     GENKY         READE     PTR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAERP
     C*
     C     GENKY         SETLL     PER01
     C     GENKY         READE     PER01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        ERSUB#
     C                   Z-ADD     K1FYR         ERFYR
     C                   UPDATE    PER01
     C     GENKY         READE     PER01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE UMPCP
     C*
     C     GENKY         SETLL     PPC01
     C     GENKY         READE     PPC01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        PCSUB#
     C                   Z-ADD     K1FYR         PCFYR
     C                   UPDATE    PPC01
     C     GENKY         READE     PPC01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE UMCHP
     C*
     C     GENKY         SETLL     PCH02
     C     GENKY         READE     PCH02                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        C2SUB#
     C                   Z-ADD     K1FYR         C2FYR
     C                   UPDATE    PCH02
     C     GENKY         READE     PCH02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE UDPOP
     C*
     C     GENKY         SETLL     PPO01
     C     GENKY         READE     PPO01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        POSUB#
     C                   Z-ADD     K1FYR         POFYR
     C                   UPDATE    PPO01
     C     GENKY         READE     PPO01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C****************************************************************
     C*
     C                   CALL      'F7S96R01'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FFYR            3
     C                   PARM                    @@TFYR            3
     C                   PARM                    @@FLAG            1
     C*
     C*** FILE FALTP
     C*
     C     GENKY         SETLL     PLT01
     C     GENKY         READE     PLT01                                  99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        LTSUB#
     C                   Z-ADD     K1FYR         LTFYR
     C                   UPDATE    PLT01
     C     GENKY         READE     PLT01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C     *IN98         IFEQ      *ON
     C                   DELETE    PCM01
     C                   ELSE
     C                   UPDATE    PCM01                                        FMCMP
     C                   ENDIF
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C                   ENDSR
