     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7T96R01 - CASE TRANSFER/DELETION PROGRAM
     H*****************************************************************
     H* DESCRIPTION: CASE TRANSFER/DELETION - WRITE ALL FILES
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     H*
     H* INDICATORS:
     H*
     H* *IN98 = *OFF - @@FLAG = 'X' (SINGLE CASE TRANSFER)
     H* *IN98 = *ON  - @@FLAG <> 'X' (DOUBLE CASE TRANSFER)
     H*
     H*****************************************************************
     FFAEOL     UF   E           K DISK
     FNMARLA    UF   E           K DISK
     FFALTL7    UF   E           K DISK
     FNDTML1    UF   E           K DISK
     FNASBL1    UF   E           K DISK
     FFAEAL1    UF   E           K DISK
     FFAEDL     UF   E           K DISK
     FFAEEL     UF   E           K DISK
     FFAEHL     UF   E           K DISK
     FFAEPL     UF   E           K DISK
     FFAESL     UF   E           K DISK
     FFAEUL     UF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFDCAL13   UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDDAL2    UF   E           K DISK
     FFDDDL     UF   E           K DISK
     FFDDEL     UF   E           K DISK
     FFDDHL     UF   E           K DISK
     FFDDPL     UF   E           K DISK
     FFDDUL     UF   E           K DISK
     FFDMGL1    UF   E           K DISK
     FFDCHL1    UF   E           K DISK
     F*

     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

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
     C*
     C     GENKY         KLIST                                                  FROM CASE
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     GENKY1        KLIST                                                  TO CASE
     C                   KFLD                    KKCO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    KKFND
     C                   KFLD                    K1FYR
     C                   KFLD                    KKCASE
     C*
     C     EOBKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CAPKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    OLDSEQ
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOSUB#        K1SUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOFYR         K1FYR
     C     *LIKE         DEFINE    CADSEQ        OLDSEQ
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     @@FLAG        IFEQ      'X'
     C                   MOVE      *OFF          *IN98
     C                   ELSE
     C                   MOVE      *ON           *IN98
     C                   ENDIF
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         K1SUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FFYR        KKFYR
     C                   MOVE      @@TFYR        K1FYR
     C                   MOVE      @@CASE        KKCASE
     C*
     C                   EXSR      MOVEFI
     C*
     C                   SETON                                        LR
     C*================================================================
     C* MOVEFI - FILES THAT ARE BEING UPDATED
     C*================================================================
     C     MOVEFI        BEGSR
     C*
     C**************************************************************
     C*** FILE FAEOP
     C**************************************************************
     C     GENKY         CHAIN     PEO01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C*** GET LAST SEQUENCE NUMBER
     C*
     C     *IN98         IFEQ      *ON                                          DOUBLE CASE
     C     GENKY1        SETGT     PCA03
     C     GENKY1        READPE    PCA03                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        HLDSEQ            5 0
     C                   ELSE
     C                   Z-ADD     *ZEROS        HLDSEQ
     C                   ENDIF                                                  *IN99 = OFF
     C                   ENDIF                                                  *IN98 = ON
     C*
     C                   Z-ADD     K1SUB#        EOSUB#
     C                   Z-ADD     K1FYR         EOFYR
     C*
     C*** GET EOB # FOR EOBKY USED LATER **************************
     C*
     C                   MOVEL(P)  EOEOB#        KKEOB#
     C*
     C**************************************************************
     C*** FILE FAEAP
     C**************************************************************
     C     EOBKY         CHAIN     PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        EASUB#
     C                   Z-ADD     K1FYR         EAFYR
     C                   UPDATE    PEA01
     C     EOBKY         READE     PEA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FALTP
     C**************************************************************
     C     EOBKY         CHAIN     PLT01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        LTSUB#
     C                   Z-ADD     K1FYR         LTFYR
     C                   UPDATE    PLT01
     C     EOBKY         READE     PLT01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE NMARP
     C**************************************************************
     C     EOBKY         CHAIN     PAR01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        ARSUB#
     C                   Z-ADD     K1FYR         ARFYR
     C                   UPDATE    PAR01
     C     EOBKY         READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE NASBP
     C**************************************************************
     C     EOBKY         CHAIN     PSB01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        SBSUB#
     C                   Z-ADD     K1FYR         SBFYR
     C                   UPDATE    PSB01
     C                   ENDIF                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE NDTMP
     C**************************************************************
     C     EOBKY         CHAIN     PTM01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        TMSUB#
     C                   Z-ADD     K1FYR         TMFYR
     C                   UPDATE    PTM01
     C     EOBKY         READE     PTM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FAEDP
     C**************************************************************
     C     EOBKY         CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        EDSUB#
     C                   Z-ADD     K1FYR         EDFYR
     C                   UPDATE    PED01
     C                   ENDIF                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FAEEP
     C**************************************************************
     C     GENKY         CHAIN     PEE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        EESUB#
     C                   Z-ADD     K1FYR         EEFYR
     C                   UPDATE    PEE01
     C     EOBKY         READE     PEE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FAEHP
     C**************************************************************
     C     EOBKY         CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        EHSUB#
     C                   Z-ADD     K1FYR         EHFYR
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FAEPP
     C**************************************************************
     C     EOBKY         CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        EPSUB#
     C                   Z-ADD     K1FYR         EPFYR
     C                   UPDATE    PEP01
     C                   ENDIF                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FAESP
     C**************************************************************
     C     EOBKY         CHAIN     PES01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        ESSUB#
     C                   Z-ADD     K1FYR         ESFYR
     C                   UPDATE    PES01
     C     EOBKY         READE     PES01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FAEUP
     C**************************************************************
     C     GENKY         CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        EUSUB#
     C                   Z-ADD     K1FYR         EUFYR
     C                   UPDATE    PEU01
     C                   ENDIF                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FDCAP
     C**************************************************************
     C                   ADD       1             HLDSEQ                         NEXT SEQ. #
     C     EOBKY         CHAIN     PCA02                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     CADSEQ        OLDSEQ                         OLD SEQ. #
     C                   Z-ADD     K1SUB#        CASUB#
     C                   Z-ADD     K1FYR         CAFYR
     C**************************************************************
     C*** FILE FDDAP
     C**************************************************************
     C     CAPKY         CHAIN     PDA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        DASUB#
     C                   Z-ADD     K1FYR         DAFYR
     C   98              Z-ADD     HLDSEQ        DADSEQ
     C                   UPDATE    PDA01
     C     CAPKY         READE     PDA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FDDDP
     C**************************************************************
     C     CAPKY         CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        DDSUB#
     C                   Z-ADD     K1FYR         DDFYR
     C   98              Z-ADD     HLDSEQ        DDDSEQ
     C                   UPDATE    PDD01
     C                   ENDIF                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FDDEP
     C**************************************************************
     C     CAPKY         CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        DESUB#
     C                   Z-ADD     K1FYR         DEFYR
     C   98              Z-ADD     HLDSEQ        DEDSEQ
     C                   UPDATE    PDE01
     C     CAPKY         READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FDCHP
     C**************************************************************
     C     CAPKY         CHAIN     PCH01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        CHSUB#
     C                   Z-ADD     K1FYR         CHFYR
     C   98              Z-ADD     HLDSEQ        DHDSEQ
     C                   UPDATE    PCH01
     C     CAPKY         READE     PCH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C**************************************************************
     C*** FILE FDDHP
     C**************************************************************
     C     CAPKY         CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        DHSUB#
     C                   Z-ADD     K1FYR         DHFYR
     C   98              Z-ADD     HLDSEQ        DHDSEQ
     C                   UPDATE    PDH01
     C                   ENDIF                                                  *IN99 = OFF
     C****************************************************************
     C*** FILE FDDPP
     C****************************************************************
     C     CAPKY         CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        DPSUB#
     C                   Z-ADD     K1FYR         DPFYR
     C   98              Z-ADD     HLDSEQ        DPDSEQ
     C                   UPDATE    PDP01
     C                   ENDIF                                                  *IN99 = OFF
     C****************************************************************
     C*** FILE FDDUP
     C****************************************************************
     C     CAPKY         CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     K1SUB#        DUSUB#
     C                   Z-ADD     K1FYR         DUFYR
     C   98              Z-ADD     HLDSEQ        DUDSEQ
     C                   UPDATE    PDU01
     C                   ENDIF                                                  *IN99 = OFF
     C***************************************************************
     C*** FILE FDMGP
     C***************************************************************
     C     CAPKY         CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     K1SUB#        MGSUB#
     C                   Z-ADD     K1FYR         MGFYR
     C   98              Z-ADD     HLDSEQ        MGDSEQ
     C                   UPDATE    PMG01
     C     CAPKY         READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C***************************************************************
     C****** UPDATE FDCAP FOR EACH EOB # ****************************
     C***************************************************************
     C   98              Z-ADD     HLDSEQ        CADSEQ                         SEQ. NUMBER
     C                   UPDATE    PCA02                                        FDCAP
     C   98              ADD       1             HLDSEQ
     C     EOBKY         READE     PCA02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C***************************************************************
     C****** UPDATE FAEOP FOR EACH EOB # ****************************
     C***************************************************************
     C                   UPDATE    PEO01                                60      FAEOP
     C     GENKY         READE     PEO01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDSR
