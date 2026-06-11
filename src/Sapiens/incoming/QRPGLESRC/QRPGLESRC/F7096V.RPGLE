     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 ************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H*****************************************************************
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCHL1    IF   E           K DISK
     FNMARLA    IF   E           K DISK
     FFADAL     IF   E           K DISK
     FFAMSL     IF   E           K DISK
     FFAPPL1    IF   E           K DISK
     FFATXL     IF   E           K DISK
     FFAWHL     IF   E           K DISK
     FFATRL     IF   E           K DISK
     FFATDL     IF   E           K DISK
     FFATIL     IF   E           K DISK
     FFAERL     IF   E           K DISK
     FNATRL     IF   E           K DISK
     F                                     RENAME(PTR01:PTR02)
     FNAPBL     IF   E           K DISK
     FFDEOL     IF   E           K DISK
     FFDE1L     IF   E           K DISK
     FFADCL     IF   E           K DISK
     FFDCALD    IF   E           K DISK
     FFALTL     IF   E           K DISK
     FUMPCL11   IF   E           K DISK
     FUMCHL1    IF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     FUDPOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
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
     IPTR02
     I              TRCO#                       T2CO#
     I              TRSUB#                      T2SUB#
     I              TRFND                       T2FND
     I              TRFYR                       T2FYR
     I              TRCASE                      T2CASE
     I              TREOB#                      T2EOB#
     I              TRUSER                      T2USER
     I              TRTIME                      T2TIME
     I              TRPGM                       T2PGM
     I              TRDT                        T2DT
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@SS#
     C                   PARM                    @@BA              1
     C                   PARM                    @@TF              1
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
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     GENKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     T1KY          KLIST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKFILE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMPGM         KKFILE
     C     *LIKE         DEFINE    CMPGM         SVFILE
     C     *LIKE         DEFINE    CMPGM         OOFILE
     C     *LIKE         DEFINE    CMPGM         KKKEY
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMSS#         @@SS#
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMSUB#        @@SUB#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    CMSS#         OOSS#
     C     *LIKE         DEFINE    CMCO#         OOCO#
     C     *LIKE         DEFINE    CMSUB#        OOSUB#
     C     *LIKE         DEFINE    CMFND         OOFND
     C     *LIKE         DEFINE    CMFYR         OOFYR
     C     *LIKE         DEFINE    CMCASE        OOCASE
     C     *LIKE         DEFINE    CMACDT        OOACDT
     C     *LIKE         DEFINE    CMPGM         OOPGM
     C     *LIKE         DEFINE    DASEQ#        OOSEQ#
     C     *LIKE         DEFINE    CADSEQ        OODSEQ
     C     *LIKE         DEFINE    CAEOB#        OOEOB
     C*
     C*================================================================
     C* Start of program processing
     C*================================================================
     C*
     C                   MOVE      *ON           *INOA
     C                   TIME                    DSDTTM
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   MOVEL(P)  @@SS#         KKSS#
     C*
     C     @@BA          IFEQ      'B'
     C                   MOVEL(P)  'BEFORE'      OOBA              6
     C                   ENDIF
     C*
     C     @@BA          IFEQ      'A'
     C                   MOVEL(P)  'AFTER '      OOBA
     C                   ENDIF
     C*
     C*
     C     @@TF          IFEQ      'T'
     C                   MOVEL(P)  'TO'          OOTF              6
     C                   ENDIF
     C*
     C     @@TF          IFEQ      'F'
     C                   MOVEL(P)  'FROM'        OOTF
     C                   ENDIF
     C*
     C                   EXSR      MOVEFI
     C*
     C                   CALL      'F7096V01'
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@SS#
     C                   PARM                    @@BA
     C                   PARM                    @@TF
     C*
     C                   EXCEPT    TOT
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* MOVEFI - FILES THAT ARE BEING UPDATED
     C*================================================================
     C     MOVEFI        BEGSR
     C*
     C*** FILE FMCDP
     C*
     C                   MOVEL(P)  'FMCDP'       KKFILE
     C*
     C     CDKY          CHAIN     PCD01                              99
     C   99              EXSR      PRINT
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CDSS#         OOSS#
     C*****                MOVELCDLSN     OOLSN
     C*****                MOVELCDFSN     OOFSN
     C*****                MOVELCDMID     OOMID
     C*****                MOVELCDADR1    OOADR1
     C*****                MOVELCDADR2    OOADR2
     C*****                MOVELCDCTY     OOCTY
     C*****                MOVELCDST      OOST
     C*****                MOVELCDZIP     OOZIP
     C*****                MOVELCDDTOB    OODTOB
     C*****                MOVELCDSEX     OOSEX
     C*
     C                   EXSR      PRINT
     C                   ENDIF                                                  *IN99
     C*
     C*** FILE FMCMP
     C*
     C                   MOVEL(P)  'FMCMP'       KKFILE
     C*
     C     GENKY         CHAIN     PCM01                              99
     C   99              EXSR      PRINT
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CMCO#         OOCO#
     C                   Z-ADD     CMSUB#        OOSUB#
     C                   Z-ADD     CMFND         OOFND
     C                   Z-ADD     CMFYR         OOFYR
     C                   Z-ADD     CMCASE        OOCASE
     C                   MOVEL     CMSS#         OOSS#
     C                   Z-ADD     CMACDT        OOACDT
     C*****                Z-ADDCMEMP#    OOEMP#
     C*****                Z-ADDCMDIV     OODIV
     C*****                MOVELCMLSN     OOLSN
     C*****                MOVELCMFSN     OOFSN
     C*****                MOVELCMMID     OOMID
     C*
     C                   MOVEL     CMPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C                   MOVEL(P)  'FMCHP'       KKFILE
     C*
     C     GENKY         SETLL     PCH01
     C     GENKY         READE     PCH01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     CHCO#         OOCO#
     C                   Z-ADD     CHSUB#        OOSUB#
     C                   Z-ADD     CHFND         OOFND
     C                   Z-ADD     CHFYR         OOFYR
     C                   Z-ADD     CHCASE        OOCASE
     C*
     C                   MOVEL     CHPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PCH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NMARP
     C*
     C                   MOVEL(P)  'NMARP'       KKFILE
     C*
     C     GENKY         SETLL     PAR01
     C     GENKY         READE     PAR01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     ARCO#         OOCO#
     C                   Z-ADD     ARSUB#        OOSUB#
     C                   Z-ADD     ARFND         OOFND
     C                   Z-ADD     ARFYR         OOFYR
     C                   Z-ADD     ARCASE        OOCASE
     C                   Z-ADD     AREOB#        OOEOB
     C*
     C                   MOVEL     ARPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FADAP
     C*
     C                   MOVEL(P)  'FADAP'       KKFILE
     C*
     C     GENKY         SETLL     PDA01
     C     GENKY         READE     PDA01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DACO#         OOCO#
     C                   Z-ADD     DASUB#        OOSUB#
     C                   Z-ADD     DAFND         OOFND
     C                   Z-ADD     DAFYR         OOFYR
     C                   Z-ADD     DACASE        OOCASE
     C                   Z-ADD     DASEQ#        OODSEQ
     C*
     C                   MOVEL     DAPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAMSP
     C*
     C                   MOVEL(P)  'FAMSP'       KKFILE
     C*
     C     GENKY         SETLL     PMS01
     C     GENKY         READE     PMS01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     MSCO#         OOCO#
     C                   Z-ADD     MSSUB#        OOSUB#
     C                   Z-ADD     MSFND         OOFND
     C                   Z-ADD     MSFYR         OOFYR
     C                   Z-ADD     MSCASE        OOCASE
     C                   Z-ADD     MSSEQ#        OODSEQ
     C*
     C                   MOVEL     MSPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PMS01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAPPP
     C*
     C                   MOVEL(P)  'FAPPP'       KKFILE
     C*
     C     GENKY         SETLL     PPP01
     C     GENKY         READE     PPP01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     PPCO#         OOCO#
     C                   Z-ADD     PPSUB#        OOSUB#
     C                   Z-ADD     PPFND         OOFND
     C                   Z-ADD     PPFYR         OOFYR
     C                   Z-ADD     PPCASE        OOCASE
     C                   Z-ADD     PPSEQ#        OODSEQ
     C*
     C                   MOVEL     PPPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PPP01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*
     C*** FILE FATXP
     C*
     C                   MOVEL(P)  'FATXP'       KKFILE
     C*
     C     GENKY         SETLL     PTX01
     C     GENKY         READE     PTX01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     TXCO#         OOCO#
     C                   Z-ADD     TXSUB#        OOSUB#
     C                   Z-ADD     TXFND         OOFND
     C                   Z-ADD     TXFYR         OOFYR
     C                   Z-ADD     TXCASE        OOCASE
     C*
     C                   MOVEL     TXPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PTX01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAWHP
     C*
     C                   MOVEL(P)  'FAWHP'       KKFILE
     C*
     C     GENKY         SETLL     PWH01
     C     GENKY         READE     PWH01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     WHCO#         OOCO#
     C                   Z-ADD     WHSUB#        OOSUB#
     C                   Z-ADD     WHFND         OOFND
     C                   Z-ADD     WHFYR         OOFYR
     C                   Z-ADD     WHCASE        OOCASE
     C                   Z-ADD     WHSEQ#        OODSEQ
     C*
     C                   MOVEL     WHPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PWH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FATRP
     C*
     C                   MOVEL(P)  'FATRP'       KKFILE
     C*
     C     GENKY         SETLL     PTR01
     C     GENKY         READE     PTR01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     TRCO#         OOCO#
     C                   Z-ADD     TRSUB#        OOSUB#
     C                   Z-ADD     TRFND         OOFND
     C                   Z-ADD     TRFYR         OOFYR
     C                   Z-ADD     TRCASE        OOCASE
     C                   Z-ADD     TRTX#         OODSEQ
     C*
     C                   MOVEL     TRPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PTR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FATDP
     C*
     C                   MOVEL(P)  'FATDP'       KKFILE
     C*
     C     GENKY         SETLL     PTD01
     C     GENKY         READE     PTD01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     TDCO#         OOCO#
     C                   Z-ADD     TDSUB#        OOSUB#
     C                   Z-ADD     TDFND         OOFND
     C                   Z-ADD     TDFYR         OOFYR
     C                   Z-ADD     TDCASE        OOCASE
     C                   Z-ADD     TDTX#         OODSEQ
     C*
     C                   MOVEL     TDPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PTD01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FATIP
     C*
     C                   MOVEL(P)  'FATIP'       KKFILE
     C*
     C     GENKY         SETLL     PTI01
     C     GENKY         READE     PTI01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     TICO#         OOCO#
     C                   Z-ADD     TISUB#        OOSUB#
     C                   Z-ADD     TIFND         OOFND
     C                   Z-ADD     TIFYR         OOFYR
     C                   Z-ADD     TICASE        OOCASE
     C*
     C                   MOVEL     TIPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PTI01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAERP
     C*
     C                   MOVEL(P)  'FAERP'       KKFILE
     C*
     C     GENKY         SETLL     PER01
     C     GENKY         READE     PER01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     ERCO#         OOCO#
     C                   Z-ADD     ERSUB#        OOSUB#
     C                   Z-ADD     ERFND         OOFND
     C                   Z-ADD     ERFYR         OOFYR
     C                   Z-ADD     ERCASE        OOCASE
     C                   MOVEL     ERSS#         OOSS#
     C                   Z-ADD     ERACDT        OOACDT
     C                   Z-ADD     ERSEQ#        OODSEQ
     C                   Z-ADD     ERREOB        OOEOB
     C*
     C                   MOVEL     ERPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PER01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NATRP
     C*
     C                   MOVEL(P)  'NATRP'       KKFILE
     C*
     C     GENKY         SETLL     PTR02
     C     GENKY         READE     PTR02                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     T2CO#         OOCO#
     C                   Z-ADD     T2SUB#        OOSUB#
     C                   Z-ADD     T2FND         OOFND
     C                   Z-ADD     T2FYR         OOFYR
     C                   Z-ADD     T2CASE        OOCASE
     C                   Z-ADD     T2EOB#        OOEOB
     C*
     C                   MOVEL     T2PGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PTR02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NAPBP
     C*
     C                   MOVEL(P)  'NAPBP'       KKFILE
     C*
     C     GENKY         SETLL     PPB01
     C     GENKY         READE     PPB01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     PBCO#         OOCO#
     C                   Z-ADD     PBSUB#        OOSUB#
     C                   Z-ADD     PBFND         OOFND
     C                   Z-ADD     PBFYR         OOFYR
     C                   Z-ADD     PBCASE        OOCASE
     C                   MOVEL     PBSS#         OOSS#
     C                   Z-ADD     PBACDT        OOACDT
     C                   Z-ADD     PBEOB#        OOEOB
     C*
     C                   MOVEL     PBPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PPB01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDEOP
     C*
     C                   MOVEL(P)  'FDEOP'       KKFILE
     C*
     C     GENKY         SETLL     PEO01
     C     GENKY         READE     PEO01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EOCO#         OOCO#
     C                   Z-ADD     EOSUB#        OOSUB#
     C                   Z-ADD     EOFND         OOFND
     C                   Z-ADD     EOFYR         OOFYR
     C                   Z-ADD     EOCASE        OOCASE
     C                   Z-ADD     EOEOB#        OOEOB
     C*
     C                   MOVEL     EOPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PEO01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDE1P
     C*
     C                   MOVEL(P)  'FDE1P'       KKFILE
     C*
     C     GENKY         SETLL     PE101
     C     GENKY         READE     PE101                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     E1CO#         OOCO#
     C                   Z-ADD     E1SUB#        OOSUB#
     C                   Z-ADD     E1FND         OOFND
     C                   Z-ADD     E1FYR         OOFYR
     C                   Z-ADD     E1CASE        OOCASE
     C                   Z-ADD     EOEOB#        OOEOB
     C*
     C                   MOVEL     E1PGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PE101                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FADCP
     C*
     C                   MOVEL(P)  'FADCP'       KKFILE
     C*
     C     GENKY         SETLL     PDC01
     C     GENKY         READE     PDC01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DCCO#         OOCO#
     C                   Z-ADD     DCSUB#        OOSUB#
     C                   Z-ADD     DCFND         OOFND
     C                   Z-ADD     DCFYR         OOFYR
     C                   Z-ADD     DCCASE        OOCASE
     C                   Z-ADD     DCDCS#        OOSEQ#
     C                   Z-ADD     EOEOB#        OOEOB
     C*
     C                   MOVEL     DCPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDC01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDCAP
     C*
     C                   MOVEL(P)  'FDCAP'       KKFILE
     C*
     C     GENKY         SETLL     PCA01
     C     GENKY         READE     PCA01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     CACO#         OOCO#
     C                   Z-ADD     CASUB#        OOSUB#
     C                   Z-ADD     CAFND         OOFND
     C                   Z-ADD     CAFYR         OOFYR
     C                   Z-ADD     CACASE        OOCASE
     C                   MOVEL     CASS#         OOSS#
     C                   Z-ADD     CAACDT        OOACDT
     C                   Z-ADD     CADCS#        OOSEQ#
     C                   Z-ADD     CADSEQ        OODSEQ
     C                   Z-ADD     CAEOB#        OOEOB
     C*
     C                   MOVEL     CAPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PCA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FALTP
     C*
     C                   MOVEL(P)  'FALTP'       KKFILE
     C*
     C     GENKY         SETLL     PLT01
     C     GENKY         READE     PLT01                                  99
     C   99              EXSR      PRINT
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     LTCO#         OOCO#
     C                   Z-ADD     LTSUB#        OOSUB#
     C                   Z-ADD     LTFND         OOFND
     C                   Z-ADD     LTFYR         OOFYR
     C                   Z-ADD     LTCASE        OOCASE
     C                   MOVEL     LTSS#         OOSS#
     C                   Z-ADD     LTACDT        OOACDT
     C*******              MOVELLTLNM     OOLNM
     C*******              MOVELLTFNM     OOFNM
     C*
     C                   MOVEL     LTPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PLT01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE UMPCP
     C*
     C                   MOVEL(P)  'UMPCP'       KKFILE
     C*
     C     GENKY         SETLL     PPC01
     C     GENKY         READE     PPC01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     PCCO#         OOCO#
     C                   Z-ADD     PCSUB#        OOSUB#
     C                   Z-ADD     PCFND         OOFND
     C                   Z-ADD     PCFYR         OOFYR
     C                   Z-ADD     PCCASE        OOCASE
     C                   MOVEL     PCSS#         OOSS#
     C                   Z-ADD     PCACDT        OOACDT
     C*
     C                   MOVEL     PCPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PPC01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE UMCHP
     C*
     C                   MOVEL(P)  'UMCHP'       KKFILE
     C*
     C     GENKY         SETLL     PCH02
     C     GENKY         READE     PCH02                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     C2CO#         OOCO#
     C                   Z-ADD     C2SUB#        OOSUB#
     C                   Z-ADD     C2FND         OOFND
     C                   Z-ADD     C2FYR         OOFYR
     C                   Z-ADD     C2CASE        OOCASE
     C*
     C                   MOVEL     C2PGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PCH02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE UDPOP
     C*
     C                   MOVEL(P)  'UDPOP'       KKFILE
     C*
     C     GENKY         SETLL     PPO01
     C     GENKY         READE     PPO01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     POCO#         OOCO#
     C                   Z-ADD     POSUB#        OOSUB#
     C                   Z-ADD     POFND         OOFND
     C                   Z-ADD     POFYR         OOFYR
     C                   Z-ADD     POCASE        OOCASE
     C*
     C                   MOVEL     POPGM         OOPGM
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PPO01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDSR
     C*
     C     PRINT         BEGSR
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       SVFILE
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C     KKFILE        IFNE      SVFILE
     C                   MOVEL     KKFILE        SVFILE
     C                   MOVEL     KKFILE        OOFILE
     C                   EXCEPT    DETH
     C                   ENDIF
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        OOSS# = MaskData('SSN':'OOSS#':OOSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DET
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        OOSS# = UnMaskData('SSN':'OOSS#':OOSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C  N99              ADD       1             XXCNT             9 0
     C                   ENDSR
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           76 'VERIFY CASE TRANSFER'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       OOTF                61
     O                       OOBA                67
     O                                           73 'IMAGE'
     O                                          126 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                           20 'CO#'
     O                                           28 'SUB #'
     O                                           35 'GRP'
     O                                           39 'YR'
     O                                           50 'CASE'
     O                                           62 'SS #'
     O                                           72 'AC DATE'
     O                                           80 'SEQ #'
     O                                           95 'PRIM. SEQ #'
     O                                          104 'EOB#'
     O                                          120 'MNT PGM'
     O*
     O          E            DETH        1  0
     O                                            5 'FILE:'
     O                       OOFILE         B    16
     O               99                          35 '==> NO RCDS PROCESSED'
     O*
     O          E            DET            1
     O                       OOCO#         ZB    20
     O                       OOSUB#        ZB    28
     O                       OOFND         ZB    35
     O                       OOFYR         ZB    39
     O                       OOCASE        ZB    50
     O                       OOSS#          B    62
     O                       OOACDT        ZB    72
     O                       OODSEQ        ZB    80
     O                       OOSEQ#        ZB    91
     O                       OOEOB         ZB   104
     O                       OOPGM          B   120
     O*
     O          E            TOT         1  2
     O                       XXCNT         Z     58
     O                                           77 'RECORDS PROCESSED'
