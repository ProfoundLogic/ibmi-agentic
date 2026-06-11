     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*
     H* PROGRAM: F7090R  CASE TRANSFER/DELETION PRE-CHECK PROGRAM
     H*
     H* DESCRIPTION: VERIFIES DATA FOR CASE EXISTING IN FILES AND
     H*              WRITES THE FILE NAMES TO THE QTEMP TRANSFER FILE
     H*
     H* CALLS TO:  F7090R01
     H*            F7090R02   CONDITIONED
     H*            F7090R03
     H*
     H* 09/21/98  C:94  PROJECT: MC151
     H*
     H*****************************************************************
     H*
     H*  REVISIONS:
     H*
     H*  03/17/00  C:62   SUPPORT:MC035 22143
     H*                   MOVE FAEAP TO 'W' SUBROUTINE.
     H*
     H*  05/05/99  C:56   MC152
     H*                   Change FAEAL to FAEAL1, duplicate logicals
     H*
     H*****************************************************************
     F*  Physical file definition (to load KFIELDS)
     FFMCMP     IF   E             DISK
     F*  Physical file definition (for refreshing subfile record)
     FFAT3P     IF   E             DISK
     F*
     FFAT3LQ    UF A E           K DISK
     F*
     F*** FEE SCHEDULE MANAGEMENT SYSTEM FILES ***********************
     F*
     FFAERL     IF   E           K DISK
     F*
     F*** BEG CHG *** 05/05/99 ****************************************
     F**********FAEAL   IF  E           K        DISK
     FFAEAL1    IF   E           K DISK
     F*** END CHG *** 05/05/99 ****************************************
     F*
     FFAEDL     IF   E           K DISK
     F*
     FFAEEL     IF   E           K DISK
     F*
     FFAEHL     IF   E           K DISK
     F*
     FFAEPL     IF   E           K DISK
     F*
     FFAESL     IF   E           K DISK
     F*
     FFAEUL     IF   E           K DISK
     F*
     FFAE1L     IF   E           K DISK
     F*
     FFAHBL     IF   E           K DISK
     F*
     FFDCAL13   IF   E           K DISK
     F*
     FFADUL     IF   E           K DISK
     F*
     FFDDAL     IF   E           K DISK
     F                                     RENAME(PDA01:PDA02)
     F*
     FFDDDL     IF   E           K DISK
     F*
     FFDDEL     IF   E           K DISK
     F*
     FFDDHL     IF   E           K DISK
     F*
     FFDDPL     IF   E           K DISK
     F*
     FFDDUL     IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     F*
     FFDHCL     IF   E           K DISK
     F*
     FFDMGL     IF   E           K DISK
     F*
     FFDEOL     IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*
     FFDE1L     IF   E           K DISK
     F                                     RENAME(PE101:PE102)
     F*
     FFADCL     IF   E           K DISK
     F*
     FFALTL     IF   E           K DISK
     F*
     F*** FILES THAT ARE ALWAYS ON SYSTEM ****************************
     F*
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*
     FFMCHL1    IF   E           K DISK
     F*
     FFMCDL     IF   E           K DISK
     F*
     FFAEOL     IF   E           K DISK
     F*
     F*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN1            9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@TD              1
     C                   PARM                    @@FLAG            1
     C                   PARM                    @@WCMS            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     T3KY          KLIST
     C                   KFLD                    KKFILE
     C*
     C     GENKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    T3FILE        KKFILE
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     @@RRN         CHAIN(N)  PCM01                              99
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      CMSS#         KKSS#
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C*** IS MC INSTALLED
     C*
     C     ##MCIF        IFEQ      'Y'
     C                   MOVE      'F'           INSTF             1
     C                   ENDIF
     C*
     C                   CLEAR                   PT302
     C*
     C                   EXSR      READ
     C     *IN95         DOWNE     *ON
     C*
     C                   EXSR      SUBSET
     C*
     C                   EXSR      READ
     C*
     C                   ENDDO
     C*
     C                   CALL      'F7090R01'
     C                   PARM      @@RRN         ##RRN             9 0
     C*
     C                   CALL      'F7090R02'                           60
     C                   PARM      @@RRN         ##RRN             9 0
     C*
     C                   MOVE      *OFF          *INKL
     C                   CALL      'F7090R03'
     C                   PARM      ' '           ##CALL            1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@RRN         ##RRN
     C                   PARM      @@RRN1        ##RRN1            9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM      @@RRN3        ##RRN3            9 0
     C                   PARM      @@TD          ##TD              1
     C                   PARM      @@FLAG        ##FLAG            1
     C                   PARM      @@WCMS        ##WCMS            1
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*================================================================
     C* FFILES - FILES THAT CASE MANAGEMENT FILES
     C*================================================================
     C     FFILES        BEGSR
     C                   SELECT
     C*
     C     T3FILE        WHENEQ    'FAERP'
     C     GENKY         CHAIN     PER01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 03/17/00 **************************************
     C********** T3FILE    WHEQ 'FAEAP'
     C********** GENKY     CHAINPEA01                99
     C********** *IN99     IFEQ *OFF
     C**********           SETON                     30
     C**********           ENDIF
     C*** END   DEL *** 03/17/00 **************************************
     C*
     C     T3FILE        WHENEQ    'FAEDP'
     C     GENKY         CHAIN     PED01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEEP'
     C     GENKY         CHAIN     PEE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEHP'
     C     GENKY         CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEPP'
     C     GENKY         CHAIN     PEP01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAESP'
     C     GENKY         CHAIN     PES01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEUP'
     C     GENKY         CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAE1P'
     C     GENKY         CHAIN     PE101                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAHBP'
     C     GENKY         CHAIN     PHB01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDCAP'
     C     GENKY         CHAIN     PCA01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FADUP'
     C     GENKY         CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDAP'
     C     GENKY         CHAIN     PDA02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDDP'
     C     GENKY         CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDEP'
     C     GENKY         CHAIN     PDE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDHP'
     C     GENKY         CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDPP'
     C     GENKY         CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDDUP'
     C     GENKY         CHAIN     PDU02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDHCP'
     C     GENKY         CHAIN     PHC01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDMGP'
     C     GENKY         CHAIN     PMG01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDEOP'
     C     GENKY         CHAIN     PEO02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FDE1P'
     C     GENKY         CHAIN     PE102                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FADCP'
     C     GENKY         CHAIN     PDC01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FALTP'
     C     GENKY         CHAIN     PLT01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* WFILES - FILES THAT ARE ALWAYS ON THE SYSTEM
     C*================================================================
     C     WFILES        BEGSR
     C                   SELECT
     C*
     C     T3FILE        WHENEQ    'FMCMP'
     C     GENKY         CHAIN(N)  PCM02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FMCHP'
     C     GENKY         CHAIN     PCH01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C*
     C     T3FILE        WHENEQ    'FMCDP'
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     T3FILE        WHENEQ    'FAEOP'
     C     GENKY         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*** BEGIN ADD *** 03/17/00 **************************************
     C     T3FILE        WHENEQ    'FAEAP'
     C     GENKY         CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*** END   ADD *** 03/17/00 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBFL - Move data base fields to File fields in Qtemp
     C*================================================================
     C     MVDBFL        BEGSR
     C*
     C*
     C                   MOVEL(P)  T3FILE        KKFILE
     C     T3KY          CHAIN     PT302                              99
     C                   MOVEL(P)  T3FILE        TQFILE
     C                   MOVEL(P)  T3DESC        TQDESC
     C                   MOVEL(P)  T3NOTE        TQNOTE
     C     *IN99         IFEQ      *ON
     C                   WRITE     PT302
     C                   ELSE
     C                   UPDATE    PT302
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C                   READ      PT301                                  95
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   SETOFF                                       3099
     C*
     C                   SELECT
     C     T3PROD        WHENEQ    'W'
     C                   EXSR      WFILES
     C     *IN30         IFEQ      *ON
     C                   EXSR      MVDBFL
     C                   ENDIF
     C*
     C     T3PROD        WHENEQ    INSTF
     C                   EXSR      FFILES
     C     *IN30         IFEQ      *ON
     C                   EXSR      MVDBFL
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
