     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
      * API - FA749R - ARCHIVE AND DELETE PROVIDER BILL INFO
      ****************************************************************
      *
      * This program will write the data from one file to another,
      * then delete it from the first file.
      *
      *  Called From: F0396R01
      *  Calls To:    None
      *
      ****************************************************************
      * Created on 10/31/2007 by c:47 for  Project MC309
      ****************************************************************
      * Revisions:
      *
      * 12/02/16 C: 079 Support: 90/43920
      *                 Do not subtract 1 from CArcn
      *
      * 01/06/15 C: 079 Support: 30916
      *                 Cycle through all FDDAP records
      *
      ****************************************************************
     FFAEAL1    UF   E           K DISK
     FFAEAPBK   O    E             DISK
     F                                     RENAME(PEA01:PEA01BK)
     FFAEEL3    UF   E           K DISK
     FFAEEPBK   O    E             DISK
     F                                     RENAME(PEE01:PEE01BK)
     FFAEOLG    UF   E           K DISK
     FFAEOPBK   O    E             DISK
     F                                     RENAME(PEO01:PEO01BK)
     FFALTL7    UF   E           K DISK
     FFALTPBK   O    E             DISK
     F                                     RENAME(PLT01:PLT01BK)
     FFAL2L2    UF   E           K DISK
     FFAL2PBK   O    E             DISK
     F                                     RENAME(PL201:PL201BK)
     FFDCAL13   UF   E           K DISK
     FFDCAPBK   O    E             DISK
     F                                     RENAME(PCA01:PCA01BK)
     F*** Beg Add *** 01/06/15 *********************************************
     FFDCAL24   UF   E           K DISK
     F                                     RENAME(PCA01:PCA024)
     F*** End Add *** 01/06/15 *********************************************
     FFDCCL1    UF   E           K DISK
     FFDCCPBK   O    E             DISK
     F                                     RENAME(PCC01:PCC01BK)
     FFDCHL3    UF   E           K DISK
     FFDCHPBK   O    E             DISK
     F                                     RENAME(PCH01:PCH01BK)
     FFDDAL4    UF   E           K DISK
     FFDDAPBK   O    E             DISK
     F                                     RENAME(PDA01:PDA01BK)
     FFDDEL3    UF   E           K DISK
     FFDDEPBK   O    E             DISK
     F                                     RENAME(PDE01:PDE01BK)
     FFDMGL4    UF   E           K DISK
     FFDMGPBK   O    E             DISK
     F                                     RENAME(PMG01:PMG01BK)
     FFEDDL     UF   E           K DISK
     FFEDDPBK   O    E             DISK
     F                                     RENAME(PDD01:PDD01BK)
     FFEDHL     UF   E           K DISK
     FFEDHPBK   O    E             DISK
     F                                     RENAME(PDH01:PDH01BK)
     FFEDPL     UF   E           K DISK
     FFEDPPBK   O    E             DISK
     F                                     RENAME(PDP01:PDP01BK)
     FFEDUL     UF   E           K DISK
     FFEDUPBK   O    E             DISK
     F                                     RENAME(PDU01:PDU01BK)
     FFEEDL     UF   E           K DISK
     FFEEDPBK   O    E             DISK
     F                                     RENAME(PED01:PED01BK)
     FFEEHL     UF   E           K DISK
     FFEEHPBK   O    E             DISK
     F                                     RENAME(PEH01:PEH01BK)
     FFEEPL     UF   E           K DISK
     FFEEPPBK   O    E             DISK
     F                                     RENAME(PEP01:PEP01BK)
     FFEEUL     UF   E           K DISK
     FFEEUPBK   O    E             DISK
     F                                     RENAME(PEU01:PEU01BK)
     FFEE1L2    UF   E           K DISK
     FFEE1PBK   O    E             DISK
     F                                     RENAME(PE101:PE101BK)
     FFELGL4    UF   E           K DISK
     FFELGPBK   O    E             DISK
     F                                     RENAME(PLG01:PLG01BK)
     FNMARLA    UF   E           K DISK
     FNMARPBK   O    E             DISK
     F                                     RENAME(PAR01:PAR01BK)
     FQFEML     UF   E           K DISK
     FQFEMPBK   O    E             DISK
     F                                     RENAME(PEM01:PEM01BK)
      *
     D/COPY SCOPYMT01D
      ***PY SCOPYMT01I

     C*
     C*================================================================
     C*   PARM LISTS
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QICO#             3 0
     C                   PARM                    QISUB#            5 0
     C                   PARM                    QIFND             3 0
     C                   PARM                    QIFYR             3 0
     C                   PARM                    QICASE            7 0
     C                   PARM                    QIEOB#            9 0
     C                   PARM                    QIRCN             3 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     EKY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     ERKY          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKSUB#            5 0
     C                   KFLD                    KKFND             3 0
     C                   KFLD                    KKFYR             3 0
     C                   KFLD                    KKCASE            7 0
     C                   KFLD                    KKEOB#            9 0
     C                   KFLD                    KKRCN             3 0
     C*
     C     ERSKY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKDSEQ
     C*
     C     SQKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ            5 0
     C*================================================================
     C* Main Processing
     C*================================================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SAVETM            6 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SAVEDT            7 0
     C*
     C                   MOVE      *BLANK        EAUSER
     C                   MOVE      *BLANK        EAPGM
     C                   Z-ADD     *ZERO         EADT
     C                   Z-ADD     *ZERO         EATIME
     C*
     C                   MOVEL(P)  Q1USER        EAIUSR
     C                   MOVEL(P)  Q1PGM         EAIPGM
     C                   Z-ADD     SAVEDT        EAIDT
     C                   Z-ADD     SAVETM        EAITIM
     C* LOAD KEY FIELDS
     C                   Z-ADD     QICO#         KKCO#
     C                   Z-ADD     QISUB#        KKSUB#
     C                   Z-ADD     QIFND         KKFND
     C                   Z-ADD     QIFYR         KKFYR
     C                   Z-ADD     QICASE        KKCASE
     C                   Z-ADD     QIEOB#        KKEOB#
     C                   Z-ADD     QIRCN         KKRCN
     C*
     C                   EXSR      FAEA
     C                   EXSR      FDCA
     C                   EXSR      EDI
     C                   EXSR      MISC
     C*
     C* -----------------------------------------------------------
     C*
     C                   MOVE      *OFF          *INLR
     C                   RETURN
     C*================================================================
     C* FAEA - FAEA*
     C*================================================================
     C     FAEA          BEGSR
     C*
     C* GET RECON RECORD.
     C*
     C     ERKY          CHAIN     PEA01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C* COPY THE RECORD FROM FAEAP.
     C*
     C                   WRITE     PEA01BK
     C*
     C* COPY, THEN UPDATE RECORD FROM FAEOP FILE.
     C*
     C     EKY           CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   WRITE     PEO01BK
     C                   SUB       EACHRG        EOCHRG
     C                   SUB       EAMRA         EOMRA
     C                   SUB       EANDIS        EONDIS
     C                   SUB       EAOTHR        EOOTHR
     C                   SUB       EAOTH2        EOOTH2
     C                   SUB       EAPAID        EOPAID
     C                   MOVE      'C'           EOBLST
     C                   MOVE      'Y'           EOSTAT
     C                   Z-ADD     EOEOB#        EOCHK#
     C                   SUB       1             EORCN
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C* FAEEP
     C     ERKY          CHAIN     PEE01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PEE01BK
TEST C                   DELETE    PEE01
     C                   ENDIF
     C*
     C* FALTP
     C     ERKY          CHAIN     PLT01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PLT01BK
TEST C                   DELETE    PLT01
     C                   ENDIF
     C* FAL2P
     C     ERKY          CHAIN     PL201                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PL201BK
TEST C                   DELETE    PL201
     C                   ENDIF
     C*
     C* DELETE RECON RECORD FROM FAEAP.
TEST C                   DELETE    PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* FDCA
     C*================================================================
     C     FDCA          BEGSR
     C* FDDAP
     C     ERKY          CHAIN     PDA01                              98
     C*** Beg Chg *** 01/06/15 *************************************
     C**** *IN98         IFEQ      *OFF
     C     *IN98         DOWEQ     *OFF
     C*** End Chg *** 01/06/15 *************************************
     C                   WRITE     PDA01BK
     C* FDCAP
     C*** Beg Del *** 01/06/15 *************************************
     C**** ERKY          CHAIN     PCA01                              99
     C*** End Del *** 01/06/15 *************************************
     C*** Beg Add *** 01/06/15 *************************************
     C*
     C                   EVAL      KKDSEQ = DADSEQ
     C                   EVAL      KKRCN  = DARCN#
     C*
     C     ERSKY         CHAIN     PCA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   WRITE     PCA01BK
     C                   DELETE    PCA01
     C*
     C                   ELSE
     C     SQKY          CHAIN     PCA024                             99
     C*** End Add *** 01/06/15 *************************************
     C     *IN99         IFEQ      *OFF
     C                   WRITE     PCA01BK
     C                   SUB       DACHRG        CACHRG
     C                   SUB       DAMRA         CAMRA
     C                   SUB       DANDIS        CANDIS
     C                   SUB       DAOTHR        CAOTHR
     C                   SUB       DAOTH2        CAOTH2
     C                   SUB       DAPAID        CAPAID
     C*** Beg Del *** 12/02/16 *************************************
     C****************** SUB       1             CARCN                          MOST CURRENT #
     C*** End Del *** 12/02/16 *************************************
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*** Beg Chg *** 01/06/15 *************************************
     C****************   UPDATE    PCA01
     C                   UPDATE    PCA024
     C*** End Chg *** 01/06/15 *************************************
     C                   ENDIF
     C*
     C*** Beg Add *** 01/06/15 *************************************
     C                   ENDIF
     C*** End Add *** 01/06/15 *************************************
     C*
     C* FDCCP
     C     ERKY          CHAIN     PCC01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PCC01BK
TEST C                   DELETE    PCC01
     C                   ENDIF
     C* FDCHP
     C     ERKY          CHAIN     PCH01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PCH01BK
TEST C                   DELETE    PCH01
     C                   ENDIF
     C* FDDEP
     C     ERKY          CHAIN     PDE01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PDE01BK
TEST C                   DELETE    PDE01
     C                   ENDIF
     C* FDMGP
     C     ERKY          CHAIN     PMG01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PMG01BK
TEST C                   DELETE    PMG01
     C                   ENDIF
     C*
TEST C                   DELETE    PDA01
     C*** Beg Chg *** 01/06/15 **************************************
     C****************** ENDIF
     C*
     C     ERKY          READE     PDA01                                  98
     C*
     C                   ENDDO
     C*** End Chg *** 01/06/15 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* EDI FILES
     C*================================================================
     C     EDI           BEGSR
     C* FEDDP
     C     ERKY          CHAIN     PDD01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PDD01BK
TEST C                   DELETE    PDD01
     C                   ENDIF
     C* FEDHP
     C     ERKY          CHAIN     PDH01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PDH01BK
TEST C                   DELETE    PDH01
     C                   ENDIF
     C* FEDPP
     C     ERKY          CHAIN     PDP01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PDP01BK
TEST C                   DELETE    PDP01
     C                   ENDIF
     C* FEDUP
     C     ERKY          CHAIN     PDU01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PDU01BK
TEST C                   DELETE    PDU01
     C                   ENDIF
     C* FEEDP
     C     ERKY          CHAIN     PED01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PED01BK
TEST C                   DELETE    PED01
     C                   ENDIF
     C* FEEHP
     C     ERKY          CHAIN     PEH01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PEH01BK
TEST C                   DELETE    PEH01
     C                   ENDIF
     C* FEEPP
     C     ERKY          CHAIN     PEP01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PEP01BK
TEST C                   DELETE    PEP01
     C                   ENDIF
     C* FEEUP
     C     ERKY          CHAIN     PEU01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PEU01BK
TEST C                   DELETE    PEU01
     C                   ENDIF
     C* FEE1P
     C     ERKY          CHAIN     PE101                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PE101BK
TEST C                   DELETE    PE101
     C                   ENDIF
     C* FELGP
     C     ERKY          CHAIN     PLG01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PLG01BK
TEST C                   DELETE    PLG01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MISC FILES
     C*================================================================
     C     MISC          BEGSR
     C* NMARP
     C     ERKY          CHAIN     PAR01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PAR01BK
TEST C                   DELETE    PAR01
     C                   ENDIF
     C* QFEMP
     C     ERKY          CHAIN     PEM01                              98
     C     *IN98         IFEQ      *OFF
     C                   WRITE     PEM01BK
TEST C                   DELETE    PEM01
     C                   ENDIF
     C*
     C                   ENDSR
