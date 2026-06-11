     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H***** Beg Add *** 06/09/20 ************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H*
     H* REVISIONS
     H*
     H* 06/09/20 C:295 Project: SY420
     H*                -Mask SSN
     H*
     H* 05/05/99 C:56  Project: MC152
     H*                   Change FAEAL to FAEAL1, duplicate logicals
     H*
     H*****************************************************************
     FFAEOL     IF   E           K DISK
     FNAMBL     IF   E           K DISK
     FNASBL     IF   E           K DISK
     FNDTML     IF   E           K DISK
     F*** BEG CHG *** 05/05/99 ****************************************
     F*********FFAEAL   IF  E           K        DISK
     FFAEAL1    IF   E           K DISK
     F*** END CHG *** 05/05/99 ****************************************
     FFAEDL     IF   E           K DISK
     FFAEEL     IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     FFAESL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAE1L     IF   E           K DISK
     FFAHBL     IF   E           K DISK
     FFDCALD    IF   E           K DISK
     FFADUL     IF   E           K DISK
     FFDDAL     IF   E           K DISK
     FFDDDL     IF   E           K DISK
     FFDDEL     IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDPL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFDHCL     IF   E           K DISK
     FFDMGL     IF   E           K DISK
     FSDCMLN    IF   E           K DISK
     FSDCMLO    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSMARL     IF   E           K DISK
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
      *  DARTS KEY (SC010R) FOR LINK WITH WCMS
      *
     D                 DS
     D  KWKEY                  1     16
     D  KWCO#                  1      3  0
     D  KWFND                  4      6  0
     D  KWFYR                  7      9  0
     D  KWCASE                10     16  0
      *
      *  DARTS KEY (SC010R) FOR OUTSIDE ACCOUNT
     D                 DS
     D  KMKEY                  1     21
     D  KKCO#                  1      3  0
     D  KKSUB#                 4      8  0
     D  KKFND                  9     11  0
     D  KKFYR                 12     14  0
     D  KKCASE                15     21  0

     I*
     IPDU02
     I              DUCO#                       D2CO#
     I              DUSUB#                      D2SUB#
     I              DUFND                       D2FND
     I              DUFYR                       D2FYR
     I              DUCASE                      D2CASE
     I              DUACDT                      D2ACDT
     I              DUSS#                       D2SS#
     I              DUUSER                      D2USER
     I              DUTIME                      D2TIME
     I              DUPGM                       D2PGM
     I              DUDT                        D2DT
      *
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
     C     DMKEY         KLIST
     C                   KFLD                    KMKEY            21
     C*
     C     DWKEY         KLIST
     C                   KFLD                    KWKEY            16
     C*
     C     ARKEYS        KLIST
     C                   KFLD                    ARKKEY           40
     C                   KFLD                    ARKSEQ
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    MBPGM         KKFILE
     C     *LIKE         DEFINE    MBPGM         SVFILE
     C     *LIKE         DEFINE    MBPGM         OOFILE
     C     *LIKE         DEFINE    MBPGM         KKKEY
     C     *LIKE         DEFINE    MBSS#         KKSS#
     C     *LIKE         DEFINE    MBSS#         @@SS#
     C     *LIKE         DEFINE    MBCO#         @@CO#
     C     *LIKE         DEFINE    MBSUB#        @@SUB#
     C     *LIKE         DEFINE    MBFND         @@FND
     C     *LIKE         DEFINE    MBFYR         @@FYR
     C     *LIKE         DEFINE    MBCASE        @@CASE
     C     *LIKE         DEFINE    MBSS#         OOSS#
     C     *LIKE         DEFINE    MBCO#         OOCO#
     C     *LIKE         DEFINE    MBSUB#        OOSUB#
     C     *LIKE         DEFINE    MBFND         OOFND
     C     *LIKE         DEFINE    MBFYR         OOFYR
     C     *LIKE         DEFINE    MBCASE        OOCASE
     C     *LIKE         DEFINE    MBACDT        OOACDT
     C     *LIKE         DEFINE    MBPGM         OOPGM
     C     *LIKE         DEFINE    TMSEQ#        OOSEQ#
     C     *LIKE         DEFINE    CADSEQ        OODSEQ
     C     *LIKE         DEFINE    CADSEQ        OOEOB#
     C     *LIKE         DEFINE    CMSEQ#        ARKSEQ
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
     C                   Z-ADD     @@CO#         KWCO#
     C                   Z-ADD     @@FND         KWFND
     C                   Z-ADD     @@FYR         KWFYR
     C                   Z-ADD     @@CASE        KWCASE
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
     C                   EXCEPT    TOT
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* MOVEFI - FILES THAT ARE BEING UPDATED
     C*================================================================
     C     MOVEFI        BEGSR
     C*
     C                   MOVEL(P)  'FAEOP'       KKFILE
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
     C                   MOVEL     EOPGM         OOPGM
     C                   Z-ADD     EOEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PEO01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NAMBP
     C*
     C                   MOVEL(P)  'NAMBP'       KKFILE
     C*
     C     GENKY         SETLL     PMB01
     C     GENKY         READE     PMB01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     MBCO#         OOCO#
     C                   Z-ADD     MBSUB#        OOSUB#
     C                   Z-ADD     MBFND         OOFND
     C                   Z-ADD     MBFYR         OOFYR
     C                   Z-ADD     MBCASE        OOCASE
     C                   MOVEL     MBSS#         OOSS#
     C                   Z-ADD     MBACDT        OOACDT
     C                   MOVEL     MBPGM         OOPGM
     C                   Z-ADD     MBEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PMB01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NASBP
     C*
     C                   MOVEL(P)  'NASBP'       KKFILE
     C*
     C     GENKY         SETLL     PSB01
     C     GENKY         READE     PSB01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     SBCO#         OOCO#
     C                   Z-ADD     SBSUB#        OOSUB#
     C                   Z-ADD     SBFND         OOFND
     C                   Z-ADD     SBFYR         OOFYR
     C                   Z-ADD     SBCASE        OOCASE
     C                   MOVEL     SBSS#         OOSS#
     C                   Z-ADD     SBACDT        OOACDT
     C                   MOVEL     SBPGM         OOPGM
     C                   Z-ADD     SBEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PSB01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE NDTMP
     C*
     C                   MOVEL(P)  'NDTMP'       KKFILE
     C*
     C     GENKY         SETLL     PTM01
     C     GENKY         READE     PTM01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     TMCO#         OOCO#
     C                   Z-ADD     TMSUB#        OOSUB#
     C                   Z-ADD     TMFND         OOFND
     C                   Z-ADD     TMFYR         OOFYR
     C                   Z-ADD     TMCASE        OOCASE
     C                   MOVEL     TMPGM         OOPGM
     C                   Z-ADD     TMEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PTM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAEAP
     C*
     C                   MOVEL(P)  'FAEAP'       KKFILE
     C*
     C     GENKY         SETLL     PEA01
     C     GENKY         READE     PEA01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EACO#         OOCO#
     C                   Z-ADD     EASUB#        OOSUB#
     C                   Z-ADD     EAFND         OOFND
     C                   Z-ADD     EAFYR         OOFYR
     C                   Z-ADD     EACASE        OOCASE
     C                   MOVEL     EASS#         OOSS#
     C                   Z-ADD     EAACDT        OOACDT
     C                   MOVEL     EAPGM         OOPGM
     C                   Z-ADD     EAEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PEA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAEDP
     C*
     C                   MOVEL(P)  'FAEDP'       KKFILE
     C*
     C     GENKY         SETLL     PED01
     C     GENKY         READE     PED01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EDCO#         OOCO#
     C                   Z-ADD     EDSUB#        OOSUB#
     C                   Z-ADD     EDFND         OOFND
     C                   Z-ADD     EDFYR         OOFYR
     C                   Z-ADD     EDCASE        OOCASE
     C                   MOVEL     EDSS#         OOSS#
     C                   Z-ADD     EDACDT        OOACDT
     C                   MOVEL     EDPGM         OOPGM
     C                   Z-ADD     EDEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PED01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAEEP
     C*
     C                   MOVEL(P)  'FAEEP'       KKFILE
     C*
     C     GENKY         SETLL     PEE01
     C     GENKY         READE     PEE01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EECO#         OOCO#
     C                   Z-ADD     EESUB#        OOSUB#
     C                   Z-ADD     EEFND         OOFND
     C                   Z-ADD     EEFYR         OOFYR
     C                   Z-ADD     EECASE        OOCASE
     C                   MOVEL     EESS#         OOSS#
     C                   Z-ADD     EEACDT        OOACDT
     C                   MOVEL     EEPGM         OOPGM
     C                   Z-ADD     EEEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PEE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAEHP
     C*
     C                   MOVEL(P)  'FAEHP'       KKFILE
     C*
     C     GENKY         SETLL     PEH01
     C     GENKY         READE     PEH01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EHCO#         OOCO#
     C                   Z-ADD     EHSUB#        OOSUB#
     C                   Z-ADD     EHFND         OOFND
     C                   Z-ADD     EHFYR         OOFYR
     C                   Z-ADD     EHCASE        OOCASE
     C                   MOVEL     EHSS#         OOSS#
     C                   Z-ADD     EHACDT        OOACDT
     C                   MOVEL     EHPGM         OOPGM
     C                   Z-ADD     EHEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PEH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAEPP
     C*
     C                   MOVEL(P)  'FAEPP'       KKFILE
     C*
     C     GENKY         SETLL     PEP01
     C     GENKY         READE     PEP01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EPCO#         OOCO#
     C                   Z-ADD     EPSUB#        OOSUB#
     C                   Z-ADD     EPFND         OOFND
     C                   Z-ADD     EPFYR         OOFYR
     C                   Z-ADD     EPCASE        OOCASE
     C                   MOVEL     EPSS#         OOSS#
     C                   Z-ADD     EPACDT        OOACDT
     C                   MOVEL     EPPGM         OOPGM
     C                   Z-ADD     EPEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PEP01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAESP
     C*
     C                   MOVEL(P)  'FAESP'       KKFILE
     C*
     C     GENKY         SETLL     PES01
     C     GENKY         READE     PES01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     ESCO#         OOCO#
     C                   Z-ADD     ESSUB#        OOSUB#
     C                   Z-ADD     ESFND         OOFND
     C                   Z-ADD     ESFYR         OOFYR
     C                   Z-ADD     ESCASE        OOCASE
     C                   MOVEL     ESSS#         OOSS#
     C                   Z-ADD     ESACDT        OOACDT
     C                   MOVEL     ESPGM         OOPGM
     C                   Z-ADD     ESEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PES01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAEUP
     C*
     C                   MOVEL(P)  'FAEUP'       KKFILE
     C*
     C     GENKY         SETLL     PEU01
     C     GENKY         READE     PEU01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EUCO#         OOCO#
     C                   Z-ADD     EUSUB#        OOSUB#
     C                   Z-ADD     EUFND         OOFND
     C                   Z-ADD     EUFYR         OOFYR
     C                   Z-ADD     EUCASE        OOCASE
     C                   MOVEL     EUSS#         OOSS#
     C                   Z-ADD     EUACDT        OOACDT
     C                   MOVEL     EUPGM         OOPGM
     C                   Z-ADD     EUEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PEU01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAE1P
     C*
     C                   MOVEL(P)  'FAE1P'       KKFILE
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
     C                   MOVEL     E1PGM         OOPGM
     C                   Z-ADD     E1EOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PE101                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FAHBP
     C*
     C                   MOVEL(P)  'FAHBP'       KKFILE
     C*
     C     GENKY         SETLL     PHB01
     C     GENKY         READE     PHB01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     HBCO#         OOCO#
     C                   Z-ADD     HBSUB#        OOSUB#
     C                   Z-ADD     HBFND         OOFND
     C                   Z-ADD     HBFYR         OOFYR
     C                   Z-ADD     HBCASE        OOCASE
     C                   MOVEL     HBSS#         OOSS#
     C                   Z-ADD     HBACDT        OOACDT
     C                   MOVEL     HBPGM         OOPGM
     C                   Z-ADD     HBEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PHB01                                  99
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
     C                   MOVEL     CAPGM         OOPGM
     C                   Z-ADD     CADSEQ        OODSEQ
     C                   Z-ADD     CAEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PCA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FADUP
     C*
     C                   MOVEL(P)  'FDDUP'       KKFILE
     C*
     C     GENKY         SETLL     PDU01
     C     GENKY         READE     PDU01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DUCO#         OOCO#
     C                   Z-ADD     DUSUB#        OOSUB#
     C                   Z-ADD     DUFND         OOFND
     C                   Z-ADD     DUFYR         OOFYR
     C                   Z-ADD     DUCASE        OOCASE
     C                   MOVEL     DUSS#         OOSS#
     C                   Z-ADD     DUACDT        OOACDT
     C                   MOVEL     DUPGM         OOPGM
     C                   Z-ADD     DUDSEQ        OODSEQ
     C                   Z-ADD     DUEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDU01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDDAP
     C*
     C                   MOVEL(P)  'FDDAP'       KKFILE
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
     C                   MOVEL     DASS#         OOSS#
     C                   Z-ADD     DAACDT        OOACDT
     C                   MOVEL     DAPGM         OOPGM
     C                   Z-ADD     DADSEQ        OODSEQ
     C                   Z-ADD     DAEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDA01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDDDP
     C*
     C                   MOVEL(P)  'FDDDP'       KKFILE
     C*
     C     GENKY         SETLL     PDD01
     C     GENKY         READE     PDD01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DDCO#         OOCO#
     C                   Z-ADD     DDSUB#        OOSUB#
     C                   Z-ADD     DDFND         OOFND
     C                   Z-ADD     DDFYR         OOFYR
     C                   Z-ADD     DDCASE        OOCASE
     C                   MOVEL     DDSS#         OOSS#
     C                   Z-ADD     DDACDT        OOACDT
     C                   MOVEL     DDPGM         OOPGM
     C                   Z-ADD     DDDSEQ        OODSEQ
     C                   Z-ADD     DDEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDD01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDDEP
     C*
     C                   MOVEL(P)  'FDDEP'       KKFILE
     C*
     C     GENKY         SETLL     PDE01
     C     GENKY         READE     PDE01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DECO#         OOCO#
     C                   Z-ADD     DESUB#        OOSUB#
     C                   Z-ADD     DEFND         OOFND
     C                   Z-ADD     DEFYR         OOFYR
     C                   Z-ADD     DECASE        OOCASE
     C                   MOVEL     DESS#         OOSS#
     C                   Z-ADD     DEACDT        OOACDT
     C                   MOVEL     DEPGM         OOPGM
     C                   Z-ADD     DEDSEQ        OODSEQ
     C                   Z-ADD     DEEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDDHP
     C*
     C                   MOVEL(P)  'FDDHP'       KKFILE
     C*
     C     GENKY         SETLL     PDH01
     C     GENKY         READE     PDH01                                  99
     C   99              EXSR      PRINT
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DHCO#         OOCO#
     C                   Z-ADD     DHSUB#        OOSUB#
     C                   Z-ADD     DHFND         OOFND
     C                   Z-ADD     DHFYR         OOFYR
     C                   Z-ADD     DHCASE        OOCASE
     C                   MOVEL     DHSS#         OOSS#
     C                   Z-ADD     DHACDT        OOACDT
     C                   MOVEL     DHPGM         OOPGM
     C                   Z-ADD     DHDSEQ        OODSEQ
     C                   Z-ADD     DHEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDH01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDDPP
     C*
     C                   MOVEL(P)  'FDDPP'       KKFILE
     C*
     C     GENKY         SETLL     PDP01
     C     GENKY         READE     PDP01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DPCO#         OOCO#
     C                   Z-ADD     DPSUB#        OOSUB#
     C                   Z-ADD     DPFND         OOFND
     C                   Z-ADD     DPFYR         OOFYR
     C                   Z-ADD     DPCASE        OOCASE
     C                   MOVEL     DPSS#         OOSS#
     C                   Z-ADD     DPACDT        OOACDT
     C                   MOVEL     DPPGM         OOPGM
     C                   Z-ADD     DPDSEQ        OODSEQ
     C                   Z-ADD     DPEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDP01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDDUP
     C*
     C                   MOVEL(P)  'FDDUP'       KKFILE
     C*
     C     GENKY         SETLL     PDU02
     C     GENKY         READE     PDU02                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     DUCO#         OOCO#
     C                   Z-ADD     DUSUB#        OOSUB#
     C                   Z-ADD     DUFND         OOFND
     C                   Z-ADD     DUFYR         OOFYR
     C                   Z-ADD     DUCASE        OOCASE
     C                   MOVEL     DUSS#         OOSS#
     C                   Z-ADD     DUACDT        OOACDT
     C                   MOVEL     DUPGM         OOPGM
     C                   Z-ADD     DUDSEQ        OODSEQ
     C                   Z-ADD     DUEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PDU02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDHCP
     C*
     C                   MOVEL(P)  'FDHCP'       KKFILE
     C*
     C     GENKY         SETLL     PHC01
     C     GENKY         READE     PHC01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     HCCO#         OOCO#
     C                   Z-ADD     HCSUB#        OOSUB#
     C                   Z-ADD     HCFND         OOFND
     C                   Z-ADD     HCFYR         OOFYR
     C                   Z-ADD     HCCASE        OOCASE
     C                   MOVEL     HCPGM         OOPGM
     C                   Z-ADD     HCDSEQ        OODSEQ
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PHC01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** FILE FDMGP
     C*
     C                   MOVEL(P)  'FDMGP'       KKFILE
     C*
     C     GENKY         SETLL     PMG01
     C     GENKY         READE     PMG01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     MGCO#         OOCO#
     C                   Z-ADD     MGSUB#        OOSUB#
     C                   Z-ADD     MGFND         OOFND
     C                   Z-ADD     MGFYR         OOFYR
     C                   Z-ADD     MGCASE        OOCASE
     C                   MOVEL     MGPGM         OOPGM
     C                   Z-ADD     MGDSEQ        OODSEQ
     C                   Z-ADD     MGDCS#        OOSEQ#
     C                   Z-ADD     MGEOB#        OOEOB#
     C*
     C                   EXSR      PRINT
     C     GENKY         READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C*** SETON DARTS OUTPUT INDICATOR
     C                   MOVE      *ON           *IN70
     C*** FILE SDCMP-M
     C*
     C                   MOVEL(P)  'SDCMP-M'     KKFILE
     C*
     C     DMKEY         SETLL     PCM01
     C     DMKEY         READE     PCM01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL(P)  'SDCMP-M'     KKFILE
     C                   MOVEL     CMKEY         OOKEY            40
     C                   MOVEL     CMPGM         OOPGM
     C                   Z-ADD     CMSEQ#        OOSEQ#
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE SMARP-M
     C*
     C                   MOVEL(P)  'SMARP-M'     KKFILE
     C                   MOVEL     CMKEY         ARKKEY
     C                   Z-ADD     CMSEQ#        ARKSEQ
     C*
     C     ARKEYS        SETLL     PAR01
     C     ARKEYS        READE     PAR01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL     ARKEY         OOKEY
     C                   MOVEL     ARPGM         OOPGM
     C                   Z-ADD     ARSEQ#        OOSEQ#
     C                   Z-ADD     ARASQ#        OODSEQ
     C*
     C                   EXSR      PRINT
     C     ARKEYS        READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C     DMKEY         READE     PCM01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*** FILE SDCMP-W
     C*
     C                   MOVEL(P)  'SDCMP-W'     KKFILE
     C*
     C     DWKEY         SETLL     PCM02
     C     DWKEY         READE     PCM02                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL(P)  'SDCMP-W'     KKFILE
     C                   MOVEL     CMKEY         OOKEY            40
     C                   MOVEL     CMPGM         OOPGM
     C                   Z-ADD     CMSEQ#        OOSEQ#
     C*
     C                   EXSR      PRINT
     C*
     C*** FILE SMARP-W
     C*
     C                   MOVEL(P)  'SMARP-W'     KKFILE
     C                   MOVEL     CMKEY         ARKKEY
     C                   Z-ADD     CMSEQ#        ARKSEQ
     C*
     C     ARKEYS        SETLL     PAR01
     C     ARKEYS        READE     PAR01                                  99
     C   99              EXSR      PRINT
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL     ARKEY         OOKEY
     C                   MOVEL     ARPGM         OOPGM
     C                   Z-ADD     ARSEQ#        OOSEQ#
     C                   Z-ADD     ARASQ#        OODSEQ
     C*
     C                   EXSR      PRINT
     C     ARKEYS        READE     PAR01                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C     DWKEY         READE     PCM02                                  99
     C                   ENDDO                                                  *IN99 = OFF
     C*
     C                   ENDSR
     C*
     C     PRINT         BEGSR
     C     *INOA         IFEQ      '1'
     C                   MOVEL     KKFILE        OOFILE
     C                   MOVE      *BLANKS       SVFILE
     C                   EXCEPT    HEAD
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
     C  N70              EXCEPT    DET
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        OOSS# = UnMaskData('SSN':'OOSS#':OOSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C   70              EXCEPT    DETS
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
     O                       OOEOB#        ZB   104
     O                       OOPGM          B   120
     O*
     O          E            DETS           1
     O                       OOKEY          B    60
     O                       OODSEQ        ZB    80
     O                       OOSEQ#        ZB    91
     O                       OOPGM          B   120
     O*
     O          E            TOT         1  2
     O                       XXCNT         Z     58
     O                                           77 'RECORDS PROCESSED'
