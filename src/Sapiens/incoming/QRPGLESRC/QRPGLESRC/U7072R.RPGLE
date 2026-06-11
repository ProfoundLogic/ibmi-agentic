     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U7072R - PROVIDER ANALYSIS REPORT WORKFILE BUILD PROGRAM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    WORKFILE BUILD PROGAM FOR PROVIDER ANALYSIS REPORT         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*   U7070C                                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*   NONE                                                        *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAA                *
     H*  CREATED: 12/10/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 01/23/13 C:287 Project: SY266_QRY                             *
     H*                Add DFTACTGRP and ACTGRP compile options       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*  60        GENERAL CHAIN                                      *
     H*****************************************************************
     FFMCML1    IF   E           K DISK
     FUXPAP     O  A E           K DISK
     FUMPCL1    IF   E           K DISK
     FUMPCL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FFDCAL5    IF   E           K DISK
     FFAEOL1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*
     IPPC02
     I              PCCO#                       P2CO#
     I              PCSUB#                      P2SUB#
     I              PCFND                       P2FND
     I              PCFYR                       P2FYR
     I              PCCASE                      P2CASE
     I              PCLIAB                      P2LIAB
     I              PCSTAT                      P2STAT
     I              PCADDT                      P2ADDT
     I              PCRVDT                      P2RVDT
     I              PCCERT                      P2CERT
     I              PCEXT                       P2EXT
     I              PCFEIN                      P2FEIN
     I              PCMOD                       P2MOD
     I              PCICD1                      P2ICD1
     I              PCICD2                      P2ICD2
     I              PCICD3                      P2ICD3
     I              PCICD4                      P2ICD4
     I              PCICD5                      P2ICD5
     I              PCCPT1                      P2CPT1
     I              PCCPT2                      P2CPT2
     I              PCCPT3                      P2CPT3
     I              PCCPT4                      P2CPT4
     I              PCCPT5                      P2CPT5
     I              PCCPT6                      P2CPT6
     I              PCCPT7                      P2CPT7
     I              PCCPT8                      P2CPT8
     I              PCSPD1                      P2SPD1
     I              PCSPD2                      P2SPD2
     I              PCSPD3                      P2SPD3
     I              PCSPD4                      P2SPD4
     I              PCSPD5                      P2SPD5
     I              PCSPD6                      P2SPD6
     I              PCSPD7                      P2SPD7
     I              PCSPD8                      P2SPD8
     I              PCEPD1                      P2EPD1
     I              PCEPD2                      P2EPD2
     I              PCEPD3                      P2EPD3
     I              PCEPD4                      P2EPD4
     I              PCEPD5                      P2EPD5
     I              PCEPD6                      P2EPD6
     I              PCEPD7                      P2EPD7
     I              PCEPD8                      P2EPD8
     I              PCA#V1                      P2A#V1
     I              PCA#V2                      P2A#V2
     I              PCA#V3                      P2A#V3
     I              PCA#V4                      P2A#V4
     I              PCA#V5                      P2A#V5
     I              PCA#V6                      P2A#V6
     I              PCA#V7                      P2A#V7
     I              PCA#V8                      P2A#V8
     I              PCTOS1                      PCTOS1
     I              PCTOS2                      P2TOS2
     I              PCTOS3                      P2TOS3
     I              PCTOS4                      P2TOS4
     I              PCTOS5                      P2TOS5
     I              PCTOS6                      P2TOS6
     I              PCTOS7                      P2TOS7
     I              PCTOS8                      P2TOS8
     I              PCINTR                      P2INTR
     I              PCPASG                      P2PASG
     I              PCREQD                      P2REQD
     I              PCCERD                      P2CERD
     I              PCRECD                      P2RECD
     I              PCPOS                       P2POS

     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C*   KEY LIST FOR THE PRE-CERT FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C*
     C*   KEY LIST FOR THE DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    EOEOB#
     C*
     C*   KEY LIST FOR THE EOMB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQTPYE            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    CMEMP#        XXEMP#                         9.0N
     C     *LIKE         DEFINE    CAFDAT        XXFDAT                         7.0N
     C     *LIKE         DEFINE    CATDAT        XXTDAT                         7.0N
     C*
     C                   Z-ADD     *ZEROS        XHFEIN            9 0          9.0N
     C                   Z-ADD     *ZEROS        XXDIFF            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXTSRV            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXAPPR            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXPEND            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXDENY            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXRQDY            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXCTDY            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXREDC            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXRCRT            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXTOTA            5 0          5.0N
     C                   Z-ADD     *ZEROS        XXSVDY            5 0          5.0N
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C*
     C* READ PRE-CERT FILE AND CHECK AGAINST CLAIM RECORDS
     C* THE PRE-CERT FILE HAS BEEN QUERIED BY THE PREVIOUS CLP SO WE
     C* ARE ONLY LOOKING AT PRE-CERT RECORDS THAT MEET THE SELECTION
     C* STATUS SELECTED ON PROMPT, IF XXEMP# IS NOT EQUAL TO BLANKS
     C* CHAIN TO CLAIM FILE AND FUTHER SELECT THE RECORS IF EMP# MATCHES
     C*
     C                   READ      PPC01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     PCSTAT        IFEQ      'C'
     C*
     C     XXEMP#        IFNE      *ZEROS
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C     CMEMP#        ANDEQ     XXEMP#
     C                   Z-ADD     CMEMP#        PAEMP#
     C                   EXSR      CRTWK
     C                   ELSE
     C                   GOTO      NXTRCD
     C                   END                                                    END *IN50
     C                   ELSE                                                   ELSE XXEMP#=0
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     CMEMP#        PAEMP#
     C                   EXSR      CRTWK
     C                   END                                                    END *IN50
     C                   END                                                    END XXEMP#
     C*
     C     NXTRCD        TAG
     C                   END                                                    END PCSTAT
     C                   READ      PPC01                                  60
     C                   END                                                    END DO *IN60
     C*
     C     XHFEIN        IFNE      *ZEROS
     C                   EXSR      WRTWRK
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C* CRTWK - SUBROUTINE TO FILL WORK FILE FIELDS TO BE WRITTEN TO WORK FILE
     C*****************************************************************
     C     CRTWK         BEGSR
     C*
     C     XHFEIN        IFNE      *ZEROS
     C     PCFEIN        IFNE      XHFEIN
     C                   EXSR      WRTWRK
     C                   END
     C                   END
     C*
     C     EOKEY         CHAIN     PEO01                              20
     C     *IN20         DOWEQ     '0'
     C     EOCERT        IFEQ      PCCERT
     C*
     C                   Z-ADD     0             XXTDAT
     C                   Z-ADD     0             XXFDAT
     C     CAKEY         CHAIN     PCA01                              60
     C   60              Z-ADD     *ZEROS        XXTSRV
     C     *IN60         DOWEQ     '0'
     C     CAFDAT        IFNE      XXFDAT
     C     CATDAT        ANDNE     XXTDAT
     C     CATDAT        SUB       CAFDAT        XXDIFF
     C     XXDIFF        IFEQ      0
     C                   ADD       1             XXTSRV
     C                   ELSE
     C                   ADD       XXDIFF        XXTSRV
     C                   END
     C                   Z-ADD     0             XXDIFF
     C                   Z-ADD     CATDAT        XXTDAT
     C                   Z-ADD     CAFDAT        XXFDAT
     C                   END                                                    END CAFDAT
     C     CAKEY         READE     PCA01                                  60
     C                   END                                                    END *IN60
     C                   END                                                    END EOCERT
     C     EOKEY         READE     PEO01                                  20
     C                   END                                                    END *IN20
     C*
     C                   MOVE      *BLANKS       XXCERT
     C                   MOVE      PCCERT        XXCERT
     C*
     C* CHAIN TO PRE-CERT FILE TO ADD RE-CERTS TO ALL NECESSARY TOTALS
     C*
     C     PCKEY         CHAIN     PPC02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* CALCULATE THE TOTAL NUMBER OF APPROVED, DENIED AND PENDED
     C*
     C     PCLIAB        IFEQ      'A'
     C                   ADD       1             XXAPPR
     C                   ELSE
     C     PCLIAB        IFEQ      'P'
     C                   ADD       1             XXPEND
     C                   ELSE
     C     PCLIAB        IFEQ      'D'
     C                   ADD       1             XXDENY
     C                   END
     C                   END
     C                   END
     C*
     C* CALCULATE THE REDUCED # OF CERTS(TOT REC.DAYS LESS THAN
     C* TOT ACTUAL DAYS FROM THE DETAIL CHARGE FILE)
     C*
     C                   ADD       P2REQD        XXRQDY
     C*********************ADD  P2CERD    PACTDY
     C                   ADD       P2RECD        XXCTDY
     C*
     C     XXCTDY        IFLT      XXTSRV
     C                   ADD       1             XXREDC
     C                   END
     C*
     C* ADD UP THE NUMBER OF RE-CERT PROCESSED FOR THIS CERT RECORD
     C*
     C     P2EXT         IFNE      *ZEROS
     C                   ADD       1             XXRCRT
     C                   END
     C*
     C* IF THE STATUS IS NOT EQUAL TO 'C' ADD TO TOTAL ACTIVITY
     C*
     C     P2STAT        IFEQ      'C'
     C                   ADD       1             XXTOTA
     C                   END
     C*
     C     PCKEY         READE     PPC02                                  60
     C                   END                                                    END *IN60
     C*
     C* CALCULATE THE TOTAL DAYS SAVED
     C*
     C     XXCTDY        SUB       XXTSRV        XXSVDY
     C     XXSVDY        IFLT      *ZEROS
     C                   Z-ADD     *ZEROS        XXSVDY
     C                   END
     C*
     C                   Z-ADD     PCFEIN        XHFEIN
     C*
     C                   ENDSR
     C*****************************************************************
     C* WRTWK - SUBROUTINE TO WRITE RECORDS TO WORKFILE
     C*****************************************************************
     C     WRTWRK        BEGSR
     C*
     C                   Z-ADD     PCCO#         PACO#
     C                   Z-ADD     PCSUB#        PASUB#
     C                   Z-ADD     PCFND         PAFND
     C                   Z-ADD     XHFEIN        PAFEIN
     C                   Z-ADD     XXAPPR        PAAPPR
     C                   Z-ADD     XXPEND        PAPEND
     C                   Z-ADD     XXDENY        PADENY
     C                   Z-ADD     XXRQDY        PARQDY
     C                   Z-ADD     XXCTDY        PACTDY
     C                   Z-ADD     XXREDC        PAREDC
     C                   Z-ADD     XXRCRT        PARCRT
     C                   Z-ADD     XXTOTA        PATOTA
     C                   Z-ADD     XXSVDY        PASVDY
     C*
     C                   Z-ADD     *ZEROS        XXAPPR
     C                   Z-ADD     *ZEROS        XXPEND
     C                   Z-ADD     *ZEROS        XXDENY
     C                   Z-ADD     *ZEROS        XXRQDY
     C                   Z-ADD     *ZEROS        XXCTDY
     C                   Z-ADD     *ZEROS        XXREDC
     C                   Z-ADD     *ZEROS        XXRCRT
     C                   Z-ADD     *ZEROS        XXTOTA
     C                   Z-ADD     *ZEROS        XXSVDY
     C*
     C                   WRITE     PPA01
     C*
     C                   ENDSR
