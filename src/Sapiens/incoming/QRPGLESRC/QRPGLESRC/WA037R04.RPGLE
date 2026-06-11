     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - WA037R04 - DELETE A CLAIM
     H*****************************************************************
     H*                                                               *
     H* CREATED: 12/21/05     C:15       Proj: 139WAI                 *
     H*****************************************************************
     H*                                                               *
     H* This API is used to delete records from all claim files for   *
     H* a selected claim.                                             *
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/12/11 C:246 SUPPORT: 75676 CLT-132                         *
     H*                Add Darts Deletion.                            *
     H*                                                               *
     H* 02/02/11 C:246 SUPPORT: 75676 CLT-132                         *
     H*                The claim import process abends beacuse of     *
     H*                record lock.Change the logical file WMCMP      *
     H*                instead of WMCML1 as this file is used in W9111R
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*  60         GENERAL USAGE (CHAIN)                             *
     H*                                                               *
     H*****************************************************************
      *
      * Claim Deletion/Detail File
     FWACMP     O    E             DISK
     F                                     RENAME(PCM01:PCM02)
      * Claim Transfer File
     FWACTP     O    E             DISK
      * Diary Transfer Work File
     FWADTL     UF   E           K DISK
      * Request Payment Transaction File
     FWARPL     UF   E           K DISK
     FWARPPML   UF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
      * Auxiliary Compensation Payee File
     FWDACL     UF   E           K DISK
      * Worker's Comp Check Reconciliation Detail File
     FWDCRLA    UF   E           K DISK
      * Deductible Detail File
     FWDDDL     UF   E           K DISK
      * Claimant Name/Address Tracking File
     FWDNTL     UF   E           K DISK
      * Payment Detail File
     FWDPAL     UF   E           K DISK
      * View Check Detail File
     FWDRPL     UF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
      * Average Weekly Wage History File
     FWMAWL     UF   E           K DISK
      * Benefit History File
     FWMBHL     UF   E           K DISK
      * Benefit Adjustment History File
     FWMBNL     UF   E           K DISK
      * Additional Body Part File
     FWMBYL     UF   E           K DISK
      * Claim History File
     FWMCHL     UF   E           K DISK
      * Claim Master File
      **** BEG CHG 02/02/11*******************************************
     F**WMCML1  UF  E           K        DISK
     FWMCMLP    UF   E           K DISK
     F                                     RENAME(PCM01:PCM08)
      **** END CHG 02/02/11*******************************************
     FWMC1L     UF   E           K DISK
      * Diary Date File
     FWMDDL     UF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FWMIHL     UF   E           K DISK
      * EDI Log History File
     FWMLGL     UF   E           K DISK
      *Claim Legal master File
     FWMLLL     UF   E           K DISK
      * Late Payment File
     FWMLPL     UF   E           K DISK
      * State NOI Info File
     FWMNIL     UF   E           K DISK
      * Reserve History File
     FWMRHL     UF   E           K DISK
      * Refund Request File
     FWMRRL     UF   E           K DISK
      * Lost Time Transfer History File
     FWMTHL     UF   E           K DISK
     FWMTIL     UF   E           K DISK
      * Treating Physician History File
     FWMTPL     UF   E           K DISK
      * Work History File
     FWMWHL     UF   E           K DISK
     F*  DWC13 Initail Summary File
     FWSFRL     UF   E           K DISK
     F*** Begin Add *** 09/12/11 **************************************
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FSMARL     UF   E           K DISK
     F*** End   Add *** 09/12/11 **************************************

     D*
     D REQ             S              1    DIM(12)
     D AMT             S              1    DIM(9) ASCEND
     D*** End   Add *** 09/12/11 **************************************
     D*
     D MSG1            C                   CONST('*** DELETION ***')
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** Begin Add *** 09/12/11 **************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3
     D  QCFND                  4      6
     D  QCFYR                  7      9
     D  QCCASE                10     16
     D*** End   Add *** 09/12/11 **************************************
     D                 DS
     D  CRKY                   1     15
     D  CRFYR                  1      3
     D  CRCASE                 4     10
     D                 DS
     D  XRKY                   1     15
     D  XRFYR                  1      3  0
     D  XRCASE                 4     10  0
     D                 DS
     D  XXAMTS                 1      9  2
     D  XXAMTC                 1      9
     D                 DS
     D  RSU                    1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** Begin Add *** 09/12/11 **************************************
     IPCM03
     I              CMKEY                       C3KEY
     I              CMSEQ#                      C3SEQ#
     I              CMSTAT                      C3STAT
     I              CMPGM                       C3PGM
     I              CMTIME                      C3TIME
     I              CMDT                        C3DT
     I              CMDCDT                      C3DCDT
     I              CMDIFL                      C3DIFL

     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
      **** BEG ADD 02/02/11*******************************************
     C                   PARM                    @@EMP#            9 0
      **** END ADD 02/02/11*******************************************
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@CALL            1
      *
      *****************************************************************
      *  KEYS
      *****************************************************************
     C*** Begin Add *** 09/12/11 **************************************
     C*
     C* DARTS Key
     C*
     C     ARKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    C3SEQ#
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*** End   Add *** 09/12/11 **************************************
      *
      * Key used to retrieve 'From' data from files
      *
     C     FRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      **** BEG ADD 02/02/11*******************************************
      * Key used to retrieve 'From' data from files
     C     FRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      **** END ADD 02/02/11*******************************************
      *
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *****************************************************************
      *  LIKE DEFINITIONS
      *****************************************************************
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CTTINC        XXTINC
     C     *LIKE         DEFINE    CTRSV         XXAMT
     C     *LIKE         DEFINE    DDAMTB        XXAMTB
     C     *LIKE         DEFINE    CMDT          XXDATE
     C*****BEG ADD 02/02/11 *************************************
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C*****BEG END 02/02/11 *************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
      *****************************************************************
      *  MAIN
      *****************************************************************
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C*****BEG ADD 02/02/11 *************************************
     C                   MOVE      @@EMP#        XXEMP#
     C*****BEG END 02/02/11 *************************************
     C                   Z-ADD     0             XXTINC
     C                   Z-ADD     0             XXAMTB
     C                   MOVE      *BLANKS       XXREQ            12
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Y                 3 0
      *
      *  Get the Claim Master record.
      *
      **** BEG CHG 02/02/11*******************************************
     C***        FRKY      CHAINPCM01                99
     C     FRKY1         CHAIN     PCM08                              99
      **** END CHG 02/02/11*******************************************
     C     *IN99         IFEQ      *OFF
      *
      *  Delete records from the various claim files.
      *
     C     FRKY          CHAIN     PDT01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PDT01
     C     FRKY          READE     PDT01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PRP01
     C     FRKY          READE     PRP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRP03                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PRP03
     C     FRKY          READE     PRP03                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAC01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PAC01
     C     FRKY          READE     PAC01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDD01                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       DDAMTB        XXAMTB
     C                   DELETE    PDD01
     C     FRKY          READE     PDD01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PNT01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PNT01
     C     FRKY          READE     PNT01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PPA01
     C                   ADD       1             XXCNT
     C     FRKY          READE     PPA01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRP02                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PRP02
     C     FRKY          READE     PRP02                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PAW01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PAW01
     C     FRKY          READE     PAW01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PBH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PBH01
     C     FRKY          READE     PBH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PBN01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PBN01
     C     FRKY          READE     PBN01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PBY01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PBY01
     C     FRKY          READE     PBY01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PCH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PCH01
     C     FRKY          READE     PCH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PDD02                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PDD02
     C     FRKY          READE     PDD02                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PLG01
     C     FRKY          READE     PLG01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PLL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PLL01
     C     FRKY          READE     PLL01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PLP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PLP01
     C     FRKY          READE     PLP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PNI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PNI01
     C     FRKY          READE     PNI01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   XFOOT     RSU           XXAMT
     C                   ADD       XXAMT         XXTINC
     C*
     C                   DELETE    PRH01
     C     FRKY          READE     PRH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PRR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PRR01
     C     FRKY          READE     PRR01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PTH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PTH01
     C     FRKY          READE     PTH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PTP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PTP01
     C     FRKY          READE     PTP01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PWH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PWH01
     C     FRKY          READE     PWH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PC101                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PC101
     C     FRKY          READE     PC101                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PIH01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PIH01
     C     FRKY          READE     PIH01                                  60
     C                   ENDDO
      *
     C     FRKY          CHAIN     PTI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PTI01
     C     FRKY          READE     PTI01                                  60
     C                   ENDDO
      *
      *
     C     FRKY          CHAIN     PFR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PFR01
     C     FRKY          READE     PFR01                                  60
     C                   ENDDO
      *
      *
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     *OFF
     C                   TESTN                   CRFYR                71
     C                   TESTN                   CRCASE               72
     C     *IN71         IFEQ      *ON
     C     *IN72         ANDEQ     *ON
     C                   MOVE      CRKY          XRKY
     C     XRFYR         IFEQ      XXFYR
     C     XRCASE        ANDEQ     XXCASE
     C                   DELETE    PCR01
     C                   ENDIF
     C                   ENDIF
     C     CRKEY         READE     PCR01                                  60
     C                   ENDDO
     C*** Begin Add *** 09/12/11 **************************************
     C*
     C* Darts Soft Deletion
     C*
     C*  INACTIVATE DARTS REMARKS AND THE CORRESPONDING ADDT'L RMKS
     C*  AND LOAD DIARY COMPLETION DATE TO DIARY ITEMS
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C*
     C*  CHAIN TO DARTS REMARKS DETAIL FILE
     C*
     C     C3KY          CHAIN     PCM03                              99
B1   C     *IN99         DOWEQ     '0'
     C*
     C     C3DIFL        IFEQ      'Y'
     C                   Z-ADD     XXDATE        C3DCDT
     C                   ELSE
     C                   MOVE      'I'           C3STAT
     C                   END                                                    ENDIF 'Y'
     C*
     C*  CHAIN TO ADDITIONAL REMARKS FILE
     C*
     C     ARKY          CHAIN     PAR01                              99
     C     *IN99         DOWEQ     '0'
     C                   MOVE      'I'           ARSTAT
     C                   MOVEL     Q1PGM         ARPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     XXDATE        ARDT
     C                   UPDATE    PAR01
     C     ARKY          READE     PAR01                                  99
     C                   ENDDO                                                  ENDDO ARKY
     C*
     C                   MOVEL     Q1PGM         C3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        C3TIME
     C                   Z-ADD     XXDATE        C3DT
     C                   UPDATE    PCM03
     C     C3KY          READE     PCM03                                  99
E1   C                   ENDDO                                                  DOWEQ '0'
     C*
     C*** End   Add *** 09/12/11 **************************************
      *
      *  Write a record to the Claim Transfer File.
      *
     C                   Z-ADD     CMCO#         CTCO#
     C                   Z-ADD     CMFND         CTFND
     C                   Z-ADD     CMFYR         CTFYR
     C                   Z-ADD     CMEMP#        CTEMP#
     C                   Z-ADD     CMDIV         CTDIV
     C                   Z-ADD     CMCASE        CTCASE
     C                   MOVE      CMSS#         CTSS#
     C                   Z-ADD     CMCLAS        CTCLAS
     C                   Z-ADD     CMACDT        CTACDT
     C*
     C                   Z-ADD     CMFYR         CTXFYR
     C                   Z-ADD     CMEMP#        CTXEMP
     C                   Z-ADD     CMDIV         CTXDIV
     C                   Z-ADD     CMCASE        CTXCAS
     C                   MOVE      CMSS#         CTXSS#
     C                   Z-ADD     CMCLAS        CTXCLS
     C                   Z-ADD     CMACDT        CTXACD
     C*
     C                   Z-ADD     XXTINC        CTTINC
     C                   Z-ADD     0             CTRSV
     C*
     C                   CLEAR                   REQ
     C                   CLEAR                   AMT
     C                   MOVE      *BLANKS       XXREQ
     C     XXAMTB        IFNE      *ZERO
     C     XXAMTB        IFGT      *ZERO
     C                   Z-ADD     XXAMTB        XXAMTS
     C                   ELSE
     C                   Z-SUB     XXAMTB        XXAMTS
     C                   ENDIF
     C                   MOVEA     XXAMTC        AMT
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y                 3 0
     C                   MOVE      '$'           REQ(X)
     C                   ADD       1             X
     C     XXAMTS        IFGT      9.99
     C     '0'           LOOKUP    AMT(Y)                             99
     C                   ELSE
     C                   Z-ADD     7             Y
     C                   END
     C*
     C     Y             DO        9             Y
     C                   MOVE      AMT(Y)        REQ(X)
     C                   ADD       1             X
     C*
     C     Y             IFEQ      7
     C                   MOVE      '.'           REQ(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C     Y             IFEQ      9
     C     XXAMTB        ANDLT     *ZERO
     C                   MOVE      '-'           REQ(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C                   ENDDO                                                  Y DO 9
     C*
     C                   MOVEA     REQ           XXREQ
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       CTREQ
     C                   MOVEL     MSG1          CTREQ
     C                   MOVE      XXREQ         CTREQ
     C                   MOVE      *BLANKS       CTDLTR
     C                   Z-ADD     XXCNT         CTDET#
     C                   MOVE      *BLANK        CTPRT
     C                   MOVEL     Q1USER        CTUSER
     C                   MOVEL     Q1PGM         CTPGM
     C                   Z-ADD     XXDATE        CTDT
     C                   Z-ADD     XXDATE        CTTRDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CTTIME
     C                   WRITE     PCT01
      *
      *  Write a record to the deleted claims file.
      *
     C                   WRITE     PCM02
      *
      *  Delete the claim master record.
      *
      **** BEG CHG 02/02/11*******************************************
     C                   DELETE    PCM08
     C***                  DELETPCM01
      **** END CHG 02/02/11*******************************************
     C                   ENDIF
      *
     C                   RETURN
      *
      *****************************************************************
