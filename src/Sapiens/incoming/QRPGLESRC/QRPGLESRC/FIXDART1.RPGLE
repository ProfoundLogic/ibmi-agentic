     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXDART1 - Delete DART records for deleted LIVE Employers
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/06/03 C:128 SUPPORT: HW095 SEQ# 35259                      *
     H*              - Test division field for numeric data.          *
     H*                                                               *
     H* 07/19/02 C:128 SUPPORT: WA109 SEQ# 33442                      *
     H*              - Insure all additional remarks are deleted.     *
     H*              - Generate spool file output                     *
     H*              - Only process 20 byte key fields                *
     H*                                                               *
     H*****************************************************************
      *
      * Employer Master
     FWMEML     IF   E           K DISK
      *
      * DARTS Remarks
     FSDCMLC    UF   E           K DISK
      *
      * DARTS Additional Remarks
     FSMARL1    UF   E           K DISK
      *
      * DARTS Custom Group ID - Format "A"
     FSMHDL     UF   E           K DISK
      *
      * DARTS Custom Group ID - Format "B"
     FSMH1L     UF   E           K DISK
      *
      * System Control
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *
      *  Output File
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)
      *
      *

     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20
     D  TEST                  19     20
     D  QCQTE                 40     40
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  DARTS Additional Remarks
      *

     C     A2KY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
      *
      *   SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  DARTS REMARK DETAIL FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    KKPNAM
      *
     C     CMKY1         KLIST
     C                   KFLD                    QCKEY
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
      *
      *
      *===============================================================
      * PLIST
      *===============================================================
      *
      *
      *
      *
      *===============================================================
      * *LIKE
      *===============================================================
     C     *LIKE         DEFINE    CMPNAM        KKPNAM
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
      *
      *
      *
      *===============================================================
      * BEGIN
      *===============================================================
      *
      *
      *  Only if DARTS installed
      *
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     SYSCIF        ANDEQ     'Y'
      *
      *  For each LIVE Employer DARTS remark
      *
     C                   MOVEL(P)  'W0050R'      KKPNAM
     C     CMKY          CHAIN     PCM01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   CLEAR                   QCKEY
     C                   MOVEL(P)  CMKEY         QCKEY
      *
     C     QCQTE         IFEQ      *BLANK
     C     QCKEY         ANDNE     *BLANKS
      *** Begin Add 01/06/03 ******************************************
     C***********TEST      ANDGT*BLANKS
     C*
     C                   MOVEL     QCDIV         TSTDIV            5
     C                   TESTN                   TSTDIV               99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVE(P)   TSTDIV        QCDIV
     C                   ELSE
     C                   MOVEL     *ZEROS        TSTDIV
     C                   MOVE(P)   TSTDIV        QCDIV
     C                   ENDIF                                                  *IN99=*ON
      *** End   Add 01/06/03 ******************************************
     C                   MOVE      QCCO#         KKCO#
     C                   MOVE      QCFND         KKFND
     C                   MOVE      QCEMP#        KKEMP#
     C                   MOVE      QCDIV         KKDIV
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   EXSR      SRDLT
     C                   EXCEPT    DETAIL
     C                   ENDIF                                                  EMKY
      *
     C                   ENDIF                                                  QCQTE = *BLANK
      *
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO                                                  *IN60=*OFF/CMKY
      *
     C                   ENDIF                                                  *IN60=*OFF/SYSCIF=Y
      *
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
      *===============================================================
      * SRDLT - Delete DARTS records for employer
      *===============================================================
     C     SRDLT         BEGSR
      *
      *  Delete Additional DARTS Remarks
      *
     C     A2KY          CHAIN     PAR01                              60
     C     *IN60         DOWEQ     *OFF
     C     A2KY          DELETE    PAR01                              60
     C     A2KY          READE     PAR01                                  60
     C                   ENDDO
      *
      *  Delete DARTS Custom Group ID records
      *
     C     CMKY1         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     *OFF
     C     CMKY1         DELETE    PHD01                              60
     C     CMKY1         READE     PHD01                                  60
     C                   ENDDO
      *
     C     CMKY1         CHAIN     PH101                              60
     C     *IN60         DOWEQ     *OFF
     C     CMKY1         DELETE    PH101                              60
     C     CMKY1         READE     PH101                                  60
     C                   ENDDO
      *
      *  Delete DARTS Remark
      *
     C                   DELETE    PCM01
      *
     C                   ENDSR                                                  SRDLT
      *
      *
     OQSYSPRT   E            DETAIL         1
     O                       CMKEY             +005
     O                       CMSEQ#            +005
      *
      ***** END ADD ******* 07/19/02 **********************************
      *
