     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXDART2
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H* DELETE Additional Dart Remark Records (SMARP) with no parent  *
     H* Dart remark (SDCMP) record.                                   *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/19/02 C:128 SUPPORT: WA109 SEQ# 33442                      *
     H*              - Insure all additional remarks are deleted.     *
     H*                                                               *
     H*****************************************************************
      *
      * DARTS Additional Remarks   KEY/SEQ#/ASQ#
     FSMARL1    UF   E           K DISK
      *
      * DARTS Remarks              KEY/SEQ#
     FSDCMLE    IF   E           K DISK
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
      *  SYSTEM CONTROL FILE
      *

     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  DARTS REMARK DETAIL FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    ARKEY
     C                   KFLD                    ARSEQ#
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
      *  For each Additional DARTS remark.
      *
     C                   READ      PAR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *  Delete Additional DARTS remark if no parent SDCMP record
      *
     C     CMKY          CHAIN     PCM01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   EXCEPT    DETAIL
     C                   DELETE    PAR01
     C                   ENDIF
      *
     C                   READ      PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF/CMKY
      *
     C                   ENDIF                                                  *IN60=*OFF/SYSCIF=Y
      *
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
     OQSYSPRT   E            DETAIL         1
     O                       ARKEY             +005
     O                       ARSEQ#            +002
     O                       ARASQ#            +002
      *
      *
