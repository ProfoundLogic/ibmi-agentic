     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMBXP - CREATE BILLING SCHEDULE AUX RECORDS FOR EACH FUND   R
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     THIS PROGRAM READS THE FUND MASTER AND PRODUCES A COPY OF *
     H*     EACH EXISTING AUXILIARY RECORD INTO THE FUND.             *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:24                                            *
     H*   CREATED:    02/16/95    PROJECT: WC367                      *
     H*                                                               *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     FWMBXL     UF A E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C     BXKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C*
     C     *LOVAL        SETLL     PFN01
     C                   Z-ADD     *ZERO         XXFND
     C                   READ      PFN01                                  50
     C                   Z-ADD     FNCO#         XXCO#
     C*
     C     *IN50         DOWEQ     *ZERO
     C     BXKY          CHAIN     PBX01                              60
     C*
     C     *IN60         DOWEQ     *ZERO
     C                   Z-ADD     FNFND         BXFND
     C*
     C     XXFND         IFEQ      *ZERO
     C                   UPDATE    PBX01
     C                   ELSE
     C                   WRITE     PBX01
     C                   ENDIF                                                  XXFND = *ZERO
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEADER
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF = *ON
     C                   EXCEPT    DETAIL
     C*
     C     BXKY          READE     PBX01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ 0
     C*
     C                   Z-ADD     FNFND         XXFND
     C                   READ      PFN01                                  50
     C*
     C     *IN50         IFEQ      *OFF
     C*
     C     FNCO#         IFNE      XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   Z-ADD     FNCO#         XXCO#
     C                   ENDIF                                                  FNCO# <> XXCO#
     C*
     C                   EXCEPT    HEADER
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *IN50 = *OFF
     C*
     C                   ENDDO                                                  *IN50 DOWEQ 0
     C*
     C                   MOVE      *ON           *INLR
     C*
     OQSYSPRT   E            HEADER         2  4
     O                                           10 'FIXWMBXP'
     O                                           38 'BILLING AUXILIARY FILE R'
     O                                           44 'ECORDS'
     O                       PAGE          Z     60
     O                                           57 'PAGE:'
     O          E            HEADER         2
     O                                            5 'CO#'
     O                                           10 'FND'
     O                                           15 'TYP'
     O                                           20 'EFF DT'
     O                                           29 'CODE'
     O          E            DETAIL         1
     O                       BXCO#         Z      5
     O                       BXFND         Z     10
     O                       BXTYPE              14
     O                       BXEFDT              20 ' 0/  '
     O                       BXBSCD              35
