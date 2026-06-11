     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*                                                              **
     H* PROGRAM:     FIXSMCD1X                                       **
     H* DESCRIPTION: 1 TIME FIX PROGRAM TO SET SDCMP/CMSMCD EQUAL TO **
     H*              THE VALUE OF STCTP/CTSMCD.                      **
     H*                                                              **
     H* CALLED FROM: COMMAND LINE                                    **
     H*                                                              **
     H* 02/25/99 C:03  PROJECT: DR069                                **
     H*                                                              **
     H*****************************************************************
     H* REVISIONS:                                                   **
     H*                                                              **
     H*                                                              **
     H*                                                              **
     H*****************************************************************
     H* INDICATOR USAGE:                                             **
     H*****************************************************************
     H* FILES:                                                       **
     H*****************************************************************
     F*
     F* DARTS COMMENT FILE
     FSDCMLK    UF   E           K DISK
     F*
     F* REMARK CODE TABLE FILE
     FSTCTL1    IF   E           K DISK
     F*
     F* OUTPUT REPORT
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*================================================================
     C*
     C*   KEY LIST FOR THE DARTS REMARK FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXRMCD
     C*
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C                   Z-ADD     0             OTCNT             7 0
     C*
     C* FIRST READ FILE AND SET SUMMARY CODE TO BLANKS
     C                   READ      PCM01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       CMSMCD
     C                   UPDATE    PCM01
     C                   ADD       1             OTCNT
     C*
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C*
     C* NOW PROCESS ALL SDCMP RECORDS ACCORDING TO STCTP/CTRMCD
     C*
     C                   Z-ADD     0             OTCNT2            7 0
     C                   EXCEPT    HEAD
     C*
     C                   READ      PCT01                                  51
     C     *IN51         DOWEQ     '0'
     C                   MOVE      *BLANKS       XXRMCD
     C                   MOVEL     CTRMCD        XXRMCD
     C                   Z-ADD     0             OTCNT             7 0
     C*
     C* GET THE DARTS RECORDS FOR THIS REMARK CODE
     C     CMKY          CHAIN     PCM01                              52
     C     *IN52         DOWEQ     '0'
     C*
     C* NOW SET CMSMCD EQUAL TO CTSMCD
     C                   MOVE      *BLANKS       CMSMCD
     C                   MOVEL     CTSMCD        CMSMCD
     C                   UPDATE    PCM01
     C                   ADD       1             OTCNT
     C                   ADD       1             OTCNT2
     C*
     C     CMKY          READE     PCM01                                  52
     C                   ENDDO
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET2
     C*
     C                   READ      PCT01                                  51
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET3
     C*
     C                   MOVE      '1'           *INLR
     O*****************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y     60
     O          E            HEAD           1
     O                       Q1PGM               10
     O                                           54 'PAGE:'
     O                       PAGE          Z     60
     O          E            HEAD           2
     O                                           27 'DARTS SUMM CD'
     O                                           46 'RECORDS CORRECTION'
     O          E            DET1           3
     O                       OTCNT               25
     O                                           60 'TTL CMSMCD CLEARED'
     O          E            DET2           1
     O                       CTRMCD              07
     O                       CTSMCD              15
     O                       OTCNT               25
     O                                           60 'CMSMCD UPDATED    '
     O          E            DET3        2  1
     O                       OTCNT2              25
     O                                           60 'TTL CMSMCD UPDATED'
