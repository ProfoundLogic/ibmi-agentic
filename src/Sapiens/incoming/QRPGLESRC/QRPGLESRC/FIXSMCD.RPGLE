     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*                                                              **
     H* PROGRAM:     FIXSMCD                                         **
     H* DESCRIPTION: PROGRAM TO SET SDCMP/CMSMCD EQUAL TO            **
     H*              THE VALUE OF STCTP/CTSMCD.                      **
     H*                                                              **
     H* CALLED FROM: W6000C OR SC305R01                              **
     H*                                                              **
     H* 02/25/99 C:03  PROJECT: DR069                                **
     H*                                                              **
     H*****************************************************************
     H* REVISIONS:                                                   **
     H*****************************************************************
     H* INDICATOR USAGE:                                             **
     H*****************************************************************
     H* FILES:                                                       **
     H*****************************************************************
     F*
     F* DART REMARK FILE; OPEN QUERY IN FIXSMCDC
     FSDCMLK    UPE  E             DISK
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
     D*
     D KEY             DS             5
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*================================================================
     C*
     C                   TIME                    DSDTTM
     C*
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C                   Z-ADD     0             OTCNT             7 0
     C*
     C                   CLEAR                   KEY
     C                   MOVE      CMRMCD        KEY
     C*
     C     KEY           CHAIN     PCT01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     CTSMCD        CMSMCD
     C*
     C                   ADD       1             OTCNT
     C*
     C                   UPDATE    PCM01
     C     KEY           READE     PCT01                                  60
     C  N60              ENDDO
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET3
     C*
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
     O          E            DET3        2  1
     O                       OTCNT               25
     O                                           60 'TTL CMSMCD UPDATED'
