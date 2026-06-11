     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CHKDUPSEQ# - CHECK PAYMENT DETAIL FOR DUP SEQ#
     H*  CREATED BY    C:65
     H*****************************************************************
     FWDCXL     IF   E           K DISK
     FWDXHL     UF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN

     D QCMD            S             50    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     XHKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPDT            1
     C*
     C*  Override the Print file to 198 CPI.
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      QCMD(1)       ##CMD            50
     C                   PARM      50            ##CMDL           15 5
     C*
     C                   OPEN      QSYSPRT
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C     *LIKE         DEFINE    XHCO#         XXCO#
     C     *LIKE         DEFINE    XHFND         XXFND
     C     *LIKE         DEFINE    XHFYR         XXFYR
     C     *LIKE         DEFINE    XHEMP#        XXEMP#
     C     *LIKE         DEFINE    XHDIV         XXDIV
     C     *LIKE         DEFINE    XHREQ#        XXREQ#
     C*
     C*  Start of main loop.
     C*
     C                   READ      PCX01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CXPGM         IFEQ      'CHKDUPCD'
      *
     C     XHKY          CHAIN     PXH01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     XHPGM         IFEQ      'FIXCANC'
     C     XHREQ#        ANDNE     CXREQ#
      *
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
      *
     C     QQUPDT        IFEQ      'Y'
     C                   Z-ADD     CXREQ#        XHREQ#
     C                   MOVEL(P)  Q1PGM         XHPGM
     C                   UPDATE    PXH01
     C                   ENDIF
     C*
     C                   ENDIF                                                  XHPGM IFEQ
     C*
     C     XHKY          READE     PXH01                                  60
     C                   ENDDO
     C*
     C                   END                                                    CO#/SEQ# EQUAL
     C*
     C                   READ      PCX01                                  60
     C                   END                                                    *IN60 OFF
     C*
     C                   MOVE      '1'           *INLR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1  3
     O                       DSTIME               8 '  :  :  '
     O                                           55 'CANCELLATION   RECORDS'
     O                                           74 'WITH DUPLICATE REQ'
     O                                           82 'NUMBERS'
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                                           10 'CHKDUPCD  '
     O                                          190 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                          103 'TRANS'
     O                                          110 'RESRV'
     O          E            HEAD           2
     O                                            3 'CO#'
     O                                            8 'FND'
     O                                           13 'FYR'
     O                                           21 'EMPLOYER'
     O                                           28 'DIV'
     O                                           38 'REQ#'
     O                                           44 'RSN'
     O                                           48 'STS'
     O                                           58 'USER'
     O                                           70 'MAINT PGM'
     O                                           80 'MAINT TM'
     O                                           91 'MAINT DT'
     O          E            DET            1
     O                       XHCO#         Z      3
     O                       XHFND         Z      8
     O                       XHFYR         Z     13
     O                       XHEMP#        Z     21
     O                       XHDIV         Z     28
     O                       XHREQ#        Z     38
     O                       XHRSN               44
     O                       XHSTAT              48
     O                       XHUSER              58
     O                       XHPGM               70
     O                       XHTIME              80 '  :  :  '
     O                       XHDT                91 '   /  /  '
      *
** QCMDEXC
OVRPRTF FILE(QSYSPRT) PAGESIZE(66 198) CPI(15)
