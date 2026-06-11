     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CHKDUPSEQ# - CHECK PAYMENT DETAIL FOR DUP SEQ#
     H* CREATED BY   C:65
     H*****************************************************************
     FWDCXL     UF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN

     D QCMD            S             50    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C     *LIKE         DEFINE    CXCO#         XXCO#
     C     *LIKE         DEFINE    CXFND         XXFND
     C     *LIKE         DEFINE    CXFYR         XXFYR
     C     *LIKE         DEFINE    CXEMP#        XXEMP#
     C     *LIKE         DEFINE    CXDIV         XXDIV
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C*
     C*  Start of main loop.
     C*
     C                   READ      PCX01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CXCO#         IFEQ      XXCO#
     C     CXFND         ANDEQ     XXFND
     C     CXFYR         ANDEQ     XXFYR
     C     CXEMP#        ANDEQ     XXEMP#
     C     CXDIV         ANDEQ     XXDIV
     C     CXREQ#        ANDEQ     XXREQ#
      *
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
      *
     C     QQUPDT        IFEQ      'Y'
     C                   ADD       1             CXREQ#
     C                   MOVEL(P)  Q1PGM         CXPGM
     C                   UPDATE    PCX01
     C                   ENDIF
     C*
     C                   ELSE
      *
     C                   MOVE      CXCO#         XXCO#
     C                   MOVE      CXFND         XXFND
     C                   MOVE      CXFYR         XXFYR
     C                   MOVE      CXEMP#        XXEMP#
     C                   MOVE      CXDIV         XXDIV
     C                   MOVE      CXREQ#        XXREQ#
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
     O                       CXCO#         Z      3
     O                       CXFND         Z      8
     O                       CXFYR         Z     13
     O                       CXEMP#        Z     21
     O                       CXDIV         Z     28
     O                       CXREQ#        Z     38
     O                       CXRSN               44
     O                       CXSTAT              48
     O                       CXUSER              58
     O                       CXPGM               70
     O                       CXTIME              80 '  :  :  '
     O                       CXDT                91 '   /  /  '
      *
** QCMDEXC
OVRPRTF FILE(QSYSPRT) PAGESIZE(66 198) CPI(15)
