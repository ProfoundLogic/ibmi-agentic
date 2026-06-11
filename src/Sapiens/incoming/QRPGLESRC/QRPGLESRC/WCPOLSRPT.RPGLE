     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE WCPOLSRPT - PRINT OUT SUBMISSION DETAILS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: 35                                                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
      *
     FWCPOLS    IF   E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D P01           E DS                  EXTNAME(WCPOLS01)
     D P02           E DS                  EXTNAME(WCPOLS02)
     D P03           E DS                  EXTNAME(WCPOLS03)
     D P04           E DS                  EXTNAME(WCPOLS04)
     D P05           E DS                  EXTNAME(WCPOLS05)
     D P06           E DS                  EXTNAME(WCPOLS06)
     D P07           E DS                  EXTNAME(WCPOLS07)
     D P08           E DS                  EXTNAME(WCPOLS08)
     D P10           E DS                  EXTNAME(WCPOLS10)
     D P42           E DS                  EXTNAME(WCPOLS42)
     D P87           E DS                  EXTNAME(WCPOLS87)
     D P88           E DS                  EXTNAME(WCPOLS88)
     D P89           E DS                  EXTNAME(WCPOLS89)
     D*
     D R99             DS
     D  R99SPC                 1     45
     D  R99RTP                46     47
     D  R99T#R                48     57  0
     D  R99THR                58     65  0
     D  R99SP2                66    300
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  ##LNK                  1     47
     D  ##CARR                 1      5  0
     D  ##POL#                 6     23
     D  ##CRT#                24     30
     D  ##PFDT                31     36  0
     D  ##TRDT                37     41  0
     D  ##TRCD                42     43  0
     D  ##RCTY                46     47
     D                 DS
     D  ##ADRS                 1    101
     D  ##ADDR                 1     60
     D  ##CITY                61     90
     D  ##ST                  91     92
     D  ##ZIP1                93     97
     D  ##ZIP2                98    101
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQMBR#           10
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   CLEAR                   P01
     C                   CLEAR                   P02
     C                   CLEAR                   P03
     C                   CLEAR                   P04
     C                   CLEAR                   P05
     C                   CLEAR                   P06
     C                   CLEAR                   P07
     C                   CLEAR                   P08
     C                   CLEAR                   P10
     C                   CLEAR                   P42
     C                   CLEAR                   P87
     C                   CLEAR                   P88
     C                   CLEAR                   P89
     C                   CLEAR                   R99
     C                   CLEAR                   DSDTTM
      *
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEADER
      *
     C                   READ      RPOLS                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEADER
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF IFEQ *ON
     C                   MOVEL     RDATA         ##LNK
     C                   SELECT
     C     ##RCTY        WHENEQ    '01'
     C                   MOVEL     RDATA         P01
     C     ##TRCD        IFEQ      1
     C                   MOVEL(P)  'New Poli'    XXTRAN           15
     C                   CAT       'cy':0        XXTRAN
     C                   ELSE
     C                   MOVEL(P)  'Renewal'     XXTRAN
     C                   CAT       'Policy':1    XXTRAN
     C                   ENDIF                                                  ##TRCD IFEQ 1
     C                   Z-ADD     ##PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C                   EXCEPT    POLICY
     C     ##RCTY        WHENEQ    '08'
     C                   MOVEL     RDATA         P08
     C                   Z-ADD     P08EFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C                   Z-ADD     P08DCM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXISDT            8 0
     C     P08TID        IFEQ      1
     C                   EXCEPT    CANCEL
     C                   ELSE
     C                   EXCEPT    REINST
     C                   ENDIF                                                  P08TID IFEQ 1
     C     ##RCTY        WHENEQ    '87'
     C                   MOVEL     RDATA         P87
     C                   Z-ADD     P87CGD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C                   EXCEPT    ENDR87
     C     ##RCTY        WHENEQ    '88'
     C                   MOVEL     RDATA         P88
     C                   Z-ADD     P88CGD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C                   EXCEPT    ENDR88
     C     ##RCTY        WHENEQ    '89'
     C                   MOVEL     RDATA         P89
     C                   Z-ADD     P89CGD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C                   EXCEPT    ENDR89
     C     ##RCTY        WHENEQ    '99'
     C                   MOVEL     RDATA         R99
     C                   EXCEPT    TOTAL
     C                   ENDSL                                                  ##RCTY WHEQ
      *
     C                   READ      RPOLS                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            HEADER         2 03
     O                                         +005 'NCCI TRANSACTION LIS'
     O                                         +000 'TING FOR FILE MEMBER'
     O                       QQMBR#            +001
     O          E            HEADER         1
     O                                         +005 'Policy Number'
     O                                         +010 'Transaction Type'
     O                                         +005 'Transaction Data'
     O          E            POLICY         1
     O                       ##POL#            +005
     O                       XXTRAN            +005
     O                                         +006 'Effective Date:'
     O                       XXEFDT            +001 '  /  /    '
     O          E            CANCEL         1
     O                       ##POL#            +005
     O                                         +005 'Cancellation'
     O                                         +009 'Issue Date:'
     O                       XXISDT            +001 '  /  /    '
     O                                         +005 'Effective Date:'
     O                       XXEFDT            +001 '  /  /    '
     O          E            REINST         1
     O                       ##POL#            +005
     O                                         +005 'Reinstatement'
     O                                         +008 'Issue Date:'
     O                       XXISDT            +001 '  /  /    '
     O                                         +005 'Effective Date:'
     O                       XXEFDT            +001 '  /  /    '
     O          E            ENDR87         1
     O                       ##POL#            +005
     O                                         +005 'Endorsement - Miscellane'
     O                                         +000 'ous'
     O                                         +005 'Endorsement:'
     O                       P87EN#            +001
     O                                         +005 'Effective Date:'
     O                       XXEFDT            +001 '  /  /    '
     O          E            ENDR88         1
     O                       ##POL#            +005
     O                                         +005 'Endorsement - Name Chang'
     O                                         +000 'e'
     O                                         +008 'Endorsement:'
     O                       P88EN#            +001
     O                                         +005 'Effective Date:'
     O                       XXEFDT            +001 '  /  /    '
     O          E            ENDR89         1
     O                       ##POL#            +005
     O                                         +005 'Endorsement - Address Ch'
     O                                         +000 'ange'
     O                                         +005 'Endorsement:'
     O                       P89EN#            +001
     O                                         +005 'Effective Date:'
     O                       XXEFDT            +001 '  /  /    '
     O          E            TOTAL       3  1
     O                       ##POL#            +005
     O                                         +005 'Total records:'
     O                       R99T#R        Z   +001
     O                                         +005 'Total headers:'
     O                       R99THR        Z   +001
