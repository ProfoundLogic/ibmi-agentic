     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE DQAMTR FOR RECORDS PRIOR TO DEDUC CHANGE INSTALL
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     FWADQP     UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDT              6
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQDT          XXDT              6 0
     C*
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*  Start of main loop
     C*
     C                   READ      PDQ01                                  60
     C*
     C                   EXCEPT    HEAD
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     DQAMTR        IFEQ      *ZERO
     C     DQDT          ANDLT     XXDATE
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   Z-ADD     DQAMT         DQAMTR
     C                   Z-ADD     DSTIME        DQTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DQDT
     C                   MOVEL(P)  Q1PGM         DQPGM
     C                   MOVEL(P)  Q1USER        DQUSER
     C                   UPDATE    PDQ01
     C                   ENDIF
     C*
     C                   READ      PDQ01                                  60
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           67 'RUN DATE:'
     O                       XXDT          Y     77
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                           10 'FIXDQAMTR'
     O                                           77 'FILL DQAMTR WITH DQAMT'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'FND'
     O                                            8 'FYR'
     O                                           19 'EMPLOYER'
     O                                           25 'DIV'
     O                                           34 'CHECK#'
     O                                           48 'DQAMT'
     O                                           60 'DQDT'
     O          E            DET
     O                       DQFND         4      3
     O                       DQFYR         4      8
     O                       DQEMP#        4     19
     O                       DQDIV         4     25
     O                       DQCHK#        4     34
     O                       DQAMT         J     48
     O                       DQDT          Y     60
