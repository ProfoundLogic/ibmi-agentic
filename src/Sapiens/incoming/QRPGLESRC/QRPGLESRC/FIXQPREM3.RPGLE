     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXQPREM3 - UPDATE WAQTP WITH QTPREM VALUES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**   UPDATE THE QTPREM FIELD WITH VALUES FROM LATEST WDELP INFO *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    08/22/03  3:30PM    PROJECT: SUPPORT/#38590       *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     FWAQTP     UF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    QTCO#
     C                   KFLD                    QTFND
     C                   KFLD                    QTEMP#
     C                   KFLD                    QTDIV
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* ENTRY POINT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *OFF          *IN66
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INOF
     C                   TIME                    DSDTTM
     C*
     C* READ ALL EMPLOYER DETAIL RECORDS.
     C*
     C                   READ      PQT01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ELENMP        ADD       ELPRTF        XXPREM           11 2
     C                   ADD       ELETER        XXPREM
     C*
     C     XXPREM        IFNE      QTPREM
     C*
     C   OF              EXCEPT    HEAD
     C   OF              MOVE      *OFF          *INOF
     C                   EXCEPT    DET
     C*
     C                   Z-ADD     XXPREM        QTPREM
     C                   MOVEL     Q1PGM         QTPGM
     C*
     C   66              UPDATE    PQT01
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* READ NEXT EMPLOYER DETAIL.
     C*
     C                   READ      PQT01                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O                  66                       28 '** UPDATE MODE ** '
     O                                           55 'UPDATE QTPREM IN WAQTP  '
     O                                           74 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           2
     O                                            3 'CO#'
     O                                            8 'FND'
     O                                           13 'FYR'
     O                                           24 'EMPLOYER#'
     O                                           30 'DIV'
     O                                           44 'QTPREM'
     O                                           58 'XXPREM'
     O          E            DET            1
     O                       ELCO#         3      3
     O                       ELFND         3      8
     O                       ELFYR         3     13
     O                       ELEMP#        3     24
     O                       ELDIV         3     30
     O                       QTPREM        J     44
     O                       XXPREM        J     58
