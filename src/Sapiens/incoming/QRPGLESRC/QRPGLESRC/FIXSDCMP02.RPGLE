     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* FIXSDCMP01
     H*************************************************************
     H*
     H* IF CMDCID starts with 'CC' or 'RR' and ends with ".PDF"
     H*   then set CMIMGF = "O".
     H*
     H*************************************************************
      *
      *
     FSDCMP     UF   E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *LIKE         DEFINE    CMDCID        XXDCID
     C     *LIKE         DEFINE    CMDCID        X1DCID
      *
      *=====================================================================
     C                   Z-ADD     0             XXCNT             5 0
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   READ      PCM01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
      *  Get first characters from document id
      *
     C                   MOVEL     CMDCID        XX1ST             2
      *
      *  If CX or Reinstatement is .pdf, then change CMIMGF = 'O'
      *
     C     XX1ST         IFEQ      'CC'
     C     XX1ST         OREQ      'RR'
     C                   MOVEL(P)  CMDCID        XXDCID
      *
      *  Right Justify
     C     ' '           CHECKR    XXDCID:12     X                 3 0
     C     12            SUB       X             X1                3 0
     C                   MOVEL(P)  *BLANKS       X1DCID
     C                   CAT(P)    XXDCID:X1     X1DCID
      *
      *  Get last characters from document id
      *
     C                   MOVE(P)   X1DCID        XXLAST            4
      *
     C     XXLAST        IFEQ      '.PDF'
     C     CMIMGF        ANDNE     'O'
     C                   MOVE      'O'           CMIMGF
     C                   MOVEL(P)  CMKEY         XXKEY            20
     C   OF              EXCEPT    HEAD
     C   OF              SETOFF                                       OF
     C                   EXCEPT    DET
     C                   ADD       1             XXCNT
      *
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
      *
     C                   UPDATE    PCM01
     C                   ENDIF                                                  .PDF
      *
     C                   ENDIF                                                  CC  RR
      *
     C                   READ      PCM01                                  60
     C                   ENDDO
      *
     C                   EXCEPT    TOT
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *
      **************************************************************************
      *
     OQSYSPRT   E            HEAD           1 04
     O                       DSTIME               8 '  :  :  '
     O                       DSDATE        Y    132
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                         + 20 'UPDATE CMIMGF = O IF'
     O                                         +  1 'RESOLVED DOCUMENT IS'
     O                                         +  1 '.PDF '
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
      *
      *
     O          E            HEAD           2
     O                                            9 'DARTS KEY'
     O                                         + 17 'DOC ID WAS'
     O                                           50 'INIT PGM'
     O                                           65 'INIT DATE'
     O                                           75 '     SEQ#'
      *
     O          E            DET            1
     O                       XXKEY               21
     O                       XXDCID            +  5
     O                       CMIPGM            +  5
     O                       CMIDT         Y   +  2
     O                       CMSEQ#            +  5
      *
     O          E            TOT         2  1
     O                                           25 '# of records:'
     O                       XXCNT         4   +  1
      *
