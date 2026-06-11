     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKDEPXFER - CHECK DEPOSIT TRANSFER BALANCES
     H*
     FSDCMP     UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D XXDESC          C                   CONST('A BILL PRINT WAS REQ-
     D                                     UESTED.  SEE ATTACHE-
     D                                     D BILL.')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXCOMM                 1    256
     D  XXPRIN                 1     70
     D  XXTYPE                 4      7
     D  XXNAME                52     62

     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*****************************************************************
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CMPGM         IFEQ      'W7615R'
     C     CMPGM         OREQ      'W7700R'
     C*
     C                   MOVEL(P)  CMCOMM        XXCOMM
     C*
     C     XXNAME        IFEQ      *BLANKS
     C     XXTYPE        ANDNE     'inst'
     C*
     C                   MOVE      *BLANKS       XXCOMM
     C                   MOVEL     XXDESC        XXCOMM
     C*
     C                   MOVE      *BLANKS       CMCOMM
     C                   MOVEL     XXDESC        CMCOMM
     C                   UPDATE    PCM01
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PCM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           26 'CHKDART   '
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           25 'PGM TO CHECK INVALID DA'
     O                                           46 'RTS RECORDS FOR BILLS'
     O          E            HEAD           2
     O                                            5 'KEY'
     O                                           55 'SEQ#'
     O                                           65 'DESC'
     O          E            DET            1
     O                       CMKEY               45
     O                       CMSEQ#        4     55
     O                       XXPRIN             130
