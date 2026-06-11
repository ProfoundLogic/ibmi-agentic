     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H****************************************************************
     H** REVISIONS                                                  **
     H**                                                            **
     H**  10/17/11 C:279 Project: SY195                             **
     H*                  - Modify program to pass/return UNC path   **
     H*                    field and IFS flag to called/calling     **
     H*                    programs.                                **
     H**                                                            **
     H****************************************************************
     FSDCMP     UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D XXUSER          C                   CONST('CAMUNDSEN')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

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
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CMPGM         IFEQ      'W7615R'
     C     CMDT          ANDEQ     1021105
     C     CMTIME        ANDGE     132355
     C     CMTIME        ANDLE     133508
     C     CMUSER        ANDEQ     XXUSER
     C     CMRMCD        ANDEQ     'PSOB'
     C*
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C   66              CALL      'FIXDARTC'
     C                   PARM                    CMDCID
     C                   PARM                    CMFOLD
     C***** BEG ADD *** 10/17/11 *********************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
     C***** END ADD *** 10/17/11 *********************************
     C*
     C   66              DELETE    PCM01
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
     O                                           26 'FIXDART   '
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           25 'PGM TO DELETE DUPLICATE'
     O                                           46 ' DARTS RCDS FOR BILLS'
     O          E            HEAD           2
     O                                            5 'KEY'
     O                                           55 'RMCD'
     O                                           65 'TIME'
     O          E            DET            1
     O                       CMKEY               45
     O                       CMRMCD              55
     O                       CMTIME              65
