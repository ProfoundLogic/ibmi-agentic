     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE
     H*****************************************************************
     F*
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     C*
     C*  Start of main loop.
     C*
     C                   READ      PEM01                                  60
     C*
     C                   MOVE      *ON           *INOF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL     EMRSK#        XXRSK#            9
     C*
     C                   TESTN                   XXRSK#               70
     C*
     C*
     C     *IN70         IFEQ      *OFF
     C     XXRSK#        ANDNE     *BLANKS
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  XXAMT IFNE
     C*
     C*
     C                   READ      PEM01                                  60
     C                   ENDDO
     C*
     C*  End of program
     C*
     C                   MOVE      *ON           *INLR
     C*
     C********************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                                           65 'CHECK FOR NON-NUMERIC R'
     O                                           76 'ISK FIELDS.'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            5 'CO#'
     O                                           10 'FND'
     O                                           22 'EMPLOYER #'
     O                                           32 'DIVISION'
     O                                           50 'RISK #'
     O          E            DET            1
     O                       EMCO#         4      5
     O                       EMFND         4     10
     O                       EMEMP#        4     22
     O                       EMDIV         4     30
     O                       EMRSK#              50
