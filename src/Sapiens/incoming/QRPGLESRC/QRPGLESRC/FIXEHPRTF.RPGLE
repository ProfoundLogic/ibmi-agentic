     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXEHPRTF - POPULATE EHPRTF WITH ANNUAL EXPENSE CONSTANT.
     H*****************************************************************
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDEHP     UF   E             DISK
      *

     C                   READ      PEH01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     EHPRTF        IFEQ      *ZERO
     C     EHPRTD        ANDNE     *ZERO
      *
     C                   MOVE      EHCO#         QQCO#
     C                   MOVE      EHFND         QQFND
     C                   MOVE      EHFYR         QQFYR
     C                   MOVE      EHEMP#        QQEMP#
     C                   MOVE      EHTRDT        QQTRDT
     C                   Z-ADD     0             QQPREM
     C                   CALL      'SRFEXP'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQTRDT            7 0
     C                   PARM                    QQPREM           11 2
     C*
     C     QQPREM        IFNE      *ZERO
     C                   Z-ADD     QQPREM        EHPRTF
     C                   UPDATE    PEH01
     C                   ENDIF
      *
     C                   ENDIF                                                  EHPRTF IFEQ ON
      *
     C                   READ      PEH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
     C                   MOVE      *ON           *INLR
