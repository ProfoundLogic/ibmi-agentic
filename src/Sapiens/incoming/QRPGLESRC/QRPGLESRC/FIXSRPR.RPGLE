     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXSRPR - MOVE SHORT RATE FLAG FROM MASTER TO DETAIL
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWDELL     UF   E           K DISK
      *

     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
      *
     C                   READ      PEM01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      EMCO#         XXCO#
     C                   MOVE      EMFND         XXFND
     C                   MOVE      EMEMP#        XXEMP#
     C                   MOVE      EMDIV         XXDIV
      *
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  60
     C     *IN60         IFEQ      *OFF
      **** BEGIN ADD **** 09/15/00 *********************************************
     C     EMSRPR        IFNE      *BLANK
      **** END   ADD **** 09/15/00 *********************************************
     C                   MOVE      EMSRPR        ELSRPR
      **** BEGIN ADD **** 09/15/00 *********************************************
     C                   ELSE
     C                   MOVE      'P'           ELSRPR
     C                   ENDIF
      **** END   ADD **** 09/15/00 *********************************************
     C                   UPDATE    PEL01
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     ELKY          READPE    PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      'P'           ELSRPR
     C                   UPDATE    PEL01
     C     ELKY          READPE    PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   READ      PEM01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
