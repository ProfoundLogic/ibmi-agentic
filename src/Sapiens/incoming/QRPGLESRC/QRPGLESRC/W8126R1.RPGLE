     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W8126R1 - CLARENDON - BUILD CALENDER EARNED/UNEARNED FILE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWXPGL     IF   E           K DISK

     C*
     C*  GROUP CONTROL FILE
     C*
     C     PGKY          KLIST
     C                   KFLD                    XXCO#             3 0
      *
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQQUE            10
      *
     C                   MOVE      QQCO#         XXCO#
      *
     C     PGKY          CHAIN     XPG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PGFLAG        IFEQ      'I'
      *
     C                   MOVE      PGGRP         QQFND
      *
     C                   CALL      'W4902R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND             3
     C                   PARM                    QQFDT
     C                   PARM                    QQTDT
     C                   PARM                    QQTDT
     C                   PARM                    QQQUE
     C                   PARM      'C'           QQDTR             1
     C                   PARM      'S'           QQSUM             1
      *
     C                   ENDIF                                                  PGFLAG IFEQ
      *
     C     PGKY          READE     XPG01                                  60
      *
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
