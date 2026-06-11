     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA056R - API to RETRIEVE NEXT DIVISION FOR PEO QUOTE TFR
     H*****************************************************************
     H*  Description:
     H*
     H*    RETRIEVE NEXT DIVISION FOR PEO QUOTE TFR
     H*    Pass DIV back to calling program.
     H*
     H*  Called From:
     H*    Program
     H*
     H*  Calls To:
     H*
     H*    None
     H*
     H*  Created:  04/20/05  C:53  Project:  WC1279
     H*
     H*****************************************************************
     H*  Revisions:
     H*
     H*
     H*****************************************************************
     H*  Indicators:
     H*
     H*    99 - General File Access
     H*****************************************************************
     H*
     FWMEML     IF   E           K DISK
     F*

     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C*
     C     KKSET         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KKREAD        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      QQFND         KKFND
     C                   Z-ADD     1             KKEMP#
     C                   MOVE      *HIVAL        KKDIV
     C*****************************************************************
     C*
     C     KKSET         SETGT     PEM01
     C     KKREAD        READPE    PEM01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVE      EMEMP#        QQEMP#
     C     EMDIV         ADD       1             XXDIV
     C                   MOVE      XXDIV         QQDIV
     C*
     C                   ELSE
     C                   Z-ADD     1             XXEMP#
     C                   MOVE      XXEMP#        QQEMP#
     C                   Z-ADD     0             XXDIV
     C                   MOVE      XXDIV         QQDIV
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C                   RETURN
