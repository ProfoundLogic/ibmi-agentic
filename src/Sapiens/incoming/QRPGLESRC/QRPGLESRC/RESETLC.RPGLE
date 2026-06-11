     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE RESETLC - RESET SEQUENCE IN UMLCP
     H DEBUG
     H*****************************************************************
     H*  PROGRAM DESCRIPTION: RESET LCSEQ TO 'AAA'                    *
     H*                                                               *
     H*  CALLED FROM: CLRFSMS                                         *
     H*                                                               *
     H*  PROGRAMMER: C:53                                             *
     H*  CREATED: 10/22/96                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     FUMLCP     UF   E           K DISK

     IPLC01
     I              LCSRC10                     LCSRCX
     I              LCSRC11                     LCSRCY
     I              LCSRC12                     LCSRCZ

     C*
     C                   READ      PLC01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      'AAA'         LCSEQ
     C                   UPDATE    PLC01
     C                   ENDIF
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
