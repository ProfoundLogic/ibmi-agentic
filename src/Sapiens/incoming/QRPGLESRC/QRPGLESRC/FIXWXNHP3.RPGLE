     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWXNHP3 - MOVE NHMIN TO NHMNPR
     H*****************************************************************
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:35                                            *
     H*   CREATED:    11/13/03    PROJECT: SUPP WA081 #37326          *
     H*                                                               *
     H*****************************************************************
     FWXNHP     UF   E           K DISK
      *

     C                   READ      PNH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     NHMNPR        IFEQ      0
      *
     C                   Z-ADD     NHMIN         NHMNPR
     C                   UPDATE    PNH01
      *
     C                   ENDIF                                                  NHMNPR IFEQ 0
      *
     C                   READ      PNH01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   MOVE      *ON           *INLR
      *
