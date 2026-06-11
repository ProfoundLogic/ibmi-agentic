     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRNAME - BUILD A COMBINED NAME.
     H*
     H*****************************************************************
     H*
     H*  DESCRIPTION:
     H*  THIS SUB-PROGRAM WILL ACCEPT THE FIRST AND LAST NAME
     H*  FIELDS AND BUILD A COMBINED NAME IN THE LAST NAME FIELD
     H*  LAST FIRST (NO COMMA)
     H*****************************************************************
     H*
     H*  07/28/99  C:56  PROJECT: MC152
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*****************************************************************

     D                 DS
     D  SLNAM                  1     56
     D  SL                     1     56
     D                                     DIM(56)
     D                 DS
     D  SFNAM                  1     15
     D  SF                     1     15
     D                                     DIM(15)

     C     *ENTRY        PLIST
     C                   PARM                    SRLNAM           56
     C                   PARM                    SRFNAM           15
     C                   PARM                    SRTERM            1
     C*
     C                   MOVE      SRFNAM        SFNAM
     C                   MOVE      SRLNAM        SLNAM
     C*
     C                   MOVE      '0'           *IN60
     C                   Z-ADD     56            SX                5 0
     C     *IN60         DOUEQ     '1'
     C     *BLANK        COMP      SL(SX)                             6060
     C     *IN60         IFEQ      '0'
     C                   SUB       1             SX
     C     SX            COMP      0                                    6060
     C                   END
     C                   END
     C                   ADD       1             SX
     C*
     C     *IN60         IFEQ      '1'
     C     SX            ANDNE     1
     C     SX            ANDLE     56
     C     SFNAM         ANDNE     *BLANK
     C*
     C                   MOVE      ' '           SL(SX)
     C                   ADD       2             SX
     C     SX            IFLE      56
     C                   MOVEA     SFNAM         SL(SX)
     C                   END
     C*
     C                   END
     C*
     C                   MOVE      SFNAM         SRFNAM
     C                   MOVE      SLNAM         SRLNAM
     C*
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
