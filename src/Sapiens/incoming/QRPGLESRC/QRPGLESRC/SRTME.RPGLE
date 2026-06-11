     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRTME - CHECK FOR A VALID TIME.
     H*
     H*  THE SUBROUTINE EXPECTS A FIELD SRTIME, 6, IF THE TIME IN
     H*  THE FIELD IS INVALID THEN A FIELD IS PASSED BACK TO THE
     H*  CALLING PROGRAM, SRERR, WITH A VALUE OF "1", AND THE TIME
     H*  IS GIVEN A VALUE NEAREST THE INVALID VALUE. THE PROGRAM ALSO
     H*  EXPECTS A FIELD FOR RETURNING TO THE PROGRAM, OR CANCELLING.
     H*

     D                 DS
     D  SRTIME                 1      6  0
     D  SRHR01                 1      2  0
     D  H1                     1      2  0
     D  SRMN01                 3      4  0
     D  M1                     3      4  0
     D  SRSC01                 5      6  0
     D  S1                     5      6  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRTME             6
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*
     C                   MOVE      SRTME         SRTIME
     C*
     C     SRTIME        IFLT      *ZERO
     C                   MOVE      *ZERO         SRTIME
     C                   END
     C*
     C                   MOVEA     '000'         *IN(11)
     C*
     C     SRHR01        IFLT      *ZERO
     C                   Z-ADD     *ZERO         SRHR01
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C     SRHR01        IFGT      23
     C                   Z-ADD     23            SRHR01
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C     SRMN01        IFLT      *ZERO
     C                   Z-ADD     *ZERO         SRMN01
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C     SRMN01        IFGT      59
     C                   Z-ADD     59            SRMN01
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C     SRMN01        IFLT      *ZERO
     C                   Z-ADD     *ZERO         SRMN01
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C     SRSC01        IFGT      59
     C                   Z-ADD     59            SRMN01
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C     *IN13         IFEQ      '1'
     C                   MOVE      '1'           SRERR
     C                   END
     C*
     C                   MOVE      SRTIME        SRTME
     C*
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
