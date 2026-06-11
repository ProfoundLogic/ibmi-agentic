     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRFUT - CALCULATE A DATE INTO THE
     H*  FUTURE
     H*
     H*  THE SUBROUTINE EXPECTS A FIELD SRDTE, 6, AND A FIELD
     H*  SRDAYS, 5.0 WHICH IS THE NUMBER OF DAYS INTO THE FUTURE
     H*  THE NEW DATE IS TO BE FOUND. THIS PROGRAM EXPECTS A VALID
     H*  DATE, IF THE DATE IS INVALID, THE RESULTS CANNOT BE EXPECTED.
     H*  THE PARM SRTERM IS EXPECTED FOR PURPOSES OF TERMINATING
     H*  THE PROGRAM.
     H*

     D NORY            S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
     D LONY            S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
     D                 DS
     D  SRDATE                 1      6  0
     D  SRMN01                 1      2  0
     D  M1                     1      2  0
     D  SRDY01                 3      4  0
     D  D1                     3      4  0
     D  SRYR01                 5      6  0
     D  Y1                     5      6  0
     D  SRYRC                  5      6

     C     *ENTRY        PLIST
     C                   PARM                    SRDTE             6
     C                   PARM                    SRFUD             5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRDTE         SRDATE
     C*
     C     SRFUD         IFLE      *ZERO
     C                   Z-ADD     *ZERO         SRFUD
     C                   GOTO      END
     C                   END
     C*
     C     SRYR01        DIV       4             SRLEAP
     C                   MVR                     SRLEAP
     C*
     C     SRLEAP        IFEQ      *ZERO
     C     LONY(M1)      ADD       SRDY01        SRDAY1            5 0
     C                   ELSE
     C     NORY(M1)      ADD       SRDY01        SRDAY1
     C                   END
     C*
     C     SRDAY1        ADD       SRFUD         SRDAY1
     C*
     C     SRLEAP        IFEQ      *ZERO
     C     SRDAY1        SUB       366           SRDAY2
     C                   ELSE
     C     SRDAY1        SUB       365           SRDAY2
     C                   END
     C*
     C     SRDAY2        DOWGT     *ZERO
     C                   ADD       1             SRYR01
     C                   Z-ADD     SRDAY2        SRDAY1
     C     SRYR01        DIV       4             SRLEAP            5 0
     C                   MVR                     SRLEAP
     C     SRLEAP        IFEQ      *ZERO
     C     SRDAY1        SUB       366           SRDAY2            5 0
     C                   ELSE
     C     SRDAY1        SUB       365           SRDAY2
     C                   END
     C                   END
     C*
     C                   ADD       1             SRMN01
     C                   Z-ADD     SRDAY1        SRDAY3            3 0
     C*
     C     SRLEAP        IFEQ      *ZERO
     C     SRDAY3        LOOKUP    LONY(M1)                           11  11
     C                   ELSE
     C     SRDAY3        LOOKUP    NORY(M1)                           11  11
     C                   END
     C*
     C     *IN11         IFEQ      '1'
     C                   SUB       1             SRMN01
     C                   ELSE
     C                   Z-ADD     12            SRMN01
     C                   END
     C*
     C     SRLEAP        IFEQ      *ZERO
     C     SRDAY1        SUB       LONY(M1)      SRDY01
     C                   ELSE
     C     SRDAY1        SUB       NORY(M1)      SRDY01
     C                   END
     C*
     C*
     C     END           TAG
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
**
000031059090120151181212243273304334
**
000031060091121152182213244274305335
