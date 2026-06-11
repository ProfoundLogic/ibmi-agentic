     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRDDIF - FIND THE DIFFERENCE BETWEEN
     H*  TWO DATES
     H*
     H*  THE SUBROUTINE EXPECTS TWO DATE FIELDS OF A VALUE 6. THE
     H*  FIELDS ARE EXPECTED TO BE VALID DATES OF VALUE MMDDYY.
     H*  THE FROM DATE IS EXPECTED IN THE FIRST PARM. THE TO DATE IS
     H*  EXPECTED IN THE SECOND PARM. IF THE DATES ARE INVERTED THEN
     H*  THE FIELD SRERR WILL BE RETURNED WITH A "1". THE FIELD SRTERM
     H*  IS ALSO EXPECTED TO TERMINATE THE PROGRAM WITH "LR" OR AS A
     H*  "RETRN"
     H*

     D NORY            S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
     D LONY            S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
     D                 DS
     D  SRFRDT                 1      6  0
     D  SRMN01                 1      2  0
     D  M1                     1      2  0
     D  SRDY01                 3      4  0
     D  D1                     3      4  0
     D  SRYR01                 5      6  0
     D  Y1                     5      6  0
     D  SRYRC1                 5      6
     D*
     D                 DS
     D  SRTODT                 1      6  0
     D  SRMN02                 1      2  0
     D  M2                     1      2  0
     D  SRDY02                 3      4  0
     D  D2                     3      4  0
     D  SRYR02                 5      6  0
     D  Y2                     5      6  0
     D  SRYRC2                 5      6

     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRFRDC            6
     C                   PARM                    SRTODC            6
     C                   PARM                    SRDAYS            5 0
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
     C                   MOVE      *BLANK        SRERR
     C                   Z-ADD     0             SRDAYS
     C                   MOVE      SRFRDC        SRFRDT
     C                   MOVE      SRTODC        SRTODT
     C*
     C                   Z-ADD     SRFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT01            6 0
     C                   Z-ADD     SRTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT02            6 0
     C*
     C     SRDT01        IFGT      SRDT02
     C                   MOVE      '1'           SRERR
     C                   GOTO      END
     C                   END
     C*
     C     SRYR01        DIV       4             SRLEAP            2 0
     C                   MVR                     SRLEAP
     C*
     C     SRLEAP        IFEQ      *ZERO
     C     LONY(M1)      ADD       SRDY01        SRDAY1            5 0
     C                   ELSE
     C     NORY(M1)      ADD       SRDY01        SRDAY1
     C                   END
     C*
     C                   Z-ADD     SRYR01        SRWK20            2 0
     C     SRWK20        DOWNE     SRYR02
     C*
     C     SRLEAP        IFEQ      *ZERO
     C                   ADD       366           SRDAYS            5 0
     C                   ELSE
     C                   ADD       365           SRDAYS
     C                   END
     C                   ADD       1             SRWK20
     C     SRWK20        DIV       4             SRLEAP            2 0
     C                   MVR                     SRLEAP
     C*
     C                   END
     C*
     C     SRYR02        DIV       4             SRLEAP            2 0
     C                   MVR                     SRLEAP
     C*
     C     SRLEAP        IFEQ      *ZERO
     C     LONY(M2)      ADD       SRDY02        SRDAY2            5 0
     C                   ELSE
     C     NORY(M2)      ADD       SRDY02        SRDAY2
     C                   END
     C*
     C     SRDAY2        SUB       SRDAY1        SRDAY1
     C     SRDAYS        ADD       SRDAY1        SRDAYS
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
