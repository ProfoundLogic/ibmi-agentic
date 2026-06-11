     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRDCK - CHECK FOR A VALID DATE.
     H*
     H*  THE SUBROUTINE EXPECTS A FIELD SRDTE, 6, IF THE DATE IN
     H*  THE FIELD IS INVALID THEN A FIELD IS PASSED BACK TO THE
     H*  CALLING PROGRAM, SRERR, WITH A VALUE OF "1", AND THE DATE
     H*  IS GIVEN A VALUE NEAREST THE INVALID VALUE. THE PROGRAM ALSO
     H*  EXPECTS A FIELD FOR RETURNING TO THE PROGRAM, OR CANCELLING.
     H*

     D NORY            S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
     D LONY            S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
     D QCA             S             55    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D                 DS
     D  SRDATE                 1      6  0
     D  SRMN01                 1      2  0
     D  M1                     1      2  0
     D  SRDY01                 3      4  0
     D  D1                     3      4  0
     D  SRYR01                 5      6  0
     D  Y1                     5      6  0
     D  SRYRC                  5      6
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@NBR                 12     21
     D  @@JOB                 26     35

     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRDTE             6
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
     C*** BEGIN ADD *** 09/24/91 *************************************
     C                   CALL      'SS001C'                             99
     C                   PARM                    QQVLD             5
     C                   PARM                    QQJOB            10
     C                   PARM                    QQNBR            10
     C                   PARM                    QQTYPE            1
     C*
     C* If anything is invalid (No SS001C or non valid parm)
B01  C     *IN99         IFEQ      '1'
     C     QQVLD         ORNE      'B1320'
     C                   MOVE      *BLANKS       @@CMD
     C                   MOVE      *BLANKS       @@NBR
     C                   MOVE      *BLANKS       @@JOB
     C*
     C* If interactive job (TYPE = 1) then SIGNOFF
B02  C     QQTYPE        IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(1)        @@CMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*
     C* If batch job (TYPE = 0) then ENDJOB
X02  C                   ELSE
     C*********            Z-ADD55        LENGTH
     C*********            MOVELQCA,2     @@CMD
     C*********            MOVELQQNBR     @@JOB
     C*********            MOVELQQJOB     @@JOB
     C*********            CALL 'QCMDEXC'
     C*********            PARM           @@CMD
     C*********            PARM           LENGTH 155
E02  C                   END
E01  C                   END
     C*** END ADD ***** 09/24/91 *************************************
     C*
     C*
     C                   MOVE      SRDTE         SRDATE
     C*
B01  C     SRDATE        IFLT      *ZERO
     C                   MOVE      *ZERO         SRDATE
E01  C                   END
     C*
     C                   MOVEA     '000'         *IN(11)
     C*
B01  C     SRYRC         IFLT      '00'
     C                   MOVE      '1'           *IN13
     C                   Z-ADD     0             SRYR01
X01  C                   ELSE
B02  C     SRYRC         IFGT      '99'
     C                   MOVE      '1'           *IN13
     C                   Z-ADD     99            SRYR01
E02  C                   END
E01  C                   END
     C*
B01  C     SRMN01        IFLT      1
     C                   MOVE      '1'           *IN13
     C                   Z-ADD     1             SRMN01
X01  C                   ELSE
B02  C     SRMN01        IFGT      12
     C                   MOVE      '1'           *IN13
     C                   Z-ADD     12            SRMN01
E02  C                   END
E01  C                   END
     C*
     C     SRYR01        DIV       4             SRLEAP            2 0
     C                   MVR                     SRLEAP
     C*
B01  C     SRLEAP        IFEQ      *ZERO
     C                   Z-ADD     LONY(M1)      SRDAY1            5 0
X01  C                   ELSE
     C                   Z-ADD     NORY(M1)      SRDAY1
E01  C                   END
     C*
     C     SRMN01        ADD       1             SW                2 0
     C*
B01  C     SW            IFLE      12
B02  C     SRLEAP        IFEQ      *ZERO
     C                   Z-ADD     LONY(SW)      SRDAY2            5 0
X02  C                   ELSE
     C                   Z-ADD     NORY(SW)      SRDAY2
E02  C                   END
X01  C                   ELSE
     C     SRDAY1        ADD       31            SRDAY2
E01  C                   END
     C*
     C     SRDAY2        SUB       SRDAY1        SRDAY1
     C*
B01  C     SRDY01        IFLT      1
     C                   Z-ADD     1             SRDY01
     C                   MOVE      '1'           *IN13
X01  C                   ELSE
B02  C     SRDY01        IFGT      SRDAY1
     C                   Z-ADD     SRDAY1        SRDY01
     C                   MOVE      '1'           *IN13
E02  C                   END
E01  C                   END
     C*
B01  C     *IN13         IFEQ      '1'
     C                   MOVE      '1'           SRERR
E01  C                   END
     C*
     C*
     C     TERM          TAG
B01  C     SRTERM        IFEQ      '1'
     C                   RETURN
X01  C                   ELSE
     C                   MOVE      '1'           *INLR
E01  C                   END
**
000031059090120151181212243273304334
**
000031060091121152182213244274305335
** QCMDEXC
SIGNOFF
ENDJOB JOB(          /*N/          ) OPTION(*IMMED)
