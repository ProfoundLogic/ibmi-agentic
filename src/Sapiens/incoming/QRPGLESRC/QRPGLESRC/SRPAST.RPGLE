     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRPAST - CALCULATE A PAST DATE
     H*
     H*   COMPUTE A NEW CALENDAR DATE A GIVEN NUMBER OF DAYS IN THE
     H*   PAST OF A GIVEN CALENDAR DATE.
     H*
     H*   THE FIELDS INPUT TO THIS ROUTINE ARE -
     H*     'SRDATE' - SIX-DIGIT NUMERIC. THE DATE TO BE USED AS THE BASE
     H*                FOR COMPUTING THE NEW DATE.
     H*     'SRPST'  - FIVE-DIGIT NUMERIC. THIS IS THE NUMBER OF DAYS TO
     H*                DECREASE THE INPUT DATE.
     H*
     H*   THE RESULTS OF THE CALCULATION ARE PLACED IN THE INPUT DATE
     H*   FIELD 'SRDATE'. THEREFORE, IF THE INITIAL CONTENTS OF 'SRDATE'
     H*   MUST BE KEPT, THEY MUST BE STORED IN ANOTHER FIELD BEFORE
     H*   CALLING THIS ROUTINE.
     H*
     H*   INDICATORS 80,81 AND 82 ARE USED AS WORK INDICATORS BY THIS
     H*   ROUTINE, THE FIELD 'SRRTCD' WILL BE EQUAL TO '1' IF THE VALUE
     H*   IN 'SRPAST' IS NOT POSITIVE.  THE VALUES OF THE WORK INDICATORS
     H*   ARE MAINTAINED.
     H*
     H*

     D NY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND      NORMAL YEAR
     D LY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND      LEAP YEAR
     D                 DS
     D  SRDATE                 1      6  0
     D  SRMN01                 1      2  0
     D  SRM1                   1      2  0
     D  SRDY01                 3      4  0
     D  SRD1                   3      4  0
     D  SRYR01                 5      6  0
     D  SRY1                   5      6  0
     D  SRYRC                  5      6
     D                 DS
     D  SRMD                   1      4  0
     D  XXM1                   1      2  0
     D  XXD1                   3      4  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRDTE             6
     C                   PARM                    SRPST             5 0
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
     C                   MOVE      SRM1          XXM1
     C                   MOVE      SRD1          XXD1
     C*
     C*
     C                   MOVE      ' '           XX80              1
     C                   MOVE      ' '           XX81              1
     C                   MOVE      ' '           XX82              1
     C   80              MOVE      '1'           XX80
     C   81              MOVE      '1'           XX81
     C   82              MOVE      '1'           XX82
     C                   MOVE      ' '           SRRTCD            1
     C*
     C     0             ADD       SRPST         SRPST             5 0  8282
     C   82              GOTO      SRPST3
     C                   SETON                                        82
     C     SRY1          DIV       4             SRX               2 0
     C                   MVR                     SRX                      80    LEAP YEAR
     C   80LY(SRM1)      ADD       SRD1          SRDYS1            5 0
     C  N80NY(SRM1)      ADD       SRD1          SRDYS1
     C     SRDYS1        SUB       SRPST         SRDYS1                 8181
     C  N81              GOTO      SRPST2
     C     SRPST1        TAG
     C     SRY1          SUB       1             SRY1                   81
     C   81              MLLZO     1             SRY1
     C     SRY1          DIV       4             SRX
     C                   MVR                     SRX                      80    LEAP YEAR
     C   80366           ADD       SRDYS1        SRDYS1                 8181
     C  N80365           ADD       SRDYS1        SRDYS1                 8181
     C   81              GOTO      SRPST1
     C     SRPST2        TAG
     C                   Z-ADD     SRDYS1        SRDYS3            3 0
     C                   Z-ADD     1             SRM1
     C   80SRDYS3        LOOKUP    LY(SRM1)                           81  81
     C  N80SRDYS3        LOOKUP    NY(SRM1)                           81  81
     C   81SRM1          SUB       1             SRM1
     C  N81              Z-ADD     12            SRM1
     C   80SRDYS1        SUB       LY(SRM1)      SRMD
     C  N80SRDYS1        SUB       NY(SRM1)      SRMD
     C                   MOVEL     SRM1          SRMD
     C                   MOVEL     SRMD          SRDATE            6 0
     C***DEL 8/10/93??***  MOVE SRY1      SRDATE
     C*
     C                   MOVE      ' '           SRRTCD
     C   82              MOVE      '1'           SRRTCD
     C*
     C                   SETOFF                                       808182
     C     XX80          IFEQ      '1'
     C                   SETON                                        80
     C                   END
     C     XX81          IFEQ      '1'
     C                   SETON                                        81
     C                   END
     C     XX82          IFEQ      '1'
     C                   SETON                                        82
     C                   END
     C*
     C     SRPST3        TAG
     C     SRRTCD        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
     C*
**  NORMAL  YEAR  ----  ARRAY NAME NY
000031059090120151181212243273304334
**  LEAP  YEAR  ----  ARRAY NAME  LY
000031060091121152182213244274305335
