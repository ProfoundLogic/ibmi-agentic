     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRFUTR - CALCULATE A FUTURE DATE
     H*
     H*   COMPUTE A NEW CALENDAR DATE A GIVEN NUMBER OF DAYS IN THE
     H*   FUTURE OF A GIVEN CALENDAR DATE.
     H*
     H*   THE FIELDS INPUT TO THIS ROUTINE ARE -
     H*     'SRDATE' - SIX-DIGIT NUMERIC. THE DATE TO BE USED AS THE BASE
     H*                FOR COMPUTING THE NEW DATE.
     H*     'SRFUTR' - FIVE-DIGIT NUMERIC. THIS IS THE NUMBER OF DAYS TO
     H*                ADVANCE THE INPUT DATE.
     H*
     H*   THE RESULTS OF THE CALCULATION ARE PLACED IN THE INPUT DATE
     H*   FIELD 'SRDATE'. THEREFORE, IF THE INITIAL CONTENTS OF 'SRDATE'
     H*   MUST BE KEPT, THEY MUST BE STORED IN ANOTHER FIELD BEFORE
     H*   CALLING THIS ROUTINE.
     H*
     H*   INDICATORS 80,81 AND 82 ARE USED AS WORK INDICATORS BY THIS
     H*   ROUTINE, THE FIELD 'SRRTCD' WILL BE EQUAL TO '1' IF THE VALUE
     H*   IN 'SRFUTR' IS NOT POSITIVE.  THE VALUES OF THE WORK INDICATORS
     H*   ARE MAINTAINED.
     H*
     H*

     D NY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND      NORMAL YEAR
     D LY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND      LEAP YEAR

     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   MOVE      ' '           XX80              1
     C                   MOVE      ' '           XX81              1
     C                   MOVE      ' '           XX82              1
     C   80              MOVE      '1'           XX80
     C   81              MOVE      '1'           XX81
     C   82              MOVE      '1'           XX82
     C                   MOVE      ' '           SRRTCD            1
     C*
     C     0             ADD       SRFUTR        SRFUTR            5 0  8282
     C   82              GOTO      SRFUT2
     C                   SETOFF                                       82
     C                   MOVEL     SRDATE        SRMD              4 0
     C                   MOVE      SRDATE        SRY1              2 0
     C                   MOVEL     SRMD          SRM1              2 0
     C                   MOVE      SRMD          SRD1              2 0
     C     SRY1          DIV       4             SRX               2 0
     C                   MVR                     SRX                      80     LEAP YEAR
     C   80LY(SRM1)      ADD       SRD1          SRDYS1            5 0
     C  N80NY(SRM1)      ADD       SRD1          SRDYS1
     C     SRDYS1        ADD       SRFUTR        SRDYS1
     C     SRFUT1        TAG
     C   80SRDYS1        SUB       366           SRDYS2            5 081
     C  N80SRDYS1        SUB       365           SRDYS2               81
     C   811             ADD       SRY1          SRY1
     C   81              Z-ADD     SRDYS2        SRDYS1
     C   81SRY1          DIV       4             SRX
     C   81              MVR                     SRX                      80
     C   81              GOTO      SRFUT1
     C                   Z-ADD     1             SRM1
     C                   Z-ADD     SRDYS1        SRDYS3            3 0
     C   80SRDYS3        LOOKUP    LY(SRM1)                           81  81
     C  N80SRDYS3        LOOKUP    NY(SRM1)                           81  81
     C   81SRM1          SUB       1             SRM1
     C  N81              Z-ADD     12            SRM1
     C   80SRDYS1        SUB       LY(SRM1)      SRMD
     C  N80SRDYS1        SUB       NY(SRM1)      SRMD
     C                   MOVEL     SRM1          SRMD
     C                   MOVEL     SRMD          SRDATE            6 0
     C                   MOVE      SRY1          SRDATE
     C*
     C   82              MOVE      '1'           SRRTCD
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
     C     SRFUT2        TAG
     C                   MOVE      '1'           *INLR
     C*
**  NORMAL  YEAR  ----  ARRAY NAME NY
000031059090120151181212243273304334
**  LEAP  YEAR  ----  ARRAY NAME  LY
000031060091121152182213244274305335
