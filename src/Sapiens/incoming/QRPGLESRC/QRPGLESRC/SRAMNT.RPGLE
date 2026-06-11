     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*  PROGRAM SUBROUTINE SRAMNT - SPELL OUT THE AMOUNT.
     H*********************************************************************
     H* DESCRIPTION:                                                      *
     H*  THIS SUB-PROGRAM WILL TAKE A DOLLAR AMOUNT UP TO                 *
     H*  $9,999,999.99 AND SPELL IT OUT IN WORDS.  THE PROGRAM            *
     H*  WILL RECEIVE A 9 CHARACTER AMOUNT FIELD.  IT WILL RETURN         *
     H*  TWO LINES OF LENGTH 60 WHICH WILL CONTAIN THE AMT SPELLED OUT.   *
     H*********************************************************************
     H* REVISIONS:                                                        *
     H*                                                                   *
     H*  07/05/95 C:15 PROJECT:                                           *
     H*                CHANGED THE WORDING FROM 'XXX DOLLARS AND XX/100   *
     H*                CENTS' TO 'XXX AND XX/100 DOLLARS'                 *
     H*********************************************************************

     D NUM             S              2  0 DIM(27) CTDATA PERRCD(1)             NUMBERS & NAMES
     D NAM             S             10    DIM(27) ALT(NUM)
     D WRD             S              1    DIM(120)                             WORDS SPELLED OUT
     D COL             S              8    DIM(3) CTDATA PERRCD(1)              $ COLUMNS     OUT
     D                 DS
     D  SRAM                   1      9  2
     D  MIL                    1      1  0
     D  HTH                    2      2  0
     D  TTH                    3      3  0
     D  THO                    4      4  0
     D  HUN                    5      5  0
     D  TEN                    6      6  0
     D  ONE                    7      7  0
     D  CEN                    8      9  0
     D  TTHO                   3      4  0
     D  TONE                   6      7  0

     C     *ENTRY        PLIST
     C                   PARM                    SRAMT             9
     C                   PARM                    SRAMT1           60
     C                   PARM                    SRAMT2           60
     C                   PARM                    SRTERM            1
     C                   MOVE      SRAMT         SRAM
     C                   MOVE      *BLANKS       WRD
     C*
     C*  DETERMINE WHICH COLUMN THE MONEY STARTS IN.
     C*
     C*  MILLION
     C*
     C                   Z-ADD     1             Y                 3 0
     C     MIL           IFNE      0
     C                   Z-ADD     MIL           SRSRCH            2 0
     C                   Z-ADD     1             X                 3 0
     C     SRSRCH        LOOKUP    NUM(X)                                 60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXNAM            10
     C                   MOVEA     NAM(X)        XXNAM
     C                   MOVEA     XXNAM         WRD(1)
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXCOL             8
     C                   MOVEA     COL(1)        XXCOL
     C                   MOVEA     XXCOL         WRD(Y)
     C                   END
     C                   END
     C                   END
     C*
     C*  HUNDRED THOUSAND
     C*
     C     HTH           IFNE      0
     C                   Z-ADD     HTH           SRSRCH            2 0
     C                   Z-ADD     1             X                 3 0
     C     SRSRCH        LOOKUP    NUM(X)                                 60
     C     *IN60         IFEQ      '1'
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXNAM            10
     C                   MOVEA     NAM(X)        XXNAM
     C                   MOVEA     XXNAM         WRD(Y)
     C                   END
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXCOL             8
     C                   MOVEA     COL(3)        XXCOL
     C                   MOVEA     XXCOL         WRD(Y)
     C                   END
     C                   END
     C                   END
     C*
     C*  TEN THOUSAND
     C*
     C     TTH           IFNE      0
     C                   Z-ADD     TTHO          SRSRCH            2 0
     C     SRSRCH        IFGT      20
     C                   MOVE      0             SRSRCH
     C                   END
     C*
     C                   Z-ADD     1             X                 3 0
     C     SRSRCH        LOOKUP    NUM(X)                                 60
     C     *IN60         IFEQ      '1'
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXNAM            10
     C                   MOVEA     NAM(X)        XXNAM
     C                   MOVEA     XXNAM         WRD(Y)
     C                   END
     C                   END
     C                   END
     C*
     C*  THOUSAND
     C*
     C     TTHO          IFLT      11
     C     TTHO          ORGT      19
     C*
     C     THO           IFNE      0
     C                   Z-ADD     THO           SRSRCH            2 0
     C                   Z-ADD     1             X                 3 0
     C     SRSRCH        LOOKUP    NUM(X)                                 60
     C     *IN60         IFEQ      '1'
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXNAM            10
     C                   MOVEA     NAM(X)        XXNAM
     C                   MOVEA     XXNAM         WRD(Y)
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C*
     C     HTH           IFNE      0
     C     TTH           ORNE      0
     C     THO           ORNE      0
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXCOL             8
     C                   MOVEA     COL(2)        XXCOL
     C                   MOVEA     XXCOL         WRD(Y)
     C                   END
     C                   END
     C*
     C*  HUNDRED
     C*
     C     HUN           IFNE      0
     C                   Z-ADD     HUN           SRSRCH            2 0
     C                   Z-ADD     1             X                 3 0
     C     SRSRCH        LOOKUP    NUM(X)                                 60
     C     *IN60         IFEQ      '1'
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXNAM            10
     C                   MOVEA     NAM(X)        XXNAM
     C                   MOVEA     XXNAM         WRD(Y)
     C                   END
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXCOL             8
     C                   MOVEA     COL(3)        XXCOL
     C                   MOVEA     XXCOL         WRD(Y)
     C                   END
     C                   END
     C                   END
     C*
     C*  TEN
     C*
     C     TEN           IFNE      0
     C                   Z-ADD     TONE          SRSRCH            2 0
     C     SRSRCH        IFGT      20
     C                   MOVE      0             SRSRCH
     C                   END
     C*
     C                   Z-ADD     1             X                 3 0
     C     SRSRCH        LOOKUP    NUM(X)                                 60
     C     *IN60         IFEQ      '1'
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXNAM            10
     C                   MOVEA     NAM(X)        XXNAM
     C                   MOVEA     XXNAM         WRD(Y)
     C                   END
     C                   END
     C                   END
     C*
     C*  ONE
     C*
     C     TONE          IFLT      11
     C     TONE          ORGT      19
     C*
     C     ONE           IFNE      0
     C                   Z-ADD     ONE           SRSRCH            2 0
     C                   Z-ADD     1             X                 3 0
     C     SRSRCH        LOOKUP    NUM(X)                                 60
     C     *IN60         IFEQ      '1'
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C                   MOVE      *BLANKS       XXNAM            10
     C                   MOVEA     NAM(X)        XXNAM
     C                   MOVEA     XXNAM         WRD(Y)
     C                   END
     C                   END
     C*
     C*** BEGIN DLT *** 07/05/95 ******************************************
     C***********          ELSE
     C***********
     C***********MIL       IFEQ 0
     C***********HTH       ANDEQ0
     C***********TTH       ANDEQ0
     C***********THO       ANDEQ0
     C***********HUN       ANDEQ0
     C***********TEN       ANDEQ0
     C***********ONE       ANDEQ0
     C***********          MOVEA'NO'      WRD,Y
     C***********          END
     C*** END DLT ***** 07/05/95 ******************************************
     C*
     C                   END
     C*
     C                   END
     C*
     C*  DOLLARS AND CENTS
     C*
     C     *BLANK        LOOKUP    WRD(Y)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       1             Y
     C*** BEGIN DLT *** 07/05/95 ******************************************
     C***********MIL       IFEQ 0
     C***********HTH       ANDEQ0
     C***********TTH       ANDEQ0
     C***********THO       ANDEQ0
     C***********HUN       ANDEQ0
     C***********TEN       ANDEQ0
     C***********ONE       ANDEQ1
     C***********          MOVEA'DOLLAR'  WRD,Y
     C***********          ADD  7         Y
     C***********
     C***********          ELSE
     C***********
     C***********          MOVEA'DOLLARS' WRD,Y
     C***********          ADD  8         Y
     C***********          END
     C***********          MOVEA'AND'     WRD,Y
     C***********          ADD  4         Y
     C***********          END
     C*** END DLT ***** 07/05/95 ******************************************
     C*** BEGIN ADD *** 07/05/95 ******************************************
     C     MIL           IFNE      0
     C     HTH           ORNE      0
     C     TTH           ORNE      0
     C     THO           ORNE      0
     C     HUN           ORNE      0
     C     TEN           ORNE      0
     C     ONE           ORNE      0
     C                   MOVEA     'AND'         WRD(Y)
     C                   ADD       4             Y
     C                   END
     C*** END ADD ***** 07/05/95 ******************************************
     C                   MOVE      '00'          XXCEN             2
     C                   MOVE      CEN           XXCEN
     C                   MOVEA     XXCEN         WRD(Y)
     C                   ADD       2             Y                              ADD CODE FOR
     C                   MOVEA     '/100'        WRD(Y)                         ERROR IN INDEX
     C                   ADD       5             Y
     C*** BEGIN DLT *** 07/05/95 ******************************************
     C***********          MOVEA'CENTS'   WRD,Y
     C*** END DLT ***** 07/05/95 ******************************************
     C*** BEGIN ADD *** 07/05/95 ******************************************
     C                   MOVEA     'DOLLARS'     WRD(Y)
     C                   END
     C*** END ADD ***** 07/05/95 ******************************************
     C*
     C*  SPLIT VERBAGE INTO TWO LINES
     C*
     C                   Z-ADD     60            X
     C     WRD(X)        DOWNE     ' '
     C                   SUB       1             X
     C                   END
     C*
     C                   MOVEA     WRD(X)        SRAMT2
     C                   MOVEA     *BLANKS       WRD(X)
     C                   MOVEA     WRD(1)        SRAMT1
     C*
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
** NUM & NAM
 1ONE
 2TWO
 3THREE
 4FOUR
 5FIVE
 6SIX
 7SEVEN
 8EIGHT
 9NINE
10TEN
11ELEVEN
12TWELVE
13THIRTEEN
14FOURTEEN
15FIFTEEN
16SIXTEEN
17SEVENTEEN
18EIGHTEEN
19NINETEEN
20TWENTY
30THIRTY
40FORTY
50FIFTY
60SIXTY
70SEVENTY
80EIGHTY
90NINETY
** COLUMNS
MILLION
THOUSAND
HUNDRED
