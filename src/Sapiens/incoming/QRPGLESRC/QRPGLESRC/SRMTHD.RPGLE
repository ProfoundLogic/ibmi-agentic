     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRMTHD - SPELL OUT MONTH FOR REPORTS
     H*
     H*  THIS SUB-PROGRAM WILL SPELL THE NAME OF A GIVEN MONTH OUT AND
     H*  RETURN THEM TO THE CALLING PROGRAM.
     H*

     D TAB1            S             02  0 DIM(12) CTDATA PERRCD(1)
     D TAB2            S             12    DIM(12) ALT(TAB1)
     D*** BEGIN ADD *** 10/19/05 **************************************
     D TAB3            S             02  0 DIM(12) CTDATA PERRCD(1)
     D TAB4            S             12    DIM(12) ALT(TAB3)
     D*** END   ADD *** 10/19/05 **************************************
     D*** BEGIN ADD *** 10/19/05 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END   ADD *** 10/19/05 **************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRMO              2 0          2 POS MONTH
     C                   PARM                    SRMNTH            9            SPELLED OUT MTH
     C                   PARM                    SRERCD            1            ERROR CODE
     C*** BEGIN ADD *** 10/19/05 **************************************
     C                   PARM                    SRCALL            1            CALL CODE
     C*
     C     Q1PRMS        IFGT      3
     C     SRCALL        ANDEQ     'L'
     C*
     C     SRMO          LOOKUP    TAB3          TAB4                     60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVEL     TAB4          SRMNTH
     C                   MOVE      '0'           SRERCD
     C                   ELSE
     C                   MOVE      *BLANK        SRMNTH
     C                   MOVE      '1'           SRERCD
     C                   END
     C*
     C                   ELSE
     C*** END   ADD *** 10/19/05 **************************************
     C*
     C     SRMO          LOOKUP    TAB1          TAB2                     60
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVEL     TAB2          SRMNTH
     C                   MOVE      '0'           SRERCD
     C                   ELSE
     C                   MOVE      *BLANK        SRMNTH
     C                   MOVE      '1'           SRERCD
     C                   END
     C*
     C*** BEGIN ADD *** 10/19/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 10/19/05 **************************************
     C*
     C                   MOVE      '1'           *INLR
**
01JANUARY
02FEBRUARY
03MARCH
04APRIL
05MAY
06JUNE
07JULY
08AUGUST
09SEPTEMBER
10OCTOBER
11NOVEMBER
12DECEMBER
**
01January
02February
03March
04April
05May
06June
07July
08August
09September
10October
11November
12December
