     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRDAY - DAY OF WEEK SUBPROGRAM
     H*
     H*  THIS SUB-PROGRAM WILL CALCULATE THE DAY OF THE WEEK BASED ON
     H*  THE DATE (MM/DD/YY) PASSED IN.
     H*
     H*  NOTE: It is assumed that the date Passed in is valid. If it is not,
     H*        this routine may provide unpredictable results.
     H*

     C* *  PARAMETERS RETURNED BY THIS SUBPROGRAM ARE:
     C*
     C*         SRDATE (6.0) - Calander Date
     C*         SRSDAY (4 A) - Day of the Week {3 Pos + '.'}
     C*         SRLDAY (9 A) - Day of the Week {9 Pos}
     C*         SRERR  (1 A) - Error Flag {'0'is OK '1' is Input Error}
     C*
     C*
     C*         January 1,1970 to December 31,2069  is the valid range of
     C*         dates.
     C*
     C*         Any Date less than 01/01/70 will be considered 21st Century
     C*         Any Date greater than 12/31/69 will be considered 20th
     C*         Century.
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRDATE            6            M/D/Y
     C                   PARM                    SRSDAY            4            Abbreviation
     C                   PARM                    SRLDAY            9            Full Day Name
     C                   PARM                    SRERR             1            Error Flag
     C*
     C*
     C* * * Move Calander Date into XXCIN.
     C                   MOVE      SRDATE        XXCIN
     C*
     C                   MOVE      *ZEROS        XXERR                          Clear Error FLG
     C* * * Seperate Calander Date.
     C                   MOVEL     XXCIN         XXWRK4            4 0
     C                   Z-ADD     XXCIN         XXCYR
     C                   MOVEL     XXWRK4        XXCMON
     C                   MOVE      XXWRK4        XXCDAY
     C                   Z-ADD     XXCYR         XXWRK4
     C* * * If the Year is prior to 70, this is for the 21st Century.
     C     XXCYR         IFLT      70
     C                   ADD       100           XXWRK4
     C                   END
     C*
     C     XXCMON        IFLT      3
     C                   SUB       1             XXWRK4
     C     XXCMON        ADD       13            XXWRK3            3 0
     C                   ELSE
     C     XXCMON        ADD       1             XXWRK3
     C                   END
     C     XXWRK4        MULT      365.25        XXHDTE
     C                   MULT      30.61         XXWRK3
     C                   ADD       XXWRK3        XXHDTE
     C                   ADD       XXCDAY        XXHDTE
     C                   SUB       63            XXHDTE
     C*
     C     XXHDTE        CABLT     25568         XXTAG3                         Error*Before
     C     XXHDTE        CABGT     62092         XXTAG3                         Error*After
     C*
     C     XXHDTE        DIV       7             XXHIN
     C                   MVR                     XXWRK1            1 0
     C                   MOVE      *BLANK        XXDAY
     C                   MOVE      *BLANK        XXDAY2            9
     C* *  Day is Sunday.
     C     XXWRK1        IFEQ      *ZERO
     C                   MOVE      'SUN.'        XXDAY
     C                   MOVEL     'SUNDAY'      XXDAY2
     C                   ELSE
     C* *  Day is Monday.
     C     XXWRK1        IFEQ      1
     C                   MOVE      'MON.'        XXDAY
     C                   MOVEL     'MONDAY'      XXDAY2
     C                   ELSE
     C* *  Day is Tuesday.
     C     XXWRK1        IFEQ      2
     C                   MOVE      'TUE.'        XXDAY
     C                   MOVEL     'TUESDAY'     XXDAY2
     C                   ELSE
     C* *  Day is Wednsday.
     C     XXWRK1        IFEQ      3
     C                   MOVE      'WED.'        XXDAY
     C                   MOVEL     'WEDNESDA'    XXDAY2
     C                   MOVE      'Y'           XXDAY2
     C                   ELSE
     C* *  Day is Thursday.
     C     XXWRK1        IFEQ      4
     C                   MOVE      'THU.'        XXDAY
     C                   MOVEL     'THURSDAY'    XXDAY2
     C                   ELSE
     C* *  Day is Friday.
     C     XXWRK1        IFEQ      5
     C                   MOVE      'FRI.'        XXDAY
     C                   MOVEL     'FRIDAY  '    XXDAY2
     C                   ELSE
     C* *  Day is Saturday.
     C     XXWRK1        IFEQ      6
     C                   MOVE      'SAT.'        XXDAY
     C                   MOVEL     'SATURDAY'    XXDAY2
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   GOTO      XXTAG4
     C     XXTAG3        TAG                                                    ERRORS
     C                   Z-ADD     *ZEROS        XXCDTE            6 0
     C                   Z-ADD     *ZEROS        XXHDTE            5 0
     C                   Z-ADD     *ZEROS        XXCMON            2 0
     C                   Z-ADD     *ZEROS        XXCDAY            2 0
     C                   Z-ADD     *ZEROS        XXCYR             2 0
     C                   MOVE      *BLANKS       XXDAY             4
     C                   MOVE      *BLANKS       XXDAY2            9
     C                   MOVE      *BLANKS       SRSDAY
     C                   MOVE      *BLANKS       SRLDAY
     C                   MOVE      *BLANKS       SRERR
     C                   MOVE      *BLANK        XXERR             1
     C     XXTAG4        TAG
     C                   Z-ADD     *ZEROS        XXHIN             5 0
     C                   Z-ADD     *ZEROS        XXCIN             6 0
     C*
     C                   MOVEL     XXDAY         SRSDAY
     C                   MOVEL     XXDAY2        SRLDAY
     C                   MOVEL     XXERR         SRERR
     C*
     C                   MOVE      '1'           *INLR
