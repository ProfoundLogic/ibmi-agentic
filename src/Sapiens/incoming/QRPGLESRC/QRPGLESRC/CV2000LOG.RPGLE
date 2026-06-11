     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/define CONVERT
      /TITLE CV2000LOG - LOG CONVERSION MESSAGES
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *****************************************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN71)
     F                                     USROPN
      **

     D                 DS                  INZ
     D  XXTMDT                 1     12  0
     D  XXTIME                 1      6  0
     D  XXDATE                 7     12  0
      **
      *****************************************************************
      ** Program Entry                                               **
      *****************************************************************

     C     *ENTRY        PLIST
     C                   PARM                    ZZPGM            10
     C                   PARM                    ZZFUNC            1
     C                   PARM                    ZZMSG           256
      **
      *****************************************************************
      ** Mainline Processing                                         **
      *****************************************************************
      **
      **         Open the printer file
      **
     C                   SELECT
     C     ZZFUNC        WHENEQ    'O'                                           >-----+
     C     XXOPEN        IFEQ      *OFF                                          >---+ :
     C                   OPEN      QSYSPRT                              91           : :
     C     *IN91         IFEQ      *OFF                                          >-+ : :
     C                   MOVE      *ON           XXOPEN                            : : :
     C                   EXSR      $GETOP                                          : : :
     C                   ENDIF                                                   <-+ : :
     C                   ENDIF                                                   <---+ :
      **                                                          :
      **         Print a message to the log file                  :
      **                                                          :
     C     ZZFUNC        WHENEQ    'M'                                           >-----+
     C     XXOPEN        IFEQ      *ON                                           >-+   :
     C                   TIME                    XXTMDT                            :   :
     C                   EXSR      $PRINT                                          :   :
     C                   ENDIF                                                   <-+   :
      **                                                          :
      **         Close the printer log file                       :
      **                                                          :
     C     ZZFUNC        WHENEQ    'C'                                           >-----+
     C     XXOPEN        IFEQ      *ON                                           >-+   :
     C                   MOVE      *OFF          XXOPEN                            :   :
     C                   CLOSE     QSYSPRT                              91         :   :
     C                   ENDIF                                                   <-+   :
     C                   MOVE      *ON           *INLR                                 :
     C                   ENDSL                                                   <-----+
     C                   RETURN
      **
      *****************************************************************
      ** Program Initialization                                      **
      *****************************************************************
     CSR   *INZSR        BEGSR                                                   >-+
     CSR                 MOVE      *OFF          XXOPEN            1               :
     CSR                 MOVE      *ON           *IN71                             :
     CSR                 Z-ADD     *ZERO         XXPAGE            4 0             :
     CSR                 ENDSR                                                   <-+
      **
      *****************************************************************
      ** Get the log file title and conversion options               **
      *****************************************************************
     CSR   $GETOP        BEGSR                                                   >---------+
     CSR                 MOVE      *BLANKS       XXTTLE          100                       :
     CSR                 MOVE      *BLANKS       XXOPTS          132                       :
      **                                                              :
      **         Find start of conversion options                     :
      **                                                              :
     CSR   '~'           SCAN      ZZMSG:1       IX                5 0                     :
     CSR   IX            IFGT      *ZERO                                         >-------+ :
     CSR   IX            ADD       1             IY                5 0                   : :
     CSR   ' '           CHECK     ZZMSG:IY      IY                                      : :
     CSR   IY            IFGT      *ZERO                                         >-----+ : :
     CSR   ' '           CHECKR    ZZMSG         IZ                5 0                 : : :
     CSR   IZ            IFGT      *ZERO                                         >---+ : : :
     CSR   IZ            SUB       IY            LN                5 0               : : : :
     CSR                 ADD       1             LN                                  : : : :
     CSR   LN            IFGT      132                                           >-+ : : : :
     CSR                 Z-ADD     132           LN                                : : : : :
     CSR                 ENDIF                                                   <-+ : : : :
     CSR   LN            SUBST     ZZMSG:IY      XXOPTS                              : : : :
     CSR                 ENDIF                                                   <---+ : : :
     CSR                 ENDIF                                                   <-----+ : :
     CSR   IX            IFEQ      1                                             >-+     : :
     CSR                 MOVE      *BLANKS       ZZMSG                             :     : :
     CSR                 ELSE                                                    >-+     : :
     CSR   IX            SUB       1             LN                                :     : :
     CSR   LN            SUBST(P)  ZZMSG:1       ZZMSG                             :     : :
     CSR                 ENDIF                                                   <-+     : :
     CSR                 ENDIF                                                   <-------+ :
      **                                                              :
      **         Center title                                         :
      **                                                              :
     CSR   ' '           CHECKR    ZZMSG         IX                                        :
     CSR   IX            IFGT      100                                           >-+       :
     CSR   100           SUBST(P)  ZZMSG:1       ZZMSG                             :       :
     CSR   ' '           CHECKR    ZZMSG         IX                                :       :
     CSR                 ENDIF                                                   <-+       :
     CSR   IX            IFGT      *ZERO                                         >-+       :
     CSR   100           SUB       IX            IX                                :       :
     CSR                 DIV       2             IX                                :       :
     CSR   ' '           CAT       ZZMSG:IX      XXTTLE                            :       :
     CSR                 ENDIF                                                   <-+       :
     CSR                 ENDSR                                                   <---------+
      **
      *****************************************************************
      ** Format and print a conversion message                       **
      *****************************************************************
     CSR   $PRINT        BEGSR                                                   >-------+
      **                                                            :
      **         Locate first non blank character                   :
      **                                                            :
     CSR   ' '           CHECK     ZZMSG:1       IX                                      :
     CSR   IX            IFGT      1                                             >-+     :
     CSR   257           SUB       IX            LN                                :     :
     CSR   LN            SUBST(P)  ZZMSG:IX      ZZMSG                             :     :
     CSR                 ENDIF                                                   <-+     :
      **                                                            :
      **         Print as many lines as needed for message          :
      **                                                            :
     CSR   ZZMSG         DOWNE     *BLANKS                                       >-----+ :
     CSR   ' '           CHECKR    ZZMSG         IX                                    : :
     CSR   1             SUBST     ZZMSG:IX      XXBYTE            1                   : :
      **                                                          : :
      **            Wrap line at end of a word if needed          : :
      **                                                          : :
     CSR   XXBYTE        DOWGT     *BLANK                                        >---+ : :
     CSR   IX            ANDGT     112                                               : : :
     CSR                 SUB       1             IX                                  : : :
     CSR   IX            IFEQ      *ZERO                                         >-+ : : :
     CSR                 MOVE      *BLANK        XXBYTE                            : : : :
     CSR                 Z-ADD     112           IX                                : : : :
     CSR                 ELSE                                                    >-+ : : :
     CSR   1             SUBST     ZZMSG:IX      XXBYTE                            : : : :
     CSR                 ENDIF                                                   <-+ : : :
     CSR                 ENDDO                                                   <---+ : :
     CSR   IX            SUBST(P)  ZZMSG:1       XXTEXT          112                   : :
      **                                                          : :
      **            Advance page, print header if needed          : :
      **                                                          : :
     CSR   *IN71         IFEQ      *ON                                           >-+   : :
     CSR                 ADD       1             XXPAGE                            :   : :
     CSR                 EXCEPT    ##HEDR                                          :   : :
     CSR                 MOVE      *OFF          *IN71                             :   : :
     CSR                 ENDIF                                                   <-+   : :
      **                                                          : :
      **            Print detail                                  : :
      **                                                          : :
     CSR                 EXCEPT    ##DETL                                              : :
     CSR                 ADD       1             IX                                    : :
     CSR   IX            IFLE      256                                           >-+   : :
     CSR   ' '           CHECK     ZZMSG:IX      IX                                :   : :
     CSR                 ENDIF                                                   <-+   : :
      **                                                          : :
      **            If msg needs more lines, indent 2 spaces      : :
      **                                                          : :
     CSR   IX            IFEQ      *ZERO                                         >-+   : :
     CSR   IX            ORGT      256                                             :   : :
     CSR                 MOVE      *BLANKS       ZZMSG                             :   : :
     CSR                 ELSE                                                    >-+   : :
     CSR   257           SUB       IX            LN                                :   : :
     CSR   LN            SUBST(P)  ZZMSG:IX      ZZMSG                             :   : :
     CSR   ' '           CAT       ZZMSG:2       ZZMSG                             :   : :
     CSR                 ENDIF                                                   <-+   : :
     CSR                 ENDDO                                                   <-----+ :
     CSR                 ENDSR                                                   <-------+
      **
      *****************************************************************
      ** Report output                                               **
      *****************************************************************
     OQSYSPRT   E            ##HEDR         2 03
     O                       XXDATE               8 '  /  / 0'
     O                       XXTTLE             116
     O                                          128 'PAGE'
     O                       XXPAGE        4    132
     O          E            ##HEDR         2
     O                       XXOPTS             132
     O          E            ##HEDR         1
     O                                            6 'TIME'
     O                                           16 'PROGRAM'
     O                                           27 'MESSAGE'
      **
     O          E            ##DETL         1
     O                       XXTIME               8 '  :  : 0'
     O                       ZZPGM               19
     O                       XXTEXT             132
