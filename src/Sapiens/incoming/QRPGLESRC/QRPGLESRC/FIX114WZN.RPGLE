     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIX114WZN - Update WCMS Claims Lost Time Number of Days
     H*****************************************************************
     H*                                                               *
     H*  This pgm will read each FMCMP detail rec. and if the group   *
     H*  is not an outside account, it will calculate the lost time   *
     H*  days for the same and update WMCMP claims file. A report     *
     H*  is also generated for the same .                             *
     H*                                                               *
     H*  Created: C:251                                               *
     H*****************************************************************
     F* FS CLAIM MASTER FILE
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*
     C                   EXCEPT    HEADER
     C*  MAIN LOOP
     C*    Read FMCMP detail
     C*
     C                   READ      PCM01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      CMCO#         ##CO#             3
     C                   MOVE      CMSUB#        ##SUB#            5
     C                   MOVE      CMFND         ##FND             3
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C*** VALID RECORD ' ' = YES
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C     ##OUTS        ANDNE     'Y'                                          NOT AN OUT ACCT
     C*
     C                   CALL      'FA771R'
     C                   PARM      *BLANKS       ##MODE            1
     C*******              PARM DBRRN     ##RRN   90
     C                   PARM      CMCO#         #2CO#             3 0
     C                   PARM      CMSUB#        #2SUB#            5 0
     C                   PARM      CMFND         #2FND             3 0
     C                   PARM      CMFYR         #2FYR             3 0
     C                   PARM      CMCASE        #2CASE            7 0
     C                   PARM                    ##LTDY            5 0
     C                   PARM                    ##WGRP            3
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C*   Print detail line
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEADER
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   ENDIF                                                   NOT AN OUT ACCOUNT
     C*
     C                   READ      PCM01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
     C*================================================================
     OQSYSPRT   E            HEADER         2  4
     O                                           24 'WCMS claims Lost Time n'
     O                                           44 'umber of days Report'
     O          E            HEADER         2
     O                                            8 'CO#'
     O                                           15 'FUND'
     O                                           23 'YEAR'
     O                                           32 'CASE'
     O                                           58 'LOST TIME DAYS'
     O                                           75 'PAGE:'
     O                       PAGE          Z     80
     O          E            DETAIL         1
     O                       CMCO#         Z      8
     O                       CMFND         Z     15
     O                       CMFYR         Z     23
     O                       CMCASE        Z     32
     O                       ##LTDY        Z     55
