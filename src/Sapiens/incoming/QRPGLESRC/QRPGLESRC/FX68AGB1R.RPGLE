     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE FX68AGB1R - RPT OF ACCTS WITH OUT OF SEQUENCE ENDORSEMENTS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM PRINTS THE CALENDAR EARNED/UNEARNED PREMIUM    *
     H*   REPORT FOR THE REQUESTED DATE RANGE.                        *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:57                                              *
     H* CREATED: 03/11/98               PROJECT: 68AGB                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10  C:249     Project: SY215                            *
     H*           C:189     Retrieve brand Name                       *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*****************************************************************
     F*
     FWXEUL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D XMSG01          C                   CONST('TWO ORIG BILLS      ')
     D XMSG02          C                   CONST('REINSTATE AFTER O/B ')
     D XMSG03          C                   CONST('O/B AFTER REVISION  ')
     D XMSG04          C                   CONST('REINSTATE AFTER REVI-
     D                                     SION')
     D XMSG05          C                   CONST('O/B AFTER AUDIT     ')
     D XMSG06          C                   CONST('CANCEL AFTER AUDIT  ')
     D XMSG07          C                   CONST('REVISION AFTER AUDIT')
     D XMSG08          C                   CONST('O/B AFTER CANCEL    ')
     D XMSG09          C                   CONST('REVISION AFTER CANCE-
     D                                     L')
     D XMSG10          C                   CONST('O/B AFTER REINSTATE ')
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    EUCO#         SVCO#
     C     *LIKE         DEFINE    EUFND         SVFND
     C     *LIKE         DEFINE    EUEMP#        SVEMP#
     C     *LIKE         DEFINE    EUDIV         SVDIV
     C     *LIKE         DEFINE    EUFYR         SVFYR
     C     *LIKE         DEFINE    EUSORT        XXSORT
     C     *LIKE         DEFINE    EUFAF         XXFAF
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQSEL             2
     C*
     I*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
     C                   SELECT
     C     QQSEL         WHENEQ    '  '
     C                   MOVE      *ON           *IN30
     C     QQSEL         WHENEQ    ' 1'
     C                   MOVE      *ON           *IN31
     C     QQSEL         WHENEQ    ' 2'
     C                   MOVE      *ON           *IN32
     C     QQSEL         WHENEQ    ' 3'
     C                   MOVE      *ON           *IN33
     C     QQSEL         WHENEQ    ' 4'
     C                   MOVE      *ON           *IN34
     C     QQSEL         WHENEQ    ' 5'
     C                   MOVE      *ON           *IN35
     C     QQSEL         WHENEQ    ' 6'
     C                   MOVE      *ON           *IN36
     C     QQSEL         WHENEQ    ' 7'
     C                   MOVE      *ON           *IN37
     C     QQSEL         WHENEQ    ' 8'
     C                   MOVE      *ON           *IN38
     C     QQSEL         WHENEQ    ' 9'
     C                   MOVE      *ON           *IN39
     C     QQSEL         WHENEQ    '10'
     C                   MOVE      *ON           *IN40
     C                   ENDSL
     C*
     C                   TIME                    DSDTTM
     C*
     C     'POSSIBLE'    CAT       'OUT OF':1    XXTTL            40
     C                   CAT       'SEQUEN':1    XXTTL
     C                   CAT       'CE TRA':0    XXTTL
     C                   CAT       'NS REP':0    XXTTL
     C                   CAT       'ORT':0       XXTTL
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   MOVE      *ON           *INOF
     C*
     C*  LOOP THROUGH EMPLOYER WORKFILE.
     C*
     C     *LOVAL        SETLL     PEU01
     C                   READ      PEU01                                  50
     C*
     C                   MOVE      EUCO#         SVCO#
     C                   MOVE      EUFND         SVFND
     C                   MOVE      EUEMP#        SVEMP#
     C                   MOVE      EUDIV         SVDIV
     C                   MOVE      EUFYR         SVFYR
     C                   MOVEL     *BLANKS       XXSORT
     C                   MOVEL     *BLANKS       XXFAF
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVE      *BLANKS       XXCMMT           40
     C*
     C*  PROCESS EMPLOYER/DIV/FYR BREAK
     C*
     C     EUCO#         IFNE      SVCO#
     C     EUFND         ORNE      SVFND
     C     EUEMP#        ORNE      SVEMP#
     C     EUDIV         ORNE      SVDIV
     C     EUFYR         ORNE      SVFYR
     C                   MOVE      EUCO#         SVCO#
     C                   MOVE      EUFND         SVFND
     C                   MOVE      EUEMP#        SVEMP#
     C                   MOVE      EUDIV         SVDIV
     C                   MOVE      EUFYR         SVFYR
     C                   MOVEL     *BLANKS       XXSORT
     C                   MOVEL     *BLANKS       XXFAF
     C*
     C                   ELSE
     C*
     C                   SELECT
     C*----------------------------------------------------
     C     XXSORT        WHENEQ    '2'                                          EMP HISTORY
     C     XXFAF         ANDEQ     'E'
     C*
     C     EUSORT        IFEQ      '2'                                          O/B
     C     EUFAF         ANDEQ     'E'
     C     *IN30         IFEQ      *ON
     C     *IN31         OREQ      *ON
     C                   MOVEL(P)  XMSG01        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C     EUSORT        IFEQ      '4'                                          REIN
     C     *IN30         IFEQ      *ON
     C     *IN32         OREQ      *ON
     C                   MOVEL(P)  XMSG02        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C*----------------------------------------------------
     C     XXSORT        WHENEQ    '2'                                          REVISIONS
     C     XXFAF         ANDNE     'R'
     C*
     C     EUSORT        IFEQ      '2'                                          O/B
     C     EUFAF         ANDEQ     'E'
     C     *IN30         IFEQ      *ON
     C     *IN33         OREQ      *ON
     C                   MOVEL(P)  XMSG03        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C     EUSORT        IFEQ      '4'                                          REIN
     C     *IN30         IFEQ      *ON
     C     *IN34         OREQ      *ON
     C                   MOVEL(P)  XMSG04        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C*----------------------------------------------------
     C     XXSORT        WHENEQ    '5'                                          AUDITS
     C*
     C     EUSORT        IFEQ      '2'                                          O/B
     C     EUFAF         ANDEQ     'E'
     C     *IN30         IFEQ      *ON
     C     *IN35         OREQ      *ON
     C                   MOVEL(P)  XMSG05        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C     EUSORT        IFEQ      '3'                                          CANCEL
     C     *IN30         IFEQ      *ON
     C     *IN36         OREQ      *ON
     C                   MOVEL(P)  XMSG06        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C     EUSORT        IFEQ      '2'                                          REVISION
     C     EUFAF         ANDNE     'E'
     C     *IN30         IFEQ      *ON
     C     *IN37         OREQ      *ON
     C                   MOVEL(P)  XMSG07        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C*----------------------------------------------------
     C     XXSORT        WHENEQ    '3'                                          CANCELLATIONS
     C*
     C     EUSORT        IFEQ      '2'                                          O/B
     C     EUFAF         ANDEQ     'E'
     C     *IN30         IFEQ      *ON
     C     *IN38         OREQ      *ON
     C                   MOVEL(P)  XMSG08        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C     EUSORT        IFEQ      '2'                                          REVISION
     C     EUFAF         ANDNE     'E'
     C     *IN30         IFEQ      *ON
     C     *IN39         OREQ      *ON
     C                   MOVEL(P)  XMSG09        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C*----------------------------------------------------
     C     XXSORT        WHENEQ    '4'                                          REINSTATES
     C*
     C     EUSORT        IFEQ      '2'                                          O/B
     C     EUFAF         ANDEQ     'E'
     C     *IN30         IFEQ      *ON
     C     *IN40         OREQ      *ON
     C                   MOVEL(P)  XMSG10        XXCMMT
     C                   ENDIF
     C                   ENDIF
     C*----------------------------------------------------
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C* IF AN ERROR OCCURRED, PRINT IT
     C*
     C     XXCMMT        IFNE      *BLANKS
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   MOVEL     EUSORT        XXSORT
     C                   MOVEL     EUFAF         XXFAF
     C*
     C* READ THE NEXT EMPLOYER DETAIL RECORD
     C*
     C     NEXTEU        TAG
     C                   READ      PEU01                                  50
     C                   ENDDO
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
     C**************************************************************
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTTL               86
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS, INC.'
     O***                                52 ', 1998'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           2 04
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTTL               96
     O                       PAGE          Z    132
     O                                          127 'PAGE:'
     O          E            HEAD           2
     O                                            4 'CO'
     O                                            9 'FND'
     O                                           20 'EMP'
     O                                           27 'DIV'
     O                                           32 'FYR'
     O                                           41 'COMMENT'
     O          E            DET         1  0
     O                       SVCO#                4
     O                       SVFND                9
     O                       SVEMP#              20
     O                       SVDIV               27
     O                       SVFYR               32
     O                       XXCMMT              74
