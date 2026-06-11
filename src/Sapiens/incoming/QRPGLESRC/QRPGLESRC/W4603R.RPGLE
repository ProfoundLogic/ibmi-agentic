     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4603R - PRINT ROUTING PAGE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  02/13/96 C:49 PROJECT: 59WDJ                                 *
     H*                Changes to Routing page, which is used as cover*
     H*                sheet and mailing label for report packets.    *
     H*                  1. Add Recipient ID to current demographic   *
     H*                     information on page.                      *
     H*                  2. Repeat the address portion of demographic *
     H*                     information at the geographic center of   *
     H*                     the page to allow display through address *
     H*                     window on current envelope stock for mail.*
     H*                                                               *
     H*****************************************************************
     FWMUDL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FROUTEPG   O    F  132        PRINTER OFLIND(*INOF)
     F*****************************************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I****************************************************************

     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER           10
     C*
     C*   KEY LIST FOR THE USER DESTINATION FILE
     C*
     C     UDKEY         KLIST
     C                   KFLD                    XXUSER
     C*************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    U3DEST            1 0
     C                   PARM                    XXUSER           10
     C*************************************************************
     C* DEFINITIONS:
     C     *LIKE         DEFINE    UPOUTQ        QQOUTQ
     C     *LIKE         DEFINE    UDATTN        XDATTN
     C     *LIKE         DEFINE    UDCOMP        XDCOMP
     C     *LIKE         DEFINE    UDADR1        XDADR1
     C     *LIKE         DEFINE    UDADR2        XDADR2
     C     *LIKE         DEFINE    UDCTY         XDCTY
     C     *LIKE         DEFINE    UDST          XDST
     C     *LIKE         DEFINE    UDZIP1        XDZIP1
     C     *LIKE         DEFINE    UDZIP2        XDZIP2
      *  A field to hold the Recipient ID
     C     *LIKE         DEFINE    UDUSID        XDUSID
     C*
     C                   TIME                    DSDTTM
     C*
     I*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
     C                   MOVE      *BLANKS       XDATTN
     C                   MOVE      *BLANKS       XDCOMP
     C                   MOVE      *BLANKS       XDADR1
     C                   MOVE      *BLANKS       XDADR2
     C                   MOVE      *BLANKS       XDCTY
     C                   MOVE      *BLANKS       XDST
     C                   MOVE      *BLANKS       XDZIP1
     C                   MOVE      *BLANKS       XDZIP2
      *  Clear the Recipient ID field each time
     C                   MOVE      *BLANKS       XDUSID
     C*
     C     U3DEST        IFEQ      1
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      UPOUTQ        QQOUTQ
     C                   MOVEL     UPUSNM        XDATTN
     C                   MOVE      *BLANKS       XDCOMP
     C                   MOVE      *BLANKS       XDADR1
     C                   MOVE      *BLANKS       XDADR2
     C                   MOVE      *BLANKS       XDCTY
     C                   MOVE      *BLANKS       XDST
     C                   MOVE      *BLANKS       XDZIP1
     C                   MOVE      *BLANKS       XDZIP2
     C                   END
     C*
     C     QQOUTQ        IFEQ      *BLANKS
     C                   MOVEL     'QPRINT'      QQOUTQ
     C                   END
     C*
     C     U3DEST        IFEQ      3
     C     UDKEY         CHAIN     PUD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UDCOMP        XDCOMP
     C                   MOVEL     UDADR1        XDADR1
     C                   MOVEL     UDADR2        XDADR2
     C                   MOVEL     UDCTY         XDCTY
     C                   MOVE      UDST          XDST
     C                   MOVE      UDZIP1        XDZIP1
     C                   MOVE      UDZIP2        XDZIP2
     C                   MOVEL     UDATTN        XDATTN
     C*
     C     X1ATTN        IFEQ      *BLANKS
     C     X1COMP        ANDEQ     *BLANKS
     C     X1ADR1        ANDEQ     *BLANKS
     C                   MOVEL     UDCOMP        X1COMP           33
     C                   MOVEL     UDADR1        X1ADR1           33
     C                   MOVEL     UDADR2        X1ADR2           33
     C                   MOVEL     UDCTY         X1CTY            20
     C                   MOVE      UDST          X1ST              2
     C                   MOVE      UDZIP1        X1ZIP1            5
     C                   MOVE      UDZIP2        X1ZIP2            4
     C                   MOVEL     UDATTN        X1ATTN           33
     C                   ELSE
     C                   MOVEL     UDCOMP        X2COMP           33
     C                   MOVEL     UDADR1        X2ADR1           33
     C                   MOVEL     UDADR2        X2ADR2           33
     C                   MOVEL     UDCTY         X2CTY            20
     C                   MOVE      UDST          X2ST              2
     C                   MOVE      UDZIP1        X2ZIP1            5
     C                   MOVE      UDZIP2        X2ZIP2            4
     C                   MOVEL     UDATTN        X2ATTN           33
     C                   MOVE      *BLANKS       X1COMP           33
     C                   MOVE      *BLANKS       X1ADR1           33
     C                   MOVE      *BLANKS       X1ADR2           33
     C                   MOVE      *BLANKS       X1CTY            20
     C                   MOVE      *BLANKS       X1ST              2
     C                   MOVE      *BLANKS       X1ZIP1            5
     C                   MOVE      *BLANKS       X1ZIP2            4
     C                   MOVE      *BLANKS       X1ATTN           33
     C                   MOVE      *BLANKS       X2COMP           33
     C                   MOVE      *BLANKS       X2ADR1           33
     C                   MOVE      *BLANKS       X2ADR2           33
     C                   MOVE      *BLANKS       X2CTY            20
     C                   MOVE      *BLANKS       X2ST              2
     C                   MOVE      *BLANKS       X2ZIP1            5
     C                   MOVE      *BLANKS       X2ZIP2            4
     C                   MOVE      *BLANKS       X2ATTN           33
     C                   END
     C*
     C                   END
      *  Move the Recipient ID into the field we created
     C                   MOVEL     UDUSID        XDUSID
     C                   END
     C*
     C* WILL NEED CODE HERE FOR DESTINATIONS EQUAL TO FAX....
     C*
     C*
     C                   EXCEPT    ROUTE
     C*
      *** END OF PROGRAM
     C                   MOVE      '1'           *INLR
     C*
     C*
     /*/////////////////////////////////////////////////////////////////*/
     OROUTEPG   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
      *  Here we are moving the Address demographic information to the
      *  geographic center of the page to display it through the window
      *
      *
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             XDUSID              28
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       XDUSID              28
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           36 'REPORT ORDERING AND'
     O                                           56 'SORTING USER REPORT'
     O                                           64 'PACKAGE'
      * ADJUST FOLLOWING OUTPUT SPECS 15 POSITIONS LEFT FROM 100 TO 85
     O          E            ROUTE          1 32
     O                       XDATTN              85
     O          E            ROUTE          1
     O                       XDCOMP              85
     O          E            ROUTE          1
     O                       XDADR1              85
     O          E            ROUTE          1
     O                       XDADR2              85
     O          E            ROUTE          1
     O                       XDCTY               70
     O                       XDST                75
     O                       XDZIP1              82
     O                                           83 '-'
     O                       XDZIP2              87
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O****                               23 '(C) Copyright Progressi'
     O****                             +000 've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
