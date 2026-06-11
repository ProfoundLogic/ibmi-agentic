     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *                                                                 *
      *============ SCIMGSR ======== SCAN AN IMAGE DOCUMENT-PROMPT =====*
      *                                                                 *
      *****************************************************************
      *
      *
     H/TITLE SCIMGSR - SCAN AN IMAGE DOCUMENT-PROMPT
      *
      *
      *****************************************************************
      * REVISIONS:
      * XX/XX/XX NNN    - @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL USE INDICATOR                                      PGM
      *
      *****************************************************************
      *
     H/EJECT
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      *
     FNXTDOC    UF A E             DISK
     F*
     F/EJECT
      *****************************************************************
      * EXTENTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D/EJECT
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  NXTIMG                 1     12
     D  PCCNST                 1      2
     D  IMG#                   3      8  0
     D  DOT                    9      9
     D  DOTIMG                10     12
      *
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I/EJECT
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*** DEFINE FEILDS
     C*          *LIKE     DEFN AAAAAA    BBBBBB
     C*
     C/EJECT
     C*** KEY LISTS
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C*********  SYKY      KLIST
     C***************      KFLD           XXSYK   2
     C*
     C*
     C/EJECT
      *****************************************************************
      *  MAINLINE CODE  *
      *****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQDESC           40
     C                   PARM                    QQDUSR           10
     C                   PARM                    QQKEY            40
     C                   PARM                    QQIQUE           10
     C                   PARM                    QQAPID           10
     C                   PARM                    QQIPTH           10
     C                   PARM                    QQPIDX            5 0
     C                   PARM                    QQQPTY            1
     C                   PARM                    QQDCTP            5
     C                   PARM                    QQRTCD            6
     C                   PARM                    QQ#PGS            3 0
     C                   PARM                    QQDCSZ            7 0
     C*
     C                   MOVEL     'PC'          PCCNST
     C                   MOVEL     '.'           DOT
     C                   MOVEL     'IMG'         DOTIMG
     C*
     C                   TIME                    DSDTTM
     C*
     C     STRT          TAG
     C*
     C**********************************************************************
     C*************************************************************************
     C*
     C****
     C     1             CHAIN     PNX01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     NXIMG#        IMG#
     C                   ADD       1             NXIMG#
     C                   UPDATE    PNX01
     C                   ELSE
     C                   Z-ADD     1             IMG#
     C                   Z-ADD     1             NXIMG#
     C                   WRITE     PNX01
     C                   END
     C*
     C*
     C                   MOVEL     'PC'          QQAPID
     C                   MOVEL     NXTIMG        QQDCID
     C*
     C* * * IF THE IMAGE IS SCANNED THE QQRTCD VALUE IS '     0'OR '      '
     C* * * PERFORM ANY DARTS DATABASE UPDATES ETC. HERE   BEFORE
     C*** EXITING TO CALLER.
     C*
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
