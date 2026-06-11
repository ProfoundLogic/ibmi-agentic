     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      * SUMMIT Validation Program
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      *****************************************************************
      *
     FEMPLOY4L  IF   E           K DISK
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *

     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  Q1KEY                  1     40
     D  QQCO#                  1      3
     D  QQFND                  4      6
     D  QQFYR                  7      9
     D  QQCASE                10     16
     D*
     D*
     D                 DS
     D  SUMKEY                 1     13
     D  INSDNO                 1      4
     D  CALNYR                 5      7  0
     D  CASENO                 8     13  0
     D*
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C                   Z-ADD     0             C                 5 0
     C                   Z-ADD     0             C1                5 0
     C*
      *
      *  SUMMIT CLAIM FILE
      *
     C     CLMKY         KLIST
     C                   KFLD                    INSDNO
     C                   KFLD                    CALNYR
     C                   KFLD                    CASENO
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQRTCD            1
     C*
     C*
     C*
     C                   MOVEL     QQKEY         Q1KEY
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C******               TESTN          QQCO#      21
     C******     *IN21     IFEQ *ON
     C******               MOVE QQCO#     ##CO#   30
     C******               ELSE
     C******               MOVEL'1'       QQRTCD
     C******              GOTO TERM
     C******               ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQFND                21
     C     *IN21         IFEQ      *ON
     C                   MOVEL     *ZERO         INSDNO
     C                   MOVE      QQFND         INSDNO
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQFYR                21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQFYR         CALNYR
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQCASE               21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQCASE        CASENO
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C                   EXSR      INIT
     C*
     C*
     C                   EXSR      EDTS01                                        EDIT SCR01 FIELDS
     C*
     C*
      *------------------------*
     C     TERM          TAG
      *------------------------*
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *    NIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   ENDSR
     C*
      *====================================================================*
      * * *   EDTS01  SUBROUTINE - EDIT SCREEN 1 FIELDS    E D T S 0 1   ***
      *====================================================================*
     C*
     C*
     C     EDTS01        BEGSR
     C*
     C*
     C* VALIDATE EMPLOYER AND DIVISION
     C*
     C     CLMKY         CHAIN     EMPLOY4L                           21
     C     *IN21         IFEQ      *ON
     C                   MOVEL     '2'           QQRTCD
     C                   ELSE
     C                   MOVEL     ' '           QQRTCD
     C                   ENDIF
     C*
     C                   ENDSR
     C*
