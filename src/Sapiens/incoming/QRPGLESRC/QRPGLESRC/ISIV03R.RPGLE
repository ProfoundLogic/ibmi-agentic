     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      *****************************************************************
      *
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:WCM02)
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
     D  ##KEY                  1     40
     D  ##CO#                  1      3  0
     D  ##FND                  4      6  0
     D  ##FYR                  7      9  0
     D  ##CASE                10     16  0
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
     C*
     C*  WC CLAIM FILE
     C*
     C     CLMKY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##FYR
     C                   KFLD                    ##CASE
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
     C                   TESTN                   QQCO#                21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQCO#         ##CO#             3 0
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQFND                21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQFND         ##FND             3 0
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQFYR                21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQFYR         ##FYR             3 0
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQCASE               21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQCASE        ##CASE            7 0
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
     C     CLMKY         CHAIN     WCM02                              21
     C     *IN21         IFEQ      *ON
     C                   MOVEL     '2'           QQRTCD
     C                   ELSE
     C                   MOVEL     ' '           QQRTCD
     C                   ENDIF
     C*
     C                   ENDSR
     C*
