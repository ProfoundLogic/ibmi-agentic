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
     FWMEML     IF   E           K DISK
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
     D  QQEMP#                 7     15
     D  QQDIV                 16     20
     D*
     D*
     D                 DS
     D  ##KEY                  1     40
     D  ##CO#                  1      3  0
     D  ##FND                  4      6  0
     D  ##EMP#                 7     15  0
     D  ##DIV                 16     20  0
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
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
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
     C                   TESTN                   QQEMP#               21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQEMP#        ##EMP#            9 0
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQDIV                21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQDIV         ##DIV             5 0
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
     C     EMKEY         CHAIN     PEM01                              21
     C     *IN21         IFEQ      *ON
     C                   MOVEL     '2'           QQRTCD
     C                   ELSE
     C                   MOVEL     ' '           QQRTCD
     C                   ENDIF
     C*
     C                   ENDSR
     C*
