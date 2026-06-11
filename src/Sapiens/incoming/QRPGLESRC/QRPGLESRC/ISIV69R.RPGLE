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
      *****************************************************************
      *
     FNONSISRF  IF   E           K DISK
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
     D  QQREF#                 1     20
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C* Key List for the NON-SIS File
     C*
     C     SISKY         KLIST
     C                   KFLD                    QQNSIS           20
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
     C                   MOVEL     QQKEY         Q1KEY
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
      * * *   INIT SUBROUTINE                                    I N I T ***
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
     C     EDTS01        BEGSR
     C*
     C* VALIDATE NON-SIS REFERENCE#
     C*
     C                   MOVEL     QQREF#        QQNSIS
     C     SISKY         CHAIN     NONSISR                            60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           QQRTCD
     C                   ELSE
     C                   MOVE      ' '           QQRTCD
     C                   END
     C*
     C                   ENDSR
     C*
