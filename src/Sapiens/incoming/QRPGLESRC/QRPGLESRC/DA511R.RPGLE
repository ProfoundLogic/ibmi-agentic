     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE DA511R - Dept Letters - EXECUTE INTERACTIVE OR BATCH.
      *****************************************************************
      *****************************************************************
      * REVISIONS:
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
      * INDICATOR USAGE
      * ALL/DIFFERENT SCREENS AND FUNCTIONS:
      *  60      GENERAL CHAIN: ALL
      *****************************************************************
      *
     FWMDOL     IF   E           K DISK
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
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C*  DEPT/DOC MASTER FILE
     C*
     C     DOK           KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
      **** QQKEY IS THE KEY DATA VALUE PASED IN FROM THE CALLING PROGRAM,
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDEPT            2
     C                   PARM                    QQDPCD            5
     C                   PARM                    QQFLDR           80
     C                   PARM                    QQDCID           12
     C                   PARM                    QQRTN             1
     C*
     C*
     C                   MOVE      *ZEROS        XXCO#             3 0
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   MOVEL     QQDEPT        XXDEPT            2
     C****                 MOVEL'CS'      XXDEPT  2
     C*
     C     DOK           CHAIN     PDO01                              55
     C*
     C     *IN55         IFEQ      *OFF
     C                   MOVEL     DOINTR        QQRTN
     C                   ELSE
     C*** No User Parm Record***
     C                   MOVE      '*'           QQRTN
     C                   GOTO      ENDIT
     C                   ENDIF
     C***
     C*
     C     ENDIT         TAG
     C*
     C                   RETURN
     C*
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
