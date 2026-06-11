     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE DAARMK - SEE IF AN ADDITONAL REMARK EXISTS FOR KEY/SEQ
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
     FSMARL     IF   E           K DISK
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
     C*   KEY LIST FOR THE KEY DEFINITION FILE
     C*
     C     ARK           KLIST
     C                   KFLD                    QQKEY            40
     C                   KFLD                    XXSEQ#            5 0
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
      **** QQKEY IS THE KEY DATA VALUE PASED IN FROM THE CALLING PROGRAM,
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ#            5
     C                   PARM                    QQRTCD            1
     C*
     C*
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   MOVE      QQSEQ#        XXSEQ#
     C*
     C     ARK           SETLL     PAR01                                  55
     C   60
     CANN60ARK           CHAIN     PAR01                              60
     C*
     C     *IN55         IFEQ      '1'
     C                   MOVE      'Y'           QQRTCD
     C                   ELSE
     C                   MOVE      *BLANKS       QQRTCD
     C                   ENDIF
     C*
     C                   RETURN
     C*
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
