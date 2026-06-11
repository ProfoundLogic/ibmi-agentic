     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
     H/TITLE DA510R - Dept Letters - Which Word processor to use .
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
     FSSYLF     IF   E           K DISK
     FSMUPL     IF   E           K DISK
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
     C*   KEY LIST FOR THE User Parm File
     C*
     C     UPK           KLIST
     C                   KFLD                    QQUSER           10
     C*
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
      **** QQKEY IS THE KEY DATA VALUE PASED IN FROM THE CALLING PROGRAM,
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUSER
     C                   PARM                    QQEDTR           10
     C                   PARM                    QQERR             1
     C*
     C*
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      *BLANKS       QQEDTR
     C*
     C*** IS DTM INSTALLED?
     C     'SY'          CHAIN     SSY01                              61
     C     *IN61         IFEQ      *OFF
     C     SYFL07        IFNE      'D'
     C*** NO DTM INSTALLED
     C                   MOVE      '3'           QQERR
     C                   GOTO      ENDIT
     C                   ENDIF
     C                   ELSE
     C*** NO SYS CTL RECORD
     C                   MOVE      '4'           QQERR
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C*
     C     UPK           CHAIN     PUP01                              55
     C*
     C     *IN55         IFEQ      *OFF
     C                   MOVEL     UPAL4         QQEDTR
     C                   ELSE
     C*** No User Parm Record***
     C                   MOVE      '1'           QQERR
     C                   GOTO      ENDIT
     C                   ENDIF
     C***
     C     QQEDTR        IFEQ      *BLANKS
     C*** CHECK SYS DEFAULT IF THERE IS NO USER DEFAULT
     C*
     C     'WC'          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     WCFL32        IFEQ      'W'
     C                   MOVEL     '*MSWORD'     QQEDTR
     C                   ELSE
     C                   MOVEL     '*WS    '     QQEDTR
     C                   ENDIF
     C*
     C                   ELSE
     C*** No SYS CTL RECORC  ***
     C                   MOVE      '2'           QQERR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ENDIT         TAG
     C*
     C                   RETURN
     C*
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
