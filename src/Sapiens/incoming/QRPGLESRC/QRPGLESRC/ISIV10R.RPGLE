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
     FWMPDL     IF   E           K DISK
      *
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************
      *
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
     D  QQFEIN                 1      9
     D  QQMOD                 10     12
     D*
     D*
     D                 DS
     D  ##KEY                  1     40
     D  ##FEIN                 1      9  0
     D  ##MOD                 10     12  0
     D*
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*  PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    ##FEIN
     C                   KFLD                    ##MOD
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
      *------------------------*
     C*
     C                   MOVEL     QQKEY         Q1KEY
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQFEIN               21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQFEIN        ##FEIN            9 0
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*** TEST THE KEY FOR NUMERIC VALUES!
     C                   TESTN                   QQMOD                21
     C     *IN21         IFEQ      *ON
     C                   MOVE      QQMOD         ##MOD             3 0
     C                   ELSE
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
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
     C     PDKY          CHAIN     PPD01                              21
     C     *IN21         IFEQ      *ON
     C                   MOVEL     '2'           QQRTCD
     C                   ELSE
     C                   MOVEL     ' '           QQRTCD
     C                   ENDIF
     C*
     C                   ENDSR
     C*
