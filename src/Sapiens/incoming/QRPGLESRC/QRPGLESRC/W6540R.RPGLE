     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *TITLE W6540R - RETURN PREMIUM CHECK PRINTING DRIVER
      *****************************************************************
      *  CALLED FROM: W6530C (RETURN PREM CHECK PRINT BATCH)          *
      *                                                               *
      *  CALLS TO   : W6550C (RETURN PREM CHECK PRINT OVERRIDE)       *
      *                                                               *
      *  01/30/04 C:136 PROJECT: WC1033                               *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *****************************************************************
      *  Check Schedule File
     FWAC6L     UF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *LIKE         DEFINE    C6CO#         KKCO#
     C     *LIKE         DEFINE    C6DATE        KKPRDT
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *   KEY LIST FOR THE CHECK PROCESSING FILE
      *
     C     C6KEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRDT
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQQUE            10
      *
     C                   MOVE      QQCO#         KKCO#
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPRDT
      *
      *   START READING THE CHECK PROCESSING FILE
      *
     C     C6KEY         CHAIN     PC601                              60
     C     *IN60         DOWEQ     *OFF
      *
      *   MAKE SURE PROCESSING FLAGS AREN'T FLIPPED
      *
     C     C6STA0        IFEQ      *BLANK
     C     C6STA1        ANDEQ     *BLANK
      *
      *   CALL THE CL TO APPLY THE OVERRIDE
      *
     C                   MOVE(P)   C6FND         QQFND
      *
     C                   CALL      'W6550C'
     C                   PARM                    QQCO#
     C                   PARM                    QQDATE
     C                   PARM                    QQFND             3
     C                   PARM      C6CKTP        QQCKTP           10
     C                   PARM                    QQQUE
      *
     C                   MOVE      'Y'           C6STA0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C6DATE
     C                   Z-ADD     DSTIME        C6TIME
     C                   UPDATE    PC601
      *
     C                   ENDIF
      *
     C     C6KEY         READE     PC601                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
