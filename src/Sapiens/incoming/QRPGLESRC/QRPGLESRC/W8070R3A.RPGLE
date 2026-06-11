     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO CREATE IP LOG RECORDS BASED ON DATES
     H*****************************************************************
     H* Description:  This pgm calls W8070R3 with the date passed     *
     H*   in and the 6 previous dates prior to insure all IP's are    *
     H*   created                                                     *
     H*                                                               *
     H* SUPPORT ITEM #: 52787                                         *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/01/09 C:136 PROJECT 139WMB                                 *
     H*                CHANGE HOW 139WMB CHANGES THE DATE SO THAT     *
     H*                IT DOESN'T PASS BACK A DIFFERENT BATCH DATE    *
     H*                FOR CLIENT 139                                 *
     H*                                                               *
     H* 08/17/2009 C:62 PROJECT 139WMB                                *
     H*                 CHANGE HOW THE DATE IS PASSED.                *
     H*                                                               *
     H*****************************************************************

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDTP             8
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           *INOF
     C*** BEGIN ADD *** 08/17/09 **************************************
     C*
     C* RETIEVE CLIENT ID
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    ##CLT#            3
     C*
     C     ##CLT#        IFEQ      '139'
      *** BEG CHG ***** 10/01/09 **************************************
     C***********          MOVE DSDATE    QQDTP
     C                   MOVE      DSDATE        XXDTP             8
      *** END CHG ***** 10/01/09 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/17/09 **************************************
     C*
     C*** RUN FOR 1ST DT
      *** BEG CHG ***** 10/01/09 **************************************
     C*****************    MOVE QQDTP     QQDATE
     C                   SELECT
     C     ##CLT#        WHENEQ    '139'
     C                   MOVE      XXDTP         QQDATE
     C                   OTHER
     C                   MOVE      QQDTP         QQDATE
     C                   ENDSL
      *** END CHG ***** 10/01/09 **************************************
     C                   EXSR      SRCALL
     C*
     C                   Z-ADD     1             X                 4 0
     C*
     C     X             DOWLE     6
     C*
      *** BEG CHG ***** 10/01/09 **************************************
     C*****************    MOVE QQDTP     QQDT
     C                   SELECT
     C     ##CLT#        WHENEQ    '139'
     C                   MOVE      XXDTP         QQDT
     C                   OTHER
     C                   MOVE      QQDTP         QQDT
     C                   ENDSL
      *** END CHG ***** 10/01/09 **************************************
     C*
     C                   CALL      'SRPAST8'
     C                   PARM                    QQDT              8 0
     C                   PARM      X             QQFUTD            5 0
     C                   PARM                    QQERR             1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          QQDATE            8
     C                   EXSR      SRCALL
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
     C******************
     C*
     C     SRCALL        BEGSR
     C                   CALL      'W8070R3'
     C                   PARM                    QQCO#
     C                   PARM                    QQDATE            8
     C                   ENDSR
