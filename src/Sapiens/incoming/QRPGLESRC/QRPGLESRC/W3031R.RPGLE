     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE COMMISSION CHECK BATCH COMMUNICATION RECORD
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/02/97 C:30 Project:  WC500                                *
     H*           Last Log Date Remains 6 chars (MMDDYY).             *
     H*           Remove all conversion code.                         *
     H*                                                               *
     H*****************************************************************
     FWZD2P     UF   E             DISK

     C*
      *** BEGIN DEL *** 12/02/97 **************************************
     C*****      *LIKE     DEFN D2TDT     XXTDT
      *** END   DEL *** 12/02/97 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXTAG            10
     C                   PARM                    XXRTRT            1
     C                   PARM                    QQTDT             6
     C                   PARM                    XXTTM             6
     C                   PARM                    QQSLR             1
     C*
      *** BEGIN DEL *** 12/02/97 **************************************
     C***        DTCYMD    PLIST
     C***                  PARM           SRDTI   80
     C***                  PARM *BLANKS   SRFFMT  8
     C***                  PARM '*CYMD'   SRTFMT  8
     C***                  PARM           SRDTO  10
     C***                  PARM           SRERR   1
      *** END   DEL *** 12/02/97 **************************************
     C*
     C*  GET THE RECORD FOR UPDATE
     C*
     C     1             CHAIN     PD201                              60
     C     *IN60         IFEQ      '0'
     C*
      *** BEGIN DEL *** 12/02/97 **************************************
     C***                  MOVE QQTDT     X1TDT   60
     C***
     C***                  Z-ADDX1TDT     SRDTI
     C***                  CALL 'SRCVTDAT'DTCYMD
     C***                  MOVELSRDTO     XXTDT
     C***
     C***                  MOVE XXTDT     D2TDT
      *** END   DEL *** 12/02/97 **************************************
      *** BEGIN ADD *** 12/02/97 **************************************
     C                   MOVE      QQTDT         D2TDT
      *** END   ADD *** 12/02/97 **************************************
     C                   MOVE      XXTTM         D2TTM
     C                   MOVE      XXTAG         D2TAG
     C                   MOVE      ' '           D2RTRT
     C*
     C*  UPDATE THE RECORD
     C*
     C                   UPDATE    PD201
     C*
     C                   END
     C*
     C*  TEST FOR CLOSE OF FILES
     C*
     C     QQSLR         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   ELSE
     C                   RETURN
     C                   END
