     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA018R - API:  Complete DARTS Diary
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  Program will update DARTS Remark as "Completed".             *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 07/02/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/11 C:062 Support: 77091                                 *
     H*                Change to delete the diary. (TASB only)        *
     H*                                                               *
     H* 11/09/09 C:232 Support: 69840                                 *
     H*                Comment the code of project WC925, Do not set  *
     H*                status to I.                                   *
     H*                                                               *
     H* 08/02/99 C:58 Project: WC925                                  *
     H*               Add return code "D" process: delete DARTS record
     H*               by setting status to 'I'
     H*                                                               *
     H* 08/13/98 C:15 Project: WC820                                  *
     H*               Changed pgm to prevent the last re-occurring    *
     H*               comp pmt diary from being completed.            *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  40      File Access Status                                   *
     H*****************************************************************
      *  DARTS Remark file
     FSDCMLA    UF   E           K DISK
     F*** BEGIN ADD *** 08/13/98 **************************************
     FWMPCL     IF   E           K DISK
     F*** END ADD ***** 08/13/98 **************************************

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** BEGIN ADD *** 08/13/98 **************************************
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D*** END ADD ***** 08/13/98 **************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *________________________________________________________________
      *  DARTS Remark key
      *

     C     CMKY          KLIST
     C                   KFLD                    @@KEY
     C                   KFLD                    @@SEQ#
     C*
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*  PAYMENT CONTROL FILE WMPCL
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END ADD ***** 08/13/98 **************************************
      *
      *  Date Conversion to CYYMMDD
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     *LIKE         DEFINE    CMKEY         @@KEY
     C     *LIKE         DEFINE    CMSEQ#        @@SEQ#
      *
      *________________________________________________________________
     C     *ENTRY        PLIST
     C                   PARM                    @@KEY
     C                   PARM                    @@SEQ#
     C                   PARM                    @@RTCD            1
     C*** BEGIN ADD *** 08/05/11 **************************************
     C*
     C* Get client id
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*** END   ADD *** 08/05/11 **************************************
     C*
     C*  Access DARTS Remark record
     C*
     C     CMKY          CHAIN     PCM01                              40
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C*** BEGIN ADD *** 08/13/98 **************************************
     C*  Determine if this diary is for the last re-occurring comp pmt.
     C*  If so, do not complete the diary.
     C*
     C                   MOVE      *BLANKS       PCRCLP
     C     CMPNAM        IFEQ      'W0070R'
     C                   MOVEL     CMKEY         QCKEY
     C     PCKY          CHAIN     PPC01                              60
     C                   ENDIF
     C*
     C     CMRMCD        IFEQ      PCRCLP
     C     PCRCLP        ANDNE     *BLANKS
     C                   GOTO      TERM
     C                   ENDIF
     C*** END ADD ***** 08/13/98 **************************************
     C*** BEGIN ADD *** 08/02/99 **************************************
     C*
     C*** BEGIN DEL *** 11/09/09 **************************************
     C*    Set delete status.
     C****       @@RTCD    IFEQ 'D'
     C****                 MOVEL'I'       CMSTAT
     C****                 ENDIF
     C*** END   DEL *** 11/09/09 **************************************
     C*** END ADD ***** 08/02/99 **************************************
     C*** BEGIN ADD *** 08/05/11 **************************************
     C*    Set delete status.
     C     @@RTCD        IFEQ      'D'
     C     ##CLT#        ANDEQ     '025'
     C                   MOVEL     'I'           CMSTAT
     C                   ENDIF
     C*** END   ADD *** 08/05/11 **************************************
     C*
     C*  Update as "Completed"
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDCDT
     C*
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     CMDCDT        CMDT
     C                   MOVE      *BLANK        CMFL04
     C*******************  MOVELQ1USER    CMUSER
     C                   MOVE      *BLANK        @@RTCD
      *
     C                   UPDATE    PCM01
      *
     C                   ELSE
      *
      *  Record Not Found.  Return with Error.
      *
     C                   MOVE      '1'           @@RTCD
     C                   ENDIF
      *
     C*** BEGIN ADD *** 08/13/98 **************************************
     C     TERM          TAG
     C*** END ADD ***** 08/13/98 **************************************
     C                   MOVE      '1'           *INLR
