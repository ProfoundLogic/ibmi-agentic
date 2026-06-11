     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     F* *****************************************************************
     F/TITLE FA207R - EDIT FOR A VALID CASE BASED ON EMPLOYER
     F* *****************************************************************
     F*                                                                 *
     F*  Created:  04/11/00  C:53  Project:  MC189                      *
     F* *****************************************************************
     F*                                                                 *
     F*  Description:                                                   *
     F*                                                                 *
     F*    VERIFY THAT NO CASES EXIST FOR AN EMPLOYER                   *
     F*                                                                 *
     F*      IF A RECORD IS FOUND IN FMCMPP FOR THE EMPLOYER THEN:      *
     F*         QQRTCD WILL = '1' OR *ON                                *
     F*             ELSE                                                *
     F*         QQRTCD WILL = '0' OR *OFF                               *
     F*                                                                 *
     F*                                                                 *
     F*  Called From:                                                   *
     F*    W0070R                                                       *
     F*    F0140R                                                       *
     F*                                                                 *
     F*  Calls To:                                                      *
     F*    None                                                         *
     F**************************************************************** **
     F*  Revisions:                                                     *
     F*                                                                 *
     F**************************************************************** **
     FFMCMLG    IF   E           K DISK

     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKY          KLIST
     C                   KFLD                    KYCO#
     C                   KFLD                    KYSUB#
     C                   KFLD                    KYFND
     C                   KFLD                    KYEMP#
     C                   KFLD                    KYDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C*
     C     *LIKE         DEFINE    CMCO#         KYCO#
     C     *LIKE         DEFINE    CMSUB#        KYSUB#
     C     *LIKE         DEFINE    CMFND         KYFND
     C     *LIKE         DEFINE    CMFYR         KYFYR
     C     *LIKE         DEFINE    CMEMP#        KYEMP#
     C     *LIKE         DEFINE    CMDIV         KYDIV
     C*
     C                   MOVE      QQCO#         KYCO#
     C                   MOVE      QQFND         KYFND
     C                   MOVE      QQSUB#        KYSUB#
     C                   MOVE      QQEMP#        KYEMP#
     C                   MOVE      QQDIV         KYDIV
     C*
     C*** CALL API FOR SUB OR GROUP XLATE IF CALLED FROM WCMS
     C*
     C     QQSUB#        IFEQ      *BLANKS
     C     QQSUB#        OREQ      *ZEROS
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##WCF#            3
     C                   PARM      *BLANKS       ##SUB#            5
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     ##RTCD        IFEQ      ' '
     C                   MOVE      ##SUB#        KYSUB#
     C                   MOVE      ##FND         KYFND
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF A RECORD IS FOUND TURN QQRTCD ON
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *ON           QQRTCD
     C                   ELSE
     C                   MOVE      *OFF          QQRTCD
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C                   RETURN
