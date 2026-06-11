     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      ***************************************************************
     H/TITLE SA012R - API to retrieve output queue from SMUPL
      ***************************************************************
      *  Description:                                               *
      *                                                             *
      *  Retrieves output queue from the User Preferences File.     *
      *  If user is not enrolled or if output queue is blank        *
      *  the program will return QPRINT as the output queue.        *
      *                                                             *
      *  Called From:                                               *
      *    Various programs                                         *
      *                                                             *
      *  Calls To:                                                  *
      *                                                             *
      *    None                                                     *
      *                                                             *
      *  Created:  01/18/01  C:128 Project: DR102 Seq# 25930        *
      *                                                             *
      ***************************************************************
      *  Revisions:                                                 *
      *                                                             *
      ***************************************************************
      *  Indicators:                                                *
      *                                                             *
      *    60 - General File Access                                 *
      ***************************************************************
      * System User Preferences
     FSMUPL     IF   E           K DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *

     C     *ENTRY        PLIST
     C                   PARM                    QQUSER           10
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQRTCD            1
      *
      *
      *  Access User Preferences File
      *
     C     QQUSER        CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPOUTQ        QQOUTQ
     C                   ELSE
     C                   MOVE      '1'           QQRTCD
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     QQOUTQ        IFEQ      *BLANK
     C                   MOVEL     'QPRINT'      QQOUTQ
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
