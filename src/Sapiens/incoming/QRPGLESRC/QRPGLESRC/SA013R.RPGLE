     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H/TITLE SA013R - API to validate user and retrieves various other
     H***************************************************************
     H*  Description:                                               *
     H*                                                             *
     H*  Validates user and retrieves user name and other info      *
     H*                                                             *
     H*  Called From:                                               *
     H*    Various programs                                         *
     H*                                                             *
     H*  Calls To:                                                  *
     H*                                                             *
     H*    None                                                     *
     H*                                                             *
     H*  Created:  06/01/01  C:062 Project: FS227                   *
     H***************************************************************
     H*  Revisions:                                                 *
     H*                                                             *
     H***************************************************************
     H*  Indicators:                                                *
     H*                                                             *
     H*    99 - General File Access                                 *
     H***************************************************************
     H* System User Preferences
     FSMUPP     IF   E             DISK
     FSMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@USER           10
     C                   PARM                    @@USNM           50
     C                   PARM                    @@LVL             3 0
     C                   PARM                    @@TYPE            1
     C*
     C*  Access User Preferences File
     C*
     C     @@RRN         IFEQ      *ZEROS
     C*
     C* Validate user
     C*
     C     @@USER        CHAIN     PUP02                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'F'           @@RTCD
     C                   ELSE
     C                   MOVEL(P)  UPUSNM        @@USNM
     C                   Z-ADD     UPLVL         @@LVL
     C                   MOVEL(P)  UPTYPE        @@TYPE
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ELSE
     C*
     C* Get the user id and user name
     C*
     C     @@RRN         CHAIN     PUP01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  UPUSR         @@USER
     C                   MOVEL(P)  UPUSNM        @@USNM
     C                   Z-ADD     UPLVL         @@LVL
     C                   MOVEL(P)  UPTYPE        @@TYPE
     C                   ELSE
     C                   MOVEL(P)  'F'           @@RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   RETURN
