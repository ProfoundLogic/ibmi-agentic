     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H* API - FA757R - DETERMINE IF PROCEDURE CODE HAS A NETWORK      *
     H*                CONTRACT SET UP                                *
     H*******************************************************************
     H*                                                               * *
     H*  Called From: F4000NTX                                        * *
     H*  Calls To:    None                                            * *
     H*                                                               * *
     H*******************************************************************
     H* Created on 04/21/2008 by c:198 for  Project TX040             * *
     H*******************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FFMS4L3    IF   E           K DISK
     FNDCOL2    IF   E           K DISK

     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@FEIN            9 0
     C                   PARM                    @@MOD             3 0
     C                   PARM                    @@NWCD            5
     C                   PARM                    @@FDAT            7 0
     C                   PARM                    @@PRCD            5
     C                   PARM                    @@PMOD            2
     C                   PARM                    @@RTCD            1
     C*================================================================
     C* Key lists
     C*================================================================
     C*
     C     S4KEY         KLIST
     C                   KFLD                    @@FEIN
     C                   KFLD                    @@MOD
     C                   KFLD                    @@NWCD
     C                   KFLD                    @@FDAT
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCODE            3
     C*
     C     COKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    @@PRCD
     C*================================================================
     C* Like Defines
     C*================================================================
     C*================================================================
     C* Main Processing
     C*================================================================
     C                   MOVE      *BLANK        MATCH             1
     C*
     C     S4KEY         SETGT     PS401
     C     NXTPHY        TAG
     C                   READP     PS401                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     S4NWCD        ANDEQ     @@NWCD
     C     S4FEIN        ANDEQ     @@FEIN
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     9999999       S4ENDT
     C                   MOVE      S4DISC        KKCODE
     C                   ENDIF
     C*
     C     @@FDAT        IFGT      S4ENDT
     C                   GOTO      NXTPHY
     C                   END
     C*
     C     S4STAT        IFEQ      'I'
     C*
     C                   GOTO      NXTPHY
     C*
     C                   ELSE
     C*
     C                   MOVE      'Y'           MATCH
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C***        NOW LET'S SEE IF THIS IS A CARVE OUT DISCOUNT CODE BY LOOKING
     C****       IN NDCOP
     C*
     C     MATCH         IFEQ      'Y'
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C***        NOW IF THIS IS A CARVE OUT LETS SEE IF THIS PROCEDURE CODE IS
     C***        IN THE TABLE
     C*
     C     *IN99         IFEQ      *OFF
     C     COKY1         CHAIN     PCO01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     @@PRCD        IFGE      CO10A1
     C     @@PRCD        ANDLE     CO10A2
     C*
     C                   MOVE      'Y'           MATCH
     C                   LEAVE
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        MATCH
     C*
     C                   ENDIF
     C*
     C     COKY          READE     PCO01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      MATCH         @@RTCD
     C                   RETURN
