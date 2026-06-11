     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXWCPD- Fixes EAWCPD
     H*
     H*****************************************************************
     H*                                                               *
     H*  Called from: None                                            *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 06/15/2007 by c:62 for Project 139WGE             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEAP     UF   E             DISK
     FWDPAL8    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM      *BLANKS       SRDTO            10
     C                   PARM      *BLANKS       SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     PAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAWCPC
     C                   KFLD                    EAEOB#
     C*================================================================
     C* LIKE DEFINES
     C*================================================================
     C*================================================================
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   READ      PEA01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     EAIDT         IFGE      1070418
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAWCPD
     C                   MOVE      'X'           EAPGM
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C                   READ      PEA01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
