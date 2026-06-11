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
     FFAEOP     IF   E             DISK
     FFAEAL1    UF   E           K DISK
     FWDPALG    IF   E           K DISK

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
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     PAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    KKEOBS
     C*================================================================
     C* LIKE DEFINES
     C*================================================================
     C     *LIKE         DEFINE    PAEOBS        KKEOBS
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
     C     1             SETLL     PEO01
     C                   READ      PEO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C* 751156402
     C     EONTWK        IFEQ      'DCRX'
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     0             EAWCNC
     C                   Z-ADD     0             EAWCND
     C                   Z-ADD     0             EAWCPC
     C                   Z-ADD     0             EAWCPD
     C                   Z-ADD     EARCN         KKEOBS
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     EAFEIN        IFEQ      PAFEIN
     C     EAMOD         ANDEQ     PAMOD
     C                   Z-ADD     PACHK#        EAWCPC
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAWCPD
     C                   ENDIF
     C*
     C     PAFEIN        IFEQ      751156402
     C     PAMOD         ANDEQ     1
     C                   Z-ADD     PACHK#        EAWCNC
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAWCND
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  99
     C                   ENDDO
     C*
     C                   MOVE      'X'           EAPGM
     C                   UPDATE    PEA01
     C     EAKY          READE     PEA01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   READ      PEO01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
