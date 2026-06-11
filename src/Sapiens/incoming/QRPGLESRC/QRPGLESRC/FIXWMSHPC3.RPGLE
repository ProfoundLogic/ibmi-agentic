     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWMSHPC2
     H*****************************************************************
     H*                                                               *
     H*   PROGRAM DESCRIPTION:                                        *
     H*     CHANGE SUBMISSIN HISTORY FILE WITH STAT OF '4', TYPE 'C'  *
     H*      FOR THE LATEST RECORD ON CURRENTLY CX'D EMPLOYERS TO     *
     H*      HAVE A STATUS OF '3', SO THEY WILL BE RE-SUBMITTED.      *
     H*                                                               *
     H*   CALLED FROM:                                                *
     H*     COMMAND ENTRY                                             *
     H*                                                               *
     H*   CALLS TO:                                                   *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:65                                            *
     H*   CREATED:    03/01/04    PROJECT: 114WHA                     *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWMSHL1    UF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C*
     C     SHKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
      *
      * READ ALL EMPLOYERS
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMCAD1        IFNE      *ZERO
      *
     C     SHKY          SETGT     PSH01
     C     SHKY          READPE    PSH01                                  60
     C     *IN60         DOWEQ     *OFF
     C     SHTYPE        ANDNE     'C'
     C     SHKY          READPE    PSH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '3'           SHSTAT
     C*
     C     SHST01        IFNE      ' '
     C                   MOVE      '3'           SHSS01
     C                   ENDIF
     C     SHST02        IFNE      ' '
     C                   MOVE      '3'           SHSS02
     C                   ENDIF
     C     SHST03        IFNE      ' '
     C                   MOVE      '3'           SHSS03
     C                   ENDIF
     C     SHST04        IFNE      ' '
     C                   MOVE      '3'           SHSS04
     C                   ENDIF
     C     SHST05        IFNE      ' '
     C                   MOVE      '3'           SHSS05
     C                   ENDIF
     C     SHST06        IFNE      ' '
     C                   MOVE      '3'           SHSS06
     C                   ENDIF
     C     SHST07        IFNE      ' '
     C                   MOVE      '3'           SHSS07
     C                   ENDIF
     C     SHST08        IFNE      ' '
     C                   MOVE      '3'           SHSS08
     C                   ENDIF
     C     SHST09        IFNE      ' '
     C                   MOVE      '3'           SHSS09
     C                   ENDIF
     C     SHST10        IFNE      ' '
     C                   MOVE      '3'           SHSS10
     C                   ENDIF
     C*
     C                   Z-ADD     0             SHSBDT
     C                   MOVEL     Q1PGM         SHPGM
     C                   UPDATE    PSH01
     C                   ENDIF
      *
     C                   ENDIF                                                  EMCAD1 IFNE
      *
     C     EMKY          READE     PEM01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      *ON           *INLR
      *
