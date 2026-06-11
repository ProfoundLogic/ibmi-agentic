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
     C     SHKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
      *
      * READ ALL EMPLOYERS
      *
     C                   READ      PEM01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMFND         IFEQ      1
     C     EMFND         OREQ      5
     C     EMFND         OREQ      100
     C     EMFND         OREQ      105
     C     EMFND         OREQ      110
     C     EMFND         OREQ      115
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
     C                   Z-ADD     0             SHSBDT
     C                   MOVEL     Q1PGM         SHPGM
     C                   UPDATE    PSH01
     C                   ENDIF
      *
     C                   ENDIF                                                  EMCAD1 IFNE
      *
     C                   ENDIF                                                  EMFND IFEQ
      *
     C                   READ      PEM01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      *ON           *INLR
      *
