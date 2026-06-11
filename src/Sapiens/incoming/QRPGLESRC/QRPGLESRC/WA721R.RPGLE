     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  WA721R - Update WMCMP/CMNRSE                                 *
     H*                                                               *
     H*  Called from: F0260R                                          *
     H*  Calls to: none                                               *
     H*                                                               *
     H*****************************************************************
     H* Created on 12/04/2006 by c:62 for Project 139MFW
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  03/12/08 C:62 PROJECT 62635                                  *
     H*                UPDATE CLAIM ALL THE TIME                      *
     H*                                                               *
     H*****************************************************************
     FWMCML1    UF   E           K DISK
     FWADTP     O    E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@NRSE            3
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C*
     C* Retrieve the Claim Record
     C*
     C     CMKY          CHAIN(N)  PCM01                              99
     C     *IN99         IFEQ      *OFF
     C     @@NRSE        IFNE      CMNRSE
     C                   EXSR      WRTDTP
     C*** BEGIN ADD *** 03/12/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/12/08 **************************************
     C*
     C     CMKY          CHAIN     PCM01                              99
     C                   MOVEL(P)  @@NRSE        CMNRSE
     C                   MOVEL(P)  Q1USER        CMUSER
     C                   MOVEL(P)  Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   UPDATE    PCM01
     C*** BEGIN DEL *** 03/12/08 **************************************
     C**********           ENDIF
     C*** END   DEL *** 03/12/08 **************************************
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*=====================================================
     C* WRTDTP - Write WADTP record
     C*=====================================================
     C     WRTDTP        BEGSR
     C*
     C                   CLEAR                   PDT01
     C*
     C                   Z-ADD     CMCO#         DTCO#
     C                   Z-ADD     CMFND         DTFND
     C                   Z-ADD     CMFYR         DTFYR
     C                   Z-ADD     CMCASE        DTCASE
     C                   MOVEL(P)  'N'           DTCTGY
     C                   MOVEL(P)  CMNRSE        DTOSTF
     C                   MOVEL(P)  @@NRSE        DTNSTF
     C*
     C                   MOVEL(P)  Q1USER        DTUSER
     C                   MOVEL(P)  Q1PGM         DTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTDT
     C                   Z-ADD     DSTIME        DTTIME
     C*
     C                   WRITE     PDT01
     C*
     C                   ENDSR
