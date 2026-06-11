     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXMYWRT - Write new records to WTMYP
     H*****************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*   Read new records in WTMYPWRT and write them for each group  *
     H*   into WTMYP.                                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/27/03  C:61 PROJECT: WC577                                 *
     H*                Revised to accept single fund as entry parm.   *
     H*                Changes not commented.                         *
     H*                                                               *
     H*****************************************************************
      *
      * New Endorsement Table records to be written
     FWTMYPWRT  IF   E             DISK
     F                                     RENAME(PMY01:PMYWRT)
     F                                     INFDS(DBWRT)
      *
      * Group master
     FWMFNL     IF   E           K DISK
      *
      * Endorsement Table
     FWTMYL     IF A E           K DISK
      *
      *
      *

     D DBWRT           DS
     D  WRTRRN               397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      *
      *
      *=================================================================
      * KLIST
      *=================================================================

     C     MYKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    KKSORT
      *
      *
      *
      *=================================================================
      * *LIKE DEFN
      *=================================================================
     C     *LIKE         DEFINE    MYSORT        KKSORT
     C     *LIKE         DEFINE    FNFND         XXFND
      *
      *
      *
      *=================================================================
      * PLIST
      *=================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@FND             3
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *
      *=================================================================
      * BEGIN
      *=================================================================
      *
      *  Process only one fund if entry parm
      *
     C     Q1PRMS        IFGE      1
     C                   MOVE      @@FND         XXFND
     C                   ELSE
     C                   MOVE      *ZERO         XXFND
     C                   ENDIF
      *
      *  For each new endorsement form
      *
     C                   READ      PMYWRT                                 60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  MYSORT        KKSORT
      *
      *  For each group
      *
     C     *LOVAL        SETLL     PFN01
     C                   READ      PFN01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     XXFND         IFEQ      *ZERO
     C     XXFND         OREQ      FNFND
      *
      *  If no endors form table record for group/form then wrt new rec
      *
     C     MYKY          CHAIN     PMY01                              60
      *
     C     *IN60         IFEQ      *ON
     C     WRTRRN        CHAIN     PMYWRT                             60
     C                   Z-ADD     FNCO#         MYCO#
     C                   Z-ADD     FNFND         MYFND
      *
     C                   MOVEL     Q1USER        MYUSER
     C                   MOVEL     Q1PGM         MYPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MYDT
      *
     C                   MOVEL     DSTIME        MYTIME
      *
     C                   WRITE     PMY01
     C                   ENDIF                                                  MYKY
      *
     C                   ENDIF                                                  XXFND
      *
     C                   READ      PFN01                                  60
     C                   ENDDO                                                  WMFNL
      *
     C                   READ      PMYWRT                                 60
     C                   ENDDO                                                  WTMYPWRT
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
