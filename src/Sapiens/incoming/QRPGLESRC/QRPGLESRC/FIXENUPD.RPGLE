     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXENUPD - Write new records to WTENP
     H*****************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*   Read records in WTENPUPD to be update any records in WTENP  *
     H*   that have the same form name.                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/27/03  C:61 PROJECT: WC577                                 *
     H*                Revised to accept single fund as entry parm.   *
     H*                Changes not commented.                         *
      *
      * Endorsement Table records to be UPDATED
     FWTENPUPD  IF   E             DISK
     F                                     RENAME(PEN01:PENUPD)
     F                                     INFDS(DBUPD)
      *
      * Group master
     FWMFNL     IF   E           K DISK
      *
      * Endorsement Table
     FWTENL     UF   E           K DISK
      *
      *
      *

     D DBUPD           DS
     D  UPDRRN               397    400B 0
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

     C     ENKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    KKFORM
      *
      *
      *
      *=================================================================
      * *LIKE DEFN
      *=================================================================
     C     *LIKE         DEFINE    ENFORM        KKFORM
     C     *LIKE         DEFINE    ENFLDR        SVFLDR
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
      *  For each endorsement form to be updated
      *
     C                   READ      PENUPD                                 60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  ENFORM        KKFORM
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
      *  If endors form table record exists for group then UPDATE record.
      *
     C     ENKY          CHAIN     PEN01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ENFLDR        SVFLDR
     C     UPDRRN        CHAIN     PENUPD                             60
     C                   MOVEL(P)  SVFLDR        ENFLDR
     C                   Z-ADD     FNCO#         ENCO#
     C                   Z-ADD     FNFND         ENFND
      *
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
      *
     C                   UPDATE    PEN01
     C                   ENDIF                                                  ENKY
      *
     C                   ENDIF                                                  XXFND
      *
     C                   READ      PFN01                                  60
     C                   ENDDO                                                  WMFNL
      *
     C                   READ      PENUPD                                 60
     C                   ENDDO                                                  WTENPUPD
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
